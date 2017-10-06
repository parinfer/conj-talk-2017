(ns pres.editors.sedit
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse->cam] :as camera]
    [pres.state :refer [state]]
    [pres.codebox :as codebox]
    [pres.reader :refer [descendant? parent common-ancestor]]
    [pres.pprint :refer [pprint]]
    [pres.examples :as examples]
    [clojure.string :as string]
    [oops.core :refer [ocall oget oset!]]))

; might be first structure editor with a story for inline editing
; new mouse language w/ interesting cursor states

; reminds me of gimp/blender.  (the UX makes the most sense to programmers)

;;----------------------------------------------------------------------
;; Codeboxes
;;----------------------------------------------------------------------

(def font-size 12)

(def box
   (codebox/make examples/medium-func
     {:xy [640 50] :font-size font-size}))

;;----------------------------------------------------------------------
;; State
;;----------------------------------------------------------------------

(def state-key :sedit)

(def init-state
  {; one selection for each mode
   :selections {} ; one or two paths
   :cursor nil
   :pending-selection nil ; one path
   :mode nil ; primary, copy
   :mousedown nil}) ; left, right, middle

(defn get-state
  ([] (get @state state-key))
  ([k] (get-in @state [state-key k])))

(defn set-state! [s]
  (swap! state assoc state-key s))

;;----------------------------------------------------------------------
;; Selection normalizing
;;----------------------------------------------------------------------

(defn space-path? [path]
  (let [i (last path)]
    (not= i (Math/floor i))))

(defn char-path? [path]
  (when-let [parent (codebox/lookup box (parent path))]
    (:text parent)))

(defn char-path->xy [path]
  (when-let [parent (codebox/lookup box (parent path))]
    (let [[x y] (:xy parent)
          x1 (+ x (last path))]
      [x1 y])))

; create pseudo-node {xy xy-end} for outlining a sequence of nodes
; precondition: head and anchor are sorted
(defn selection->node [[a b]]
  (if-not b
    (when-not (space-path? a)
      (select-keys (codebox/lookup box a) [:xy :xy-end]))
    (if (char-path? a)
      {:xy (char-path->xy a)
       :xy-end (char-path->xy b)
       :range? true}
      {:xy (:xy (codebox/lookup box a))
       :xy-end (:xy-end (codebox/lookup box b))
       :range? true})))

(defn level-paths [paths]
  (let [n (count (apply common-ancestor (take 2 paths)))]
    (->> paths
         (map #(take (inc n) %))
         (sort-by last))))

(defn normalize-selection [[a b & others]]
  (let [paths (reduce
                #(level-paths (cons %2 %1))
                (level-paths [a b])
                others)]
    (if (< (count paths) 2)
      (vec paths)
      (let [[a b] [(first paths) (last paths)]
            a (update a (dec (count a)) Math/ceil)
            b (update b (dec (count a)) Math/floor)]
        [a b]))))

(defn normalized-selection [mode]
  (let [{:keys [selections pending-selection]} (get-state)]
    (when-let [sel (seq (selections mode))]
      (normalize-selection
        (cond-> sel
          (and (= mode (get-state :mode)) pending-selection)
          (conj pending-selection))))))

;;----------------------------------------------------------------------
;; Draw Editor
;;----------------------------------------------------------------------

(defn draw-selection [mode]
  (when-let [sel (normalized-selection mode)]
    (case mode
      :copy
      (do
        (ocall ctx "save")
        (ocall ctx "translate" 0 2)
        (ocall ctx "setLineDash" #js[3 1])
        (oset! ctx "strokeStyle" "#888")
        (codebox/draw-underline box sel)
        (ocall ctx "restore"))

      :primary
      (do
        (oset! ctx "strokeStyle" "#333")
        (if (:range? sel)
          (codebox/draw-bounding-box box sel)
          (ocall ctx "stroke")
          (codebox/draw-underline box sel))))))

(defn draw-cursor []
  (let [{:keys [cursor mousedown]} (get-state)]
    (when cursor
      (if mousedown
        nil ; TODO draw cursor pipe
        nil)))) ; TODO: draw cursor arrow

(defn draw-editor [box]
  (oset! ctx "fillStyle" "#333")
  (codebox/draw box)
  (draw-selection :primary)
  (draw-selection :copy)
  (draw-cursor))

;;----------------------------------------------------------------------
;; Draw all
;;----------------------------------------------------------------------

(defn draw []
  (draw-editor box))

;;----------------------------------------------------------------------
;; Picker
;;----------------------------------------------------------------------

; NOTE: a cursor is just a path
; inside text: path to text + char index (cursor to left of index)
; inside list: path to list + fractional index (same as space path)

; NOTE: a selection is 1 or 2 paths
; edit selection: text path + char index
; structure selection: list path
; space selection: normal path +/- 0.5

(defn pick-node [box [x y]]
  (or (->> (codebox/pick-nodes box [x y] :text)
           (sort-by #(count (:path %)))
           last)
      (codebox/pick-space box [x y])))

; ( foo )
;  ^   ^ ^
;  |   | | next outer
;  |   |
;  |   | last inner
;  |
;  | first inner
(defn push-to-space [node box [x y]]
  (let [char-xy (codebox/char-coord-at box [x y])
        cursor-xy (codebox/cursor-coord-at box [x y])
        path (:path node)]
    (let [opener? (= (:xy node) char-xy)
          cursor-left? (= char-xy cursor-xy)
          first-inner (conj path -0.5)
          last-inner (conj path (+ (count (:children node)) -0.5))
          next-outer (update path (dec (count path)) + 0.5)]
      {:path (cond opener? first-inner, cursor-left? last-inner, :else next-outer)
       :space? true})))

(defn bordering-paths [path]
  [(update path (dec (count path)) - 0.5)
   (update path (dec (count path)) + 0.5)])

(defn region-side [xy xy-end [mx my]]
  (let [[x0 y0] xy
        [x1 y1] xy-end
        x1 (inc x1)
        [x y] (codebox/cam->code-frac box (codebox/rel-cam box [mx my]))
        y (Math/floor y)]
    (when-not (= [x0 y0] [x1 y1])
      (if (= y0 y1)
        ([:left :right] (Math/round (/ (- x x0) (- x1 x0))))
        ({y0 :left y1 :right} y)))))

(defn structure-cursor
  [{:keys [path] :as node} [x y]]
  (cond
    (:space? node) path

    (:text node)
    (let [side (region-side (:xy node) (:xy-end node) [x y])
          dx ({:left -0.5 :right 0.5} side)]
      (update path (dec (count path)) + dx))

    (:paren node)
    (let [char-xy (codebox/char-coord-at box [x y])
          cursor-xy (codebox/cursor-coord-at box [x y])
          left? (= char-xy cursor-xy)
          right? (not left?)
          open? (= (:xy node) char-xy)
          close? (not at-open?)]
      (cond
        (and left? open?) (update path (dec (count path)) - 0.5)
        (and right? close?) (update path (dec (count path)) + 0.5)
        (and right? open?) (conj path -0.5)
        (and left? close?) (conj path (- (count (:children node)) 0.5))
        :else nil))))

(defn force-structure-cursor? [space-node [x y]]
  ; ( | (    => structure
  ; ( | a
  ; ( | )    => structure
  ; -----
  ; a |>(    => structure
  ; a | a
  ; a | )
  ; -----
  ; ) | (    => structure
  ; )<| a    => structure
  ; ) | )    => structure
  (let [[left right] (map #(codebox/lookup box %) (bordering-paths (:path space-node)))
        lo? (nil? left) ; left open (
        rc? (nil? right) ; right close )
        lc? (:paren left) ; left close )
        ro? (:paren right) ; right open (
        ra? (:text right) ; right atom
        la? (:text left) ; left atom
        side (region-side (:xy space-node) (:xy-end space-node) xy)]
    (or (and lo? ro?)
        (and lo? rc?)
        (and la? ro? (= :right side))
        (and lc? ro?)
        (and lc? ro?)
        (and lc? ra?) (= :left side)
        (and lc? rc?))))

(defn edit-cursor [node [x y]]
  (let [[cx cy] (codebox/cursor-coord-at box [x y])]
    (cond
      (:text node)
      (conj (:path node) (- cx (first (:xy node))))

      (:space? node)
      (if (force-structure-cursor? node [x y])
        (structure-cursor node [x y])
        (let [[left right] (map #(codebox/lookup box %) (bordering-paths (:path node)))
              side (if (and (:text left) (:text right))
                     (region-side (:xy node) (:xy-end node) [x y])
                     (if (:text left) :left :right))]
          (case side
            :left (conj (:path left) (count (:text node)))
            :right (conj (:path right) 0)))))))

(defn pick-edit [box [x y]]
  (when-let [node (pick-node box [x y])]
    (let [node (cond
                 (:paren node) (push-to-space node box [x y]) node
                 (:text node)
                 (let [[cx cy] (codebox/char-coord-at box [x y])]
                   {:path (conj (:path node) (- cx (first (:xy node))))
                    :char? true}))
          cursor (edit-cursor node [x y])]
      {:node node
       :cursor cursor})))

(defn pick-structure [box [x y]]
  (when-let [node (pick-node box [x y])]
    {:node node
     :cursor (structure-cursor node [x y])}))

;;----------------------------------------------------------------------
;; Mouse
;;----------------------------------------------------------------------

(defn mouse-down [[x y] button]
  (let [{:keys [cursor node]} (case button
                                :left (pick-edit box [x y])
                                :right (pick-edit box [x y])
                                :middle (pick-structure box [x y])
                                nil)
        mode (get-state :mode)]
    (cond
      (#{:left :middle} button)
      (set-state!
        (cond-> (get-state)
          true (assoc-in [:selections mode] node)
          (= mode :primary) (assoc :cursor cursor)))

      (= :right button)
      (when (seq (get-state :selections))
        (set-state!
          (-> (get-state)
              (assoc :pending-selection node
                     :cursor nil)))))))

(defn click-info [e]
  {:xy (mouse->cam e)
   :button ([:left :middle :right] (oget e "button"))})

(defn on-mouse-down [e]
  (let [{:keys [xy button]} (click-info e)]
    (mouse-down xy button)))

(defn on-mouse-move [e]
  (when-let [button (get-state :mousedown)]
    (let [{:keys [xy]} (click-info e)]
      (mouse-down xy button))))

(defn on-mouse-up [e]
  (let [sel (normalized-selection (get-state :mode))]
    (set-state!
      (-> (get-state)
          (assoc :mousedown nil
                 :selections sel
                 :pending-selection nil)))))

(defn on-key-down [e]
  (when (= "Shift" (oget e "key"))
    (set-state! (assoc (get-state) :mode :copy))))

(defn on-key-up [e]
  (when (= "Shift" (oget e "key"))
    (set-state! (assoc (get-state) :mode :primary))))

(defn on-context-menu [e]
  (ocall e "preventDefault"))

;;----------------------------------------------------------------------
;; Load
;;----------------------------------------------------------------------

(defn init! []
  (set-state! init-state)
  (ocall js/window "addEventListener" "mousedown" on-mouse-down)
  (ocall js/window "addEventListener" "mouseup" on-mouse-up)
  (ocall js/window "addEventListener" "keydown" on-key-down)
  (ocall js/window "addEventListener" "keyup" on-key-up)
  (ocall js/window "addEventListener" "mousemove" on-mouse-move)
  (ocall js/window "addEventListener" "contextmenu" on-context-menu))

(defn cleanup! []
  (ocall js/window "removeEventListener" "mousedown" on-mouse-down)
  (ocall js/window "removeEventListener" "mouseup" on-mouse-up)
  (ocall js/window "removeEventListener" "keydown" on-key-down)
  (ocall js/window "removeEventListener" "keyup" on-key-up)
  (ocall js/window "removeEventListener" "mousemove" on-mouse-move)
  (ocall js/window "removeEventListener" "contextmenu" on-context-menu))
