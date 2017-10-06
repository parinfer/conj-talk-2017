(ns pres.editors.sedit
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse->cam] :as camera]
    [pres.state :refer [state]]
    [pres.codebox :as codebox]
    [pres.reader :refer [descendant? parent common-ancestor]]
    [pres.examples :as examples]
    [clojure.string :as string]
    [oops.core :refer [ocall oget oset!]]
    [clojure.pprint :refer [pprint]]))

; might be first structure editor with a story for inline editing
; new mouse language w/ interesting cursor states

; reminds me of gimp/blender.  (the UX makes the most sense to programmers)

;;----------------------------------------------------------------------
;; Codeboxes
;;----------------------------------------------------------------------

(def font-size 20)

(def box
   (codebox/make examples/medium-func
     {:xy [300 100] :font-size font-size}))

;;----------------------------------------------------------------------
;; State
;;----------------------------------------------------------------------

(def state-key :sedit)

(def init-state
  {; one selection for each mode
   :selections {} ; one or two paths
   :cursor nil
   :pending-selection nil ; one path
   :mode :primary ; primary, copy
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
  (when-let [node (codebox/lookup box (parent path))]
    (:text node)))

(defn char-path->xy [path]
  (when-let [node (codebox/lookup box (parent path))]
    (let [[x y] (:xy node)
          x1 (+ x (last path))]
      [x1 y])))

; create pseudo-node {xy xy-end} for outlining a sequence of nodes
; precondition: head and anchor are sorted
(defn selection->node [[a b]]
  (if-not b
    (cond
      (space-path? a) nil ; space selections are not drawn
      (char-path? a) (let [xy (char-path->xy a)]
                       {:xy xy :xy-end xy})
      :else (select-keys (codebox/lookup box a) [:xy :xy-end]))
    (if (char-path? a)
      {:xy (char-path->xy a)
       :xy-end (char-path->xy b)
       :range? true}
      {:xy (:xy (codebox/lookup box a))
       :xy-end (:xy-end (codebox/lookup box b))
       :range? true})))

(defn level-paths [paths]
  (let [n (apply min
            (inc (count (apply common-ancestor paths)))
            (map count paths))]
    (->> paths
         (map #(vec (take n %)))
         (sort-by last))))

(defn normalize-selection [[a b & others]]
  (if-not b
    [a]
    (let [paths (reduce
                  #(level-paths (cons %2 %1))
                  (level-paths [a b])
                  others)
          [a b] [(first paths) (last paths)]
          a (update a (dec (count a)) Math/ceil)
          b (update b (dec (count a)) Math/floor)]
      [a b])))

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
    (when-let [node (selection->node sel)]
      (case mode
        :copy
        (do
          (ocall ctx "save")
          (ocall ctx "translate" 0 2)
          (ocall ctx "setLineDash" #js[3 1])
          (oset! ctx "strokeStyle" "#888")
          (codebox/draw-underline box node)
          (ocall ctx "restore"))

        :primary
        (do
          (oset! ctx "strokeStyle" "#333")
          (if (:range? node)
            (do
              (codebox/draw-bounding-box box node)
              (ocall ctx "stroke"))
            (codebox/draw-underline box node)))))))

(defn cursor->char-xy [path]
  (if (char-path? path)
    (let [[x y] (:xy (codebox/lookup box (parent path)))]
      [(+ x (last path)) y])

    (let [right (codebox/lookup box path)
          left (codebox/lookup box (update path (dec (count path)) dec))
          parent-node (codebox/lookup box (parent path))]
      (cond
        (and left right) (codebox/add-x (:xy-end left) 1.6)
        (nil? left) (codebox/add-x (:xy parent-node) 1)
        (nil? right) (:xy-end parent-node)))))

(defn draw-cursor []
  (let [{:keys [cursor mousedown mode]} (get-state)]
    (when cursor
      (oset! ctx "strokeStyle" "#333")
      (oset! ctx "fillStyle" "#333")
      (let [[x y] (cursor->char-xy cursor)]
        (if (and (= mode :primary) mousedown)
          (do
            (codebox/draw-cursor box [x y])
            (ocall ctx "stroke"))
          (do
            ; TODO: blink
            (codebox/draw-cursor-arrow box [x y])
            (ocall ctx "stroke")
            (when-not (char-path? cursor)
              (ocall ctx "fill"))))))))

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
  (pprint (get-state))
  (draw-editor box))

;;----------------------------------------------------------------------
;; Picker
;;----------------------------------------------------------------------

; NOTE: a cursor is just a path (index means cursor is to left of it)
; inside text: path to text + char index
; inside list: path to list

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
        opener? (= (:xy node) char-xy)
        cursor-left? (= char-xy cursor-xy)
        path (:path node)
        path
        (cond
          opener? (conj path -0.5)
          cursor-left? (conj path (- (count (:children node)) 0.5))
          :else (update path (dec (count path)) + 0.5))]
    (codebox/space-node box path)))

(defn paths-around-space [path]
  [(update path (dec (count path)) - 0.5)
   (update path (dec (count path)) + 0.5)])

(defn region-side [xy xy-end [mx my]]
  (let [[x0 y0] xy
        [x1 y1] xy-end
        x1 (inc x1)
        [x y] (codebox/cam->code-frac (codebox/rel-cam box [mx my]))
        y (Math/floor y)]
    (when-not (= [x0 y0] [x1 y1])
      (if (= y0 y1)
        ([:left :right] (Math/round (/ (- x x0) (- x1 x0))))
        ({y0 :left y1 :right} y)))))

(defn structure-cursor
  [{:keys [path] :as node} [x y]]
  (cond
    (:space? node)
    (update path (dec (count path)) Math/ceil)

    (:text node)
    (let [side (region-side (:xy node) (:xy-end node) [x y])]
      (if (= :left side)
        path
        (update path (dec (count path)) inc)))

    (:paren node)
    (let [char-xy (codebox/char-coord-at box [x y])
          cursor-xy (codebox/cursor-coord-at box [x y])
          left? (= char-xy cursor-xy)
          right? (not left?)
          open? (= (:xy node) char-xy)
          close? (not open?)]
      (cond
        (and left? open?) path
        (and right? close?) (update path (dec (count path)) inc)
        (and right? open?) (conj path 0)
        (and left? close?) (conj path (count (:children node)))
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
  (let [[left right] (map #(codebox/lookup box %) (paths-around-space (:path space-node)))
        lo? (nil? left) ; left open (
        rc? (nil? right) ; right close )
        lc? (:paren left) ; left close )
        ro? (:paren right) ; right open (
        ra? (:text right) ; right atom
        la? (:text left) ; left atom
        side (region-side (:xy space-node) (:xy-end space-node) [x y])]
    (or (and lo? ro?)
        (and lo? rc?)
        (and la? ro? (= :right side))
        (and lc? ro?)
        (and lc? ro?)
        (and lc? ra? (= :left side))
        (and lc? rc?))))

(defn edit-cursor [{:keys [path] :as node} [x y]]
  (let [[cx cy] (codebox/cursor-coord-at box [x y])]
    (cond
      (:char? node)
      (if (= [cx cy] (codebox/char-coord-at box [x y]))
        path
        (update path (dec (count path)) inc))

      (:space? node)
      (if (force-structure-cursor? node [x y])
        (structure-cursor node [x y])
        (let [[left right] (map #(codebox/lookup box %) (paths-around-space (:path node)))
              side (if (and (:text left) (:text right))
                     (region-side (:xy node) (:xy-end node) [x y])
                     (if (:text left) :left :right))]
          (case side
            :left (conj (:path left) (count (:text left)))
            :right (conj (:path right) 0)))))))

(defn pick-edit [box [x y]]
  (when-let [node (pick-node box [x y])]
    (let [node (cond
                 (:paren node) (push-to-space node box [x y])
                 (:space? node) node
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
    (println "cursor:" (pr-str cursor))
    (println "node:" (pr-str node))
    (cond
      (#{:left :middle} button)
      (set-state!
        (cond-> (-> (get-state)
                    (assoc-in [:selections mode] [(:path node)])
                    (assoc :mousedown button))
          (= mode :primary)
          (assoc :cursor cursor)))

      (= :right button)
      (when-let [sel (seq (get-in (get-state) [:selections mode]))]
        (let [cursor (when (= mode :copy)
                       (get-state :cursor))]
          (if (space-path? (first sel))
            (set-state!
              (-> (get-state)
                  (assoc :pending-selection nil
                         :cursor cursor)))
            (set-state!
              (-> (get-state)
                  (assoc :pending-selection (:path node)
                         :cursor cursor ; TODO: do not do this if copy mode
                         :mousedown button)))))))))

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
  (let [mode (get-state :mode)
        sel (normalized-selection mode)]
    (set-state!
      (-> (get-state)
          (assoc-in [:selections mode] sel)

          (assoc :mousedown nil
                 :pending-selection nil)))))

(defn on-key-down [e]
  (when (= "Shift" (oget e "key"))
    (set-state! (assoc (get-state) :mode :copy))))

(defn on-key-up [e]
  (when (= "Shift" (oget e "key"))
    (set-state!
      (-> (get-state)
          (assoc :mode :primary)

          ; We clear the copy selection since it is supposed to be used to insert at
          ; text at the cursor or replace text at primary selection range.
          ; No operations are performed yet, so we discard as it would be after such
          ; an operation.
          (assoc-in [:selections :copy] nil)))))

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
