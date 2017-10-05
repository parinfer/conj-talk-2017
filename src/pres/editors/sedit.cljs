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
; interesting cursor states

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
   :pending-selection [] ; one path
   :mode nil ; primary, copy
   :mousedown nil}) ; left, right, middle

(defn get-state
  ([] (get @state state-key))
  ([k] (get-in @state [state-key k])))

(defn set-state! [s]
  (swap! state assoc state-key s))

;;----------------------------------------------------------------------
;; Selection utilities
;;----------------------------------------------------------------------

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
    (select-keys (codebox/lookup box a) [:xy :xy-end])
    (if (char-path? a)
      {:xy (char-path->xy a)
       :xy-end (char-path->xy b)
       :range? true}
      {:xy (:xy (codebox/lookup box a))
       :xy-end (:xy-end (codebox/lookup box b))
       :range? true})))

(defn normalize-selection* [paths]
  (let [n (count (apply common-ancestor (take 2 paths)))]
    (->> paths
         (map #(take (inc n) %))
         (sort-by last))))

(defn normalize-selection [[a b & others]]
  (reduce
    #(normalize-selection* (cons %2 %1))
    (normalize-selection* [a b])
    others))

;;----------------------------------------------------------------------
;; Draw Editor
;;----------------------------------------------------------------------

(defn draw-selection [mode]
  (let [{:keys [mode selections]} (get-state)]
    (when-let [sel (some-> (seq (selections mode))
                           (normalize-selection)
                           (selection->node))]
      (if (:range? sel)
        ; draw single (underline)
        ()
        ; draw range (in box if primary, underline if copy)
        ()))))

(defn draw-editor [box]
  (oset! ctx "fillStyle" "#333")
  (codebox/draw box)
  (draw-selection :primary)
  (draw-selection :copy))
  ;(draw-cursor))

;;----------------------------------------------------------------------
;; Draw all
;;----------------------------------------------------------------------

(defn draw []
  (draw-editor box))

;;----------------------------------------------------------------------
;; Mouse
;;----------------------------------------------------------------------

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
    (when (:paren node)
      (let [opener? (= (:xy node) char-xy)
            cursor-left? (= char-xy cursor-xy)
            first-inner (conj path -0.5)
            last-inner (conj path (+ (count (:children node)) -0.5))
            next-outer (update path (dec (count path)) + 0.5)]
        {:path (cond opener? first-inner, cursor-left? last-inner, :else next-outer)
         :space? true}))))

(defn bordering-paths [space-path]
  [(update path (dec (count path)) - 0.5)
   (update path (dec (count path)) + 0.5)])

(defn region-side [[x0 y0] [x1 y1] [mx my]]
  (let [x1 (inc x1)
        [x y] (codebox/cam->code-frac box (codebox/rel-cam box [mx my]))
        y (Math/floor y)]
    (when-not (= [x0 y0] [x1 y1])
      (if (= y0 y1)
        ([:left :right] (Math/round (/ (- x x0) (- x1 x0))))
        ({y0 :left y1 :right} y)))))

(defn structure-cursor [node [x y]]
  (cond
    (:space? node)
    (:path node)

    (:text node)
    nil ; TODO: update path +/- 0.5

    (:paren node)
    nil))
    ; TODO: if left of open paren, or right of close paren => update path +/- 0.5
    ; TODO: if right of open paren, or left of close-paren => conj path -0.5 or (+ (count (:children node)) -0.5)

; NOTE: a cursor is just a path
; inside text: text path + char index
; inside list: list path + fractional index (same as space path)

(defn force-structure-cursor? [space left right [x y]]
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
  (let [lo? (nil? left)
        rc? (nil? right)
        lc? (:paren left)
        ro? (:paren right)
        ra? (:text right)
        la? (:text left)
        side (region-side (:xy space) (:xy-end space) xy)]
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
      (let [[left right] (map #(codebox/lookup box %) (bordering-paths (:path node)))]
        (if (force-structure-cursor? node left right [x y])
          (structure-cursor node [x y])
          (let [side (if (and (:text left) (:text right))
                       (region-side (:xy node) (:xy-end node) [x y])
                       (if (:text left) :left :right))]
            (case side
              :left (conj (:path left) (inc (first (:xy-end left))))
              :right (conj (:path right) 0))))))))

(defn pick-edit [box [x y]]
  (when-let [node (pick-node box [x y])]
    (let [node (or (push-to-space node box [x y]) node)
          ; TODO: if (:text node), set node to char-node
          cursor (edit-cursor node [x y])]
      {:node node
       :cursor cursor})))

(defn pick-structure [box [x y]]
  (when-let [node (pick-node box [x y])]
    {:node node
     :cursor (structure-cursor node [x y])}))

(defn mouse-down [[x y] button]
  (let [(pick-node box [x y])]))

(defn click-info [e]
  {:xy (mouse->cam e)
   :button ([:left :middle :right] (oget e "button"))})

(defn on-mouse-down [e]
  (let [{:keys [xy button]} (click-info e)]
    (mouse-down xy button)))

(defn on-mouse-up [e]
  ; set selection
  (set-state! (assoc (get-state) :mousedown nil)))

(defn on-mouse-move [e])

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
