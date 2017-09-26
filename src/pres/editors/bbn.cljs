(ns pres.editors.bbn
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse->cam]]
    [pres.reader :refer [read walk]]
    [pres.state :refer [state]]
    [clojure.string :as string]
    [oops.core :refer [ocall oget oset!]]))

(def close-paren {"(" ")" "{" "}" "[" "]"})

; code example from page 54 of BBN manual:
; https://github.com/shaunlebron/history-of-lisp-parens/blob/master/papers/656771.pdf
(def code-string
 "
 (LAMBDA (X Y)
   (COND
     ((NUL X) Z)
     (T (CONS
          (CAR X)
          (APPEND (CDR X) Y)))))
")

(def code-lines (vec (.split code-string "\n")))
(def code-tree (read code-string))
(def code-nodes (walk code-tree))

;;----------------------------------------------------------------------
;; Font drawing
;;----------------------------------------------------------------------

;; character size and padding
(def char-h 20)
(def char-w nil)
(def char-padh 10)

(defn set-font! []
  (oset! ctx "font" (str char-h "px Menlo"))
  (oset! ctx "textBaseline" "middle")
  (oset! ctx "textAlign" "left"))

(defn calc-char-size! []
  (let [text "abcdef"
        text-width (oget (ocall ctx "measureText" text) "width")]
    (set! char-w (/ text-width (count text)))))

;;----------------------------------------------------------------------
;; Paths
;;----------------------------------------------------------------------

(defn nav-path [path]
  (mapv inc path))

(defn common-ancestor [a b]
  (when (and a b (= (first a) (first b)))
    (cons (first a) (common-ancestor (next a) (next b)))))

(defn nav-diff [from to]
  (let [i (count (common-ancestor from to))]
    (concat
      (vec (repeat (- (count from) i) 0))
      (subvec to i))))
(assert (= (nav-diff [1 2 2 1] [1 3]) [0 0 0 3]))
(assert (= (nav-diff [1 3] [1 2 2 1]) [0 2 2 1]))

;;----------------------------------------------------------------------
;; Printing
;;----------------------------------------------------------------------

(defn print-node* [{:keys [children paren text]} depth]
  (if paren
    (if (zero? depth)
      "&"
      (str paren
           (string/join " " (map #(print-node* % (dec depth)) children))
           (close-paren paren)))
    text))

(defn print-node [node]
  (print-node* node 2))

;;----------------------------------------------------------------------
;; Math
;;----------------------------------------------------------------------

(defmulti inside-area? (fn [m [name & coords]] name))
(defmethod inside-area? :rect [[mx my] [_name x y w h]]
  (and (<= x mx (+ x w))
       (<= y my (+ y h))))
(defmethod inside-area? :crect [m [_name x0 y0 x1 y1 x2 y2]]
  (or (inside-area? m [:rect x0 y0 (- x2 x0) (- y2 y0)])
      (inside-area? m [:rect x0 y2 (- x1 x0) (- y1 y2)])))

;;----------------------------------------------------------------------
;; Cam Coordinates
;;----------------------------------------------------------------------

(def code-x 580)
(def code-y 200)
(def editor-x 100)
(def editor-y 200)

(defn code-size->cam [[w h]]
  [(* w char-w)
   (* h (+ char-h char-padh))])

(defn code->cam [[x y]]
  (let [[w h] (code-size->cam [x y])]
    [(+ code-x w)
     (+ code-y h)]))

(defmulti code-area->cam (fn [[name & coords]] name))
(defmethod code-area->cam :rect [[_name x y w h]]
  (let [[cx cy] (code->cam [x y])
        [cw ch] (code-size->cam [w h])]
    [:rect cx cy cw ch]))
(defmethod code-area->cam :crect [[_name x y x1 y1 x2 y2]]
  (->> [[x y] [x1 y1] [x2 y2]]
       (map code->cam)
       (flatten)
       (cons :crect)
       (vec)))

;;----------------------------------------------------------------------
;; Code Coordinates
;;----------------------------------------------------------------------

(defn multiline-node? [{:keys [xy xy-end] :as node}]
  (if (and xy xy-end)
    (let [[_ y] xy
          [_ y1] xy-end]
      (not= y y1))))

; x,y -> +-----------+
;        |(foo       |
;        |  (+ 1 2 3)|
;        |      +----+ <- x2,y2
;        |  bar)|
;        +------+ <- x1,y1
(defn node->multiline-area [{:keys [xy xy-end] :as node}]
  (let [[x y] xy
        [x1 y1] (map inc xy-end)
        w (- x1 x)
        h (- y1 y)
        y2 (dec y1)
        x2 (apply max x1 (map count (subvec code-lines y y2)))]
    (if (= x1 x2)
      [:rect x y w h]
      [:crect x y x1 y1 x2 y2])))

(defn node->area [{:keys [text xy xy-end paren] :as node}]
  (if (multiline-node? node)
    (node->multiline-area node)
    (let [[x y] xy
          [x1 _] xy-end]
      (if paren
        [:rect x y (- (inc x1) x) 1]
        [:rect x y (count text) 1]))))

(defn inside-node? [m node]
  (inside-area? m (code-area->cam (node->area node))))

(defn node-at [[x y]]
  (->> code-nodes
       (filter #(inside-node? [x y] %))
       (sort-by #(count (:path %)))
       (last)))

;;----------------------------------------------------------------------
;; Drawing
;;----------------------------------------------------------------------

(defn draw-text [text pos]
  (let [[x y] (code->cam pos)
        fy (+ y (* 0.5 (+ char-h char-padh)))]
    (ocall ctx "fillText" text x fy)))

(defn draw-node [{:keys [paren text xy xy-end children]}]
  (cond
    paren (do
            (draw-text paren xy)
            (draw-text (close-paren paren) xy-end)
            (run! draw-node children))
    text (draw-text text xy)))

(defn debug-node-str [node]
  (str (pr-str (:path node)) ":" (or (:paren node) (:text node))))

(defn draw-editor []
  (let [node (get-in @state [:bbn :current-node])
        hover (get-in @state [:bbn :hover-node])]
    (when hover
      (ocall ctx "fillText" (print-node hover) editor-x editor-y))))

(defn draw-cursor []
  (let [hover (get-in @state [:bbn :hover-node])
        cursor (if hover "pointer" "default")]
    (oset! js/document "body.style.cursor" cursor)))

(defmulti draw-area (fn [[name & coords]] name))
(defmethod draw-area :rect [[_name x y w h]]
  (ocall ctx "strokeRect" x y w h))
(defmethod draw-area :crect [[_name x0 y0 x1 y1 x2 y2]]
  (ocall ctx "beginPath")
  (ocall ctx "moveTo" x0 y0)
  (doseq [[x y] [[x2 y0] [x2 y2] [x1 y2] [x1 y1] [x0 y1]]]
    (ocall ctx "lineTo" x y))
  (ocall ctx "closePath")
  (ocall ctx "stroke"))

(defn draw []
  (set-font!)
  (when (nil? char-w)
    (calc-char-size!))
  (draw-node code-tree)
  (when-let [node (get-in @state [:bbn :hover-node])]
    (draw-area (code-area->cam (node->area node))))
  (draw-editor)
  (draw-cursor))

;;----------------------------------------------------------------------
;; Mouse
;;----------------------------------------------------------------------

(defn on-mouse-down [e]
  (let [[x y] (mouse->cam e)
        node (node-at [x y])]
    (when node
      (swap! state assoc-in [:bbn :current-node] node))))

(defn on-mouse-move [e]
  (let [[x y] (mouse->cam e)
        node (node-at [x y])
        prev-node (get-in @state [:bbn :hover-node])]
    (when-not (= node prev-node)
      (swap! state assoc-in [:bbn :hover-node] node))))

;;----------------------------------------------------------------------
;; Load
;;----------------------------------------------------------------------

(defn init! []
  (ocall js/window "addEventListener" "mousedown" on-mouse-down)
  (ocall js/window "addEventListener" "mousemove" on-mouse-move))

(defn cleanup! []
  (ocall js/window "removeEventListener" "mousedown" on-mouse-down)
  (ocall js/window "removeEventListener" "mousemove" on-mouse-move))
