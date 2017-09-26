(ns pres.editors.bbn
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse->cam] :as camera]
    [pres.reader :refer [read walk]]
    [pres.state :refer [state]]
    [clojure.string :as string]
    [oops.core :refer [ocall oget oset!]]))

(def close-paren {"(" ")" "{" "}" "[" "]"})

; code example from page 54 of BBN manual:
; https://github.com/shaunlebron/history-of-lisp-parens/blob/master/papers/656771.pdf
(def code-string
 "
 (lambda (x y)
   (cond
     ((nul x) z)
     (t (cons
          (car x)
          (append (cdr x) y)))))
")

(def code-lines (vec (.split code-string "\n")))
(def code-tree (read code-string))
(def code-nodes (walk code-tree))

(def init-state
  {:history [{:nav [] :node code-tree}]
   :current-node code-tree})

;;----------------------------------------------------------------------
;; State
;;----------------------------------------------------------------------

(def key- :bbn)
(def key-hover [key- :hover-node])
(def key-curr [key- :current-node])
(def key-history [key- :history])
(def key-nav [key- :nav])

;;----------------------------------------------------------------------
;; Font drawing
;;----------------------------------------------------------------------

;; character size and padding
(def char-h 20)
(def char-w nil)
(def char-padh 10)
(def line-h (+ char-h char-padh))

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

(defn path->nav [path]
  (mapv inc path))

(defn path-diff [from to]
  (nav-diff
    (path->nav from)
    (path->nav to)))

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

(defn code-size->cam [[w h]]
  [(* w char-w)
   (* h line-h)])

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

(defn nodes-at [[x y]]
  (->> code-nodes
       (filter #(inside-node? [x y] %))
       (sort-by #(count (:path %)))))

(defn node-at [[x y]]
  (->> (nodes-at [x y])
       (filter :paren)
       (last)))

;;----------------------------------------------------------------------
;; Draw Editor
;;----------------------------------------------------------------------

(def editor-x 100)
(def editor-y 400)

(defn print-cmd [nav]
 (string/join " " (conj (vec nav) "P")))

(defn draw-editor []
  (let [curr (get-in @state key-curr)
        hover (get-in @state key-hover)
        nav (get-in @state key-nav)
        history (get-in @state key-history)]
    (ocall ctx "save")
    (ocall ctx "translate" editor-x editor-y)
    (oset! ctx "globalAlpha" 0.6)
    (ocall ctx "fillText" (str "*" (when nav (print-cmd nav))) 0 0)
    (ocall ctx "fillText" (str " " (when hover (print-node hover))) 0 line-h)
    (ocall ctx "fillText" (str " " (when curr (print-node curr))) 0 (* -2 line-h))
    (oset! ctx "globalAlpha" 0.3)
    (doseq [{:keys [nav node]} (take 4 (reverse history))]
      (ocall ctx "translate" 0 (* -3 line-h))
      (ocall ctx "fillText" (str "*" (print-cmd nav)) 0 0)
      (ocall ctx "fillText" (str " " (print-node node)) 0 line-h))
    (ocall ctx "restore")))

;;----------------------------------------------------------------------
;; Draw Code (todo, make relative to some coord)
;;----------------------------------------------------------------------

(defn draw-text [text pos]
  (let [[x y] (code->cam pos)
        fy (+ y (* 0.5 line-h))]
    (ocall ctx "fillText" text x fy)))

(defn draw-node [{:keys [paren text xy xy-end children]}]
  (cond
    paren (do
            (draw-text paren xy)
            (draw-text (close-paren paren) xy-end)
            (run! draw-node children))
    text (draw-text text xy)))

(defn draw-cursor []
  (let [hover (get-in @state key-hover)
        cursor (if hover "pointer" "default")]
    (oset! js/document "body.style.cursor" cursor)))

(defmulti draw-area (fn [[name & coords]] name))
(defmethod draw-area :rect [[_name x y w h]]
  (ocall ctx "beginPath")
  (ocall ctx "rect" x y w h))
(defmethod draw-area :crect [[_name x0 y0 x1 y1 x2 y2]]
  (ocall ctx "beginPath")
  (ocall ctx "moveTo" x0 y0)
  (doseq [[x y] [[x2 y0] [x2 y2] [x1 y2] [x1 y1] [x0 y1]]]
    (ocall ctx "lineTo" x y))
  (ocall ctx "closePath"))

(defn draw-code-window []
  (let [x (- code-x 20)
        w (- camera/w x)]
    (oset! ctx "fillStyle" "#F5F5F5")
    (ocall ctx "fillRect" x 0 w camera/h)
    (oset! ctx "fillStyle" "#000")))

;;----------------------------------------------------------------------
;; Draw all
;;----------------------------------------------------------------------

(defn draw []
  (set-font!)
  (when (nil? char-w)
    (calc-char-size!))
  (draw-code-window)
  (when-let [node (get-in @state key-hover)]
    (draw-area (code-area->cam (node->area node)))
    (oset! ctx "strokeStyle" "#000")
    (ocall ctx "stroke"))
  (oset! ctx "fillStyle" "#CCD")
  (draw-node code-tree)
  (when-let [node (get-in @state key-curr)]
    (oset! ctx "fillStyle" "#333")
    (draw-node node))
  (draw-editor)
  (draw-cursor))

;;----------------------------------------------------------------------
;; Mouse
;;----------------------------------------------------------------------

(defn on-mouse-down [e]
  (let [curr (get-in @state key-curr)
        hover (get-in @state key-hover)
        nav (get-in @state key-nav)]
    (when hover
      (swap! state update-in key-history conj {:nav nav :node hover})
      (swap! state assoc-in key-curr hover)
      (swap! state assoc-in key-hover nil)
      (swap! state assoc-in key-nav nil))))

(defn on-mouse-move [e]
  (let [[x y] (mouse->cam e)
        hover (node-at [x y])
        curr (get-in @state key-curr)
        prev-hover (get-in @state key-hover)]
    (when-not (= hover prev-hover)
      (swap! state assoc-in key-hover hover)
      (swap! state assoc-in key-nav
        (when hover
          (path-diff (:path curr) (:path hover)))))))

;;----------------------------------------------------------------------
;; Load
;;----------------------------------------------------------------------

(defn init! []
  (swap! state assoc key- init-state)
  (ocall js/window "addEventListener" "mousedown" on-mouse-down)
  (ocall js/window "addEventListener" "mousemove" on-mouse-move))

(defn cleanup! []
  (ocall js/window "removeEventListener" "mousedown" on-mouse-down)
  (ocall js/window "removeEventListener" "mousemove" on-mouse-move))
