(ns pres.codebox
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse->cam] :as camera]
    [pres.reader :refer [read walk node-from-path]]
    [pres.misc :refer [close-paren]]
    [oops.core :refer [ocall oget oset!]]))

;;----------------------------------------------------------------------
;; Font drawing
;;----------------------------------------------------------------------

;; character size and padding
(def char-h nil)
(def char-w nil)
(def line-h nil)

(defn use-font! [h]
  (oset! ctx "font" (str h "px Menlo"))
  (oset! ctx "textBaseline" "middle")
  (oset! ctx "textAlign" "left"))

(def get-char-w
  (memoize
    (fn [h]
      (use-font! h)
      (let [text "abcdef"
            text-width (oget (ocall ctx "measureText" text) "width")]
        (/ text-width (count text))))))

(defn set-font-size! [size]
  (set! char-h size)
  (set! line-h (* size 1.5))
  (set! char-w (get-char-w size)))

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

; cam coords relative to our box
(defn rel-cam [g [x y]]
  (let [[gx gy] (:xy g)]
    [(- x gx)
     (- y gy)]))

(defn cam->code [[x y]]
  [(Math/floor (/ x char-w))
   (Math/floor (/ y line-h))])

(defn cam->cursor [[x y]]
  [(Math/round (/ x char-w))
   (Math/floor (/ y line-h))])

(defn code->cam [[x y]]
  [(* x char-w)
   (* y line-h)])

(defn code-area->cam [[name & coords]]
  (->> (partition 2 coords)
       (map code->cam)
       (flatten)
       (cons name)
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
(defn node->multiline-area [g {:keys [xy xy-end] :as node}]
  (let [[x y] xy
        [x1 y1] (map inc xy-end)
        w (- x1 x)
        h (- y1 y)
        y2 (dec y1)
        x2 (apply max x1 (map count (subvec (:lines g) y y2)))]
    (if (= x1 x2)
      [:rect x y w h]
      [:crect x y x1 y1 x2 y2])))

(defn node->area [g {:keys [text xy xy-end paren] :as node}]
  (if (multiline-node? node)
    (node->multiline-area g node)
    (let [[x y] xy
          [x1 _] xy-end]
      (if paren
        [:rect x y (- (inc x1) x) 1]
        [:rect x y (count text) 1]))))

(defn inside-node? [g m node]
  (inside-area? m (code-area->cam (node->area g node))))

;;----------------------------------------------------------------------
;; Draw
;;----------------------------------------------------------------------

(defn draw-text [text pos]
  (let [[x y] (code->cam pos)
        fy (+ y (* 0.5 line-h))]
    (ocall ctx "fillText" text x fy)))

(defn draw* [g {:keys [paren text xy xy-end children]}]
   (cond
     paren (do
             (draw-text paren xy)
             (draw-text (close-paren paren) xy-end)
             (run! #(draw* g %) children))
     text (draw-text text xy)))

(defn setup-font [g]
  (set-font-size! (:font-size g))
  (use-font! char-h))

(defn setup-draw [g]
  (setup-font g)
  (ocall ctx "save")
  (let [[x y] (:xy g)]
    (ocall ctx "translate" x y)))

(defn restore []
  (ocall ctx "restore"))

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

(defn draw-region* [g node]
  (draw-area (code-area->cam (node->area g node))))

;;----------------------------------------------------------------------
;; Public
;;
;; Usage:
;;  (let [g (make "(foo bar\n  baz)")]
;;    (draw g)
;;    (draw-region g)) ;; fill or stroke after
;;----------------------------------------------------------------------

;; Coord naming conventions:
;;   mx/my = mouse coords
;;   cx/cy = char coords

(defn draw
  ([g] (draw g (:tree g)))
  ([g node]
   (setup-draw g)
   (draw* g node)
   (restore)))

(defn draw-region
  ([g] (draw-region g (:tree g)))
  ([g node]
   (setup-draw g)
   (draw-region* g node)
   (restore)))

(defn draw-cursor [g [cx cy]]
  (setup-draw g)
  (ocall ctx "beginPath")
  (let [[x y] (code->cam [cx cy])]
    (ocall ctx "moveTo" x y)
    (ocall ctx "lineTo" x (+ y line-h))
    (ocall ctx "stroke"))
  (restore))

(defn pick-nodes [g [mx my]]
  (setup-font g)
  (let [[x y] (rel-cam g [mx my])]
    (->> (:nodes g)
         (filter #(inside-node? g [x y] %)))))

(defn char-coord-at [g [mx my]]
  (setup-font g)
  (cam->code (rel-cam g [mx my])))

(defn cursor-coord-at [g [mx my]]
  (setup-font g)
  (when (seq (pick-nodes g [mx my]))
    (cam->cursor (rel-cam g [mx my]))))

(defn char-at [g [cx cy]]
  (aget (get (:lines g) cy) cx))

(defn lookup [g path]
  (when path
    (node-from-path (:tree g) (next path)))) ; ignore first key since we assume top-node))

(defn make
  [string {:keys [xy font-size]}]
  (let [lines (vec (.split string "\n"))
        tree (read string)
        nodes (walk tree)]
    {:lines lines
     :tree tree
     :nodes nodes
     :font-size font-size
     :xy xy}))
