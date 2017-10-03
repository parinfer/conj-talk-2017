(ns pres.codebox
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse->cam] :as camera]
    [pres.reader :refer [read walk node-from-path]]
    [pres.misc :refer [close-paren]]
    [oops.core :refer [ocall oget oset!]]))

;; Coord naming conventions:
;;   x/y = cam coords (relative to box)
;;   mx/my = mouse coords (absolute)
;;   cx/cy = char coords

;;----------------------------------------------------------------------
;; Font drawing
;;----------------------------------------------------------------------

;; character size and padding
(def char-h nil)
(def char-w nil)
(def line-h nil)

(def underline-pad 0.1) ; distance of underline from line bottom (in fraction of char-h)

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

(defmulti inside-shape? (fn [xy [name & coords]] name))
(defmethod inside-shape? :rect [[x y] [_name x0 y0 w h]]
  (and (<= x0 x (+ x0 w))
       (<= y0 y (+ y0 h))))
(defmethod inside-shape? :crect [xy [_name x0 y0 x1 y1 x2 y2]]
  (or (inside-shape? xy [:rect x0 y0 (- x2 x0) (- y2 y0)])
      (inside-shape? xy [:rect x0 y2 (- x1 x0) (- y1 y2)])))

;;----------------------------------------------------------------------
;; Cam Coordinates
;;----------------------------------------------------------------------

; cam coords relative to our box
(defn rel-cam [g [mx my]]
  (let [[gx gy] (:xy g)]
    [(- mx gx)
     (- my gy)]))

(defn cam->code [[x y]]
  [(Math/floor (/ x char-w))
   (Math/floor (/ y line-h))])

(defn cam->cursor [[x y]]
  [(Math/round (/ x char-w))
   (Math/floor (/ y line-h))])

(defn code->cam [[cx cy]]
  [(* cx char-w)
   (* cy line-h)])

(defn code-shape->cam [[name & coords]]
  (->> (partition 2 coords)
       (map code->cam)
       (flatten)
       (cons name)
       (vec)))

;;----------------------------------------------------------------------
;; Node helpers
;;----------------------------------------------------------------------

(defn multiline-range? [xy xy-end]
  (let [[_ y] xy
        [_ y1] xy-end]
    (not= y y1)))

(defn multiline-node? [{:keys [xy xy-end] :as node}]
  (multiline-range? xy xy-end))

;;----------------------------------------------------------------------
;; Node box coordinates
;;----------------------------------------------------------------------

; Case 1
;   x,y -> +------------+
;          |(foo baz    |
;          |            |
;          |  (f 1 2 3) |
;          |      +-----+ <- x2,y2
;          |  bar)|
;          +------+ <- x1,y1

; Case 2
;         x,y -> +------+
;           (foo | baz  |
; x3,y3 -> +-----+      |
;          |  (f 1 2 3) |
;          |      +-----+ <- x2,y2
;          |  bar |)
;          +------+ <- x1,y1

(defn multiline-box-shape [g xy xy-end]
  (let [[x y] xy
        [x1 y1] (map inc xy-end)
        w (- x1 x)
        h (- y1 y)
        y2 (dec y1)
        x2 (apply max x1 (map count (subvec (:lines g) y y2)))
        y3 (inc y)
        x3 (apply min x (map #(count (re-find #"^\s*" %)) (subvec (:lines g) (inc y) y2)))]
    (if (= x1 x2)
      [:rect x y w h]
      [:crect x y x1 y1 x2 y2 x3 y3])))

(defn box-shape [g xy xy-end]
  (if (multiline-range? xy xy-end)
    (multiline-box-shape g xy xy-end)
    (let [[x y] xy
          [x1 _] xy-end]
      [:rect x y (- (inc x1) x) 1])))

(defn node->box-shape [g {:keys [xy xy-end] :as node}]
  (box-shape g xy xy-end))

(defn inside-node-box? [g xy node]
  (inside-shape? xy (code-shape->cam (node->box-shape g node))))

;;----------------------------------------------------------------------
;; Node underline coordinates
;;----------------------------------------------------------------------

(defn multiline-underline-shapes [g xy xy-end]
  (let [[x0 y0] xy
        [x1 y1] xy-end]
    (for [y (range y0 (inc y1))]
      (let [line (get (:lines g) y)
            xl (if (= y y0) x0 (count (re-find #"\s*" line))) ; left
            xr (if (= y y1) (inc x1) (count line)) ; right
            yb (- (inc y) underline-pad)] ; bottom
        [:line xl yb xr yb]))))

(defn underline-shapes [g xy xy-end]
  (if (multiline-range? xy xy-end)
    (multiline-underline-shapes g xy xy-end)
    (let [[xl y] xy
          [xr _] xy-end
          xr (inc xr)
          yb (- (inc y) underline-pad)]
      [; shape list of size 1
       [:line xl yb xr yb]])))

(defn node->underlines [g {:keys [xy xy-end] :as node}]
  (underline-shapes g xy xy-end))

(declare char-at)

(defn inside-node-text? [g xy node]
  (and (inside-node-box? g xy node)
       (when-let [char (char-at g (cam->code xy))]
         (not= char " "))))

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

(defmulti draw-shape (fn [[name & coords]] name))
(defmethod draw-shape :rect [[_name x y w h]]
  (ocall ctx "beginPath")
  (ocall ctx "rect" x y w h))
(defmethod draw-shape :crect [[_name x0 y0 x1 y1 x2 y2 x3 y3]]
  (ocall ctx "beginPath")
  (ocall ctx "moveTo" x0 y0)
  (let [pairs (if (= x0 x3)
                [[x2 y0] [x2 y2] [x1 y2] [x1 y1] [x0 y1]]
                [[x2 y0] [x2 y2] [x1 y2] [x1 y1] [x3 y1] [x3 y3] [x0 y3]])]
    (doseq [[x y] pairs]
      (ocall ctx "lineTo" x y)))
  (ocall ctx "closePath"))
(defmethod draw-shape :line [[_name x0 y0 x1 y1]]
  (ocall ctx "beginPath")
  (ocall ctx "moveTo" x0 y0)
  (ocall ctx "lineTo" x1 y1)
  (ocall ctx "stroke"))

(defn draw-bounding-box* [g node]
  (draw-shape (code-shape->cam (node->box-shape g node))))

(defn draw-underline* [g node]
  (doseq [shape (node->underlines g node)]
    (draw-shape (code-shape->cam shape))))

;;----------------------------------------------------------------------
;; Public
;;
;; Usage:
;;  (let [g (make "(foo bar\n  baz)")]
;;    (draw g)
;;    (draw-bounding-box g)) ;; fill or stroke after
;;----------------------------------------------------------------------

(defn draw
  ([g] (draw g (:tree g)))
  ([g node]
   (setup-draw g)
   (draw* g node)
   (restore)))

(defn draw-bounding-box
  ([g] (draw-bounding-box g (:tree g)))
  ([g node]
   (setup-draw g)
   (draw-bounding-box* g node)
   (restore)))

(defn draw-underline
  ([g] (draw-bounding-box g (:tree g)))
  ([g node]
   (setup-draw g)
   (draw-underline* g node)
   (restore)))

(defn draw-cursor [g [cx cy]]
  (setup-draw g)
  (ocall ctx "beginPath")
  (let [[x y] (code->cam [cx cy])]
    (ocall ctx "moveTo" x y)
    (ocall ctx "lineTo" x (+ y line-h))
    (ocall ctx "stroke"))
  (restore))

(defn pick-nodes
  ([g [mx my]] (pick-nodes g [mx my] :box))
  ([g [mx my] shape]
   (setup-font g)
   (let [[x y] (rel-cam g [mx my])
         inside? ({:box inside-node-box?
                   :text inside-node-text?} shape)]
     (->> (:nodes g)
          (filter #(inside? g [x y] %))))))

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
