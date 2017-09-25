(ns pres.editors.bbn
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse-pos]]
    [pres.reader :refer [read]]
    [pres.state :refer [state]]
    [oops.core :refer [ocall oget oset!]]))

(def mx 0)
(def my 0)

; code example from page 54 of BBN manual:
; https://github.com/shaunlebron/history-of-lisp-parens/blob/master/papers/656771.pdf
(def code
 "
 (LAMBDA (X Y)
   (COND
     ((NUL X) Z)
     (T (CONS
          (CAR X)
          (APPEND (CDR X) Y)))))
")

(def tree (read code))

(def code-x 580)
(def code-y 200)

;; character size and padding
(def char-h 20)
(def char-w nil)
(def char-padh 8)

(defn set-font! []
  (oset! ctx "font" (str char-h "px monospace")))

(defn calc-char-size! []
  (let [text "abcdef"
        text-width (oget (ocall ctx "measureText" text) "width")]
    (set! char-w (/ text-width (count text)))))

(defn code-pos [[x y]]
  [(+ code-x (* x char-w))
   (+ code-y (* y (+ char-h char-padh)))])

(defn draw-text [text pos]
  (let [[x y] (code-pos pos)]
    (ocall ctx "fillText" text x y)))

(def close-paren
  {"(" ")"
   "{" "}"
   "[" "]"})

(defn draw-node [{:keys [paren text xy xy-end children]}]
  (cond
    paren (do
            (draw-text paren xy)
            (draw-text (close-paren paren) xy-end)
            (doseq [child children]
              (draw-node child)))
    text (draw-text text xy)))

(defn draw []
  (set-font!)
  (when (nil? char-w)
    (calc-char-size!))
  (draw-node tree))

(defn on-mouse-down [e])

(defn on-mouse-move [e]
  (let [[x y] (mouse-pos e)]
    (set! mx x)
    (set! my y)))

(defn init! []
  (ocall js/window "addEventListener" "mousedown" on-mouse-down)
  (ocall js/window "addEventListener" "mousemove" on-mouse-move))

(defn cleanup! []
  (ocall js/window "removeEventListener" "mousedown" on-mouse-down)
  (ocall js/window "removeEventListener" "mousemove" on-mouse-move))
