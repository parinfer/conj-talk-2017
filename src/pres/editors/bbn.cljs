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

(def code-x 800)
(def code-y 200)

;; character size and padding
(def char-h 20)
(def char-w nil)
(def char-padh 4)

(defn set-font! []
  (oset! ctx "font" (str char-h "px monospace")))

(defn calc-char-size! []
  (set-font!)
  (let [text "abcdef"
        text-width (oget (ocall ctx "measureText" text) "width")]
    (set! char-w (/ text-width (count text)))
    (println char-w)))

(defn code-xy [[x y]]
  [(+ code-x (* x char-w))
   (+ code-y (* y (+ char-h char-padh)))])

(defn draw-code [node]
  (set-font!)
  (ocall ctx "fillText" "foobar" code-x code-y))

(defn draw []
  (when (nil? char-w)
    (calc-char-size!))
  (draw-code tree))

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
