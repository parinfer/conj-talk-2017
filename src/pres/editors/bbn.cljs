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

(defn draw-code [])

(defn draw []
  (draw-code))

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
