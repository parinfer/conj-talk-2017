(ns pres.editors.bbn
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse-pos]]
    [pres.reader :refer [read]]
    [oops.core :refer [ocall oget oset!]]))

(def mx 0)
(def my 0)

(defn draw [])

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
