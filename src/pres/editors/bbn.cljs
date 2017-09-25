(ns pres.editors.bbn
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse-pos]]
    [pres.reader :refer [read]]
    [pres.state :refer [state]]
    [oops.core :refer [ocall oget oset!]]))

(def mx 0)
(def my 0)
(def r 5)

(defn draw []
  (let [[x y] (get-in @state [:bbn :xy])]
    (ocall ctx "beginPath")
    (ocall ctx "ellipse" x y r r 0 0 (* 2 Math/PI))
    (ocall ctx "fill")))

(defn on-mouse-down [e])

(defn on-mouse-move [e]
  (let [[x y] (mouse-pos e)]
    (set! mx x)
    (set! my y)
    (swap! state assoc-in [:bbn :xy] [x y])))

(defn init! []
  (ocall js/window "addEventListener" "mousedown" on-mouse-down)
  (ocall js/window "addEventListener" "mousemove" on-mouse-move))

(defn cleanup! []
  (ocall js/window "removeEventListener" "mousedown" on-mouse-down)
  (ocall js/window "removeEventListener" "mousemove" on-mouse-move))
