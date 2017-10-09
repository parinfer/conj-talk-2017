(ns pres.colors
  (:require
    [pres.canvas :refer [ctx]]
    [oops.core :refer [ocall oget oset!]]))

;; add a common palette in case we need to change all of them quickly

(def highlight-fill "rgba(0,50,100, 0.05)")
(def highlight-stroke "rgba(0,50,100, 0.6)")
(def blur-fill "rgba(0,40,80,0.3)")
(def focus-fill "#000")

(defn highlight-box []
  (oset! ctx "strokeStyle" highlight-stroke)
  (ocall ctx "stroke")
  (oset! ctx "fillStyle" highlight-fill)
  (ocall ctx "fill"))
