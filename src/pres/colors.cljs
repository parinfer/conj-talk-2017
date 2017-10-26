(ns pres.colors
  (:require
    [pres.canvas :refer [ctx]]
    [oops.core :refer [ocall oget oset!]]
    [quil.core :as q :include-macros true]))

;; add a common palette in case we need to change all of them quickly

(def highlight-fill [0 50 100 (* 255 0.05)])
(def highlight-stroke [0 50 100 (* 255 0.6)])
(def blur-fill [0 40 80 (* 255 0.3)])
(def focus-fill [0 0 0])

(defn highlight-box []
  (apply q/stroke highlight-stroke)
  (apply q/fill highlight-fill))
