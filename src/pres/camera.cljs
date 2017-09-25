(ns pres.camera
  (:require
    [pres.canvas :as canvas :refer [ctx]]
    [oops.core :refer [ocall oget oset!]]))

(def aspect (/ 16 9))
(def w 1024)
(def h (/ w aspect))

;; to be set on update
(def x nil)
(def y nil)
(def scale nil)

(defn recalc! []
  (set! x 0)
  (set! y 0)
  (if (> canvas/aspect aspect)
    (do (set! scale (/ canvas/h h))
        (set! x (* 0.5 (- canvas/w (* w scale)))))
    (do (set! scale (/ canvas/w w))
        (set! y (* 0.5 (- canvas/h (* h scale)))))))

(defn transform []
  (ocall ctx "translate" x y)
  (ocall ctx "scale" scale scale)
  (ocall ctx "strokeRect" 0 0 w h))
