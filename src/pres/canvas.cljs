(ns pres.canvas
  (:require
    [oops.core :refer [ocall oget oset!]]))

;; canvas size (without pixel ratio)
(def w nil)
(def h nil)
(def aspect nil)
(def scale nil)

(def canvas (ocall js/document "querySelector" "canvas"))
(def ctx (ocall canvas "getContext" "2d"))

(defn recalc! []
  (set! scale (or (oget js/window "devicePixelRatio") 1))
  (set! w (oget js/window "innerWidth"))
  (set! h (oget js/window "innerHeight"))
  (set! aspect (/ w h))
  (oset! canvas "width" (* w scale))
  (oset! canvas "height" (* h scale))
  (oset! canvas "style.width" (str w "px"))
  (oset! canvas "style.height" (str h "px")))

(defn transform []
  (ocall ctx "scale" scale scale))

(defn clear []
  (ocall ctx "clearRect" 0 0 w h))
