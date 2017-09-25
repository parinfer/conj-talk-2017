(ns pres.app
  (:require
    [pres.canvas :refer [canvas ctx]]
    [pres.editor-bbn :as bbn]
    [oops.core :refer [ocall oget oset!]]))

;; canvas size (without pixel ratio)
(def w nil)
(def h nil)

;; virtual size
(def aspect (/ 16 9))
(def vw 1024)
(def vh (/ vw aspect))

;; camera
(def x nil)
(def y nil)
(def scale nil)

(defn set-cam! []
  (set! x 0)
  (set! y 0)
  (if (> (/ w h) aspect)
    (do (set! scale (/ h vh))
        (set! x (* 0.5 (- w (* vw scale)))))
    (do (set! scale (/ w vw))
        (set! y (* 0.5 (- h (* vh scale))))))
  (ocall ctx "translate" x y)
  (ocall ctx "scale" scale scale)
  (ocall ctx "strokeRect" 0 0 vw vh))

(defn draw []
  (ocall ctx "clearRect" 0 0 w h)
  (set-cam!)
  (bbn/draw))

(defn resize []
  (let [ratio (or (oget js/window "devicePixelRatio") 1)]
    (set! w (oget js/window "innerWidth"))
    (set! h (oget js/window "innerHeight"))
    (oset! canvas "width" (* w ratio))
    (oset! canvas "height" (* h ratio))
    (oset! canvas "style.width" (str w "px"))
    (oset! canvas "style.height" (str h "px"))
    (ocall ctx "scale" ratio ratio)
    (draw)))

(oset! js/document "body.onresize" resize)
(resize)
