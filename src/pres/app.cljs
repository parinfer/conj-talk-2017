(ns pres.app
  (:require
    [pres.canvas :as canvas :refer [ctx]]
    [pres.camera :as camera]
    [pres.editors.bbn :as bbn]
    [oops.core :refer [ocall oget oset!]]))

(defn draw []
  (ocall ctx "save")
  (canvas/transform)
  (canvas/clear)
  (camera/transform)
  (bbn/draw)
  (ocall ctx "restore"))

(defn on-resize []
  (canvas/recalc!)
  (camera/recalc!)
  (draw))

(oset! js/document "body.onresize" on-resize)
(on-resize)
