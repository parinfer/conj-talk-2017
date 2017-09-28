(ns pres.app
  (:require
    [pres.canvas :as canvas :refer [ctx]]
    [pres.camera :as camera]
    [pres.editors.bbn :as bbn]
    [pres.editors.noko :as noko]
    [pres.editors.zmacs :as zmacs]
    [pres.state :refer [state]]
    [oops.core :refer [ocall oget oset!]]))

(def slide-funcs
  {:bbn {:init bbn/init! :draw bbn/draw}
   :noko {:init noko/init! :draw noko/draw}
   :zmacs {:init zmacs/init! :draw zmacs/draw}})

(defn run-slide-func [func-name]
  (let [slide-name (:slide @state)
        f (get-in slide-funcs [slide-name func-name])]
    (f)))

(defn draw []
  (ocall ctx "save")
  (canvas/transform)
  (canvas/clear)
  (camera/transform)
  (camera/draw-outline)
  (run-slide-func :draw)
  (ocall ctx "restore"))

(defn on-resize []
  (canvas/recalc!)
  (camera/recalc!)
  (draw))

(defn init! []
  (swap! state assoc :slide :zmacs)
  (oset! js/document "body.onresize" on-resize)
  (on-resize)
  (add-watch state :repaint draw)
  (run-slide-func :init))

(init!)
