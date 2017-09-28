(ns pres.app
  (:require
    [pres.canvas :as canvas :refer [ctx]]
    [pres.camera :as camera]
    [pres.editors.bbn :as bbn]
    [pres.editors.noko :as noko]
    [pres.editors.zmacs :as zmacs]
    [pres.editors.lispedit :as lispedit]
    [pres.state :refer [state]]
    [oops.core :refer [ocall oget oset!]]))

;;----------------------------------------------------------------------
;; Slides
;;----------------------------------------------------------------------

(def slides
  {:bbn {:init bbn/init! :cleanup bbn/cleanup! :draw bbn/draw}
   :noko {:init noko/init! :cleanup noko/cleanup! :draw noko/draw}
   :zmacs {:init zmacs/init! :cleanup zmacs/cleanup! :draw zmacs/draw}
   :lispedit {:init lispedit/init! :cleanup lispedit/cleanup! :draw lispedit/draw}})

(defn run-slide
  ([func-name] (run-slide func-name (:slide @state)))
  ([func-name slide-name] ((get-in slides [slide-name func-name]))))

(def slide-order
  [:bbn :noko :zmacs :lispedit])

(defn slide-index [name]
  (first (keep-indexed #(when (= name %2) %1) slide-order)))

(defn next-slide [dir]
  (when-let [i (slide-index (:slide @state))]
    (let [next-i (+ i dir)]
      (when (<= 0 next-i (dec (count slide-order)))
        (slide-order next-i)))))

(defn set-slide! [name-]
  (when (slides name-)
    (when-let [prev-slide (:slide @state)]
      (run-slide :cleanup))
    (swap! state assoc :slide name-)
    (ocall js/location "replace" (str "#" (name name-)))
    (run-slide :init)))

(defn slide-from-hash []
  (let [slide (keyword (subs (oget js/location "hash") 1))]
    (when (slides slide)
      slide)))

;;----------------------------------------------------------------------
;; Top-level
;;----------------------------------------------------------------------

(defn draw []
  (ocall ctx "save")
  (canvas/transform)
  (canvas/clear)
  (camera/transform)
  (camera/draw-outline)
  (run-slide :draw)
  (ocall ctx "restore"))

;;----------------------------------------------------------------------
;; Events
;;----------------------------------------------------------------------

(defn on-key-down [e]
  (let [key (oget e "key")
        shift (oget e "shiftKey")]
    (when shift
      (case key
         "ArrowRight" (set-slide! (next-slide 1))
         "ArrowLeft" (set-slide! (next-slide -1))
         nil))))

(defn on-resize []
  (canvas/recalc!)
  (camera/recalc!)
  (draw))

(defn on-hash-change [e]
  (when-let [next-slide (slide-from-hash)]
    (when-not (= next-slide (:state @state))
      (set-slide! next-slide))))

;;----------------------------------------------------------------------
;; Init
;;----------------------------------------------------------------------

(defn init! []
  (set-slide! (or (slide-from-hash) (first slide-order)))
  (ocall js/window "addEventListener" "keydown" on-key-down)
  (ocall js/window "addEventListener" "hashchange" on-hash-change)
  (oset! js/document "body.onresize" on-resize)
  (on-resize)
  (add-watch state :repaint draw))

(init!)
