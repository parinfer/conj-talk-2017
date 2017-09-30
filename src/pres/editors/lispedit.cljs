(ns pres.editors.lispedit
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse->cam] :as camera]
    [pres.state :refer [state]]
    [pres.codebox :as codebox]
    [pres.reader :refer [print-node path-diff descendant?]]
    [pres.pprint :refer [pprint]]
    [pres.examples :as examples]
    [clojure.string :as string]
    [oops.core :refer [ocall oget oset!]]))

;;----------------------------------------------------------------------
;; Codeboxes
;;----------------------------------------------------------------------

(def box-full
  (codebox/make examples/long-func
    {:xy [580 50]
     :font-size 7.5}))

(def top-node (:tree box-full))

(def box-curr)
(defn set-box-curr! [focus-path]
  (set! box-curr
    (codebox/make
      (pprint top-node
        {:depth 4
         :width 70
         :focus focus-path
         :focus-depth 8})
      {:xy [100 50]
       :font-size 7.5})))

;;----------------------------------------------------------------------
;; State
;;----------------------------------------------------------------------

(def state-key :bbn)

(def init-state
  {:path-curr [0]
   :path-hover nil})

(defn get-state
  ([] (get @state state-key))
  ([k] (get-in @state [state-key k])))

(defn set-state! [s]
  (swap! state assoc state-key s))

;;----------------------------------------------------------------------
;; Draw Editor
;;----------------------------------------------------------------------

(defn update-cursor []
  (let [{:keys [path-hover]} (get-state)
        cursor (if path-hover "pointer" "default")]
    (oset! js/document "body.style.cursor" cursor)))

(defn draw-box-full []
  (let [{:keys [path-curr path-hover]} (get-state)
        curr (codebox/lookup box-full path-curr)]
    (oset! ctx "fillStyle" "#CCD")
    (codebox/draw box-full)
    (oset! ctx "fillStyle" "#333")
    (codebox/draw box-full curr)
    (when-let [hover (codebox/lookup box-full path-hover)]
      (codebox/draw-region box-full hover)
      (oset! ctx "strokeStyle" "#000")
      (ocall ctx "stroke"))))

(defn draw-box-curr []
  (oset! ctx "fillStyle" "#333")
  (codebox/draw box-curr))

;;----------------------------------------------------------------------
;; Draw all
;;----------------------------------------------------------------------

(defn draw []
  (draw-box-full)
  (draw-box-curr)
  (update-cursor))

;;----------------------------------------------------------------------
;; Mouse
;;----------------------------------------------------------------------

(defn on-mouse-down [e]
  (let [{:keys [path-curr path-hover nav]} (get-state)
        hover (codebox/lookup box-full path-hover)]
    (when hover
      (set-box-curr! path-hover)
      (set-state!
        (-> (get-state)
            (assoc :path-curr path-hover)
            (dissoc :path-hover))))))

(defn pick-node [code [x y]]
  (->> (codebox/pick-nodes code [x y])
       (filter #(or (:paren %) (= (:text %) "&")))
       (sort-by #(count (:path %)))
       (last)))

(defn on-mouse-move [e]
  (let [[x y] (mouse->cam e)
        {:keys [path-hover path-curr]} (get-state)
        new-path-hover (:path (pick-node box-full [x y]))]
    (when-not (= new-path-hover path-hover)
      (if new-path-hover
        (set-box-curr! new-path-hover)
        (set-box-curr! path-curr))
      (set-state!
        (-> (get-state)
            (assoc :path-hover new-path-hover))))))

;;----------------------------------------------------------------------
;; Load
;;----------------------------------------------------------------------

(defn init! []
  (set-box-curr! (:path top-node))
  (set-state! init-state)
  (ocall js/window "addEventListener" "mousedown" on-mouse-down)
  (ocall js/window "addEventListener" "mousemove" on-mouse-move))

(defn cleanup! []
  (ocall js/window "removeEventListener" "mousedown" on-mouse-down)
  (ocall js/window "removeEventListener" "mousemove" on-mouse-move))
