(ns pres.editors.lispedit
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse->cam] :as camera]
    [pres.state :refer [state]]
    [pres.codebox :as codebox]
    [pres.reader :refer [print-node path-diff descendant?]]
    ; [pres.pprint :refer [pprint]]
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
(defn set-box-curr! []
  (set! box-curr
    (codebox/make
      (print-node top-node)
      {:xy [100 100]
       :font-size 12})))

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
      (set-box-curr!)
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
        {:keys [path-hover]} (get-state)
        new-path-hover (:path (pick-node box-full [x y]))]
    (when-not (= new-path-hover path-hover)
      (set-box-curr!)
      (set-state!
        (-> (get-state)
            (assoc :path-hover new-path-hover))))))

;;----------------------------------------------------------------------
;; Load
;;----------------------------------------------------------------------

(defn init! []
  (set-box-curr!)
  (set-state! init-state)
  (ocall js/window "addEventListener" "mousedown" on-mouse-down)
  (ocall js/window "addEventListener" "mousemove" on-mouse-move))

(defn cleanup! []
  (ocall js/window "removeEventListener" "mousedown" on-mouse-down)
  (ocall js/window "removeEventListener" "mousemove" on-mouse-move))
