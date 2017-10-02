(ns pres.editors.dedit
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse->cam] :as camera]
    [pres.state :refer [state]]
    [pres.codebox :as codebox]
    [pres.reader :refer [descendant? parent common-ancestor]]
    [pres.pprint :refer [pprint]]
    [pres.examples :as examples]
    [clojure.string :as string]
    [oops.core :refer [ocall oget oset!]]))

; first structure editor with mouse interface
; novel selection stack for commands
; novel typein <> structure window flow

; pretty-print windows
; click-to-select, stack-based selections for command args, multi-window

; edit window
; - expressions converted to structure when complete for selection
; - CTRL-Z to execute as a teletype command


;;----------------------------------------------------------------------
;; Codeboxes
;;----------------------------------------------------------------------

(def boxes
  {:a (codebox/make examples/short-func {:xy [100 50] :font-size 9})
   :b (codebox/make examples/short-interlisp-func {:xy [320 50] :font-size 9})
   :c (codebox/make examples/medium-func {:xy [540 50] :font-size 9})
   :edit-a (codebox/make "(fact (difference x 1))" {:xy [320 200] :font-size 9})
   :edit-b (codebox/make "(lessp x 3)" {:xy [320 210] :font-size 9})})

;;----------------------------------------------------------------------
;; State
;;----------------------------------------------------------------------

(def state-key :dedit)

(def init-state
  {:top nil
   :nxt nil
   :mousedown false})

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

(defn draw-editor [id]
  (let [box (boxes id)
        {:keys [top nxt]} (get-state)
        top-node (when (= (:id top) id) (codebox/lookup box (:path top)))
        nxt-node (when (= (:id nxt) id) (codebox/lookup box (:path nxt)))]
    (oset! ctx "fillStyle" "#333")
    (codebox/draw box)
    (when top-node
      (oset! ctx "strokeStyle" "#000")
      (codebox/draw-underline box top-node))
    (when nxt-node
      (ocall ctx "save")
      ; (ocall ctx "translate" 0 -0.5)
      (ocall ctx "setLineDash" #js[3 1])
      (oset! ctx "strokeStyle" "#000")
      (codebox/draw-underline box nxt-node)
      (when (and top-node nxt-node)
        (cond
          (descendant? (:path top) (:path nxt))
          (do
            (ocall ctx "setLineDash" #js[])
            (codebox/draw-underline box top-node))

          (descendant? (:path nxt) (:path top))
          (do
            (oset! ctx "strokeStyle" "#fff")
            (codebox/draw-underline box nxt-node))

          :else nil))
      (ocall ctx "restore"))))

;;----------------------------------------------------------------------
;; Draw all
;;----------------------------------------------------------------------

(defn draw []
  (doseq [id (keys boxes)]
    (draw-editor id))
  (update-cursor))

;;----------------------------------------------------------------------
;; Mouse
;;----------------------------------------------------------------------

(defn set-top [curr]
  (let [prev (get-state :top)]
    (set-state!
      (-> (get-state)
          (assoc :top curr :nxt prev)))))

(defn click [{:keys [id path button]}]
  (let [top (get-state :top)
        path (case button
               :left path
               :middle (parent path)
               :right (when (= id (:id top))
                        (common-ancestor path (:path top)))
               nil)]
    (when path
      (set-top {:id id :path path}))))

(defn pick-node [box [x y]]
  (->> (codebox/pick-nodes box [x y])
       (sort-by #(count (:path %)))
       (last)))

(defn click-info [e]
  (let [[x y] (mouse->cam e)
        pick (fn [[id box]]
               {:id id
                :path (:path (pick-node box [x y]))})]
    (when-let [node (first (filter :path (map pick boxes)))]
      (assoc node :button
        ([:left :middle :right] (oget e "button"))))))

(defn on-mouse-down [e]
  (click (click-info e)))

(defn on-mouse-up [e]
  (set-state! (assoc (get-state) :mousedown false)))

(defn on-mouse-move [e])

(defn on-context-menu [e]
  (ocall e "preventDefault"))

;;----------------------------------------------------------------------
;; Load
;;----------------------------------------------------------------------

(defn init! []
  (set-state! init-state)
  (ocall js/window "addEventListener" "mousedown" on-mouse-down)
  (ocall js/window "addEventListener" "mouseup" on-mouse-up)
  (ocall js/window "addEventListener" "mousemove" on-mouse-move)
  (ocall js/window "addEventListener" "contextmenu" on-context-menu))

(defn cleanup! []
  (ocall js/window "removeEventListener" "mousedown" on-mouse-down)
  (ocall js/window "removeEventListener" "mouseup" on-mouse-up)
  (ocall js/window "removeEventListener" "mousemove" on-mouse-move)
  (ocall js/window "removeEventListener" "contextmenu" on-context-menu))
