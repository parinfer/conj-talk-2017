(ns pres.editors.dedit
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse->cam] :as camera]
    [pres.state :refer [state]]
    [pres.codebox :as codebox]
    [pres.reader :refer [print-node path-diff descendant? walk node-from-path]]
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

(def state-key :bbn)

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
      (ocall ctx "translate" 0 -2)
      (oset! ctx "strokeStyle" "#000")
      (codebox/draw-underline box nxt-node)
      (when (and top-node nxt-node
              (or (descendant? (:path top) (:path nxt))
                  (descendant? (:path nxt) (:path top))))
        (oset! ctx "strokeStyle" "#fff")
        (codebox/draw-underline box nxt-node))
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

(defn on-mouse-down [e]
  (let [{:keys [top nxt]} (get-state)
        hover (codebox/lookup box-full path-hover)]
    (when hover
      (set-state!
        (-> (get-state)
            (assoc :path-curr path-hover)
            (dissoc :path-hover)
            (assoc :mousedown true))))))

(defn on-mouse-up [e]
  (set-state! (assoc (get-state) :mousedown false)))

(defn pick-node [code [x y]]
  (->> (codebox/pick-nodes code [x y])
       (filter #(or (:paren %)
                    (= (:text %) "&")
                    (= (:text %) "...")))
       (sort-by #(count (:path %)))
       (last)))

(defn on-mouse-move [e]
  (let [[x y] (mouse->cam e)
        {:keys [mousedown path-hover path-curr]} (get-state)
        new-path-hover (or (:path (pick-node box-full [x y]))
                           (when-let [node (pick-node box-curr [x y])]
                             (let [pnode (node-from-path top-pprint (next (:path node)))]
                               (or (:path pnode)
                                   (first (:elided-paths pnode))))))]
    (when-not (= new-path-hover path-hover)
      (when mousedown
        (set-box-curr! new-path-hover))
      (set-state!
        (-> (get-state)
            (assoc :path-hover new-path-hover
                   :path-curr (if mousedown new-path-hover path-curr)))))))

;;----------------------------------------------------------------------
;; Load
;;----------------------------------------------------------------------

(defn init! []
  (set-state! init-state)
  (ocall js/window "addEventListener" "mousedown" on-mouse-down)
  (ocall js/window "addEventListener" "mouseup" on-mouse-up)
  (ocall js/window "addEventListener" "mousemove" on-mouse-move))

(defn cleanup! []
  (ocall js/window "removeEventListener" "mousedown" on-mouse-down)
  (ocall js/window "removeEventListener" "mouseup" on-mouse-up)
  (ocall js/window "removeEventListener" "mousemove" on-mouse-move))
