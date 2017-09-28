(ns pres.editors.bbn
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse->cam] :as camera]
    [pres.state :refer [state]]
    [pres.codebox :as codebox]
    [pres.reader :refer [print-node path-diff descendant?]]
    [clojure.string :as string]
    [oops.core :refer [ocall oget oset!]]))

;;----------------------------------------------------------------------
;; Codeboxes
;;----------------------------------------------------------------------

; code example from page 54 of BBN manual:
; https://github.com/shaunlebron/history-of-lisp-parens/blob/master/papers/656771.pdf
(def box-full (codebox/make (subs "
 (lambda (x y)
   (cond
     ((nul x) z)
     (t (cons
          (car x)
          (append (cdr x) y)))))
" 1) {:xy [580 200] :font-size 20}))

(def top-node (:tree box-full))

(def box-curr)
(defn set-box-curr! [node]
  (set! box-curr
    (codebox/make (print-node node)
      {:xy [100 200] :font-size 20})))

;;----------------------------------------------------------------------
;; State
;;----------------------------------------------------------------------

(def state-key :bbn)

(def init-state
  {:path-curr [0]
   :path-hover nil
   :nav nil})

(defn get-state
  ([] (get @state state-key))
  ([k] (get-in @state [state-key k])))

(defn set-state! [s]
  (swap! state assoc state-key s))

;;----------------------------------------------------------------------
;; Draw Editor
;;----------------------------------------------------------------------

(defn print-cmd [nav]
 (string/join " " (conj (vec nav) "P")))

(defn draw-editor []
  (let [{:keys [path-curr path-hover nav]} (get-state)
        hover (codebox/lookup box-full path-hover)
        line-h codebox/line-h
        [x y] (:xy box-curr)]
      (ocall ctx "save")
      (codebox/setup-font box-curr)
      (ocall ctx "translate" x (+ y (* 2.5 line-h)))
      (oset! ctx "globalAlpha" 0.3)
      (ocall ctx "fillText" (str "*") 0 0)
      (when-not (= path-curr path-hover)
        (ocall ctx "fillText" (str " " (when nav (print-cmd nav))) 0 0)
        (oset! ctx "globalAlpha" 0.6)
        (ocall ctx "fillText" (str " " (when hover (print-node hover))) 0 line-h))
      (ocall ctx "restore")))

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
      (ocall ctx "stroke")))

 (defn draw-box-curr []
   (let [{:keys [path-curr path-hover]} (get-state)]
     (oset! ctx "fillStyle" "#333")
     (codebox/draw box-curr)
     (when (and (descendant? path-hover path-curr)
             (not= path-hover path-curr))
       (let [path (vec (cons 0 (drop (count path-curr) path-hover)))
             hover (codebox/lookup box-curr path)]
         (when hover
           (codebox/draw-region box-curr hover)
           (oset! ctx "strokeStyle" "#000")
           (ocall ctx "stroke")))))))

;;----------------------------------------------------------------------
;; Draw all
;;----------------------------------------------------------------------

(defn draw []
  (draw-box-full)
  (draw-box-curr)
  (draw-editor)
  (update-cursor))

;;----------------------------------------------------------------------
;; Mouse
;;----------------------------------------------------------------------

(defn on-mouse-down [e]
  (let [{:keys [path-curr path-hover nav]} (get-state)
        hover (codebox/lookup box-full path-hover)]
    (when hover
      (set-box-curr! hover)
      (set-state!
        (-> (get-state)
            (assoc :path-curr path-hover)
            (dissoc :path-hover :nav))))))

(defn pick-node [code [x y]]
  (->> (codebox/pick-nodes code [x y])
       (filter #(or (:paren %) (= (:text %) "&")))
       (sort-by #(count (:path %)))
       (last)))

(defn on-mouse-move [e]
  (let [[x y] (mouse->cam e)
        {:keys [path-curr path-hover]} (get-state)
        new-path-hover (or (:path (pick-node box-full [x y]))
                         (when-let [node (pick-node box-curr [x y])]
                           (vec (concat path-curr (next (:path node))))))]
    (when-not (= new-path-hover path-hover)
      (set-state!
        (-> (get-state)
            (assoc :path-hover new-path-hover
                   :nav (when new-path-hover (path-diff path-curr new-path-hover))))))))

;;----------------------------------------------------------------------
;; Load
;;----------------------------------------------------------------------

(defn init! []
  (set-box-curr! top-node)
  (set-state! init-state)
  (ocall js/window "addEventListener" "mousedown" on-mouse-down)
  (ocall js/window "addEventListener" "mousemove" on-mouse-move))

(defn cleanup! []
  (ocall js/window "removeEventListener" "mousedown" on-mouse-down)
  (ocall js/window "removeEventListener" "mousemove" on-mouse-move))
