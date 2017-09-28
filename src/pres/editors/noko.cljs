(ns pres.editors.noko
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse->cam] :as camera]
    [pres.state :refer [state]]
    [pres.codebox :as codebox]
    [pres.reader :refer [print-node path-diff descendant?]]
    [pres.misc :refer [close-paren]]
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

(defn noko-string [{:keys [paren children]}]
  (string/join "\n"
    [paren
     (string/join "\n" (map #(str "    " (print-node %)) children))
     (close-paren paren)]))

(def box-curr)
(defn set-box-curr! [node]
  (set! box-curr
    (codebox/make (noko-string node)
      {:xy [100 200] :font-size 20})))

;;----------------------------------------------------------------------
;; State
;;----------------------------------------------------------------------

(def state-key :noko)

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
;; Path utils
;;----------------------------------------------------------------------

(defn atom-path? [path]
  (let [{:keys [text paren]} (codebox/lookup box-full path)]
    (and (not= text "&")
         (not paren))))

(defn parent-path [path]
  (vec (butlast path)))

(defn parent-if-atom [path]
  (when path
    (if (atom-path? path) (parent-path path) path)))

;;----------------------------------------------------------------------
;; Draw Editor
;;----------------------------------------------------------------------

(defn print-cmd [nav]
 (string/join " " (conj (vec nav) "P")))

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
  (let [{:keys [path-curr path-hover]} (get-state)]
    (oset! ctx "fillStyle" "#333")
    (codebox/draw box-curr)
    (when (and (descendant? path-hover path-curr)
            (not= path-hover path-curr))
      (let [path (vec (cons 0 (drop (count path-curr) path-hover)))
            hover (codebox/lookup box-curr path)
            line-h codebox/line-h
            [x y] (:xy box-curr)]
        (when hover
          (codebox/draw-region box-curr hover)
          (oset! ctx "strokeStyle" "#000")
          (ocall ctx "stroke"))
        (when (= 2 (count path))
          (ocall ctx "save")
          (codebox/setup-font box-curr)
          (ocall ctx "translate" x (+ y (* 1.5 line-h)))
          (ocall ctx "fillText" (str "_") 0 (* (second path) line-h))
          (ocall ctx "restore"))))))

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
  (let [{:keys [path-hover nav]} (get-state)
        path-curr (parent-if-atom path-hover)
        curr (codebox/lookup box-full path-curr)]
    (when curr
      (set-box-curr! curr)
      (set-state!
        (-> (get-state)
            (assoc :path-curr path-curr)
            (dissoc :path-hover :nav))))))

(defn pick-node-full [[x y]]
  (->> (codebox/pick-nodes box-full [x y])
       (filter #(or (:paren %) (= (:text %) "&")))
       (sort-by #(count (:path %)))
       (last)))

(defn pick-node-curr [[x y]]
  (->> (codebox/pick-nodes box-curr [x y])
       (filter #(= 2 (count (:path %))))
       (first)))

(defn on-mouse-move [e]
  (let [[x y] (mouse->cam e)
        {:keys [path-curr path-hover]} (get-state)
        new-path-hover (or (:path (pick-node-full [x y]))
                         (when-let [node (pick-node-curr [x y])]
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
