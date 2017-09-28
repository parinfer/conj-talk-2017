(ns pres.editors.lispedit
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

; code example from page 38 of LISP/VM User's Guide:
; https://github.com/shaunlebron/history-of-lisp-parens/blob/master/papers/SH20-6477_LispVMUG_Jul84.pdf
(def box-full (codebox/make (subs "
 (lambda (ex)
   (prog (fcn args simpx)
     (cond ((atom ex) (return ex)))
     (setq fcn (car ex))
     (setq args (mapcar simplify (cdr ex)))
     (setq simpx
       (cond
         ((eq 0 (car args))
          (cond
            ((eq fcn 'plus) (cadr args))
            ((eq fcn 'times) 0)
            ((eq fcn 'minus) 0)
            ((eq fcn 'difference) (list 'minus (cadr args)))
            ((eq fcn 'quotient) 0)
            ((eq fcn 'exp) 1)
            ((eq fcn 'expt) 0)
            ((eq fcn 'sin) 0)
            ((eq fcn 'cos) 1)
            ('else (cons fcn args))))
         ((eq 0 (card args))
          (cond
            ((eq fcn 'plus) (car args))
            ((eq fcn 'times) 0)
            ((eq fcn 'difference) (car args))
            ((eq fcn 'expt) 1)
            ('else (cons fcn args))))
         ((and (eq 1 (car args)) (eq fcn 'times)) (cadr args))
         ((and (eq 1 (cadr args)) (eq fcn 'times)) (car args))
         ((and (eq 1 (car args)) (eq fcn 'expt)) 1)
         ((and (eq 1 (cadr args)) (eq fcn 'expt)) (car args))
         ((and (eq 1 (car args)) (eq fcn 'log)) 0)
         ((nump (car args))
          (cond
            ((eq fcn 'minus) (minus (car args)))
            ((and (eq fcn 'plus) (nump (cadr args))
               (plus (car args) (cadr args))))
            ((and (eq fcn 'times) (nump (cadr args))
               (times (car args) (cadr args))))
            ((and (eq fcn 'difference) (nump (cadr args))
               (difference (car args) (cadr args))))
            ('else (cons fcn args))))
         ('else (cons fcn args))))
     (return simpx)))
" 1) {:xy [580 50] :font-size 7.5}))

(def top-node (:tree box-full))

(def box-curr)
(defn set-box-curr! [node]
  (set! box-curr (codebox/make (print-node node) [100 200])))

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

;;----------------------------------------------------------------------
;; Draw all
;;----------------------------------------------------------------------

(defn draw []
  (draw-box-full)
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
      (set-state!
        (-> (get-state)
            (assoc :path-hover new-path-hover))))))

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
