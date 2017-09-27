(ns pres.editors.zmacs
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse->cam] :as camera]
    [pres.state :refer [state]]
    [pres.codebox :as codebox]
    [pres.misc :refer [close-paren]]
    [clojure.string :as string]
    [oops.core :refer [ocall oget oset!]]))

;;----------------------------------------------------------------------
;; Codeboxes
;;----------------------------------------------------------------------

; code example from page 54 of BBN manual:
; https://github.com/shaunlebron/history-of-lisp-parens/blob/master/papers/656771.pdf
(def box (codebox/make (subs "
 (lambda (x y)
   (cond
     ((nul x) z)
     (t (cons
          (car x)
          (append (cdr x) y)))))
" 1) [380 200]))

;;----------------------------------------------------------------------
;; State
;;----------------------------------------------------------------------

(def state-key :zmacs)

(def init-state
  {:cursor nil
   :paren nil
   :blink-on false})

(defn get-state
  ([] (get @state state-key))
  ([k] (get-in @state [state-key k])))

(defn set-state! [s]
  (swap! state assoc state-key s))

;;----------------------------------------------------------------------
;; Draw Editor
;;----------------------------------------------------------------------

(defn draw-box []
  (oset! ctx "fillStyle" "#333")
  (codebox/draw box)
  (let [{:keys [cursor paren blink-on]} (get-state)]
    (when cursor
      (oset! ctx "strokeStyle" "#333")
      (codebox/draw-cursor box (get-state :cursor)))
    (when (and paren blink-on)
      (ocall ctx "save")
      (let [[x y] (:xy box)]
        (ocall ctx "translate" x y))
      (codebox/set-font!)
      (let [[x y] (codebox/code->cam (:xy paren))]
        (ocall ctx "translate" x y))
      (oset! ctx "fillStyle" "#333")
      (ocall ctx "fillRect" 0 0 codebox/char-w codebox/line-h)
      (oset! ctx "fillStyle" "#f5f5f5")
      (ocall ctx "fillText" (:paren paren) 0 (* 0.5 codebox/line-h))
      (ocall ctx "restore"))))

;;----------------------------------------------------------------------
;; Draw all
;;----------------------------------------------------------------------

(defn update-cursor []
  (oset! js/document "body.style.cursor" (if (get-state :cursor) "text" "default")))

(defn draw []
  (draw-box)
  (update-cursor))

;;----------------------------------------------------------------------
;; Animation
;;----------------------------------------------------------------------

(def blink-duration 250)

(def anim-time 0)
(defn advance-anim [dt]
  (set! anim-time (+ anim-time dt))
  (let [on (even? (Math/floor (/ anim-time blink-duration)))]
    (when (get-state :paren)
      (set-state! (assoc (get-state) :blink-on on)))))

(def should-anim? false)
(def last-time nil)

(defn tick [t]
  (let [dt (if last-time (- t last-time) 0)]
    (set! last-time t)
    (advance-anim dt)
    (when should-anim?
      (ocall js/window "requestAnimationFrame" tick))))

;;----------------------------------------------------------------------
;; Mouse
;;----------------------------------------------------------------------

(defn open-paren-to-blink [[cx cy]]
  (let [transform (fn [node] (select-keys node [:xy :paren]))]
    (->> (:nodes box)
         (filter :paren)
         (filter #(= [(dec cx) cy] (:xy-end %)))
         (map transform)
         (first))))

(defn close-paren-to-blink [[cx cy]]
  (let [transform (fn [{:keys [xy-end paren]}]
                    {:xy xy-end :paren (close-paren paren)})]
    (->> (:nodes box)
         (filter :paren)
         (filter #(= [cx cy] (:xy %)))
         (map transform)
         (first))))

(defn paren-to-blink [[cx cy]]
  (or (open-paren-to-blink [cx cy])
      (close-paren-to-blink [cx cy])))

(defn on-mouse-move [e]
  (let [[x y] (mouse->cam e)
        {:keys [cursor]} (get-state)
        new-cursor (codebox/cursor-coord-at box [x y])
        paren (paren-to-blink new-cursor)]
     (when-not (= new-cursor cursor)
       (when paren
         (set! anim-time 0))
       (set-state!
         (-> (get-state)
             (assoc :cursor new-cursor
                    :paren paren))))))

;;----------------------------------------------------------------------
;; Load
;;----------------------------------------------------------------------

(defn init! []
  (set-state! init-state)
  (set! should-anim? true)
  (ocall js/window "requestAnimationFrame" tick)
  (ocall js/window "addEventListener" "mousemove" on-mouse-move))

(defn cleanup! []
  (set! should-anim? false)
  (ocall js/window "removeEventListener" "mousemove" on-mouse-move))
