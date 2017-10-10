(ns pres.editors.gassanenko
  (:require
    [pres.canvas :refer [ctx]]
    [pres.state :refer [state]]
    [pres.examples :as examples]
    [pres.colors :as c]
    [oops.core :refer [ocall oget oset!]]))


(def num-parens 3)
(def width 140)
(def height 100)
(def wpad 20)
(def hpad 20)

(def left 200)
(def top 100)

;;----------------------------------------------------------------------
;; State
;;----------------------------------------------------------------------

(def state-key :gas)

(def init-state
  {:t 0
   :pos 0}) ;; 0 -> 1 -> 2

(defn get-state
  ([] (get @state state-key))
  ([k] (get-in @state [state-key k])))

(defn set-state! [s]
  (swap! state assoc state-key s))

;;----------------------------------------------------------------------
;; Draw
;;----------------------------------------------------------------------

(defn draw-paren [i])

(defn draw-box [i]
  (let [x (- left (* i wpad))
        y (- top (* i hpad))
        w (+ width (* 2 i wpad))
        h (+ height (* 2 i hpad))]
    (oset! ctx "lineWidth" 2)
    (oset! ctx "strokeStyle" c/focus-fill)
    (ocall ctx "strokeRect" x y w h)))

(defn draw-bounding [])

(defn draw []
  (dotimes [i (inc num-parens)]
    (draw-box i))
  (dotimes [i num-parens]
    (draw-paren i))
  (draw-bounding))

;;----------------------------------------------------------------------
;; Animation
;;----------------------------------------------------------------------

(defn advance-anim [dt]
  (set-state! (update (get-state) :t + dt)))

(def last-time nil)
(def should-anim? false)

(defn tick [t]
  (let [dt (if last-time (- t last-time) 0)]
    (set! last-time t)
    (advance-anim dt)
    (when should-anim?
      (ocall js/window "requestAnimationFrame" tick))))

;;----------------------------------------------------------------------
;; Load
;;----------------------------------------------------------------------

(defn init! []
  (set-state! init-state)
  (set! should-anim? true)
  (ocall js/window "requestAnimationFrame" tick))

(defn cleanup! []
  (set! should-anim? false))
