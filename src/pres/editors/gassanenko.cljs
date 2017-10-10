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

(def char-w 20)
(def char-h 20)

(def left 200)
(def top 100)

;;----------------------------------------------------------------------
;; State
;;----------------------------------------------------------------------

(def state-key :gas)

(def init-state
  {:time 0
   :t 0}) ;; 0 -> 1 -> 2 -> 3

(defn get-state
  ([] (get @state state-key))
  ([k] (get-in @state [state-key k])))

(defn set-state! [s]
  (swap! state assoc state-key s))

;;----------------------------------------------------------------------
;; Draw
;;----------------------------------------------------------------------

(defn get-box [i]
  (let [x (- left (* i wpad))
        y (- top (* i hpad))
        w (+ width (* 2 i wpad))
        h (+ height (* 2 i hpad))]
    {:x x
     :y y
     :w w
     :h h}))

(defn paren-pos* [i t]
  (let [{:keys [x y w h]} (get-box (inc i))]
    (case t
      -1 {:x x
          :y y}
      0 {:x (+ x w (- wpad))
         :y (+ y h (* (+ i 2) (- hpad)))}
      1 {:x (+ x w (- wpad))
         :y (+ y h (- hpad))}
      2 {:x x
         :y (+ y h (- hpad))}
      3 {:x x
         :y (+ y h (* (inc i) (- hpad)))}
      nil)))

(defn interp-pos [a b t]
  (let [{:keys [x y]} a
        dx (- (:x b) x)
        dy (- (:y b) y)]
    {:x (+ x (* t dx))
     :y (+ y (* t dy))}))

(defn paren-pos [i t]
  (let [t0 (Math/floor t)
        t1 (Math/ceil t)]
    (if (= t0 t1)
      (paren-pos* i t0)
      (interp-pos
        (paren-pos* i t0)
        (paren-pos* i t1)
        (- t t0)))))

(defn draw-paren [i]
  (let [{:keys [t]} (get-state)
        {:keys [x y]} (paren-pos i t)]
    (ocall ctx "save")
    (ocall ctx "translate" (+ 0.5 (* 0.5 wpad)) (+ -0.5 (* 0.5 hpad)))
    (oset! ctx "fillStyle" c/focus-fill)
    (oset! ctx "font" (str "bold " (* 0.6 hpad) "px Menlo"))
    (oset! ctx "textBaseline" "middle")
    (oset! ctx "textAlign" "center")
    (ocall ctx "fillText" ")" x y)
    (let [{:keys [x y]} (paren-pos i -1)]
      (ocall ctx "fillText" "(" (+ 1 x) (+ 1 y)))
    (ocall ctx "restore")))

(defn draw-box [i]
  (let [{:keys [x y w h]} (get-box i)]
    (oset! ctx "lineWidth" 2)
    (oset! ctx "strokeStyle" c/blur-fill)
    (ocall ctx "strokeRect" x y w h)))

(defn draw-bounding []
  (when-let [t (#{0 2 3} (get-state :t))]
    (let [all (map #(paren-pos % t) (range num-parens))
          minx (apply min (map :x all))
          maxx (apply max (map :x all))
          miny (apply min (map :y all))
          maxy (apply max (map :y all))
          w (+ wpad (- maxx minx))
          h (+ hpad (- maxy miny))]
      (oset! ctx "strokeStyle" c/focus-fill)
      (ocall ctx "strokeRect" minx miny w h))))

(defn draw []
  (dotimes [i (inc num-parens)]
    (draw-box i))
  (dotimes [i num-parens]
    (draw-paren i))
  (draw-bounding))

;;----------------------------------------------------------------------
;; Animation
;;----------------------------------------------------------------------
(def anim-time 0)

(def easing
  {:linear (fn [t] t)
   :easeInQuad (fn [t] (* t t))
   :easeOutQuad (fn [t] (* t (- 2 t)))
   :easeOutCubic (fn [t] (let [t (dec t)] (+ 1 (* t t t))))})

(def phases
  [{:i 0 :delay 2000 :duration 400 :ease :easeOutCubic}
   {:i 1 :delay 0 :duration 1000 :ease :easeOutCubic}
   {:i 2 :delay 2000 :duration 600 :ease :easeOutCubic}
   {:i 3 :delay 3000 :duration 0}])

(def total-time
  (reduce
    (fn [t {:keys [i delay duration]}]
      (+ t delay duration))
    0
    phases))

(defn time->t []
  (reduce
    (fn [t {:keys [i delay duration ease]}]
      (let [t0 (- t delay)]
        (cond
          (<= t0 0)
          (reduced i)

          (<= t0 duration)
          (reduced (+ i ((easing ease) (/ t0 duration))))

          :else (- t0 duration))))
    (js-mod anim-time total-time)
    phases))

(defn advance-anim [dt]
  (set! anim-time (+ anim-time dt))
  (let [t (time->t)]
    (set-state!
      (assoc (get-state) :t t))))

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
