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
   :paren nil})

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
  (when-let [cursor (get-state :cursor)]
    (oset! ctx "strokeStyle" "#333")
    (codebox/draw-cursor box (get-state :cursor))))

;;----------------------------------------------------------------------
;; Draw all
;;----------------------------------------------------------------------

(defn update-cursor []
  (oset! js/document "body.style.cursor" (if (get-state :cursor) "text" "default")))

(defn draw []
  (draw-box)
  (update-cursor))

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
       (set-state!
         (-> (get-state)
             (assoc :cursor new-cursor
                    :paren paren))))))

;;----------------------------------------------------------------------
;; Load
;;----------------------------------------------------------------------

(defn init! []
  (set-state! init-state)
  (ocall js/window "addEventListener" "mousemove" on-mouse-move))

(defn cleanup! []
  (ocall js/window "removeEventListener" "mousemove" on-mouse-move))
