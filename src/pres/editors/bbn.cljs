(ns pres.editors.bbn
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse->cam] :as camera]
    [pres.state :refer [state]]
    [pres.codebox :as codebox]
    [clojure.string :as string]
    [oops.core :refer [ocall oget oset!]]))

(def close-paren {"(" ")" "{" "}" "[" "]"})

;;----------------------------------------------------------------------
;; Printing
;;----------------------------------------------------------------------

(defn print-node* [{:keys [children paren text]} depth]
  (if paren
    (if (zero? depth)
      "&"
      (str paren
           (string/join " " (map #(print-node* % (dec depth)) children))
           (close-paren paren)))
    text))

(defn print-node [node]
  (print-node* node 2))

;;----------------------------------------------------------------------
;; Codeboxes
;;----------------------------------------------------------------------

; code example from page 54 of BBN manual:
; https://github.com/shaunlebron/history-of-lisp-parens/blob/master/papers/656771.pdf
(def box-full (codebox/make "
 (lambda (x y)
   (cond
     ((nul x) z)
     (t (cons
          (car x)
          (append (cdr x) y)))))
" [580 200]))

(def top-node (:tree box-full))

(def box-active)
(defn set-box-active! [node]
  (set! box-active (codebox/make (print-node node) [100 400])))

(set-box-active! top-node)

(def init-state
  {:history [{:nav [] :node top-node}]
   :current-node top-node})

;;----------------------------------------------------------------------
;; State
;;----------------------------------------------------------------------

(def key- :bbn)
(def key-hover [key- :hover-node])
(def key-hover-box [key- :hover-box])
(def key-curr [key- :current-node])
(def key-history [key- :history])
(def key-nav [key- :nav])

;;----------------------------------------------------------------------
;; Paths
;;----------------------------------------------------------------------

(defn common-ancestor [a b]
  (when (and a b (= (first a) (first b)))
    (cons (first a) (common-ancestor (next a) (next b)))))

(defn nav-diff [from to]
  (let [i (count (common-ancestor from to))]
    (concat
      (vec (repeat (- (count from) i) 0))
      (subvec to i))))
(assert (= (nav-diff [1 2 2 1] [1 3]) [0 0 0 3]))
(assert (= (nav-diff [1 3] [1 2 2 1]) [0 2 2 1]))

(defn path->nav [path]
  (mapv inc path))

(defn path-diff [from to]
  (nav-diff
    (path->nav from)
    (path->nav to)))

;;----------------------------------------------------------------------
;; Draw Editor
;;----------------------------------------------------------------------

(defn print-cmd [nav]
 (string/join " " (conj (vec nav) "P")))

(defn draw-editor []
  (let [curr (get-in @state key-curr)
        hover (get-in @state key-hover)
        nav (get-in @state key-nav)
        history (get-in @state key-history)
        line-h codebox/line-h
        [x y] (:xy box-active)]
    (ocall ctx "save")
    (codebox/set-font!)
    (ocall ctx "translate" (- x codebox/char-w) (+ y (* 2.5 line-h)))
    (oset! ctx "globalAlpha" 0.6)
    (ocall ctx "fillText" (str "*" (when nav (print-cmd nav))) 0 0)
    (ocall ctx "fillText" (str " " (when hover (print-node hover))) 0 line-h)
    (oset! ctx "globalAlpha" 0.3)
    (doseq [{:keys [nav node]} (take 4 (reverse history))]
      (ocall ctx "translate" 0 (* -3 line-h))
      (ocall ctx "fillText" (str "*" (print-cmd nav)) 0 0)
      (ocall ctx "fillText" (str " " (print-node node)) 0 line-h))
    (ocall ctx "restore")))

(defn draw-code-window []
  (let [x (- (:x box-full) 20)
        w (- camera/w x)]
    (oset! ctx "fillStyle" "#F5F5F5")
    (ocall ctx "fillRect" x 0 w camera/h)
    (oset! ctx "fillStyle" "#000")))

(defn draw-cursor []
  (let [hover (get-in @state key-hover)
        cursor (if hover "pointer" "default")]
    (oset! js/document "body.style.cursor" cursor)))

;;----------------------------------------------------------------------
;; Draw all
;;----------------------------------------------------------------------

(defn draw []
  (draw-code-window)
  (when-let [node (get-in @state key-hover)]
    (when-let [box (get-in @state key-hover-box)]
      (codebox/draw-region box node)
      (oset! ctx "strokeStyle" "#000")
      (ocall ctx "stroke")))
  (oset! ctx "fillStyle" "#CCD")
  (codebox/draw box-full)
  (when-let [node (get-in @state key-curr)]
    (oset! ctx "fillStyle" "#333")
    (codebox/draw box-full node))
  (draw-editor)
  (codebox/draw box-active)
  (draw-cursor))

;;----------------------------------------------------------------------
;; Mouse
;;----------------------------------------------------------------------

(defn on-mouse-down [e]
  (let [curr (get-in @state key-curr)
        hover (get-in @state key-hover)
        nav (get-in @state key-nav)]
    (when hover
      (set-box-active! hover)
      (swap! state update-in key-history conj {:nav nav :node hover})
      (swap! state assoc-in key-curr hover)
      (swap! state assoc-in key-hover nil)
      (swap! state assoc-in key-nav nil))))

(defn pick-node [code [x y]]
  (->> (codebox/pick-nodes code [x y])
       (filter :paren)
       (sort-by #(count (:path %)))
       (last)))

(defn on-mouse-move [e]
  (let [[x y] (mouse->cam e)
        hover-full (pick-node box-full [x y])
        hover-active (pick-node box-active [x y])
        hover (or hover-full hover-active)
        hover-box (cond
                    hover-full box-full
                    hover-active box-active
                    :else nil)
        curr (get-in @state key-curr)
        hover-prev (get-in @state key-hover)]
    (when-not (= hover hover-prev)
      (swap! state assoc-in key-hover hover)
      (swap! state assoc-in key-hover-box hover-box)
      (swap! state assoc-in key-nav
        (when hover
          (path-diff (:path curr) (:path hover)))))))

;;----------------------------------------------------------------------
;; Load
;;----------------------------------------------------------------------

(defn init! []
  (swap! state assoc key- init-state)
  (ocall js/window "addEventListener" "mousedown" on-mouse-down)
  (ocall js/window "addEventListener" "mousemove" on-mouse-move))

(defn cleanup! []
  (ocall js/window "removeEventListener" "mousedown" on-mouse-down)
  (ocall js/window "removeEventListener" "mousemove" on-mouse-move))
