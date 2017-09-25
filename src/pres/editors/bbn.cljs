(ns pres.editors.bbn
  (:require
    [pres.canvas :refer [ctx]]
    [pres.camera :refer [mouse->cam]]
    [pres.reader :refer [read walk]]
    [pres.state :refer [state]]
    [oops.core :refer [ocall oget oset!]]))

(def close-paren {"(" ")" "{" "}" "[" "]"})

; code example from page 54 of BBN manual:
; https://github.com/shaunlebron/history-of-lisp-parens/blob/master/papers/656771.pdf
(def code
 "
 (LAMBDA (X Y)
   (COND
     ((NUL X) Z)
     (T (CONS
          (CAR X)
          (APPEND (CDR X) Y)))))
")


(def tree (read code))
(def nodes (walk tree))

;;----------------------------------------------------------------------
;; Font drawing
;;----------------------------------------------------------------------

;; character size and padding
(def char-h 20)
(def char-w nil)
(def char-padh 8)

(defn set-font! []
  (oset! ctx "font" (str char-h "px monospace"))
  (oset! ctx "textBaseline" "top")
  (oset! ctx "textAlign" "left"))

(defn calc-char-size! []
  (let [text "abcdef"
        text-width (oget (ocall ctx "measureText" text) "width")]
    (set! char-w (/ text-width (count text)))))

;;----------------------------------------------------------------------
;; Coordinates
;;----------------------------------------------------------------------

(def code-x 580)
(def code-y 200)

(defn code->cam [[x y]]
  [(+ code-x (* x char-w))
   (+ code-y (* y (+ char-h char-padh)))])

(defn node->cam-rect [{:keys [xy text paren]}]
  (let [[x y] (code->cam xy)
        w (* char-w (if paren 1 (count text)))
        h (+ char-h char-padh)]
    [x y w h]))

(defn inside-rect? [[mx my] [x y w h]]
  (and (<= x mx (+ x w))
       (<= y my (+ y h))))

(defn node-at [[x y]]
  (->> nodes
       (filter #(inside-rect? [x y] (node->cam-rect %)))
       (first)))

;;----------------------------------------------------------------------
;; Drawing
;;----------------------------------------------------------------------

(defn draw-text [text pos]
  (let [[x y] (code->cam pos)]
    (ocall ctx "fillText" text x y)))

(defn draw-node [{:keys [paren text xy xy-end children]}]
  (cond
    paren (do
            (draw-text paren xy)
            (draw-text (close-paren paren) xy-end)
            (run! draw-node children))
    text (draw-text text xy)))

(defn debug-node-str [node]
  (str (pr-str (:path node)) ":" (or (:paren node) (:text node))))

(defn draw-editor []
  (let [node (get-in @state [:bbn :current-node])
        hover (get-in @state [:bbn :hover-node])]
    (ocall ctx "fillText" (debug-node-str hover) 0 0)))

(defn draw []
  (set-font!)
  (when (nil? char-w)
    (calc-char-size!))
  (draw-node tree)
  (draw-editor))

;;----------------------------------------------------------------------
;; Mouse
;;----------------------------------------------------------------------

(defn on-mouse-down [e]
  (let [[x y] (mouse->cam e)
        node (node-at [x y])]
    (when node
      (swap! state assoc-in [:bbn :current-node] node))))

(defn on-mouse-move [e]
  (let [[x y] (mouse->cam e)
        node (node-at [x y])
        prev-node (get-in @state [:bbn :hover-node])]
    (when-not (= node prev-node)
      (swap! state assoc-in [:bbn :hover-node] node))))

;;----------------------------------------------------------------------
;; Load
;;----------------------------------------------------------------------

(defn init! []
  (ocall js/window "addEventListener" "mousedown" on-mouse-down)
  (ocall js/window "addEventListener" "mousemove" on-mouse-move))

(defn cleanup! []
  (ocall js/window "removeEventListener" "mousedown" on-mouse-down)
  (ocall js/window "removeEventListener" "mousemove" on-mouse-move))
