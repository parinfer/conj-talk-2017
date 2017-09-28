(ns pres.reader
  (:require
    [pres.misc :refer [close-paren]]
    [clojure.string :as string]))

(defn init-state [text]
  {:xy [0 0]
   :nodes [{:paren :root, :children []}]
   :path [0]
   :text text})

(defn public-path [path]
  (subvec (vec (remove #{:children} path)) 1))

(defn list-node [paren xy path i]
  {:paren paren
   :xy xy
   :children []
   :path (conj (public-path path) i)})

(defn atom-node [text xy path i]
  {:xy xy
   :text text
   :path (conj (public-path path) i)})

(defn skip-space [{:keys [xy text] :as state}]
  (let [[x y] xy
        [char] text
        new-xy (case char " " [(inc x) y] "\n" [0 (inc y)] xy)]
    (if (= new-xy xy)
      state
      (recur (assoc state :xy new-xy :text (subs text 1))))))

(defn on-atom [{:keys [xy text nodes path] :as state}]
  (let [word (re-find #"[^\s\(\{\[\]\}\)]+" text)
        dx (count word)
        [x y] xy
        i (count (get-in nodes (conj path :children)))
        node (atom-node word xy path i)]
    (assoc state
      :xy [(+ x dx) y]
      :text (subs text dx)
      :nodes (update-in nodes (conj path :children) conj node))))

(def paren? #{"(" "{" "[" "]" "}" ")"})
(def opener? #{"(" "{" "["})

(defn on-paren [{:keys [xy text nodes path] :as state}]
  (let [[char] text
        open (opener? char)
        i (count (get-in nodes (conj path :children)))
        [x y] xy]
    (assoc state
      :text (subs text 1)
      :xy [(inc x) y]
      :nodes (if open
               (update-in nodes (conj path :children) conj (list-node char xy path i))
               (assoc-in nodes (conj path :xy-end) xy))
      :path (if open
               (vec (concat path [:children i]))
               (subvec path 0 (- (count path) 2))))))

(defn read-next [state]
  (let [state (skip-space state)
        [char] (:text state)]
    (cond
      (nil? char) state
      (paren? char) (on-paren state)
      :else (on-atom state))))

(defn read* [text]
  (loop [state (init-state text)]
    (if (= "" (:text state))
      state
      (recur (read-next state)))))

(defn read [text]
  (-> (read* text)
      (get-in [:nodes 0 :children 0]))) ;; assume a top-level list

(defn walk [node]
  (cons node (flatten (map walk (:children node)))))

;;----------------------------------------------------------------------
;; Lookup
;;----------------------------------------------------------------------

(defn node-from-path [src path]
  (if (seq path)
    (node-from-path
      (nth (:children src) (first path))
      (next path))
    src))

;;----------------------------------------------------------------------
;; Paths
;;----------------------------------------------------------------------

(defn common-ancestor [a b]
  (when (and a b (= (first a) (first b)))
    (cons (first a) (common-ancestor (next a) (next b)))))

(defn descendant? [a b]
  (and a b (= b (take (count b) a))))

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

; params {focus, depth, width, lines}
;  - focus (path)
;  - depth (max depth)
;  - width (max width)
;  - lines (max lines) => if on last allowable line (width-3)
;    - 3 limits (top, focus, global)

; what the focus ancestors might look like when top max lines is at limit
; upper bound might have to subtract the number of focus ancestors?
; (foo ...
;  (bar ...
;   (baz ...

; TODO: pprint-fit-width? (allows early exit without descending too far)
(defn pprint-fit-width? [node params])

; if forced but doesn't fit, try again with width-3 for '...', but use '&' only one unprinted list
(defn pprint-force-single? [node params]
  (and (not (descendant? (:path node) (:focus params)))
       (not (descendant? (:focus params) (:path node)))))

(defn pprint-node [node params])
