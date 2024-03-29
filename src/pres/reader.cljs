;; TODO: rename ast
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
  (let [[x y] xy
        x1 (+ x (count text) -1)]
    {:xy xy
     :xy-end [x1 y]
     :text text
     :path (conj (public-path path) i)}))

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

(defn walk
  ([node] (walk node [0]))
  ([node path]
   (cons
     (assoc node :walk-path path)
     (flatten
       (map-indexed
         #(walk %2 (conj path %1))
         (:children node))))))

;;----------------------------------------------------------------------
;; Lookup
;;----------------------------------------------------------------------

(defn node-from-path [src path]
  (when src
    (if (seq path)
      (node-from-path
        (get (:children src) (first path))
        (next path))
      src)))

;;----------------------------------------------------------------------
;; Paths
;;----------------------------------------------------------------------

(defn common-ancestor [& paths]
  (when (and (every? identity paths)
             (apply = (map first paths)))
    (cons (first (first paths))
      (apply common-ancestor (map next paths)))))

(defn parent [a]
  (when-let [p (seq (butlast a))]
    (vec p)))

(defn descendant? [a b]
  (and a b (= b (take (count b) a))))

(defn above? [a b]
  (when-let [[ai bi] (->> (map vector a b)
                          (filter (fn [[ai bi]] (not= ai bi)))
                          (first))]
    (< ai bi)))

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
