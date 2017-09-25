(ns pres.reader)

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
