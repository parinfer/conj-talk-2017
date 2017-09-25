(ns pres.reader)

(defn init-state [text]
  {:xy [0 0]
   :nodes [{:type :root, :value []}]
   :path [0]
   :text text})

(defn list-node [paren xy]
  {:type paren
   :xy xy
   :value []})

(defn atom-node [text xy]
  {:xy xy
   :value text})

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
        node (atom-node word xy)]
    (assoc state
      :xy [(+ x dx) y]
      :text (subs text dx)
      :nodes (update-in nodes (conj path :value) conj node))))

(def paren? #{"(" "{" "[" "]" "}" ")"})
(def opener? #{"(" "{" "["})

(defn on-paren [{:keys [xy text nodes path] :as state}]
  (let [[char] text
        open (opener? char)
        [x y] xy]
    (assoc state
      :text (subs text 1)
      :xy [(inc x) y]
      :nodes (if open
               (update-in nodes (conj path :value) conj (list-node char xy))
               (assoc-in nodes (conj path :xy-end) xy))
      :path (if open
              (let [i (count (get-in nodes (conj path :value)))]
                (vec (concat path [:value i])))
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
  (:nodes (read* text)))
