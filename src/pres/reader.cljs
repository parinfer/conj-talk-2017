(ns pres.reader)

;; take a string, return a tree
;; tree contains tokens and their positions positions of tokens

;; node structure
;; :type "(" "[" "{" or nil for atom
;; :value nodes or string
;; :xy [start, end] of {:line, :x}

(defn init-state [text]
  {:xy [0 0]
   :nodes []
   :path []
   :text text})

(defn skip-space [{:keys [xy text] :as state}]
  (let [[x y] xy
        [char] text
        new-xy (case char " " [(inc x) y] "\n" [0 (inc y)] xy)]
    (if (= new-xy xy)
      state
      (recur (assoc state :xy new-xy :text (subs text 1))))))

(defn on-atom [{:keys [xy text nodes path] :as state}]
  (let [word (re-find #"[^\s]+" text)
        dx (len word)
        [x y] xy
        node {:value word :xy xy}]
    (assoc state
      :xy [(+ x dx) y]
      :text (sub text dx)
      :nodes (update-in nodes (conj path :value) conj node))))

(def paren? #{"(" "{" "[" "]" "}" ")"})
(def opener? #{"(" "{" "["})

(defn list-node [type xy]
  {:type type
   :xy xy
   :value []})

(defn on-paren [{:keys [xy text nodes path]}]
  (let [[char] (:text state)
        open (opener? char)]
       [x y] xy
    (assoc state
      :text (subs text 1)
      :xy [(inc x) y]
      :nodes (if open
               (update-in nodes (conj path :value) conj (list-node char xy))
               (assoc-in nodes (conj path :xy-end) xy))
      :path (if open
              (let [i (len (get-in nodes (concat path :value)))]
                (concat path [:value i]))
              (subvec path 0 (- (len path) 2))))))

(defn read-next [state]
  (let [state (skip-space state)
        [char] (:text state)]
    (cond
      (nil? char) state
      (paren? char) (on-paren state)
      :else (on-atom state))))

(defn read [text]
  (loop [state (init-state text)]
    (if (nil? (:text state))
      state
      (recur state))))
