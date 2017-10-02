;; Pretty-print a tree created by pres.reader
;; (uses "fisheye" technique to compress expressions that are out of focus)
(ns pres.pprint
  (:require
    [pres.reader :refer [descendant? print-node above?]]
    [clojure.string :as string]))

(declare pprint*)

(defn in-focus? [path focus]
  (or (descendant? path focus)
      (descendant? focus path)))

(defn path->lines-type [path focus]
  (cond
    (descendant? path focus) :focus-lines
    (above? path focus) :pre-lines
    :else :post-lines))

(defn pprint-text
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}
   own-line?]
  (when (<= (count text) width)
    ; final result
    {:pretty text
     :path path
     :limits (cond-> limits
               own-line? (update (path->lines-type path focus) dec))}))

(defn inline-children
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}
   own-line?]
  (loop [[child & next-children] children
         limits limits
         results []]
    (when (>= (:width limits) 0)
      (if-not child

        ; final result
        {:children results
         :pretty (string/join " " (map :pretty results))
         :limits (cond-> limits
                   own-line? (update (path->lines-type path focus) dec))}

        ; process child
        (when-let [result (pprint* child limits false)]
          (let [child-width (count (:pretty result))
                space-width (if (seq results) 1 0)]
            (recur
              next-children
              (update limits :width - child-width space-width)
              (conj results result))))))))

(defn inline-children-truncated
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}
   own-line?]
  (when (>= width (count "& ..."))
    (loop [[child & next-children] children
           limits (update limits :width - (count " ..."))
           results []]
      (let [result (pprint* child limits false)]
        (if-not result

          ; final result
          {:children results
           :pretty (string/join " " (conj (mapv :pretty results) "..."))
           :limits (cond-> limits
                     own-line? (update (path->lines-type path focus) dec))}

          ; process child
          (let [child-width (count (:pretty result))
                space-width (if (seq results) 1 0)]
            (recur
              next-children
              (update limits :width - child-width space-width)
              (conj results result))))))))

(def inline-first-arg?
  #{"lambda" "prog" "setq"})

(defn children-in-render-order
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}]
  (if-let [focus-child (->> children
                            (filter #(descendant? focus (:path %)))
                            (first))]
    (let [i (last (:path focus-child))
          pre (filter #(< 0 (last (:path %)) i) children)
          post (filter #(> (last (:path %)) i) children)]
      (concat
        (distinct [(first children) focus-child])
        (reverse pre)
        post))
    children))

(defn ellipsis-node [a b]
  ; all indexes i such that a<i<b
  {:ellipsis (set (range (inc a) b))})

(defn line-per-child
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}]
  (let [indent (if (:paren (first children)) 1 2)
        line-sep (apply str "\n" (repeat indent " "))]

    (loop [[child & next-children] (children-in-render-order node limits)
           results []
           limits limits
           first-arg-inline? false]
      (if-not child

        ; create final result
        (let [results (vec (sort-by #(last (:path %)) results))
              finalize
              (fn [{:keys [prev-i pretty new-children] :as result}
                   {:keys [path] :as child}]
                (let [i (last path)
                      s (string/join line-sep (string/split-lines (:pretty child)))
                      mid-gap? (and prev-i (not= i (inc prev-i)))
                      end? (= child (last results))
                      end-i (dec (count children))
                      end-gap? (and end? (not= i end-i))
                      new-children (cond-> new-children
                                    mid-gap? (conj (ellipsis-node prev-i i))
                                    true (conj child)
                                    end-gap? (conj (ellipsis-node i (inc end-i))))
                      pretty (cond
                               (= i 0)                         s
                               (and (= i 1) first-arg-inline?) (str pretty " " s)
                               mid-gap?                        (str pretty line-sep "..." line-sep s)
                               :else                           (str pretty line-sep s))
                      pretty (cond-> pretty end-gap? (str " ..."))]
                  (assoc result
                    :prev-i i
                    :pretty pretty
                    :new-children new-children)))

              final (reduce finalize
                      {:new-children []}
                      results)]

          {:children (:new-children final)
           :limits limits
           :pretty (:pretty final)})

        ; process child
        (let [w (- width (if (seq results) indent 0))

              ; special case, allow the first arg of certain functions to be inline
              ; e.g. `bar` is inline here: `(foo bar`
              inline? (and (= 1 (last (:path child)))
                           (inline-first-arg? (:text (first children))))]

          (if-let [result (pprint* child (assoc limits :width w) (not inline?))]

            ; child fits
            (recur next-children
              (conj results result)
              (:limits result)
              (or inline? first-arg-inline?))

            ; child does not fit
            (recur next-children
              results
              limits
              first-arg-inline?)))))))

(defn pprint-list
  [{:keys [text paren children path] :as node}
   {:keys [focus width lines depth focus-depth] :as limits}
   own-line?]
  (let [focus? (in-focus? path focus)
        depth-key (if focus? :focus-depth :depth)
        dec-depth? (if focus? (>= (count path) (count focus)) true)]
    (if (zero? (limits depth-key))
      (when (>= width (count "&"))
        {:pretty "&"
         :path path
         :limits (cond-> limits
                   own-line? (update (path->lines-type path focus) dec))})
      (when (>= width (count "()"))
        (let [limits (cond-> limits
                       true (update :width - (count "(")) ; FIXME: ")" is only present for single line
                       dec-depth? (update depth-key dec))
              result (or (inline-children node limits own-line?)
                         (if (in-focus? path focus)
                           (line-per-child node limits)
                           (inline-children-truncated node limits own-line?)))]
          (when result
            (assoc result
              :path path
              :pretty (str "(" (:pretty result) ")")
              :limits (assoc (:limits result)
                        :depth depth
                        :focus-depth focus-depth))))))))

(defn pprint*
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}
   own-line?]
  (let [lines-type (path->lines-type path focus)]
    (when-let [line-fit? (or (not own-line?)
                             (pos? (limits lines-type)))]
      (when (pos? width)
        (if text
          (pprint-text node limits own-line?)
          (pprint-list node limits own-line?))))))

(defn normalize-limits
  [{:keys [focus lines pre-lines focus-lines] :as limits}]
  (let [pre-lines (max pre-lines (count focus))
        lines (max lines (+ pre-lines focus-lines))]
    (assoc limits
      :pre-lines pre-lines
      :post-lines (- lines pre-lines focus-lines)
      :lines lines)))

(defn pprint [node limits]
  (let [limits (normalize-limits limits)]
    (pprint* node limits true)))
