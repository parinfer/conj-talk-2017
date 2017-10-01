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
  ; (println "pprint-text" (debug node limits))
  (when (<= (count text) width)
    ; final result
    {:pprint text
     :path path
     :limits (cond-> limits
               own-line? (update (path->lines-type path focus) dec))}))

(defn inline-children
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}
   own-line?]
  (loop [w width
         [child & next-children] children
         results []]
    (when (>= w 0)
      (if-not child

        ; final result
        {:children results
         :pprint (string/join " " (map :pprint results))
         :limits (cond-> limits
                   own-line? (update (path->lines-type path focus) dec))}

        ; process child
        (when-let [result (pprint* child (assoc limits :width w) false)]
          (recur
            (- w (count result) (if (seq results) 1 0)) ; new width w/ space after previous sibling
            next-children
            (conj results result)))))))

(defn inline-children-truncated
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}
   own-line?]
  ; (println "inline-children-truncated" (debug node limits))
  (when (>= width (count "& ..."))
    (loop [w (- width (count "..."))
           [child & next-children] children
           results []]
      (let [result (pprint* child (assoc limits :width w) false)]
        (if-not result

          ; final result
          {:children results
           :pprint (string/join " " (map :pprint (conj results "...")))
           :limits (cond-> limits
                     own-line? (update (path->lines-type path focus) dec))}

          ; process child
          (recur
            (- w (count result) (if (seq results) 1 0))
            next-children
            (conj results result)))))))

(def inline-first-arg?
  #{"lambda" "prog" "setq"})

(defn children-in-render-order
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}]
  (let [[first-child & others] children]
    (if-let [focus-child (first (filter #(descendant? focus (:path %))))]
      (let [i (last (:path focus-child))
            pre (filter #(< 0 (last (:path %)) i) children)
            post (filter #(> (last (:path %)) i) children)]
        (concat
          (distinct [first-child focus-child])
          (reverse pre)
          post))
      children)))

(defn line-per-child
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}]
  ; (println "line-per-child of" (debug node limits))
  (let [indent (if (:paren (first children)) 1 2)
        line-sep (apply str "\n" (repeat indent " "))]

    (loop [[child & next-children] (children-in-render-order node limits)
           results []
           limits limits
           ellipsis nil
           first-arg-inline? false]
      (if-not child

        ; create final result
        ; TODO: insert an ellipsis inside each gap in index (append to previous line)
        (let [[func arg & others] (sort #(last (:path %)) results)
              both (str (:pprint func) " " (:pprint arg))
              result-lines (apply concat (map #(string/split-lines (:pprint %)) children))
              result-lines (if first-arg-inline? ; NOTE: assuming (>= width (count both))
                             (cons both (drop 2 result-lines))
                             result-lines)]
          {:children results
           :pprint (string/join line-sep result-lines)
           :limits limits})

        ; process child
        (let [w (- width (if (seq results) indent 0))

              ; special case, allow the first arg of certain functions to be inline
              ; e.g. `bar` is inline here: `(foo bar`
              inline? (and (= 1 (last (:path child)))
                           (inline-first-arg? (:text (first children))))]

          (if-let [result (pprint* child (assoc limits :width w) (not inline?))]

            ; child fits
            (recur
              next-children
              (conj results result)
              (:limits result)
              false
              (or inline? first-arg-inline?))

            ; child does not fit
            (recur
              next-children
              results
              limits
              true
              first-arg-inline?)))))))

(defn pprint-list
  [{:keys [text paren children path] :as node}
   {:keys [focus width lines] :as limits}
   own-line?]
  ; (println "pprint-list" (debug node limits))
  (let [focus? (in-focus? path focus)
        depth-key (if focus? :focus-depth :depth)
        dec-depth? (if focus? (>= (count path) (count focus)) true)]
    (if (zero? (limits depth-key))
      (when (>= width (count "&"))
        {:pprint "&"
         :path path
         :limits (cond-> limits
                   own-line? (update (path->lines-type path focus) dec))})
      (when (>= width (count "()"))
        (let [limits (cond-> limits
                       true (update :width - (count "()")) ; FIXME: ")" is only present for single line
                       dec-depth? (update depth-key dec))
              result (or (inline-children node limits own-line?)
                         (if (in-focus? path focus)
                           (line-per-child node limits)
                           (inline-children-truncated node limits own-line?)))]
          (when result
            (assoc result
              :path path
              :pprint (str "(" (:pprint result) ")"))))))))

(defn pprint*
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}
   own-line?]
  ; (println "pprint" (debug node limits))
  (let [lines-type (path->lines-type path focus)]
    (when-let [line-fit? (or (not own-line?)
                             (pos? (limits lines-type)))]
      (when (pos? width)
        (if text
          (pprint-text node limits own-line?)
          (pprint-list node limits own-line?))))))

(defn normalize-limits
  [{:keys [focus lines pre-lines focus-lines] :as limits}]
  (let [pre-lines (- (max pre-lines (count focus))
                     (count focus))
        lines (max lines (+ pre-lines focus-lines))]
    ; (println (count focus) context-lines)
    (assoc limits
      :pre-lines pre-lines
      :post-lines (- lines pre-lines focus-lines)
      :lines lines)))

(defn pprint [node limits]
  (pprint* node (normalize-limits limits) false))
