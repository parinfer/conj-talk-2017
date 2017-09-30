;; Pretty-print a tree created by pres.reader
;; (uses "fisheye" technique to compress expressions that are out of focus)
(ns pres.pprint
  (:require
    [pres.reader :refer [descendant? print-node below?]]
    [clojure.string :as string]))

(declare pprint*)

(declare in-focus?)
(defn debug [node limits]
  (str (print-node node) " " (:context-lines limits) (in-focus? node limits)))

(defn in-focus?
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}]
  (or (descendant? path focus)
      (descendant? focus path)))

(defn path->lines-type
  [{:keys [path] :as node}
   {:keys [focus] :as limits}]
  (cond
    (descendant? path focus) :focus-lines
    (above? path focus) :pre-lines
    :else :post-lines))

(defn pprint-text
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}
   own-line?]
  ; (println "pprint-text" (debug node limits))
  (let [lines-type (path->lines-type node limits)]
    (when-let [line-fit? (or (not own-line?)
                             (pos? (limits lines-type)))]
      (when (<= (count text) width)
        ; final result
        {:pprint text
         :orig-path path
         :limits (cond-> limits
                   own-line? (update lines-type dec))}))))

(defn inline-children
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}]
  (loop [w width
         [child & next-children] children
         results []]
    (when (>= w 0)
      (if-not child

        ; final result
        {:children results
         :pprint (string/join " " (map :pprint results))
         :limits limits}

        ; process child
        (when-let [result (pprint* child (assoc limits :width w) false)]
          (recur
            (- w (count result) (if (seq results) 1 0)) ; new width w/ space after previous sibling
            next-children
            (conj results result)))))))

(defn inline-children-truncated
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}]
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
           :limits limits}

          ; process child
          (recur
            (- w (count result) (if (seq results) 1 0))
            next-children
            (conj results result)))))))

(def inline-first-arg?
  #{"lambda" "prog" "setq"})

(defn line-per-child
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}]
  ; (println "line-per-child of" (debug node limits))
  (let [indent (if (:paren (first children)) 1 2)
        line-sep (apply str "\n" (repeat indent " "))]

    ; TODO: if proper ancestor of focus, create new children in order:
    ;       1. first child
    ;       2. focus child
    ;       3. children before focus child (reversed)
    ;       4. children after focus child
    ; (just sort the results by original index when done)

    (loop [[child & next-children] children
           results []
           limits limits
           ellipsis nil
           first-arg-inline? false]
      (if-not child

        ; create final result
        (let [[func arg & others] results
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
              inline? (and (= 1 (count results))
                           (let [func (:pprint (first results))]
                             (inline-first-arg? func)))]

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
  ; TODO: have inline decrement the line limits
  ; (println "pprint-list" (debug node limits))
  (let [focus? (in-focus? node limits)
        depth-key (if focus? :focus-depth :depth)
        dec-depth? (if focus? (>= (count path) (count focus)) true)]
    (if (zero? (limits depth-key))
      (when (>= width (count "&"))
        {:pprint "&"
         :orig-path path
         :limits limits})
      (when (>= width (count "()"))
        (let [limits (cond-> limits
                       true (update :width - (count "()"))
                       dec-depth? (update depth-key dec))
              result (or (inline-children node limits)
                         (if (in-focus? node limits)
                           (line-per-child node limits)
                           (inline-children-truncated node limits)))]
          (when result
            (assoc result
              :orig-path path
              :pprint (str "(" (:pprint result) ")"))))))))

(defn pprint*
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}
   own-line?]
  ; (println "pprint" (debug node limits))
  (when (pos? width)
    (if text
      (pprint-text node limits own-line?)
      (pprint-list node limits own-line?))))

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
