;; Pretty-print a tree created by pres.reader
;; (uses "fisheye" technique to compress expressions that are out of focus)
(ns pres.pprint
  (:require
    [pres.reader :refer [descendant? print-node]]
    [clojure.string :as string]))

; params {focus, depth, width, lines}
;  - focus (path)
;  - depth (max depth)
;  - wrap-width (return nil as soon as it prints past width, so we can wrap to multi-line)
;  - trunc-w (abbreviate to fit)
;  - lines (max lines) => if on last allowable line (width-3)
;    - 3 limits (top, focus, global)

; what the focus ancestors might look like when top max lines is at limit
; upper bound might have to subtract the number of focus ancestors?
; (foo ...
;  (bar ...
;   (baz ...

(declare pprint)

(defn debug [node limits]
  (str (print-node node) " " (:width limits)))

(defn truncate-at-width?
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}]
  ; (println "truncate-at-width?" (debug node limits))
  (and (not (descendant? path focus))
       (not (descendant? focus path))))

(defn pprint-text
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}]
  ; (println "pprint-text" (debug node limits))
  (when (<= (count text) width)
    text))

(defn inline-children
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}]
  ; (println "inline-children" (debug node limits))
  (loop [w width
         [child & next-children] children
         results []]
    (when (>= w 0)
      (if-not child
        (string/join " " results)
        (when-let [result (pprint child (assoc limits :width w))]
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
      (let [result (pprint child (assoc limits :width w))]
        (if-not result
          (string/join " " (conj results "..."))
          (recur
            (- w (count result) (if (seq results) 1 0))
            next-children
            (conj results result)))))))

(def inline-first-arg?
  #{"lambda" "prog" "setq"})

(defn line-per-child
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}]
  ; (println "line-per-child" (debug node limits))
  (let [indent (if (:paren (first children)) 1 2)
        line-sep (apply str "\n" (repeat indent " "))]
    (loop [[child & next-children] children
           result-lines []]
      (if-not child
        (let [[func arg & others] result-lines
              both (str func " " arg)
              result-lines (if (and (inline-first-arg? func)
                                    (>= width (count both)))
                             (cons both others)
                             result-lines)]
          (string/join line-sep result-lines))
        (let [w (- width (if (seq result-lines) indent 0))]
          (when-let [result (pprint child (assoc limits :width w))]
            (recur
               next-children
               (concat result-lines (string/split-lines result)))))))))

(defn pprint-list
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}]
  ; (println "pprint-list" (debug node limits))
  (if (zero? depth)
    (when (>= width (count "&"))
      "&")
    (when (>= width (count "()"))
      (let [limits (assoc limits
                     :width (- width (count "()"))
                     :depth (dec depth))
            children-str (or (inline-children node limits)
                             (if (truncate-at-width? node limits)
                               (inline-children-truncated node limits)
                               (line-per-child node limits)))]
        (when children-str
          (str "(" children-str ")"))))))

(defn pprint
  [{:keys [text paren children path] :as node}
   {:keys [focus depth width lines] :as limits}]
  ; (println "pprint" (debug node limits))
  (when (pos? width)
    (if text
      (pprint-text node limits)
      (pprint-list node limits))))
