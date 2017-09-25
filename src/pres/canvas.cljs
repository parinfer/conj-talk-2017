(ns pres.canvas
  (:require
    [oops.core :refer [ocall oget oset!]]))

(def canvas (ocall js/document "querySelector" "canvas"))
(def ctx (ocall canvas "getContext" "2d"))
