(ns pres.editors.bbn
  (:require
    [pres.canvas :refer [ctx]]
    [pres.reader :refer [read]]
    [oops.core :refer [ocall oget oset!]]))

(defn draw []
  (ocall ctx "fillRect" 0 0 800 400))

(defn init! [])
