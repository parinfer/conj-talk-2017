(ns org.processingjs.Processing
  (:require ["processing-js" :as processing-js]))

;; original provided here: https://github.com/quil/processing-js/blob/master/resources/deps.cljs
;; remapping to: https://github.com/processing-js/processing-js

;; Processing is already exported since it is not a proper UMD
;; (js/goog.exportSymbol "Processing" processing-js)
