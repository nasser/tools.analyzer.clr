(ns clojure.tools.analyzer.clr.util)

;; TODO replace with error multimethod
(defmacro throw! [& e]
  `(throw (Exception. (str ~@e))))