(ns clojure.tools.analyzer.clr.util)

(defmacro throw! [& e]
  `(throw (Exception. (str ~@e))))