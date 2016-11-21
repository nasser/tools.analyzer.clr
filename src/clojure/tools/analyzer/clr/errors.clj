(ns clojure.tools.analyzer.clr.errors
  (:require [clojure.tools.analyzer.clr.types :refer [clr-type]]))

;; TODO different Exception types?
(defmacro throw! [& e]
  `(throw (Exception. (str ~@e))))

(defn user-form
  "The form as the user typed it"
  [{:keys [raw-forms form]}]
  (if raw-forms
    ;; TODO is it always the first?
    (first raw-forms)
    form))

(defmulti error
  "Throw an error"
  (fn [err data] err))

(defmethod error :default
  [err ast]
  (throw! "Unknown error " err " with AST " ast))

(defmethod error ::missing-type
  [err {:keys [type] :as ast}]
  (throw! "Could not find type " type " while analyzing form " (user-form ast)))

(defmethod error ::missing-constructor-arity
  [err {:keys [args class] :as ast}]
  (throw! "Could not find constructor for " (clr-type class)
          " taking " (count args)
          " arguments while analyzing form " (user-form ast)))

(defmethod error ::missing-static-zero-arity
  [err {:keys [field m-or-f target] :as ast}]
  (throw! "Could not find static method, field, or property " (or m-or-f field)
          " for type " (clr-type target)
          " while analyzing form " (user-form ast)))

(defmethod error ::missing-static-method
  [err {:keys [m-or-f args target] :as ast}]
  (throw! "Could not find static method " m-or-f
          " with args " (mapv clr-type args)
          " for type " (clr-type target)
          " while analyzing form " (user-form ast)))

(defmethod error ::missing-instance-zero-arity
  [err {:keys [field m-or-f target] :as ast}]
  (throw! "Could not find instance method, field, or property " (or m-or-f field)
          " for type " (clr-type target)
          " while analyzing form " (user-form ast)))

(defmethod error ::missing-instance-method
  [err {:keys [method args target] :as ast}]
  (throw! "Could not find instance method " method
          " with args " (mapv clr-type args)
          " for type " (clr-type target)
          " while analyzing form " (user-form ast)))

(defmethod error ::missing-instance-method-arity
  [err {:keys [method args target] :as ast}]
  (throw! "Could not find overload of instance method " method
          " taking " (count args) " arguments"
          " for type " (clr-type target)
          " while analyzing form " (user-form ast)))

