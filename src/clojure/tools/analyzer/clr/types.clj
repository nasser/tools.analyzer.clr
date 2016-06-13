(ns clojure.tools.analyzer.clr.types
  (:refer-clojure :exclude [resolve])
  (:require [clojure.tools.analyzer.clr
             [util :refer [throw!]]
             [reflection :refer [find-method]]]))

(defn superchain [t]
  (if-let [b (.BaseType t)]
    (cons b (superchain b))))

(defn zero-arity-type [class name]
  (or (if-let [info (.GetField class name)]
        (.FieldType info))
      (if-let [info (.GetProperty class name)]
        (.PropertyType info))
      (if-let [info (.GetMethod class name Type/EmptyTypes)]
        (.ReturnType info))))

(defn n-arity-type [class method params]
  (if-let [info (.GetMethod class name params)]
    (.ReturnType info)))

(defn zero-arity-info [class name]
  (or (.GetField class name)
      (.GetProperty class name)
      (.GetMethod class name Type/EmptyTypes)))

(defn resolve
  ([t]
   (or (clojure.core/resolve t)
       (throw! "Could not resolve " t " as  type.")))
  ([t ast]
   (or (clojure.core/resolve t)
       (throw! "Could not resolve " t " as  type in " (:form ast)))))

(defmulti clr-type
  "The CLR type of an AST node"
  :op)

(defmethod clr-type :default [ast]
  (throw! "clr-type not implemented for " (pr-str ast)))

(defmethod clr-type :maybe-class
  [{:keys [class] :as ast}]
  (resolve class ast))

(defmethod clr-type :var [ast]
  Object)

(defmethod clr-type :the-var [ast]
  Object)

(defmethod clr-type :const [ast]
  (-> ast :val type))

(defmethod clr-type :vector [ast]
  clojure.lang.IPersistentVector)

(defmethod clr-type :set [ast]
  clojure.lang.IPersistentSet)

(defmethod clr-type :map [ast]
  clojure.lang.IPersistentMap)

(defmethod clr-type :invoke
  [{:keys [fn args] {:keys [op]} :fn}]
  (condp = op
    ;; (class/field args...)
    :maybe-host-form
    (let [{:keys [class field]} fn
          method (or (.GetMethod (resolve class)
                                 (str field)
                                 (into-array (map clr-type args)))
                     (throw! "Could not find method " class "/" field " matching types"))]
      (.ReturnType method))
    
    ;; (fn args...)
    :var
    (resolve (or (->> fn
                      :meta
                      :arglists
                      (filter #(= (count %) (count args)))
                      first
                      meta
                      :tag)
                 'Object))
    
    (clr-type fn)
    ; (throw! "Invoking " op " not supported")
    ))

(defmethod clr-type :new [ast]
  (-> ast :class :class resolve))

(defmethod clr-type :maybe-host-form
  [{:keys [class field] :as ast}]
  (let [class (resolve class)]
    (or (zero-arity-type class (str field))
        (throw! "Maybe host form type " (:form ast) " not supported"))))

(defmethod clr-type :host-interop
  [{:keys [m-or-f target] :as ast}]
  (let [target-type (clr-type target)]
    (or (zero-arity-type target-type (str m-or-f))
        (throw! "Host interop " (:form ast) " not supported"))))

(defmethod clr-type :binding [ast]
  (or
    (if-let [init (:init ast)]
      (clr-type init))
    (if-let [tag (-> ast :name meta :tag)]
      (resolve tag))
    Object))

(defmethod clr-type :local
  [{:keys [name local arg-id] {:keys [locals]} :env}]
  (if (= local :arg)
    (if-let [tag (-> name meta :tag)]
      (resolve tag)
      Object)
    (clr-type (locals name))))

(defmethod clr-type :let [ast]
  (-> ast :body :ret clr-type))

(defmethod clr-type :if
  [{:keys [test then else] :as ast}]
  (let [test-type (clr-type test)
        else-type (clr-type else)]
    (if (= test-type else-type)
      test-type
      ;; TODO compute common type  
      Object)))

;; TODO warn on truncate?
(defn convertable? [from to]
  (or (and (nil? from) (nil? to))
      (= from to)
      (and (nil? from) (not (.IsValueType to)))
      (= to Boolean)
      (and (= System.Void from) (not (.IsValueType to)))
      (find-method from "op_Implicit" to)
      (find-method from "op_Explicit" to)
      (and (.IsPrimitive from) (.IsPrimitive to))
      (and (.IsValueType from) (= to Object))
      (and (= from Object) (.IsValueType to))
      (.IsSubclassOf to from)
      (.IsSubclassOf from to)))

(defn specificity [sig]
  (->> (.GetParameters sig)
       (map #(-> (.ParameterType %) superchain count))
       (apply +)))

;; TODO sort by distance between sig and params, not specificity 
(defn matching-signatures [sigs params]
  (->> sigs
       (filter (fn [sig]
                 (let [sig-params (map #(.ParameterType %) (.GetParameters sig))]
                   (and (= (count params)
                           (count sig-params))
                        (every? true? (map convertable? params sig-params))))))
       (sort-by specificity)
       reverse))

(defn matching-methods [type name params]
  (let [sigs (filter #(= name (.Name %)) (.GetMethods type))]
    (matching-signatures sigs params)))

(defn matching-constructors [type params]
  (matching-signatures (.GetConstructors type) params))