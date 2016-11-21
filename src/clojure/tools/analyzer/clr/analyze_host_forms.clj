(ns clojure.tools.analyzer.clr.analyze-host-forms
  (:require
    [clojure.tools.analyzer.passes
     [uniquify :refer [uniquify-locals]]]
    [clojure.tools.analyzer.clr
     [errors :refer [error] :as errors]
     [types :refer [clr-type class-for-name]]]))

(def public-instance (enum-or BindingFlags/Instance BindingFlags/Public))
(def public-static (enum-or BindingFlags/Static BindingFlags/Public))
(def public-instance-static (enum-or BindingFlags/Instance BindingFlags/Static BindingFlags/Public))

(defn ensure-class [c form]
  (or (class-for-name c)
      (error
        ::errors/missing-type
        {:type c :form form})))

(defn analyze-type
  "Analyze foo into a type"
  {:pass-info {:walk :post :depends #{} :after #{#'uniquify-locals}}}
  [{:keys [op children class] :as ast}]
  (if (= :maybe-class op)
    (let [target-type (ensure-class (name class) (:form ast))]
      (merge (dissoc ast :class)
             {:children (vec (remove #(= % :class) children))}
             {:op :const
              :type :class
              :literal? true
              :val target-type
              :form target-type}))
    ast))

(defn analyze-host-field
  "Analyze (.-Bar Foo) Foo/Bar (. Foo -Bar) into field, method group or property"
  {:pass-info {:walk :post :depends #{} :after #{#'uniquify-locals}}}
  [{:keys [field target op] :as ast}]
  (if (= :host-field op)
    (let [target-type (clr-type target)
          static? (= :class (:type target))
          binding-flags (if static? public-static public-instance)
          ast* (merge (dissoc ast :field)
                      ;; TODO is it ever a method?
                      (when-let [methods (.GetMethod target-type
                                                     (str field)
                                                     binding-flags 
                                                     nil
                                                     Type/EmptyTypes
                                                     nil)]
                        {:op (if static? :static-method :instance-method)
                         :methods methods})
                      (when-let [field-info (.GetField target-type (str field)
                                                       binding-flags)]
                        {:op (if static? :static-field :instance-field)
                         :field field-info})
                      (when-let [property-info (.GetProperty target-type (str field)
                                                             binding-flags)]
                        {:op (if static? :static-property :instance-property)
                         :property property-info}))]
      (if (= :host-field (:op ast*))
        (if static?
          (error ::errors/missing-static-zero-arity ast)
          (assoc ast :inexact? true))
        ast*))
    ast))

;; TODO deal with parameter type conversions 
;; e.g. (System.Collections.ArrayList. 12) should know to cast to int
(defn analyze-constructor
  "Analyze (Foo. a b) into object or valuetype constructor
  produces :new or :initobj"
  {:pass-info {:walk :post :depends #{} :after #{#'uniquify-locals}}}
  [{:keys [args children class op] :as ast}]
  (if (= :new op)
    (let [target-type (clr-type class)]
      ;; TODO OK to drop :class like this? 
      (merge (dissoc ast :class)
             {:type target-type
              :children (vec (remove #(= % :class) children))}
             (if (and (.IsValueType target-type)
                      (empty? args))
               {:op :initobj}
               (if-let [ctor-info (.GetConstructor target-type (->> args
                                                                    (map clr-type)
                                                                    (into-array Type)))]
                 {:constructor ctor-info}
                 ;; no exact match, look for arity match
                 (let [ctors (->> (.GetConstructors target-type)
                                  (filter #(= (count (.GetParameters %))
                                              (count args))))]
                   (if (empty? ctors)
                     (error ::errors/missing-constructor-arity ast)
                     {:inexact? true
                      :constructors ctors}))))))
    ast))

(defn analyze-host-interop
  "Analyze (.foo a) (. a foo) into instance method invocation, field access, or property getter"
  {:pass-info {:walk :post :depends #{} :after #{#'uniquify-locals}}}
  [{:keys [m-or-f args target op] :as ast}]
  (if (= :host-interop op)
    (let [target-type (clr-type target)
          m-or-f (str m-or-f)
          static? (= :class (:type target))
          binding-flags (if static? public-static public-instance)
          ast* (merge ast
                      (when-let [method (.GetMethod target-type (str m-or-f) binding-flags nil Type/EmptyTypes nil)]
                        {:op (if static? :static-method :instance-method)
                         :method method})
                      (when-let [field (.GetField target-type m-or-f binding-flags)]
                        {:op (if static? :static-field :instance-field)
                         :field field})
                      (when-let [property (.GetProperty target-type m-or-f binding-flags)]
                        {:op (if static? :static-property :instance-property)
                         :property property}))
          matched? (not= :host-interop (:op ast*))]
      (cond matched?                      (dissoc ast* :m-or-f)
            (and static? (empty? args))   (error ::errors/missing-static-zero-arity ast)
            static?                       (error ::errors/missing-static-method ast)
            :else                         (assoc ast* :inexact? true)))
    ast))

;; TODO analyze away the identity invoke hack
(defn analyze-host-call
  "Analyze (Foo/Bar a) into static method invocation"
  {:pass-info {:walk :post :depends #{} :after #{#'uniquify-locals}}}
  [{:keys [method target args op] :as ast}]
  (if (= :host-call op)
    (let [target-type (clr-type target)]
      (merge ast
             {:op (case (-> target :type)
                    :class :static-method
                    :instance-method)}
             (if-let [meth (.GetMethod target-type (str method) (->> args
                                                                     (map clr-type)
                                                                     (into-array Type)))]
               {:method meth}
               ;; no exact match, look for arity match
               (let [ctors (->> (.GetMethods target-type)
                                (filter #(and
                                           (= (str method) (.Name %))
                                           (= (count (.GetParameters %))
                                              (count args)))))]
                 (if (empty? ctors)
                   (error ::errors/missing-instance-method-arity ast)
                   {:inexact? true
                    :methods ctors})))))
    ast))
