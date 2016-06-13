(ns clojure.tools.analyzer.clr
  (:refer-clojure :exclude [macroexpand-1])
  (:require [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.ast :refer [postwalk]]
            [clojure.tools.analyzer.passes :refer [schedule]]
            [clojure.tools.analyzer.env :refer [*env* with-env] :as env]
            [clojure.tools.analyzer.utils :refer [resolve-sym]]
            [clojure.tools.analyzer.clr
             [types :refer [clr-type]]]))

(defn desugar-host-expr [[op & expr :as form]]
  (if (symbol? op)
    (let [opname (name op)]
      (cond

       (= (first opname) \.) ; (.foo bar ..)
       (let [[target & args] expr
             args (list* (symbol (subs opname 1)) args)]
         (with-meta (list '. target (if (= 1 (count args)) ;; we don't know if (.foo bar) ia
                                      (first args) args)) ;; a method call or a field access
           (meta form)))

       (= (last opname) \.) ;; (class. ..)
       (with-meta (list* 'new (symbol (subs opname 0 (dec (count opname)))) expr)
         (meta form))

       :else form))
    form))

(defn macroexpand-1 [form env]
  (if (seq? form)
    (let [op (first form)]
      (if (ana/specials op)
        form
        (let [v (resolve-sym op env)]
          (if (and (not (-> env :locals (get op))) ;; locals cannot be macros
                   (:macro (meta v)))
            (apply v form env (rest form)) ; (m &form &env & args)
            (desugar-host-expr form)))))
        form))

(defn empty-env
  "Returns an empty env map"
  []
  {:context    :ctx/expr
   :locals     {}
   :ns         (ns-name *ns*)})

(def e1 (atom {:namespaces {'user         {:mappings (ns-map 'clojure.core)
                                           :aliases  {}
                                           :ns       'user}
                            'clojure.core {:mappings (ns-map 'clojure.core)
                                           :aliases {}
                                           :ns      'clojure.core}}}))

(def public-static (enum-or BindingFlags/Static BindingFlags/Public))

(defn class-for-name [s]
  (clojure.lang.RT/classForName (str s)))

(defn analyze-type
  "Analyze foo into a type"
  {:pass-info {:walk :post :depends #{} :after #{}}}
  [{:keys [op class] :as ast}]
  (if (= :maybe-class op)
    (if-let [t (class-for-name (name class))]
      (merge (dissoc ast :class)
             {:op :type
              :type t})
      (throw (Exception. (str "Could not find type " class))))
    ast))

(defn analyze-host-form
  "Analyze Foo/Bar into static field, method or property"
  ;; TODO deal with unfound fields
  {:pass-info {:walk :post :depends #{} :after #{}}}
  [{:keys [class field op] :as ast}]
  (if (= :maybe-host-form op)
    (if-let [t (class-for-name class)]
      (merge (dissoc ast :class :field)
             (let [methods (->> (.GetMethods t public-static)
                                (filter #(= (str field) (.Name %))))]
               (when-not (empty? methods) 
                 {:op :static-method
                  :methods methods}))
             (when-let [field-info (.GetField t (str field)
                                              public-static)]
               {:op :static-field
                :field field-info})
             (when-let [property-info (.GetProperty t (str field)
                                                    public-static)]
               {:op :static-property
                :property property-info}))
      (throw (Exception. (str "Could not find type " class))))
    ast))

(defn analyze-constructor
  {:pass-info {:walk :post :depends #{} :after #{}}}
  [{:keys [args class op] :as ast}]
  (if (= :new op)
    (let [t (:type (analyze-type class))]
      (merge (dissoc ast :class)
             {:type t}
             (if-let [ctor-info (.GetConstructor t (->> args
                                                        (map clr-type)
                                                        (into-array Type)))]
               {:constructor ctor-info}
               (if (.IsValueType t)
                 {:op :initobj}
                 (throw (Exception. (str "Could not find constructor for type " t " with args " args)))))))
    ast))

(def default-passes
  #{#'analyze-type
    #'analyze-host-form
    #'analyze-constructor})

(def scheduled-default-passes
  (schedule default-passes))

(defn run-passes [ast]
  (scheduled-default-passes ast))

(defn analyze
  ([form] (analyze form (empty-env)))
  ([form env]
   (binding [ana/macroexpand-1 macroexpand-1
             ana/create-var    (fn [sym env]
                                 (doto (intern (:ns env) sym)
                                   (reset-meta! (meta sym))))
             ana/parse         ana/-parse
             ana/var?          var?]
     (with-env e1 (run-passes (ana/analyze form env))))))