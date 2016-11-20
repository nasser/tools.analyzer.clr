(ns clojure.tools.analyzer.clr
  (:refer-clojure :exclude [macroexpand-1])
  (:require [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.ast :refer [postwalk]]
            [clojure.tools.analyzer.passes :refer [schedule]]
            [clojure.tools.analyzer.passes
             [source-info :refer [source-info]]
             [cleanup :refer [cleanup]]
             [elide-meta :refer [elide-meta elides]]
             [warn-earmuff :refer [warn-earmuff]]
             [collect-closed-overs :refer [collect-closed-overs]]
             [add-binding-atom :refer [add-binding-atom]]
             [uniquify :refer [uniquify-locals]]]
            [clojure.tools.analyzer.env :refer [*env* with-env] :as env]
            [clojure.tools.analyzer.utils :refer [resolve-sym ctx -source-info resolve-ns obj? dissoc-env]]
            [clojure.tools.analyzer.clr
             [errors :refer [error] :as error]
             [types :refer [clr-type]]]
            :reload))

(defn class-for-name [s]
  (if s
    (clojure.lang.RT/classForName (str s))))

(defn ensure-class [c form]
  (or (class-for-name c)
      (error ::error/missing-type {:type c :form form})))

(defn maybe-class [c]
  (or (class-for-name c) c))

(defn desugar-host-expr [form env]
  (cond
   (symbol? form)
   (let [target (maybe-class (namespace form))]
     (if (and target
              (not (resolve-ns (symbol (namespace form)) env))
              (ensure-class target form))       ;; Class/field
       (with-meta (list '. target (symbol (str "-" (symbol (name form))))) ;; transform to (. Class -field)
         (meta form))
       form))

   (seq? form)
   (let [[op & expr] form]
     (if (symbol? op)
       (let [opname (name op)
             opns   (namespace op)]
         (cond

          (.StartsWith opname ".") ; (.foo bar ..)                                    ;;; .startsWith
          (let [[target & args] expr
                ;; TODO why do we need this???
                target (if-let [target (and (not (get (:locals env) target))
                                            (maybe-class target))]
                         (with-meta (list 'clojure.core/identity target)
                           {:tag 'System.Type})                                       ;;; java.lang.Class
                         target)
                args (list* (symbol (subs opname 1)) args)]
            (with-meta (list '. target (if (= 1 (count args)) ;; we don't know if (.foo bar) is
                                         (first args) args))  ;; a method call or a field access
              (meta form)))

          (and (maybe-class opns)
               (not (resolve-ns (symbol opns) env))) ; (class/field ..)
          (let [target (maybe-class opns)
                op (symbol opname)]
            (ensure-class target form)
            (with-meta (list '. target (if (zero? (count expr))
                                         op
                                         (list* op expr)))
                       (meta form)))

          (.EndsWith opname ".") ;; (class. ..)                      ;;; .endsWith
          (with-meta (list* 'new (symbol (subs opname 0 (dec (count opname)))) expr)
            (meta form))

          :else form))
       form))

   :else form))

(defn build-ns-map []
  (into {} (mapv #(vector (ns-name %)
                          {:mappings (ns-map %)
                           :aliases  (reduce-kv (fn [a k v] (assoc a k (ns-name v)))
                                                {} (ns-aliases %))
                           :ns       (ns-name %)})
                 (all-ns))))

(defn global-env []
  (atom {:namespaces (build-ns-map)}))

(def specials
  "Set of the special forms for clojure in the CLR"
  (into ana/specials
        '#{var monitor-enter monitor-exit clojure.core/import* reify* deftype* case*}))

(defn empty-env
  "Returns an empty env map"
  []
  {:context    :ctx/expr
   :locals     {}
   :ns         (ns-name *ns*)})

(defn update-ns-map! []
  (swap! *env* assoc-in [:namespaces] (build-ns-map)))

(defn macroexpand-1
  "If form represents a macro form or an inlineable function,
   returns its expansion, else returns form."
  ([form] (macroexpand-1 form (empty-env)))
  ([form env]
     (env/ensure (global-env)
       (if (seq? form)
         (let [[op & args] form]
           (if (specials op)
             form
             (let [v (resolve-sym op env)
                   m (meta v)
                   local? (-> env :locals (get op))
                   macro? (and (not local?) (:macro m)) ;; locals shadow macros
                   inline-arities-f (:inline-arities m)
                   inline? (and (not local?)
                                (or (not inline-arities-f)
                                    (inline-arities-f (count args)))
                                (:inline m))
                   t (:tag m)]
               (cond

                macro?
                (let [res (apply v form (:locals env) (rest form))] ; (m &form &env & args)
                  (update-ns-map!)
                  (if (obj? res)
                    (vary-meta res merge (meta form))
                    res))

                inline?
                (let [res (apply inline? args)]
                  (update-ns-map!)
                  (if (obj? res)
                    (vary-meta res merge
                               (and t {:tag t})
                               (meta form))
                    res))

                :else
                (desugar-host-expr form env)))))
         (desugar-host-expr form env)))))

;;;; analyze-host-forms.clj

(def public-instance (enum-or BindingFlags/Instance BindingFlags/Public))
(def public-static (enum-or BindingFlags/Static BindingFlags/Public))
(def public-instance-static (enum-or BindingFlags/Instance BindingFlags/Static BindingFlags/Public))

(defn analyze-type
  "Analyze foo into a type"
  {:pass-info {:walk :post :depends #{} :after #{#'uniquify-locals}}}
  [{:keys [op class] :as ast}]
  (if (= :maybe-class op)
    (let [target-type (ensure-class (name class) (:form ast))]
      (merge (dissoc ast :class)
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
          (error ::error/missing-static-zero-arity ast)
          (assoc ast :op :dynamic-field))
        ast*))
    ast))

;; TODO deal with parameter type conversions 
;; e.g. (System.Collections.ArrayList. 12) should know to cast to int
(defn analyze-constructor
  "Analyze (Foo. a b) into object or valuetype constructor
  produces :new :new-value-type :new-dynamic"
  {:pass-info {:walk :post :depends #{} :after #{#'uniquify-locals}}}
  [{:keys [args class op] :as ast}]
  (if (= :new op)
    (let [target-type (clr-type class)]
      (merge (dissoc ast :class)
             {:type target-type}
             (if (.IsValueType target-type) {:op :new-value-type})
             (if-let [ctor-info (.GetConstructor target-type (->> args
                                                                  (map clr-type)
                                                                  (into-array Type)))]
               {:constructor ctor-info}
               ;; no exact match, look for arity match
               (let [ctors (->> (.GetConstructors target-type)
                                (filter #(= (count (.GetParameters %))
                                            (count args))))]
                 (if (empty? ctors)
                   (error ::error/missing-constructor-arity ast)
                   {:op :new-dynamic
                    :constructors ctors})))))
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
            (and static? (empty? args))   (error ::error/missing-static-zero-arity ast)
            static?                       (error ::error/missing-static-method ast)
            :else                         (assoc ast* :op :dynamic-interop)))
    ast))

;; TODO analyze away the identity invoke hack
(defn analyze-host-call
  "Analyze (Foo/Bar a) into static method invocation"
  {:pass-info {:walk :post :depends #{} :after #{#'uniquify-locals}}}
  [{:keys [method target args op] :as ast}]
  (if (= :host-call op)
    (let [target-type (clr-type target)]
      (if-let [meth (.GetMethod target-type (str method) (->> args
                                                              (map clr-type)
                                                              (into-array Type)))]
        (merge ast
               {:method meth
                :op (case (-> target :type)
                      :class :static-method
                      :instance-method)})
        ;; no exact match, look for arity match
        (let [ctors (->> (.GetMethods target-type)
                         (filter #(and
                                    (= (str method) (.Name %))
                                    (= (count (.GetParameters %))
                                       (count args)))))]
          (if (empty? ctors)
            (error ::error/missing-instance-method-arity ast)
            (merge ast
                   {:op :dynamic-call
                    :methods ctors})))))
    ast))

(def default-passes
  #{#'analyze-type          ;; Foo
    #'analyze-host-field    ;; Foo/Bar
    #'analyze-constructor   ;; (Foo.)
    #'analyze-host-interop  ;; (.foo a)
    #'analyze-host-call     ;; (Foo/Bar a)
    #'source-info
    #'cleanup
    #'elide-meta
    #'warn-earmuff
    #'collect-closed-overs
    #'add-binding-atom
    #'uniquify-locals})

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
     (with-env (global-env) (run-passes (ana/analyze form env))))))

(comment 
  (analyze '(. DateTime Now))
  
  (defmacro envo [a]
    `(quote ~(keys &env)))
  
  (eval
    '(let [a 99]
      (envo 9)))
  
  (clojure.core/macroexpand-1 '(let [a 12] &env))
  (clojure.core/macroexpand-1 '(. DateTime -Now))
  
  (defn same-expansion [f]
    (= (clojure.core/macroexpand-1 f) (macroexpand-1 f)))
  
  (macroexpand-1 '(. DateTime Now))
  (pprint (analyze '(.-Now DateTime)))
  
  (same-expansion '(.Now DateTime))
  
  (:op (analyze '(. "target" bar 1)))    ;; :host-call
  (:op (analyze '(. "target" (bar 1))))  ;; :host-call
  (:op (analyze '(. "target" bar)))      ;; :host-interop
  (:op (analyze '(. "foo" -bar)))        ;; :host-field
  (:op (analyze '(foo/bar 1)))           ;; :host-call
  (:op (analyze 'foo/bar))               ;; :host-field
  (:op (analyze '(.foo "target" 1)))     ;; :host-call
  (:op (analyze '(.foo "target")))       ;; :host-interop
  (:op (analyze '(.-foo "target")))      ;; :host-field
  
  (binding [*compile-path* "."]
    (compile 'aot))
  
  (aot/floo 1 2 3)
  
  (-> (analyze '(let [d (DateTime. 12)] d))
      :body
      :ret
      :env
      :locals
      (get 'd)
      :children
      pprint
      )
  
  (pprint (clr-type (analyze '(inc 3))))
  (pprint (clr-type (analyze '(int 3))))
  (pprint (analyze 'Strin))
  (pprint (analyze 'String))
  (pprint (analyze 'DateTim/Now))
  (pprint (analyze 'DateTime/Now))
  (pprint (analyze '(.-Date DateTime/Now)))
  (pprint (analyze '(Math/Sin 90)))
  (pprint (analyze '(System.Collections.ArrayList.)))
  (pprint (analyze '(System.Collections.ArrayList. (int 12))))
  (pprint (analyze '(.Reverse (System.Collections.ArrayList. (int 45)))))
  (pprint (analyze '(.. (System.Collections.ArrayList. (int 45)) ToArray Rank)))
  (pprint (analyze '(let [a 12] a)))
  (pprint (analyze '(do 1 2 3 4)))
  (pprint (analyze '(+ 1 2)))
  (pprint (analyze '(+ 1 90.1)))
  (binding [*unchecked-math* true]
    (pprint (analyze '(+ 1 2))))
  
  (require 'clojure.test)
  (require 'clojure.tools.analyzer.clr.tests :reload)
  (clojure.test/run-tests 'clojure.tools.analyzer.clr.tests))

