;; a place for new ideas
(ns clojure.tools.analyzer.clr.novel
  (:require [clojure.tools.analyzer.clr.analyze-host-forms :as host-forms]
            [clojure.string :as string]))

(def operators
  '{++ op_Increment
    -- op_Decrement
    ; -  op_UnaryNegation
    ; +  op_UnaryPlus
    !  op_LogicalNot
    |~|  op_OnesComplement
    /  op_Division
    %  op_Modulus
    *  op_Multiply
    +  op_Addition
    -  op_Subtraction
    << op_LeftShift
    >> op_RightShift
    >  op_GreaterThan
    <  op_LessThan
    >= op_GreaterThanOrEqual
    <= op_LessThanOrEqual
    == op_Equality
    != op_Inequality
    &  op_BitwiseAnd
    |^|  op_ExclusiveOr
    ; |  op_BitwiseOr
    })

(defn csharp-operators
  "Analyze (Foo/+ bar) into (Foo/op_Addition bar)"
  {:pass-info {:walk :post :depends #{} :before #{#'host-forms/analyze-host-call}}}
  [{:keys [method target args op] :as ast}]
  (if (= :host-call op)
    (if-let [operator-method (operators method)]
      (assoc ast :method operator-method
        :novel true)
      ast)
    ast))

(defn generic-type-syntax 
  "Analyze Foo|[String, Int32]| into Foo`2[String, Int32]"
  {:pass-info {:walk :post :depends #{} :before #{#'host-forms/analyze-type}}}
  [{:keys [op children class] :as ast}]
  (if (= :maybe-class op)
    (if (re-find #"\[" (str class))
      (let [reader (-> class str
                       System.IO.StringReader.
                       clojure.lang.PushbackTextReader.)
            class-name (read reader)
            type-args (read reader)
            type-args-str (str "[" (string/join "," type-args) "]")
            ]
        (merge ast {:class (symbol (str class-name
                                        "`" (count type-args)
                                        type-args-str))}))
      ast)
    ast))