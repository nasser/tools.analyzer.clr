;; a place for new ideas
(ns clojure.tools.analyzer.clr.novel
  (:require [clojure.tools.analyzer.clr.analyze-host-forms :as host-forms]))

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