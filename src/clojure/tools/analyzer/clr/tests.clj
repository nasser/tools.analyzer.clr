(ns clojure.tools.analyzer.clr.tests
  (:require [clojure.tools.analyzer.clr :as clr])
  (:use clojure.test))

(defn op? [frm op]
  (is (= (:op frm)
         op)))

(defn right-type? [frm frm*]
  (is (= (-> frm* :val)
         (resolve frm))))

(defn test-type [frm]
  (testing (str frm)
    (let [frm* (clr/analyze frm)]
      (and (op? frm* :const)
           (is (= :class (:type frm*)))
           (is (= true (:literal? frm*)))
           (right-type? frm frm*)))))

(deftest primitive-types
  (doall (map test-type '[String Int16 Int64 Int32 Single Double
                          Char Byte DateTime Decimal Boolean
                          UInt32 UInt64 Object])))

(deftest qualified-types
  (doall (map test-type '[System.String System.Int16 System.Int64
                          System.Int32 System.Single System.Double
                          System.Char System.Byte System.DateTime
                          System.Decimal System.Boolean System.UInt32
                          System.UInt64 System.Object
                          System.Drawing.Point])))

;; TODO depends on import analysis
#_
(deftest imported-type
  (test-type '(do (import System.IO.File) File)))

;; TODO specific exceptions?
(defn throws [frm]
  (testing (str frm)
    (is (thrown? System.Exception
                 (clr/analyze frm)))))

(deftest missing-types
  (doall (map throws '[Flambix string SystemInt64
                       Int63 System.Prambo])))

;; host 

(defn test-static-property [frm]
  (testing (str frm)
    (let [frm* (clr/analyze frm)]
      (and (op? frm* :static-property)
           (is (isa? (-> frm* :property type)
                     System.Reflection.PropertyInfo))
           (is (= (name frm)
                  (-> frm* :property .Name)))))))

(defn test-static-field [frm]
  (testing (str frm)
    (let [frm* (clr/analyze frm)]
      (and (op? frm* :static-field)
           (is (isa? (-> frm* :field type)
                     System.Reflection.FieldInfo))
           (is (= (name frm)
                  (-> frm* :field .Name)))))))

(defn test-static-method [frm]
  (testing (str frm)
    (let [frm* (clr/analyze frm)]
      (and (op? frm* :static-method)
           ;; its a method
           (is (isa? (-> frm* :method type)
                     System.Reflection.MethodInfo))
           ;; correct name 
           (is (= (-> frm first name)
                  (-> frm* :method .Name)))
           ;; correct number of parameters
           (is (= (-> frm* :method .GetParameters count)
                  (-> frm count dec)))))))

(defn test-static-method-dynamic [frm]
  (testing (str frm)
    (let [frm* (clr/analyze frm)]
      (and (op? frm* :static-method)
           (is (-> frm* :inexact?))
           (is (isa? (-> frm* :methods into-array type)
                     |System.Reflection.MethodInfo[]|))
           (doseq [m (-> frm* :methods)]
             (is (= (-> m .GetParameters count)
                    (-> frm count dec))))
           (is (= (-> frm first name)
                  (-> frm* :method str)))))))

(defn test-value-type-constructor [frm]
  (testing (str frm)
    (let [frm* (clr/analyze frm)]
      (and (op? frm* :new)
           (is (isa? (-> frm* :constructor type)
                     System.Reflection.ConstructorInfo))
           (is (= (-> frm* :form second name)
                  (-> frm* :type .Name)))
           (is (= (-> frm* :constructor .GetParameters count)
                  (-> frm count dec)))))))

(deftest static-properties
  (doall (map test-static-property '[DateTime/Now])))

(deftest static-fields
  (doall (map test-static-field '[Int32/MaxValue Int64/MaxValue
                                  Int32/MinValue Int64/MinValue
                                  System.Int32/MaxValue System.Int64/MaxValue
                                  System.Int32/MinValue System.Int64/MinValue]))
  (doall (map throws '[Int32/Max_Value DateTime/Parse])))

(deftest static-methods
  (doall (map test-static-method
              '[(DateTime/Compare DateTime/Now DateTime/Now)
                (DateTime/FromBinary 89)
                (DateTime/Parse "89")]))
  (doall (map test-static-method-dynamic
              '[(DateTime/Compare 1 2)
                (DateTime/FromBinary "89")
                (DateTime/Parse 1)
                (DateTime/Parse 1 {:foo 12})]))
  (doall (map throws
              '[(DateTime/Jango 1 2)
                (DateTime/Parse)
                (DateTime/Parse :lots :of :arguments :way :too :many)])))

(deftest value-type-constructors
  (doall (map throws '[(DateTime. :lots :of :arguments :way :too)]))
  (doall (map test-value-type-constructor
              '[(DateTime. 1)]))
  )