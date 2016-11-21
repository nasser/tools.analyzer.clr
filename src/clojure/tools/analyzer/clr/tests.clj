(ns clojure.tools.analyzer.clr.tests
  (:require [clojure.tools.analyzer.clr :as clr])
  (:use clojure.test))

(defn tests [& fns]
  (fn [frm frm*]
    (testing (str frm)
      (doseq [f fns]
        (f frm frm*)))))

(defn should-be [fns & frms]
  (let [testfn (apply tests fns)]
    (doseq [frm frms]
      (let [frm* (clr/analyze frm)]
        (testfn frm frm*)))))

;; TODO other exceptions
(defn should-throw [& frms]
  (doseq [frm frms]
    (testing (str frm)
      (is (thrown? System.Exception
                   (clr/analyze frm))))))

(defn op? [frm* op]
  (is (= (:op frm*)
         op)))

(defn literal [frm frm*]
  (is (:literal? frm*)))

(defn const [frm frm*]
  (is (= (:op frm*) :const)))

(defn correct-type [frm frm*]
  (is (= (-> frm* :val)
         (resolve frm))))

(defn test-type [frm]
  (testing (str frm)))

(deftest primitive-types
  (should-be
    [const correct-type literal]
    'String 'Int16 'Int64
    'Int32 'Single 'Double
    'Char 'Byte 'DateTime
    'Decimal 'Boolean 'UInt32
    'UInt64 'Object))

(deftest qualified-types
  (should-be
    [const correct-type literal]
    'System.String 'System.Int16 'System.Int64
    'System.Int32 'System.Single 'System.Double
    'System.Char 'System.Byte 'System.DateTime
    'System.Decimal 'System.Boolean 'System.UInt32
    'System.UInt64 'System.Object
    'System.Drawing.Point))

;; TODO depends on import analysis
#_
(deftest imported-type
  (test-type '(do (import System.IO.File) File)))

(deftest missing-types
  (should-throw
    'Flambix 'string 'SystemInt64
    'Int63 'System.Prambo))

;; host 
(defn inexact [frm frm*]
  (is (-> frm* :inexact?)))

(defn exact [frm frm*]
  (is (not (-> frm* :inexact?))))

(defn static-property [frm frm*]
  (and (op? frm* :static-property)
       (is (isa? (-> frm* :property type)
                 System.Reflection.PropertyInfo))
       (is (= (name frm)
              (-> frm* :property .Name)))))

(deftest static-properties
  (should-be
    [static-property]
    'DateTime/Now))

(defn static-field [frm frm*]
  (and (op? frm* :static-field)
       (is (isa? (-> frm* :field type)
                 System.Reflection.FieldInfo))
       (is (= (name frm)
              (-> frm* :field .Name)))))

(deftest static-fields
  (should-be
    [static-field]
    'Int32/MaxValue 'Int64/MaxValue
    'Int32/MinValue 'Int64/MinValue
    'System.Int32/MaxValue 'System.Int64/MaxValue
    'System.Int32/MinValue 'System.Int64/MinValue)
  (should-throw
    'Int32/Max_Value
    'DateTime/Parse))

(defn static-method [frm frm*]
  (op? frm* :static-method))

(defn arity-match [kw]
  (let [info-type {:method System.Reflection.MethodInfo
                   :constructor System.Reflection.ConstructorInfo}]
    (fn [frm frm*]
      (and (is (isa? (-> frm* kw type)
                     (info-type kw)))
           (is (= (-> frm* kw .GetParameters count)
                  (-> frm count dec)))))))

(defn all-arity-match [kw]
  (let [info-type {:methods |System.Reflection.MethodInfo[]|
                   :constructors |System.Reflection.ConstructorInfo[]|}]
    (fn [frm frm*]
      (and (is (isa? (-> frm* kw into-array type)
                     (info-type kw)))
           (doseq [m (-> frm* kw)]
             (is (= (-> m .GetParameters count)
                    (-> frm count dec))))))))

#_
(defn static-method [frm frm*]
  (and (op? frm* :static-method)
       (is (isa? (-> frm* :method type)
                 System.Reflection.MethodInfo))
       (is (= (-> frm first name)
              (-> frm* :method .Name)))
       (is (= (-> frm* :method .GetParameters count)
              (-> frm count dec)))))

(defn static-method-dynamic [frm frm*]
  (and (is (isa? (-> frm* :methods into-array type)
                 |System.Reflection.MethodInfo[]|))
       (doseq [m (-> frm* :methods)]
         (is (= (-> m .GetParameters count)
                (-> frm count dec))))
       (is (= (-> frm first name)
              (-> frm* :method str)))))

(deftest static-methods
  (should-be
    [exact static-method (arity-match :method)]
    '(DateTime/Compare DateTime/Now DateTime/Now)
    '(DateTime/FromBinary 89)
    '(DateTime/Parse "89"))
  
  (should-be
    [inexact static-method (all-arity-match :methods)]
    '(DateTime/Compare 1 2)
    '(DateTime/FromBinary "89")
    '(DateTime/Parse 1)
    '(DateTime/Parse 1 {:foo 12}))
  
  (should-throw
    '(DateTime/Jango 1 2)
    '(DateTime/Parse)
    '(DateTime/Parse :lots :of :arguments :way :too :many)))

(defn constructor [frm frm*]
  (and (op? frm* :new)
       (is (or (= (-> frm* :form second name)
                  (-> frm* :type .Name))
               (= (-> frm* :form second name)
                  (-> frm* :type .FullName))))))

(defn initobj-constructor [frm frm*]
  (and (op? frm* :initobj)
       (is (= (-> frm* :form second name symbol resolve)
              (-> frm* :type)))))

(deftest value-type-constructors
  (should-throw
    '(DateTime. :lots :of :arguments :way :too))
  (should-be
    [exact constructor (arity-match :constructor)]
    '(DateTime. 1))
  (should-be
    [inexact constructor (all-arity-match :constructors)]
    '(System.Drawing.Point. 1)
    '(System.Drawing.Point. 1 2)
    '(System.Drawing.Size. 1 2)) 
  (should-be
    [exact initobj-constructor]
    '(DateTime.)
    '(System.Drawing.Point.)
    '(System.Drawing.Size.)))

(deftest constructors
  (should-be
    [exact constructor (arity-match :constructor)]
    '(System.Drawing.Bitmap. "Hello")
    '(System.Drawing.Bitmap. "Hello" false))
  (should-be
    [inexact constructor (all-arity-match :constructors)]
    '(System.Drawing.Bitmap. 8 8)
    '(System.Drawing.Bitmap. :foo :bar))
  (should-throw
    '(System.Drawing.Bitmap.)))