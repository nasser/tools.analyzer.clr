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
  (let [frm* (clr/analyze frm)]
    (and (op? frm* :const)
         (is (= :class (:type frm*)))
         (is (= true (:literal? frm*)))
         (right-type? frm frm*))))

(defn primitive-types []
  (doall (map test-type '[String Int16 Int64 Int32 Single Double
                          Char Byte DateTime Decimal Boolean
                          UInt32 UInt64 Object])))

(defn qualified-types []
  (doall (map test-type '[System.String System.Int16 System.Int64
                          System.Int32 System.Single System.Double
                          System.Char System.Byte System.DateTime
                          System.Decimal System.Boolean System.UInt32
                          System.UInt64 System.Object
                          System.Drawing.Point])))

;; TODO depends on import analysis
(defn imported-type []
  (test-type '(do (import System.IO.File) File)))

;; TODO specific exceptions?
(defn throw-on-missing-type [frm]
  (is (thrown? System.Exception
               (clr/analyze frm))))

(defn missing-types []
  (doall (map throw-on-missing-type '[Flambix string SystemInt64
                                      Int63 System.Prambo])))

(deftest types
  (primitive-types)
  (qualified-types)
  ; (imported-type)
  (missing-types)
  )


;; host 

(defn test-static-property [frm]
  (let [frm* (clr/analyze frm)]
    (and (op? frm* :static-property)
         (is (isa? (-> frm* :property type)
                   System.Reflection.PropertyInfo))
         (is (= (name frm)
                (-> frm* :property .Name))))))

(defn static-properties []
  (doall (map test-static-property '[DateTime/Now])))

(deftest statics
  (static-properties))