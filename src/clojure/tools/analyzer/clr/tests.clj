(ns clojure.tools.analyzer.clr.tests
  (:require [clojure.tools.analyzer.clr :as clr])
  (:use clojure.test))

(defn op? [frm op]
  (is (= (-> frm clr/analyze :op)
         op)))

(defn right-type? [frm]
  (is (= (-> frm clr/analyze :type)
         (resolve frm))))

(defn test-type [frm]
  (and (op? frm :type)
       (right-type? frm)))

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

(defn throw-on-missing-type [frm]
  (is (thrown? System.TypeLoadException
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


(defn test-static-property [frm]
  (and (op? frm :static-property)
       (is (isa? (-> frm clr/analyze :property type)
                 System.Reflection.PropertyInfo))
       (is (= (name frm)
              (-> frm clr/analyze :property .Name)))))

(defn static-properties []
  (doall (map test-static-property '[DateTime/Now])))

(deftest statics
  (static-properties))