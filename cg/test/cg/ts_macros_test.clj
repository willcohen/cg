;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; The JVM expansion of define-all-ts-fns. cg.geo's behavior on both runtimes
;; is in geo_test.cljc; these tests check the shape of the emitted code, which
;; a passing behavior test does not show: a reflective call gives the same
;; result as a direct one.
(ns cg.ts-macros-test
  (:require [clojure.test :refer [deftest is testing]]
            [cg.ts-macros :as tm]))

(set! *warn-on-reflection* true)

(def ^:private dispatchers
  {:receiver-call 'dispatch-receiver-call
   :static-call   'dispatch-static-call
   :ctor          'dispatch-ctor})

(def ^:private dispatcher?
  (into #{'cg.geo/dispatch-receiver-call 'cg.geo/dispatch-static-call 'cg.geo/dispatch-ctor}
        (vals dispatchers)))

(defn- dispatch-calls [forms]
  (filter #(and (seq? %) (dispatcher? (first %)))
          (tree-seq coll? seq forms)))

(defn- invoker [call] (nth call 2))

(defn- direct-call?
  "A `.` or `new` form, or an `if` on the receiver's class between two of
   them (a Geometry op that a PreparedGeometry receiver also reaches)."
  [form]
  (and (seq? form)
       (or (contains? '#{. new} (first form))
           (and (= 'if (first form))
                (= 'clojure.core/instance? (first (second form)))
                (every? direct-call? (drop 2 form))))))

(defn- invoker-fn? [form]
  (and (seq? form)
       (= 'clojure.core/fn (first form))
       (direct-call? (last form))))

(defn- jvm-forms [] (tm/emit-all (tm/load-registry) dispatchers true))

(deftest jvm-invokers-call-jts-directly
  (let [jvm   (dispatch-calls (jvm-forms))
        other (dispatch-calls (tm/emit-all (tm/load-registry) dispatchers))]
    (testing "each JVM dispatcher call passes an invoker fn whose body calls the method"
      (is (< 1500 (count jvm)))
      (is (every? (comp invoker-fn? invoker) jvm)))
    (testing "no JVM form names Reflector or the old reflective helpers"
      (is (not-any? #(and (symbol? %)
                          (re-find #"Reflector|invoke-instance$|invoke-static$|invoke-ctor$" (str %)))
                    (tree-seq coll? seq (jvm-forms)))))
    (testing "a Geometry op that PreparedGeometry also has branches on the receiver"
      (is (some #(= 'if (first (last (invoker %)))) jvm)))
    (testing "the squint expansion has no invoker"
      (is (= (count jvm) (count other)))
      (is (not-any? (comp invoker-fn? invoker) other)))))

(deftest jvm-invokers-compile-without-reflection
  (let [err (java.io.StringWriter.)]
    (binding [*warn-on-reflection* true
              *err* err]
      (doseq [call (dispatch-calls (jvm-forms))]
        (eval (invoker call))))
    (is (= "" (str err)))))
