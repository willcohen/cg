;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.eval-destructuring-test
  "The Clojure 1.13 map destructuring that .cg code can use. Each test sends
   .cg source through cg.run/eval-cg. On the JVM, SCI 0.15.57 or later
   evaluates it. On the JS runtime, squint 0.14.207 or later compiles it. The
   expected values are the results of Clojure 1.13.0-alpha8.

   Because squint uses one string for a key of each type, a test uses
   keyword keys only. Because the error text gives the key as :b on the JVM
   and as b on squint, a test of an error reads only the key name."
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [clojure.string :as str]
                    [cg.run :as run])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     [clojure.string :as str]
                     ["../../src/cg/run.mjs" :as run]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private fns-src
  "(defn dest13-place
     [{:keys! [rows] :keys [buffer label & :crs] :or {buffer 10}
       :select picked :all every :defaults dflt}]
     [rows buffer label picked every dflt])
   (defn dest13-both [{:keys! [a b]}] [a b])
   (defn dest13-amp [{:keys! [a & :b]}] a)")

(defn- eval-error [src]
  (try
    (run/eval-cg src)
    nil
    (catch #?(:clj Exception :cljs :default) e
      #?(:clj (ex-message e) :cljs (.-message e)))))

(defn- missing-key? [msg k]
  (and (string? msg)
       (str/starts-with? msg "Missing required key: ")
       (str/ends-with? msg k)))

(deftest directives-bind-as-clojure-1-13-does
  (run/eval-cg fns-src)
  (testing ":keys!, :keys with & keys, :or, :select, :all and :defaults"
    (is (= [1 10 "x"
            {:rows 1 :buffer 10 :label "x" :crs "EPSG:2249"}
            {:rows 1 :buffer 10 :label "x" :crs "EPSG:2249" :extra true}
            {:buffer 10}]
           (run/eval-cg "(dest13-place {:rows 1 :label \"x\" :crs \"EPSG:2249\" :extra true})"))))
  (testing "a key that is present with nil keeps nil, and the default does not apply"
    (is (= [2 nil nil {:rows 2 :buffer nil} {:rows 2 :buffer nil} {:buffer 10}]
           (run/eval-cg "(dest13-place {:rows 2 :buffer nil})")))))

(deftest a-missing-required-key-throws
  (run/eval-cg fns-src)
  (testing ":keys! names the key that is missing"
    (is (missing-key? (eval-error "(dest13-both {:a 1})") "b")))
  (testing "a key after & in :keys! is required but has no binding"
    (is (missing-key? (eval-error "(dest13-amp {:a 1})") "b"))
    (is (= 1 (run/eval-cg "(dest13-amp {:a 1 :b 2})")))))

(deftest req!-and-some-vals-are-in-clojure-core
  (is (= 1 (run/eval-cg "(req! {:a 1} :a)")))
  (is (missing-key? (eval-error "(req! {:a 1} :b)") "b"))
  (is (= {:a 1} (run/eval-cg "(some-vals {:a 1 :b nil})"))))

#?(:cljs (run_tests_and_exit_BANG_ "cg.eval-destructuring-test"))
