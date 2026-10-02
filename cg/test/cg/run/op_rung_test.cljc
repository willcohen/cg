;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.run.op-rung-test
  "A defn with ^:op is a rung on both runtimes. On the JVM, cg.run registers
   the op from its var after the defn evaluates. On the JS runtime, because
   squint compiles a whole eval at once, cg.run registers each op from the
   parser decl before the compile. Without this, a rung in the same eval
   as its op got {:args [10 100]}, and the op returned \"110,100undefined\"
   with no error.

   The op takes 9 params, more than an array map keeps in order on the
   JVM."
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [clojure.string :as str]
                    [cg.run :as run])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     [clojure.string :as str]
                     ["../../../src/cg/run.mjs" :as run]
                     ["../../../src/cg/parser/core.mjs" :as parser]
                     ["../test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private same-eval-src
  "(require '[cg.macros :refer [item->]])
   (defn ^:op rung-nine [^:row x a b c d e f g h i] [x a b c d e f g h i])
   (item-> :xs (rung-nine 1 2 3 4 5 6 7 8 9))")

(def ^:private module-rung-src
  "(require '[cg.macros :refer [item->]])
   (item-> :xs (test.op-mod/rung-sub 1))")

(def ^:private row-model-src
  "(require '[cg.macros :refer [item->]])
   (defn ^:op rung-row-model [^:row x] (item-> :x (inc)))
   (item-> :xs (rung-row-model))")

;; D14: an op that ends in an option map. A rung gives the map as one
;; value, or as keyword args. n is a local, as a rung arg often is.
(def ^:private option-map-src
  "(require '[cg.macros :refer [item->]])
   (defn ^:op rung-opts [^:row x & {:keys! [a] :keys [b] :or {b 100}}] (+ x a b))
   (let [n 5]
     [(item-> :xs (rung-opts :a 10 :b 1))
      (item-> :xs (rung-opts {:a 10}))
      (item-> :xs (rung-opts :a n))])")

(defn- check-option-map! [results]
  (is (= [[12 13] [111 112] [106 107]] (mapv vec results))
      "keyword args, a map, and keyword args with a local"))

(defn- row-model-error? [msg]
  (and (string? msg)
       (str/includes? msg "op 'rung-row-model' takes a ^:row param and returns a model")))

(defn- op-params [m]
  (mapv :params (filter #(= :operation (:type %)) (:nodes m))))

(defn- check-same-eval! [m result]
  (is (= [(zipmap [:a :b :c :d :e :f :g :h :i] [1 2 3 4 5 6 7 8 9])] (op-params m))
      "the rung gets the op params by name, in arglist order")
  (is (= [[0 1 2 3 4 5 6 7 8 9]] (vec result))))

(defn- check-module-rung! [result]
  (is (= [9 19] (vec result))))

#?(:cljs
   (do
     (defn ^:async run-same-eval! []
       (let [m (run/eval-cg same-eval-src)]
         (check-same-eval! m (await (run/run-model m {:xs [0]} {})))))

     (defn ^:async run-row-model! []
       (is (row-model-error?
            (try (await (run/run-model (run/eval-cg row-model-src) {:xs [1]} {}))
                 nil
                 (catch :default e (.-message e))))))

     (defn ^:async run-option-map! []
       (let [ms (run/eval-cg option-map-src)
             results #js []]
         (doseq [m ms]
           (.push results (await (run/run-model m {:xs [1 2]} {}))))
         (check-option-map! (vec results))))

     (defn ^:async run-module-rung! []
       (await (run/load-module "test/fixtures/modules/op_mod.cg"))
       (check-module-rung!
        (await (run/run-model (run/eval-cg module-rung-src) {:xs [10 20]} {}))))))

(deftest a-rung-in-the-same-eval-as-its-op
  #?(:clj (let [m (run/eval-cg same-eval-src)]
            (check-same-eval! m (run/run-model m {:xs [0]} {})))
     :cljs (run-same-eval!)))

(deftest a-module-op-is-a-rung
  (testing "(item-> :xs (test.op-mod/rung-sub 1)) over 10 and 20"
    #?(:clj (do (run/load-module "test/fixtures/modules/op_mod.cg")
                (check-module-rung!
                 (run/run-model (run/eval-cg module-rung-src) {:xs [10 20]} {})))
       :cljs (run-module-rung!))))

(deftest a-rung-gives-an-option-map
  #?(:clj (check-option-map! (mapv (fn [m] (run/run-model m {:xs [1 2]} {}))
                                   (run/eval-cg option-map-src)))
     :cljs (run-option-map!)))

(deftest an-odd-count-of-option-args-fails-when-the-rung-expands
  (let [msg (try (run/eval-cg (str option-map-src " (item-> :xs (rung-opts :a 10 :b))"))
                 nil
                 (catch #?(:clj Exception :cljs :default) e
                   (str (ex-message e) " " (some-> (ex-cause e) ex-message))))]
    (is (and (string? msg)
             (str/includes? msg "op 'rung-opts'")
             (str/includes? msg "as one value or as keyword args"))
        msg)))

(deftest a-row-op-that-returns-a-model-fails-when-it-runs
  #?(:clj (is (row-model-error?
               (try (run/run-model (run/eval-cg row-model-src) {:xs [1]} {})
                    nil
                    (catch Exception e (ex-message e)))))
     :cljs (run-row-model!)))

#?(:cljs (-> (parser/init! nil)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.run.op-rung-test")))))
