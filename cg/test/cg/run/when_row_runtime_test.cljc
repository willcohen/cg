;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.run.when-row-runtime-test
  "Runtime tests for when-row (cg.run/when-row) driven through run-model,
   exercising the predicate routing:

     predicate true                  -> body applied
     predicate false + on-miss drop  -> empty dataset
     predicate false + pass-through  -> dataset unchanged

   when-row is a :whole-dataset table op. operation->step routes it through the
   :whole-dataset branch (cg.run), which threads the entire batch through
   cg.run/when-row; when-row is registered with :row-shape :whole-dataset so
   op-info classifies it. (Without that branch, when-row falls into the
   per-geometry map-geometry path and fails.)

   ONE .cljc suite covers both runtimes. The JVM expands the table-> macro at
   compile time. cljs sends the same DSL text through eval-cg, because squint
   has no macro expansion at run time. The assertions are the same."
  #?(:clj (:require [clojure.test :refer [deftest is]]
                    [cg.run :as run]
                    [cg.dataset :as ds]
                    [cg.macros :refer [table->]]
                    [cg.test-runner :refer [after]]
                    [tech.v3.dataset :as tds])
     :cljs (:require [cljs.test :refer [deftest is]]
                     ["../../../src/cg/run.mjs" :as run]
                     ["../../../src/cg/dataset.mjs" :as ds]
                     ["../wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["../test_runner.mjs" :refer [run_tests_and_exit_BANG_ after]])))

#?(:clj (set! *warn-on-reflection* true))

(defn- row-count
  "The row count of a run-model result. A flow with one sink gives a sequence
   that holds the dataset on the JVM, and the dataset itself on cljs."
  [result]
  (let [d (if (sequential? result) (first result) result)]
    (if (nil? d) 0 (ds/row-count d))))

#?(:clj
   (defn- run-jvm [model]
     (run/run-model model {:input (tds/->>dataset [{:n 1} {:n 2}])} {:timeout-ms 10000})))

#?(:cljs
   (defn ^:async when-row-result [body]
     (await (ds/init-arquero!))
     ;; when-row resolves as "cg.run/when-row", and the pre-filled cljs
     ;; registry holds cg.geo, cg.feature and cg.dataset only.
     (run/register-namespace! "cg.run" run)
     (let [model (run/eval-cg (str "(require '[cg.macros :refer [table->]])"
                                   "(require '[cg.dataset :as ds])"
                                   body))
           d     (ds/->dataset {:n [1 2]})]
       (await (run/run-model model {:input d} {:timeout-ms 10000})))))

(deftest when-row-predicate-true
  (after #?(:clj (run-jvm (table-> :input (when-row {:has-column? :n} (ds/head 1))))
            :cljs (when-row-result
                   "(table-> :input (when-row {:has-column? :n} (ds/head 1)))"))
         #(is (= 1 (row-count %)) "head 1 applied -> 1 row")))

(deftest when-row-predicate-false-drop
  (after #?(:clj (run-jvm (table-> :input (when-row {:has-column? :nope :on-miss :drop}
                                                    (ds/head 1))))
            :cljs (when-row-result
                   (str "(table-> :input (when-row {:has-column? :nope"
                        " :on-miss :drop} (ds/head 1)))")))
         #(is (= 0 (row-count %)) "dropped -> 0 rows")))

(deftest when-row-predicate-false-pass-through
  (after #?(:clj (run-jvm (table-> :input (when-row {:has-column? :nope} (ds/head 1))))
            :cljs (when-row-result
                   "(table-> :input (when-row {:has-column? :nope} (ds/head 1)))"))
         #(is (= 2 (row-count %)) "pass-through -> 2 rows")))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.run.when-row-runtime-test")))))
