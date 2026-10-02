;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.run.op-component-test
  "A table op that builds a Model returns the Model, which is data. The
   runner decides when the Model runs: run-component runs it with its own
   opts, and with the args of the op as the bindings."
  (:require [clojure.test :refer [deftest is testing]]
            [cg.run :as run]
            [cg.model :as model]
            [cg.dataset :as ds]))

(set! *warn-on-reflection* true)

(def ^:private src
  "(require '[cg.macros :refer [table->]])
   (defn ^:op comp-tag [^:row row k] (assoc! row :k k))
   (defn ^:op comp-run [^:table rows k] (table-> :rows (comp-tag k)))")

(defn- table-rows [d]
  (ds/rows (if (sequential? d) (first d) d) :as-maps))

(deftest a-direct-call-gives-the-model
  (run/eval-cg src)
  (is (model/model? (run/eval-cg "(comp-run nil 7)"))))

(deftest run-component-runs-the-model-with-its-opts
  (run/eval-cg src)
  (let [rows (ds/->dataset {:id [1 2]})
        calls (atom [])
        real-run-model run/run-model]
    (with-redefs [run/run-model (fn [m b o]
                                  (swap! calls conj {:bindings b :opts o})
                                  (real-run-model m b o))]
      (is (= [7 7] (mapv :k (table-rows (run/run-component "user/comp-run"
                                                            {:rows rows :k 7}
                                                            {:timeout-ms 12345}))))))
    (testing "the first run-model call is the run of the op body"
      (is (= {:rows rows :k 7} (:bindings (first @calls))))
      (is (= 12345 (:timeout-ms (:opts (first @calls)))))
      (is (= "comp-run-body" (:name (:opts (first @calls))))))))

(deftest a-bare-table-binds-the-table-param
  (run/eval-cg src)
  (is (= [1 2] (mapv :id (table-rows (run/run-component "user/comp-run"
                                                         (ds/->dataset {:id [1 2]})
                                                         {}))))))
