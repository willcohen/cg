;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.dsl.signatures-test
  #?(:clj (:require [clojure.test :refer [deftest is are]]
                    [cg.dsl.signatures :as sigs])
     :cljs (:require [cljs.test :refer [deftest is are]]
                     ["../../../src/cg/dsl/signatures.mjs" :as sigs]
                     ["../test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(deftest output-level-metadata
  (is (= :groups (:level (sigs/output "result" :dataset :level :groups))))
  (is (nil? (:level (sigs/output "result" :dataset))) "back-compat")
  (is (= #{:feature :collection :dataset :groups :map} sigs/level-kinds)))

(deftest builtin-op-levels
  ;; Keys are source spellings (ds/load-geojson, bare filter), not the
  ;; qualified cg.dataset/* names.
  (are [qn lvl] (= lvl (-> (sigs/lookup qn) :outputs first :level))
    "ds/load-geojson"     :dataset
    "filter"              :dataset
    "ds/filter-by-column" :dataset
    "group-by"            :groups
    "partition-by"        :groups
    "map"                 :groups
    "into"                :map))

(deftest map-is-a-transformer
  (is (= :transformer (:kind (sigs/lookup "map")))))

(deftest table-input-role
  (is (= :table (sigs/role-kind "table")))
  (is (= :table (sigs/role-kind :table)))
  (is (= :row (sigs/role-kind "row")))
  (is (= :broadcast (sigs/role-kind "broadcast")))
  (is (= :reducer (sigs/role-kind "reducer")))
  (is (nil? (sigs/role-kind nil))))

#?(:cljs (run_tests_and_exit_BANG_ "cg.dsl.signatures-test"))
