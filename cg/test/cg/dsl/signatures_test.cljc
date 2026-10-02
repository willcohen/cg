;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.dsl.signatures-test
  (:require [clojure.test :refer [deftest testing is are]]
            [cg.dsl.signatures :as sigs]))

#?(:clj (set! *warn-on-reflection* true))

(deftest output-level-metadata
  (testing "output sig carries an optional :level"
    (let [o (sigs/output "result" :dataset :level :groups)]
      (is (= :groups (:level o)))))
  (testing "level defaults to nil (back-compat)"
    (let [o (sigs/output "result" :dataset)]
      (is (nil? (:level o)))))
  (testing "level-kinds is the closed set"
    (is (= #{:feature :collection :dataset :groups :map} sigs/level-kinds))))

(deftest builtin-op-levels
  ;; Keys are source spellings (ds/load-geojson, bare filter), not the
  ;; qualified cg.dataset/* names.
  (are [qn lvl] (= lvl (-> (sigs/lookup qn) :outputs first :level))
    "ds/load-geojson"     :dataset
    "filter"              :dataset
    "ds/filter-by-column" :dataset
    "group-by"            :groups
    "partition-by"        :groups))

(deftest higher-order-and-demote-ops
  (testing "map is a higher-order op whose output stays at :groups"
    (let [s (sigs/lookup "map")]
      (is (= :groups (-> s :outputs first :level)))
      (is (= :transformer (:kind s)))))
  (testing "into demotes groups -> a keyed map"
    (let [s (sigs/lookup "into")]
      (is (= :map (-> s :outputs first :level))))))

(deftest table-input-role
  (is (= :table (sigs/role-kind "table")))
  (is (= :table (sigs/role-kind :table)))
  (testing "existing roles unchanged"
    (is (= :row (sigs/role-kind "row")))
    (is (= :broadcast (sigs/role-kind "broadcast")))
    (is (= :reducer (sigs/role-kind "reducer")))
    (is (nil? (sigs/role-kind nil)))))
