;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.feature-test
  (:require [clojure.test :refer [deftest is testing]]
            [cg.feature :as f]
            [cg.run.fusion :as fusion]
            ;; load so its register-operation! calls have run
            [cg.dataset]))

(set! *warn-on-reflection* true)

(deftest register-operation-row-shape-fields
  (testing "register-operation! stores :row-shape, :reads, :writes when supplied"
    (f/register-operation! "test.feature/per-row-op"
                           {:type :transformer
                            :row-shape :per-row
                            :reads #{:geometry}
                            :writes #{:area}
                            :params []})
    (let [op (f/get-operation-info "test.feature/per-row-op")]
      (is (= :per-row (:row-shape op)))
      (is (= #{:geometry} (:reads op)))
      (is (= #{:area} (:writes op))))))

(deftest register-operation-row-shape-defaults
  (testing "register-operation! auto-derives :row-shape from :type + :params"
    (f/register-operation! "test.feature/source-op"  {:type :source :params []})
    (f/register-operation! "test.feature/sink-op"    {:type :sink :params []})
    (f/register-operation! "test.feature/agg-op"     {:type :transformer :params []})
    (f/register-operation! "test.feature/per-row-auto"
                           {:type :transformer
                            :params [{:name :row :role :row}]})
    (is (= :source        (:row-shape (f/get-operation-info "test.feature/source-op"))))
    (is (= :terminal      (:row-shape (f/get-operation-info "test.feature/sink-op"))))
    (is (= :whole-dataset (:row-shape (f/get-operation-info "test.feature/agg-op"))))
    (is (= :per-row       (:row-shape (f/get-operation-info "test.feature/per-row-auto")))))
  (testing "register-operation! defaults :reads and :writes to empty sets"
    (f/register-operation! "test.feature/no-rw" {:type :transformer :params []})
    (let [op (f/get-operation-info "test.feature/no-rw")]
      (is (= #{} (:reads op)))
      (is (= #{} (:writes op))))))

(deftest row-shape-derivation
  (testing "group-by consumes the whole dataset"
    (is (= :whole-dataset (:row-shape (f/get-operation-info "cg.dataset/group-by")))))
  (testing "map over groups is its own shape"
    (is (= :over-groups (:row-shape (f/get-operation-info "cg.dataset/map")))))
  (testing "into demotes the whole groups container"
    (is (= :whole-dataset (:row-shape (f/get-operation-info "cg.dataset/into"))))))

(deftest higher-order-ops-do-not-fuse
  (testing "map and into break a per-row chain"
    (f/register-operation! "test.feature/row-op"
                           {:type :transformer :row-shape :per-row :params []})
    (let [ids   ["a" "m" "b" "i" "c"]
          ops   ["test.feature/row-op" "cg.dataset/map" "test.feature/row-op"
                 "cg.dataset/into" "test.feature/row-op"]
          model {:nodes (mapv (fn [id op] {:id id :op op}) ids ops)
                 :edges (mapv (fn [a b] {:from a :to b}) ids (rest ids))}]
      (is (= [["a"] ["m"] ["b"] ["i"] ["c"]]
             (:groups (fusion/plan-fusion model)))))))
