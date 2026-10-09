;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.feature-test
  (:require [clojure.test :refer [deftest is]]
            [cg.feature :as f]
            [cg.run.fusion :as fusion]
            ;; load so its register-operation! calls have run
            [cg.dataset]))

(set! *warn-on-reflection* true)

(deftest register-operation-row-shape-fields
  (f/register-operation! "test.feature/per-row-op"
                         {:type :transformer
                          :row-shape :per-row
                          :reads #{:geometry}
                          :writes #{:area}
                          :params []})
  (let [op (f/get-operation-info "test.feature/per-row-op")]
    (is (= :per-row (:row-shape op)))
    (is (= #{:geometry} (:reads op)))
    (is (= #{:area} (:writes op)))))

(deftest register-operation-row-shape-defaults
  (f/register-operation! "test.feature/source-op"  {:type :source :params []})
  (f/register-operation! "test.feature/sink-op"    {:type :sink :params []})
  (f/register-operation! "test.feature/agg-op"     {:type :transformer :params []})
  (f/register-operation! "test.feature/per-row-auto"
                         {:type :transformer
                          :params [{:name :row :role :row}]})
  (is (= :source        (:row-shape (f/get-operation-info "test.feature/source-op"))))
  (is (= :terminal      (:row-shape (f/get-operation-info "test.feature/sink-op"))))
  (is (= :whole-dataset (:row-shape (f/get-operation-info "test.feature/agg-op"))))
  (is (= :per-row       (:row-shape (f/get-operation-info "test.feature/per-row-auto"))))
  (f/register-operation! "test.feature/no-rw" {:type :transformer :params []})
  (let [op (f/get-operation-info "test.feature/no-rw")]
    (is (= #{} (:reads op)))
    (is (= #{} (:writes op)))))

(deftest row-shape-derivation
  (is (= :whole-dataset (:row-shape (f/get-operation-info "cg.dataset/group-by"))))
  (is (= :over-groups (:row-shape (f/get-operation-info "cg.dataset/map"))))
  (is (= :whole-dataset (:row-shape (f/get-operation-info "cg.dataset/into")))))

(deftest higher-order-ops-do-not-fuse
  (f/register-operation! "test.feature/row-op"
                         {:type :transformer :row-shape :per-row :params []})
  (let [ids   ["a" "m" "b" "i" "c"]
        ops   ["test.feature/row-op" "cg.dataset/map" "test.feature/row-op"
               "cg.dataset/into" "test.feature/row-op"]
        model {:nodes (mapv (fn [id op] {:id id :op op}) ids ops)
               :edges (mapv (fn [a b] {:from a :to b}) ids (rest ids))}]
    (is (= [["a"] ["m"] ["b"] ["i"] ["c"]]
           (:groups (fusion/plan-fusion model))))))
