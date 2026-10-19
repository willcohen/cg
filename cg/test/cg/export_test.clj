;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.export-test
  "cg.export/column-order: the order of the export columns, from a spec."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cg.run :as run]))

(set! *warn-on-reflection* true)

(use-fixtures :once
  (fn [t]
    (run/load-module "src/cg/export.cg")
    (t)))

(defn- column-order [cols spec]
  ((deref (run/eval-cg "#'cg.export/column-order")) cols spec))

(def ^:private new-columns {:any [{:prefix "new_"} {:suffix "_new"}]})

(def ^:private spec
  {:ordered [["id" "name"] ["area"]]
   :groups  [{:prefix "actual_"}
             {:contains "conformity" :not new-columns}
             new-columns]})

(deftest column-order-follows-its-spec
  (testing "the listed names come first, in their lists' order"
    (is (= ["id" "name" "area" :geometry]
           (column-order [:geometry "area" "name" "id"] spec))))
  (testing "the first group that matches wins, and :not keeps a new column
            out of the conformity group"
    (is (= ["actual_width" "lot_conformity" "conformity_new" "new_district"
            "zoning" :geometry]
           (column-order ["zoning" "new_district" "conformity_new" :geometry
                          "lot_conformity" "actual_width"]
                         spec))))
  (testing "each key of a rule must hold"
    (is (= ["a_base" "a_other" "zz_base"]
           (column-order ["zz_base" "a_other" "a_base"]
                         {:groups [{:prefix "a" :suffix "_base"}]})))))
