;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.columns-test
  "cg.columns: the value kinds of rows, and the rows that a cache read
   gives back with them.

   A parquet write turns a keyword into a string. A cache write records
   the columns that held keywords (value-kinds), and a cache read gives
   those columns their keywords again (rehydrate-rows)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cg.dataset :as ds]
            [cg.run :as run]))

(set! *warn-on-reflection* true)

(use-fixtures :once
  (fn [t]
    (run/load-module "src/cg/columns.cg")
    (t)))

(defn- columns-fn [n]
  (deref (run/eval-cg (str "#'cg.columns/" n))))

(deftest value-kinds-names-the-columns-that-a-write-turns-into-strings
  (let [rows [{:district :rs-2 :in-flood-zone true  :corner? true  :lot-type nil
               :nbhd "Upper Town" :lot-area 5000.0}
              {:district :rs-3 :in-flood-zone :unknown :corner? false :lot-type :corner
               :nbhd "Lower Town" :lot-area 6000.0}]]
    (is (= {:keyword-cols #{"district" "lot-type"}
            :three-valued-cols #{"in-flood-zone"}
            :boolean-cols #{"corner?"}}
           ((columns-fn "value-kinds") rows)))))

(deftest rehydrate-rows-restores-the-recorded-kinds
  (let [dset  (ds/->dataset [{"district" "rs-2" "in-flood-zone" "unknown" "nbhd" "Lower Town" "corner?" 1}
                             {"district" "rs-3" "in-flood-zone" "true"    "nbhd" "rs-2"       "corner?" 0}])
        kinds {:keyword-cols #{"district"} :three-valued-cols #{"in-flood-zone"}
               :boolean-cols #{"corner?"}}
        rows  ((columns-fn "rehydrate-rows") dset kinds)]
    (is (= [{:district :rs-2 :in-flood-zone :unknown :nbhd "Lower Town" :corner? true}
            {:district :rs-3 :in-flood-zone true     :nbhd "rs-2"       :corner? false}]
           rows)
        "keys become keywords; a column that kinds does not list stays a string")
    (testing "a GeoPackage read gives a boolean as 1 or 0, and a nil stays nil"
      (is (= [nil] (mapv :corner? ((columns-fn "rehydrate-rows")
                                   (ds/->dataset [{"corner?" nil "id" "a"}]) kinds)))))))
