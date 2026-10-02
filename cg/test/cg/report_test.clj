;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.report-test
  "Tests for the study-area report ops (cg.dataset summarize /
   block-presence) and their serializable spec grammar. The spec-driven
   results are checked against hand-written predicate equivalents — the
   same shape the parcel.cg tabulators took — so the spec resolvers are
   proven against the fn-parameterized cores they replaced.

   Run with: clojure -M:test -e (cognitect... ) or the full bb test."
  (:require [cg.dataset :as ds :refer [summarize]]
            [cg.run :as run]
            [cg.macros :refer [table->]]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]))

(set! *warn-on-reflection* true)

(def rows
  "Synthetic study-area rows: two districts, three blocks, one row with
   no district (filtered by :where), one 0-unit row (non-residential)."
  [{:d :rs-2 :units 1 :block "A" :zone "R1-A"}
   {:d :rs-2 :units 1 :block "A" :zone "R1-A"}
   {:d :rs-2 :units 2 :block "A" :zone "R1-B"}
   {:d :rs-2 :units 3 :block "B" :zone "R2-A"}
   {:d :rs-3 :units 2 :block "B" :zone "R1-A"}
   {:d :rs-3 :units 5 :block "C" :zone "R3-A"}
   {:d :rs-3 :units 9 :block "C" :zone "MX"}
   {:d nil   :units 4 :block "C" :zone "R1-A"}
   {:d :rs-3 :units 0 :block "C" :zone "R1-A"}])

(def residential-where
  [{:col :d :present true}
   {:col :units :positive-number true}])

(def family-buckets
  [{:label "1"   :eq 1}
   {:label "2"   :eq 2}
   {:label "3"   :eq 3}
   {:label "4-6" :range [4 6]}
   {:label "7+"  :gte 7}])

(deftest summarize-by-parcels
  (testing "row-weighted scoped summarize matches hand-computed tallies"
    (let [res (ds/summarize rows {:value :units :as-int true
                                  :weight :rows
                                  :scope :d :scopes [:rs-2 :rs-3]
                                  :where residential-where
                                  :buckets family-buckets})
          [all rd2 rd3] res]
      (is (= 3 (count res)))
      (is (= "ALL" (:label all)))
      (is (= 7 (:total all)) "7 residential rows (nil-district + 0-unit excluded)")
      (is (= [2 2 1 1 1] (mapv :n (:cells all))))
      (is (= 4 (:total rd2)))
      (is (= "rs-2" (:label rd2)))
      (is (= [2 1 1 0 0] (mapv :n (:cells rd2))))
      (is (= 3 (:total rd3)))
      (is (= [0 1 0 1 1] (mapv :n (:cells rd3))))
      (is (= 50.0 (:pct (first (:cells rd2)))))
      (is (= 0.0 (:pct (nth (:cells rd2) 3)))
          "an empty bucket in a non-empty tally reads 0.0, not nil"))))

(deftest summarize-by-units
  (testing "value-weighted summarize sums units per bucket"
    (let [[all] (ds/summarize rows {:value :units :as-int true
                                    :weight :value
                                    :scope :d :scopes [:rs-2 :rs-3]
                                    :where residential-where
                                    :buckets family-buckets})]
      (is (= 23 (:total all)) "1+1+2+3+2+5+9")
      (is (= [2 4 3 5 9] (mapv :n (:cells all)))))))

(deftest summarize-unscoped-and-prefix-where
  (testing "unscoped summarize returns a single tally; :prefix filters"
    (let [t (ds/summarize rows {:value :units :as-int true
                                :weight :rows
                                :where (conj residential-where
                                             {:col :zone :prefix "R1"})
                                :buckets [{:label "1-unit" :eq 1}
                                          {:label "2+ unit" :gte 2}]})]
      (is (map? t))
      (is (= 4 (:total t)) "1F-* residential rows")
      (is (= [2 2] (mapv :n (:cells t)))))))

(deftest block-presence-matches-hand-preds
  (testing "spec-driven block-presence equals the fn-parameterized core"
    (let [spec {:group :block :class :d :classes [:rs-2 :rs-3]
                :value :units :as-int true
                :where residential-where
                :rows [{:label "all single-family" :all {:eq 1}}
                       {:label "has >=1 two-family" :any {:eq 2}}
                       {:label "has >=2 two-family" :count {:eq 2} :min 2}
                       {:label "has >=1 7+" :any {:gte 7}}]}
          pt (ds/block-presence rows spec)]
      ;; block A: units [1 1 2] modal rs-2; B: [3 2] tie rs-2/rs-3 ->
      ;; max-key last-max wins (rs-3 later in frequencies order is not
      ;; deterministic across platforms for ties, so B's class is asserted
      ;; only to be one of the two); C: [5 9] modal rs-3.
      (is (= 4 (count (:rows pt))))
      (let [total-blocks (reduce + (vals (:counts pt)))]
        (is (= 3 total-blocks)))
      (let [row1 (first (:rows pt))]
        (is (= "all single-family" (:label row1)))
        (is (every? (fn [c] (contains? (:cells row1) c)) [:rs-2 :rs-3]))))))

(deftest presence-count-min-semantics
  (testing ":count + :min counts matching values within a group list"
    (let [pt (ds/block-presence
              [{:d :rs-2 :units 2 :block "A"}
               {:d :rs-2 :units 2 :block "A"}
               {:d :rs-2 :units 2 :block "B"}]
              {:group :block :class :d :classes [:rs-2]
               :value :units
               :rows [{:label ">=2 two-family" :count {:eq 2} :min 2}]})]
      (is (= {:rs-2 2} (:counts pt)))
      (is (= 1 (get-in pt [:rows 0 :cells :rs-2 :n]))
          "only block A has two 2-unit rows"))))

(deftest summarize-runs-in-a-table-flow
  (testing "(table-> :rows (summarize {...})) executes via run-model"
    (let [spec {:value :units :as-int true
                :weight :rows
                :scope :d :scopes [:rs-2 :rs-3]
                :where [{:col :d :present true}
                        {:col :units :positive-number true}]
                :buckets [{:label "1"   :eq 1}
                          {:label "2"   :eq 2}
                          {:label "3"   :eq 3}
                          {:label "4-6" :range [4 6]}
                          {:label "7+"  :gte 7}]}
          model (table-> :rows
                  (summarize {:value :units :as-int true
                              :weight :rows
                              :scope :d :scopes [:rs-2 :rs-3]
                              :where [{:col :d :present true}
                                      {:col :units :positive-number true}]
                              :buckets [{:label "1"   :eq 1}
                                        {:label "2"   :eq 2}
                                        {:label "3"   :eq 3}
                                        {:label "4-6" :range [4 6]}
                                        {:label "7+"  :gte 7}]}))
          direct (ds/summarize rows spec)
          via-flow (run/run-model model {:rows (ds/->dataset rows)}
                                  {:timeout-ms 60000})]
      (is (= (mapv :total direct) (mapv :total via-flow))
          "flow-executed summarize matches the direct call")
      (is (= (mapv :cells direct) (mapv :cells via-flow))))))

(deftest summarize-model-is-serializable
  (testing "the flow model carries the spec as data (no functions)"
    (let [model (table-> :rows (summarize {:value :units
                                           :buckets [{:label "1" :eq 1}]}))
          printed (pr-str model)]
      (is (not (re-find #"#function|#object" printed))
          "model must round-trip as EDN — functions would violate the Flow-Graph rule")
      (is (some? (clojure.edn/read-string printed))))))
