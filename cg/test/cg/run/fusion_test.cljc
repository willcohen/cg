;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.run.fusion-test
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [clojure.string :as str]
                    [cg.run.fusion :as fusion]
                    [cg.feature :as f])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../../src/cg/run/fusion.mjs" :as fusion]
                     ["../../../src/cg/feature.mjs" :as f]
                     ["../test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(defn- setup-fixture-ops! []
  (f/register-operation! "test.fusion/a" {:type :transformer :row-shape :per-row :params []})
  (f/register-operation! "test.fusion/b" {:type :transformer :row-shape :per-row :params []})
  (f/register-operation! "test.fusion/c" {:type :transformer :row-shape :whole-dataset :params []})
  (f/register-operation! "test.fusion/d" {:type :transformer :row-shape :per-row :params []}))

(deftest fused-chain-applies-row-fns-with-per-row-catch
  (let [row-fns [(fn [r] (assoc! r :a (* (:n r) 2)))
                 (fn [r] (assoc! r :b (inc (:a r))))
                 (fn [r] (if (zero? (:n r))
                           (throw (ex-info "no zero" {}))
                           (assoc! r :c (str "ok-" (:n r)))))]
        rows [{:n 0} {:n 1} {:n 2}]
        results (mapv #(fusion/apply-row-chain % row-fns) rows)]
    (is (= "no zero" (-> results first :error)) "row 0 captured :error")
    (is (= 3 (-> results second :b)) "row 1 ran through op2")
    (is (= "ok-2" (-> results last :c)) "row 2 ran through op3")
    (is (every? (fn [r] (or (contains? r :a) (contains? r :error))) results)
        "all rows touched op1 (or short-circuited via :error)")
    (is (nil? (-> results second :error)) "row 1 has no :error")))

#?(:clj
   (deftest apply-row-chain-diag-names-the-row-by-its-id-keys
     (let [boom (fn [_] (throw (ex-info "boom" {})))
           row  {:PID "p-7" :LOT_ID "m-1" :id "i-1"}]
       (testing "the row-fn's :row-id-keys metadata picks the id in the DIAG line"
         (let [out (with-out-str
                     (fusion/apply-row-chain row [(with-meta boom {:row-id-keys [:PID]})]))]
           (is (str/includes? out "row-id= p-7") out)))
       (testing "with no :row-id-keys, the DIAG line reads :id"
         (let [out (with-out-str (fusion/apply-row-chain row [boom]))]
           (is (str/includes? out "row-id= i-1") out))))))

#?(:clj
   (deftest topology-retry-repairs-geometry-when-an-op-declares-no-reads
     ;; register-op! registers #{} for an op with no :row-reads. The retry must then
     ;; repair :geometry, as it does for an op with no metadata.
     (let [bowtie (f/polygon [[0 0] [10 10] [10 0] [0 10] [0 0]])
           needs-valid (fn [r]
                         (if (.isValid ^org.locationtech.jts.geom.Geometry (:geometry r))
                           (assoc! r :ok true)
                           (throw (org.locationtech.jts.geom.TopologyException. "invalid"))))
           out (fusion/apply-row-chain {:geometry bowtie}
                                       [(with-meta needs-valid {:row-reads #{}})])]
       (is (nil? (:error out)) (:error out))
       (is (true? (:ok out))))))

#?(:clj
   (deftest parallel-map-preserves-order
     (testing "output order matches input order even when the slowest task is first"
       (let [exec (java.util.concurrent.Executors/newFixedThreadPool 4)]
         (try
           (let [f #(do (Thread/sleep (long (- 50 (* % 5)))) (* % 2))
                 out (fusion/parallel-map exec f [0 1 2 3 4 5 6 7])]
             (is (= [0 2 4 6 8 10 12 14] out)))
           (finally (.shutdown exec)))))))

(deftest plan-fusion-groups-consecutive-per-row
  (setup-fixture-ops!)
  (testing "two consecutive per-row nodes fuse; whole-dataset breaks the chain"
    (let [model {:nodes [{:id "n1" :op "test.fusion/a"}
                         {:id "n2" :op "test.fusion/b"}
                         {:id "n3" :op "test.fusion/c"}]
                 :edges [{:from "n1" :to "n2"} {:from "n2" :to "n3"}]}
          plan (fusion/plan-fusion model)]
      (is (= [["n1" "n2"] ["n3"]] (:groups plan)))))
  (testing "per-row → whole-dataset → per-row stays as three separate groups"
    (let [model {:nodes [{:id "n1" :op "test.fusion/a"}
                         {:id "n2" :op "test.fusion/c"}
                         {:id "n3" :op "test.fusion/d"}]
                 :edges [{:from "n1" :to "n2"} {:from "n2" :to "n3"}]}
          plan (fusion/plan-fusion model)]
      (is (= [["n1"] ["n2"] ["n3"]] (:groups plan)))))
  (testing "single per-row node returns one-element group"
    (let [model {:nodes [{:id "n1" :op "test.fusion/a"}]
                 :edges []}
          plan (fusion/plan-fusion model)]
      (is (= [["n1"]] (:groups plan)))))
  (testing "all-per-row chain fuses into one group"
    (let [model {:nodes [{:id "n1" :op "test.fusion/a"}
                         {:id "n2" :op "test.fusion/b"}
                         {:id "n3" :op "test.fusion/d"}]
                 :edges [{:from "n1" :to "n2"} {:from "n2" :to "n3"}]}
          plan (fusion/plan-fusion model)]
      (is (= [["n1" "n2" "n3"]] (:groups plan))))))

#?(:cljs (run_tests_and_exit_BANG_ "cg.run.fusion-test"))
