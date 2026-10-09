;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.run.eos-test
  "A flow completes when its sink gets end-of-stream from each input, not
   after a count. The step factory ops here send a number of batches that the
   sources do not know, and the item ops drop items or join them."
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [cg.run :as run]
                    [cg.run.eos :as eos]
                    [cg.dataset :as ds]
                    [cg.feature :as feature]
                    [cg.test-runner :refer [after error-message]])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../../src/cg/run.mjs" :as run]
                     ["../../../src/cg/run/eos.mjs" :as eos]
                     ["../../../src/cg/dataset.mjs" :as ds]
                     ["../../../src/cg/feature.mjs" :as feature]
                     ["../wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["../test_runner.mjs" :refer [run_tests_and_exit_BANG_ after error-message]])))

#?(:clj (set! *warn-on-reflection* true))

(defn- step-of
  "A step factory. For each batch msg, its step sends the vector (out msg)."
  [out]
  (fn []
    (fn
      ([] {:ins {:in "Batches"} :outs {:out "Batches"}})
      ([_args] {})
      ([state _transition] state)
      ([state _in msg] [state {:out (out msg)}]))))

(defn- register-ops! []
  (doseq [[op-name f] [["twice" (step-of (fn [msg] [msg msg]))]
                       ["pass" (step-of vector)]
                       ["boom" (step-of (fn [_] (throw (ex-info "boom" {}))))]]]
    (run/register-bare-op! op-name "cg.eos-test" f)
    (feature/register-operation! (str "cg.eos-test/" op-name)
                                 {:type :step-factory :params []})))

(defn- total [items] (reduce + 0 items))

(defn- keep-odd [x] (when (odd? x) x))

(defn- register-item-ops! []
  (doseq [[op-name f] [["total" total] ["keep-odd" keep-odd]]]
    (run/register-bare-op! op-name "cg.eos-test" f)
    (feature/register-operation! (str "cg.eos-test/" op-name)
                                 {:type :transformer :params []})))

(defn- item-node [id op-name mode]
  {:type :operation :id id :fn (str "cg.eos-test/" op-name)
   :params {} :mode mode})

(defn- chain-model
  "A model of the source `src`, the ops `nodes` in a chain, and a sink."
  [src & nodes]
  (let [ids (concat [(:id src)] (map :id nodes) [:result])]
    {:nodes (vec (concat [src] nodes [{:type :sink :id :result :output-binding :result}]))
     :edges (mapv (fn [a b] {:from a :to b :type :data-flow}) ids (rest ids))
     :config {}}))

(defn- fan-in-model
  "A model of the sources :a and :b, both into the op `node`, and a sink.
   `src` holds the other keys of each source."
  [node src]
  {:nodes [(merge {:type :source :id :a-src :data-ref :a} src)
           (merge {:type :source :id :b-src :data-ref :b} src)
           node
           {:type :sink :id :result :output-binding :result}]
   :edges [{:from :a-src :to (:id node) :type :data-flow}
           {:from :b-src :to (:id node) :type :data-flow}
           {:from (:id node) :to :result :type :data-flow}]
   :config {}})

(def ^:private nums-src {:type :source :id :nums-src :data-ref :nums})

(def ^:private input-src {:type :source :id :input-src :data-ref :input :schema {}})

(def ^:private two-coll-model
  (chain-model nums-src (item-node :total-1 "total" :coll) (item-node :total-2 "total" :coll)))

(def ^:private keep-odd-model
  (chain-model nums-src (item-node :keep-1 "keep-odd" :item)))

(def ^:private item-fan-in-model
  (fan-in-model (item-node :keep-1 "keep-odd" :item) {}))

(defn- op-node [id op-name]
  {:type :operation :id id :fn (str "cg.eos-test/" op-name)
   :params {} :mode :table :config {}})

(def ^:private twice-model (chain-model input-src (op-node :op-1 "twice")))

(def ^:private boom-model (chain-model input-src (op-node :op-1 "boom")))

(def ^:private table-fan-in-model
  (fan-in-model (op-node :op-1 "pass") {:schema {}}))

(defn- result-count
  "The row count of a table-mode result. The JVM can give the dataset in a
   sequence."
  [result]
  (ds/row-count (if (sequential? result) (first result) result)))

(defn- n-rows [n]
  (ds/->dataset {:n (vec (range n))}))

(defn- batch-step
  "A step that gives back its batch and keeps no state, as lift1->step does."
  []
  (fn
    ([] {:ins {:in "Batches"} :outs {:out "Batches"}})
    ([_args] nil)
    ([_state _transition] nil)
    ([_state _in msg] [nil {:out [msg]}])))

(deftest eos-is-a-keyword
  (is (eos/eos? eos/eos))
  (is (eos/eos? :cg.run/eos))
  (is (not (eos/eos? nil)))
  (is (not (eos/eos? {:cg.run/eos true}))))

(deftest forward-eos-waits-for-each-input
  (let [step (eos/forward-eos (batch-step) 2)
        s0 (step {})
        [s1 out1] (step s0 :in :batch)
        [s2 out2] (step s1 :in eos/eos)
        [_ out3] (step s2 :in eos/eos)]
    (is (= {:out [:batch]} out1) "a batch goes to the step")
    (is (empty? (:out out2)) "one input of two sent end-of-stream")
    (is (= {:out [eos/eos]} out3) "both inputs sent end-of-stream")))

(deftest forward-eos-keeps-its-count-when-the-step-gives-nil
  (let [step (eos/forward-eos (batch-step) 2)
        [s1 _] (step (step {}) :in eos/eos)
        s2 (step s1 :clojure.core.async.flow/pause)
        [s3 _] (step s2 :in :batch)
        [_ out] (step s3 :in eos/eos)]
    (is (= {:out [eos/eos]} out)
        "the count survives a transition and a batch, which give nil state")))

(deftest forward-eos-sends-eos-for-one-input
  (let [step (eos/forward-eos (batch-step) 1)
        [_ out] (step (step {}) :in eos/eos)]
    (is (= {:out [eos/eos]} out))))

(deftest add-eos-follows-each-emission
  (is (= [{:emitted true} {:out [1 eos/eos]}]
         (eos/add-eos [{:emitted true} {:out [1]}])))
  (is (= [{} {}] (eos/add-eos [{} {}])) "no emission, no end-of-stream"))

(deftest a-step-with-more-batches-out-than-in-completes
  (testing "the sink waits for end-of-stream, not for the batch count of the source"
    (register-ops!)
    (after (run/run-model twice-model {:input (n-rows 3)} {:timeout-ms 10000 :batch-size 2})
           #(is (= 6 (result-count %)) "each of the 3 rows comes two times"))))

(deftest an-op-with-two-inputs-gets-the-rows-of-both
  (register-ops!)
  (after (run/run-model table-fan-in-model {:a (n-rows 2) :b (n-rows 3)}
                        {:timeout-ms 10000 :batch-size 2})
         #(is (= 5 (result-count %)) "the rows of both sources")))

(deftest a-chain-of-two-collection-ops-completes
  (testing "the second op gets one item and completes on end-of-stream"
    (register-item-ops!)
    (after (run/run-model two-coll-model {:nums [1 2 3]} {:timeout-ms 3000})
           #(is (= [6] (vec %))))))

(deftest an-item-op-that-drops-items-completes
  (register-item-ops!)
  (after (run/run-model keep-odd-model {:nums [1 2 3]} {:timeout-ms 3000})
         #(is (= [1 3] (vec %)))))

(deftest a-source-with-no-items-completes
  (register-item-ops!)
  (after (run/run-model keep-odd-model {:nums []} {:timeout-ms 3000})
         #(is (= [] (vec %)))))

(deftest an-item-op-with-two-inputs-gets-the-items-of-both
  (register-item-ops!)
  (after (run/run-model item-fan-in-model {:a [1 2] :b [3 4 5]} {:timeout-ms 3000})
         #(is (= [1 3 5] (vec (sort %))))))

(deftest a-failed-batch-fails-the-run
  (testing "end-of-stream completes the sink, but the error of the step wins"
    (register-ops!)
    (after (error-message #(run/run-model boom-model {:input (n-rows 3)} {:timeout-ms 10000}))
           #(is (re-find #"boom" (str %)) "the message names the error of the step"))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (ds/init-arquero!)))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.run.eos-test")))))
