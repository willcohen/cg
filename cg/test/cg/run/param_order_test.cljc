;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.run.param-order-test
  "An op gets its named params in the order of its registered :params schema.
   On the JVM, a map of more than 8 entries is a hash map, which keeps no
   insertion order. Because of this, each op here takes 9 params. When `vals`
   set the order, the JVM ops got the params map below as (5 7 3 8 2 4 6 9 1).

   ONE .cljc suite runs on both runtimes. Each path that changes a params map to
   positional args has a test: a source op, an item op, a per-row chain (fused
   on the JVM), and the body of a when-row."
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [cg.run :as run]
                    [cg.dataset :as ds]
                    [cg.feature :as feature])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../../src/cg/run.mjs" :as run]
                     ["../../../src/cg/dataset.mjs" :as ds]
                     ["../../../src/cg/feature.mjs" :as feature]
                     ["../wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["../test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private param-names [:a :b :c :d :e :f :g :h :i])

(def ^:private params (zipmap param-names (range 1 10)))

(def ^:private in-order [1 2 3 4 5 6 7 8 9])

(defn- source-op [a b c d e f g h i] [a b c d e f g h i])

(defn- item-op [_item a b c d e f g h i] [a b c d e f g h i])

(defn- dataset-op [_dataset a b c d e f g h i] [a b c d e f g h i])

(defn- row-op-1 [row a b c d e f g h i] (assoc! row :got-1 (str a b c d e f g h i)))

(defn- row-op-2 [row a b c d e f g h i] (assoc! row :got-2 (str a b c d e f g h i)))

(defn- register-ops! []
  (let [schema (mapv (fn [n] {:name n}) param-names)
        register! (fn [op-name op-fn info]
                    (run/register-bare-op! op-name "cg.param-order" op-fn)
                    (feature/register-operation! (str "cg.param-order/" op-name)
                                                 (assoc info :params schema)))]
    (register! "source-op" source-op {:type :source})
    (register! "item-op" item-op {:type :transformer})
    (register! "dataset-op" dataset-op {:type :transformer})
    (register! "row-op-1" row-op-1 {:type :transformer :row-shape :per-row})
    (register! "row-op-2" row-op-2 {:type :transformer :row-shape :per-row})))

(def ^:private source-model
  {:nodes [{:type :operation :id :op-1 :fn "cg.param-order/source-op"
            :params params :execution :item-level}
           {:type :sink :id :result :output-binding :result}]
   :edges [{:from :op-1 :to :result :type :data-flow}]
   :config {}})

(def ^:private item-model
  {:nodes [{:type :source :id :data-src :data-ref :data}
           {:type :operation :id :op-1 :fn "cg.param-order/item-op"
            :params params :execution :item-level}
           {:type :sink :id :result :output-binding :result}]
   :edges [{:from :data-src :to :op-1 :type :data-flow}
           {:from :op-1 :to :result :type :data-flow}]
   :config {}})

(def ^:private row-chain-model
  {:nodes [{:type :source :id :input-src :data-ref :input :schema {}}
           {:type :operation :id :op-1 :fn "cg.param-order/row-op-1"
            :params params :mode :table :config {}}
           {:type :operation :id :op-2 :fn "cg.param-order/row-op-2"
            :params params :mode :table :config {}}
           {:type :sink :id :result :output-binding :result}]
   :edges [{:from :input-src :to :op-1 :type :data-flow}
           {:from :op-1 :to :op-2 :type :data-flow}
           {:from :op-2 :to :result :type :data-flow}]
   :config {}})

(defn- result-rows
  "The rows of a table-mode result as maps. A flow with one sink gives a
   sequence that holds the dataset on the JVM, and the dataset itself on cljs."
  [result]
  #?(:clj (ds/rows (if (sequential? result) (first result) result) :as-maps)
     :cljs (let [d (if (array? result) (nth result 0) result)]
             (if (array? d) d (ds/rows d)))))

(defn- check-rows! [rows]
  (is (= 2 (count rows)) "both rows pass through")
  (is (= ["123456789" "123456789"] (mapv :got-1 rows)) "the first op gets 1 to 9")
  (is (= ["123456789" "123456789"] (mapv :got-2 rows)) "the second op gets 1 to 9"))

(defn- when-row-args []
  {:predicate {:kind "has-column?" :arg :n}
   :body {:fn "cg.param-order/dataset-op" :params params}})

#?(:cljs
   (do
     (defn ^:async run-source! []
       (register-ops!)
       (let [result (await (run/run-model source-model {} {:timeout-ms 5000}))]
         (is (= [in-order] (mapv vec result)))))

     (defn ^:async run-item! []
       (register-ops!)
       (let [result (await (run/run-model item-model {:data [:x]} {:timeout-ms 5000}))]
         (is (= [in-order] (mapv vec result)))))

     (defn ^:async run-row-chain! []
       (await (ds/init-arquero!))
       (register-ops!)
       (let [d (ds/->dataset {:n [1 2]})
             result (await (run/run-model row-chain-model {:input d} {:timeout-ms 10000}))]
         (check-rows! (result-rows result))))

     (defn ^:async run-when-row! []
       (await (ds/init-arquero!))
       (register-ops!)
       (is (= in-order (vec (run/when-row (ds/->dataset {:n [1 2]}) (when-row-args))))))))

(deftest a-source-op-gets-its-params-in-schema-order
  #?(:clj (do (register-ops!)
              (is (= [in-order] (run/run-model source-model {} {:timeout-ms 5000}))))
     :cljs (run-source!)))

(deftest an-item-op-gets-its-params-in-schema-order
  #?(:clj (do (register-ops!)
              (is (= [in-order] (run/run-model item-model {:data [:x]} {:timeout-ms 5000}))))
     :cljs (run-item!)))

(deftest a-per-row-chain-gets-its-params-in-schema-order
  (testing "two per-row ops in table mode, fused into one step on the JVM"
    #?(:clj (do (register-ops!)
                (check-rows! (result-rows (run/run-model row-chain-model
                                                         {:input (ds/->dataset {:n [1 2]})}
                                                         {:timeout-ms 10000}))))
       :cljs (run-row-chain!))))

(deftest a-when-row-body-gets-its-params-in-schema-order
  #?(:clj (do (register-ops!)
              (is (= in-order (run/when-row (ds/->dataset {:n [1 2]}) (when-row-args)))))
     :cljs (run-when-row!)))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.run.param-order-test")))))
