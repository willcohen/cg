;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; cg.run/run-model and run-file on both runtimes.
(ns cg.run-exec-test
  #?(:clj (:require [clojure.test :refer [deftest is]]
                    [cg.run :as run]
                    [cg.model :as model]
                    [cg.geo :as geo]
                    [cg.feature :as feature]
                    [cg.test-runner :refer [after error-message]])
     :cljs (:require [cljs.test :refer [deftest is]]
                     ["../../src/cg/run.mjs" :as run]
                     ["../../src/cg/geo.mjs" :as geo]
                     ["../../src/cg/feature.mjs" :as feature]
                     ["../../src/cg/dataset.mjs" :as ds]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_ after error-message]])))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private point-model
  {:nodes [{:type :operation
            :id :point-1
            :fn "cg.feature/point"
            :params {:x 0 :y 0 :z 0}
            :execution :item-level}
           {:type :sink
            :id :result
            :output-binding :result}]
   :edges [{:from :point-1 :to :result :type :data-flow}]
   :config {}})

(def ^:private point-buffer-model
  {:nodes [{:type :operation
            :id :point-1
            :fn "cg.feature/point"
            :params {:x 10 :y 20}
            :execution :item-level}
           {:type :operation
            :id :buffer-2
            :fn "cg.geo/buffer"
            :params {:distance 100}
            :execution :item-level}
           {:type :sink
            :id :result
            :output-binding :result}]
   :edges [{:from :point-1 :to :buffer-2 :type :data-flow}
           {:from :buffer-2 :to :result :type :data-flow}]
   :config {}})

(def ^:private buffer-binding-model
  {:nodes [{:type :source
            :id :data-src
            :data-ref :data}
           {:type :operation
            :id :buffer-1
            :fn "cg.geo/buffer"
            :params {:distance 10}
            :execution :item-level}
           {:type :sink
            :id :result
            :output-binding :result}]
   :edges [{:from :data-src :to :buffer-1 :type :data-flow}
           {:from :buffer-1 :to :result :type :data-flow}]
   :config {}})

(def ^:private two-models
  "(require '[cg.feature :as f])
(require '[cg.geo :as geo])
(require '[cg.macros :refer [item->]])
(def pts [(f/point 0 0)])
(def buffered (item-> :pts (geo/buffer 1)))
(def centers (item-> :pts (geo/centroid)))")

(def ^:private layer-models
  "(require '[cg.dataset :as ds])
(require '[cg.feature :as f])
(require '[cg.geo :as geo])
(require '[cg.macros :refer [table->]])
(def shapes (ds/->dataset {:geometry [(f/point 0 0) (f/point 1 1)] :name [\"a\" \"b\"]}))
(def rings (table-> :shapes (geo/buffer 1) (map-layer {:name \"rings\" :kind :fill})))
(def count-only (table-> :shapes (geo/centroid)))
(def broken (table-> :missing (map-layer {:name \"broken\"})))
(def dots (table-> :shapes (map-layer {:name \"dots\" :kind :point})))")

(defn- check-layers! [r]
  (is (= "dots" (:model r)))
  (is (= ["rings" "broken"] (mapv :model (:layers r)))
      "the other map-layer models, with no model of another sink")
  (is (some? (:error (second (:layers r)))) "a failed layer does not fail the run")
  ;; The JS lane gives the sink's map, and the JVM its collected vector.
  (let [res (:result (first (:layers r)))]
    (is (= "rings" (:var (if (map? res) res (first res)))))))

(defn- check-one-point! [result]
  (is (= 1 (count result)))
  (let [point (nth result 0)]
    (is (feature/is-geometry? point))
    (is (= "Point" (geo/geometry-type point)))))

(defn- check-one-buffer! [result]
  (is (= 1 (count result)))
  (let [buffered (nth result 0)]
    (is (feature/is-geometry? buffered))
    (is (= "Polygon" (geo/geometry-type buffered)))
    (is (> (double (geo/area buffered)) 0))))

(deftest test-source-operation-point
  (after (run/run-model point-model {} {:timeout-ms 5000}) check-one-point!))

(deftest test-source-plus-transform
  (after (run/run-model point-buffer-model {} {:timeout-ms 5000}) check-one-buffer!))

(deftest test-simple-flow-with-bindings
  (after (run/run-model buffer-binding-model {:data [(feature/point 0 0)]} {:timeout-ms 5000})
         (fn [result]
           (is (= 1 (count result)))
           (is (feature/is-geometry? (nth result 0))))))

(deftest run-file-runs-the-last-model
  (after (run/run-file two-models)
         (fn [r]
           (is (= "centers" (:model r)))
           (check-one-point! (:result r)))))

(deftest run-file-runs-the-named-model
  (after (run/run-file two-models {:model "buffered"})
         (fn [r]
           (is (= "buffered" (:model r)))
           (check-one-buffer! (:result r)))))

(deftest run-file-runs-the-other-map-layer-models-on-request
  (after (run/run-file layer-models {:map-layers true})
         (fn [r]
           (check-layers! r)
           (after (run/run-file layer-models) #(is (nil? (:layers %)))))))

(deftest run-file-rejects-a-binding-that-is-not-a-model
  (after (error-message #(run/run-file two-models {:model "pts"}))
         #(is (re-find #"buffered.*centers" (str %))
              "the message names the models of the file")))

#?(:clj
   (deftest test-eval-and-run-model
     ;; JVM only: reading a .cg file needs SCI and a filesystem.
     (let [my-model (run/eval-cg-file "test/fixtures/model-inspection.cg")]
       (is (model/model? my-model))
       (let [test-geoms [(feature/point 0 0) (feature/point 1 1)]
             result (run/run-model my-model {:parcels test-geoms} {})]
         (is (coll? result))))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (ds/init-arquero!)))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.run-exec-test")))))
