;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; cg.run/run-model on both runtimes. The models are literal data, and a cljs
;; body is a named ^:async fn whose Promise the deftest returns.
(ns cg.run-exec-test
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [cg.run :as run]
                    [cg.model :as model]
                    [cg.geo :as geo]
                    [cg.feature :as feature])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../src/cg/run.mjs" :as run]
                     ["../../src/cg/geo.mjs" :as geo]
                     ["../../src/cg/feature.mjs" :as feature]
                     ["../../src/cg/dataset.mjs" :as ds]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

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
  (is (= 1 (count result)) "Should produce one point")
  (let [point (nth result 0)]
    (is (some? point) "Point should exist")
    (is (feature/is-geometry? point) "Should be a geometry")
    (is (= "Point" (geo/geometry-type point)) "Should be a Point")))

(defn- check-one-buffer! [result]
  (is (= 1 (count result)) "Should produce one buffered point")
  (let [buffered (nth result 0)]
    (is (feature/is-geometry? buffered) "Buffered point should be a geometry")
    (is (= "Polygon" (geo/geometry-type buffered)) "A buffered point is a polygon")
    (is (> (double (geo/area buffered)) 0) "Buffer should have positive area")))

#?(:cljs
   (do
     (defn ^:async run-point! []
       (check-one-point! (await (run/run-model point-model {} {:timeout-ms 5000}))))

     (defn ^:async run-point-buffer! []
       (check-one-buffer!
        (await (run/run-model point-buffer-model {} {:timeout-ms 5000}))))

     (defn ^:async run-bindings! []
       (let [result (await (run/run-model buffer-binding-model
                                          {:data [(feature/point 0 0)]}
                                          {:timeout-ms 5000}))]
         (is (= 1 (count result)) "Should produce one result")
         (is (feature/is-geometry? (nth result 0)) "the result is a geometry")))

     (defn ^:async run-file-last! []
       (let [r (await (run/run-file two-models))]
         (is (= "centers" (:model r)))
         (check-one-point! (:result r))))

     (defn ^:async run-file-named! []
       (let [r (await (run/run-file two-models {:model "buffered"}))]
         (is (= "buffered" (:model r)))
         (check-one-buffer! (:result r))))

     (defn ^:async run-file-map-layers! []
       (check-layers! (await (run/run-file layer-models {:map-layers true})))
       (is (nil? (:layers (await (run/run-file layer-models))))))

     (defn ^:async run-file-not-a-model! []
       (let [e (try (await (run/run-file two-models {:model "pts"}))
                    nil
                    (catch :default e e))]
         (is (some? e) "a binding that is not a model fails")
         (is (re-find #"buffered.*centers" (str (.-message e)))
             "the message names the models of the file")))))

(deftest test-source-operation-point
  (testing "Source operation that creates geometry (point)"
    #?(:clj (check-one-point! (run/run-model point-model {} {:timeout-ms 5000}))
       :cljs (run-point!))))

(deftest test-source-plus-transform
  (testing "Source operation + buffer transform"
    #?(:clj (check-one-buffer!
             (run/run-model point-buffer-model {} {:timeout-ms 5000}))
       :cljs (run-point-buffer!))))

(deftest test-simple-flow-with-bindings
  (testing "Simple flow with data binding"
    #?(:clj (let [result (run/run-model buffer-binding-model
                                        {:data [(feature/point 0 0)]}
                                        {:timeout-ms 5000})]
              (is (= 1 (count result)) "Should produce one result")
              (is (feature/is-geometry? (nth result 0)) "the result is a geometry"))
       :cljs (run-bindings!))))

(deftest run-file-runs-the-last-model
  #?(:clj (let [r (run/run-file two-models)]
            (is (= "centers" (:model r)))
            (check-one-point! (:result r)))
     :cljs (run-file-last!)))

(deftest run-file-runs-the-named-model
  #?(:clj (let [r (run/run-file two-models {:model "buffered"})]
            (is (= "buffered" (:model r)))
            (check-one-buffer! (:result r)))
     :cljs (run-file-named!)))

(deftest run-file-runs-the-other-map-layer-models-on-request
  #?(:clj (do (check-layers! (run/run-file layer-models {:map-layers true}))
              (is (nil? (:layers (run/run-file layer-models)))))
     :cljs (run-file-map-layers!)))

(deftest run-file-rejects-a-binding-that-is-not-a-model
  #?(:clj (let [e (try (run/run-file two-models {:model "pts"}) nil
                       (catch clojure.lang.ExceptionInfo e e))]
            (is (some? e) "a binding that is not a model fails")
            (is (re-find #"buffered.*centers" (str (ex-message e)))
                "the message names the models of the file"))
     :cljs (run-file-not-a-model!)))

#?(:clj
   (deftest test-eval-and-run-model
     (testing "Load model from file and execute"
       ;; JVM only: reading a .cg file needs SCI and a filesystem.
       (let [my-model (run/eval-cg-file "test/fixtures/model-inspection.cg")]
         (is (model/model? my-model) "Should load a valid model")
         (testing "Execute model with test data"
           (let [test-geoms [(feature/point 0 0) (feature/point 1 1)]
                 result (run/run-model my-model {:parcels test-geoms} {})]
             (is (some? result) "Should return results")
             (is (coll? result) "Results should be a collection")))))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (ds/init-arquero!)))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.run-exec-test")))))
