;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.backend.api-test
  "Integration tests for the backend API endpoints. They run Flow Graphs built
  from real CG DSL code."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.async :as async]
            [clojure.string]
            [cg-app.backend.api :as api]
            [cg.feature :as feature]
            [cg.macros :refer [item-> coll->]]
            [cg.geo :as geo]
            [cg.trace :as trace]
            [ring.core.protocols :as protocols]))

(set! *warn-on-reflection* true)

(def sample-point
  "A sample point feature for testing."
  (feature/make-feature
    (feature/point 0 0)
    {:id "p1" :name "Origin"}))

(def sample-polygon
  "A sample polygon feature for testing."
  (feature/make-feature
    (feature/polygon [[0 0] [10 0] [10 10] [0 10] [0 0]])
    {:id "poly1" :name "Square"}))

(defn mock-request
  "Create a mock Ring request with given body."
  [body]
  {:request-method :post
   :uri "/api/execute"
   :headers {"content-type" "application/json"}
   :body body})

(deftest session-vars-list-only-user-vars-test
  (testing "a loaded file's vars, and none from the cg library namespaces"
    (let [sid (get-in (api/session-create {}) [:body :session-id])
          req (fn [body] {:headers {"x-session-id" sid} :body body})
          r   (api/session-load (req {:code "(def my-model 42)"
                                      :filename "vars.cg"}))]
      (try
        (is (= 200 (:status r)))
        (is (= ["my-model"] (mapv :name (get-in r [:body :vars])))
            (pr-str (mapv :qualified (get-in r [:body :vars]))))
        (finally (api/session-delete (req nil)))))))

(deftest health-endpoint-test
  (testing "Health endpoint returns OK status"
    (let [response (api/health {})]
      (is (= 200 (:status response)))
      (is (= "ok" (get-in response [:body :status])))
      (is (= "cg-app-backend" (get-in response [:body :service])))
      (is (= "jvm" (get-in response [:body :backend]))))))

(deftest execute-simple-buffer-test
  (testing "Execute simple buffer operation via CG DSL"
    (let [model (item-> :features
                  (geo/buffer 100))
          bindings {:features [sample-point]}
          request (mock-request {:flow-graph model
                                 :bindings bindings})
          response (api/execute-model request)]
      (is (= 200 (:status response))
          "Backend should execute successfully")
      (is (contains? (:body response) :results)
          "Response should contain results")
      (let [results (get-in response [:body :results])]
        (is (seq results)
            "Results should not be empty")
        (is (= 1 (count results))
            "Should have one buffered feature")))))

(deftest execute-buffer-simplify-pipeline-test
  (testing "Execute buffer + simplify pipeline via CG DSL"
    (let [model (item-> :polygons
                  (geo/buffer 50)
                  (geo/simplify 1.0))
          bindings {:polygons [sample-polygon]}
          request (mock-request {:flow-graph model
                                 :bindings bindings})
          response (api/execute-model request)]
      (is (= 200 (:status response)))
      (is (seq (get-in response [:body :results]))
          "Should return buffered and simplified results"))))

(deftest execute-collection-union-test
  (testing "Execute collection-level union via CG DSL"
    (let [poly1 (feature/make-feature
                  (feature/polygon [[0 0] [5 0] [5 5] [0 5] [0 0]])
                  {:id "p1"})
          poly2 (feature/make-feature
                  (feature/polygon [[3 3] [8 3] [8 8] [3 8] [3 3]])
                  {:id "p2"})
          model (coll-> :features
                  (geo/union))
          bindings {:features [poly1 poly2]}
          request (mock-request {:flow-graph model
                                 :bindings bindings})
          response (api/execute-model request)]
      (is (= 200 (:status response)))
      (let [result (first (get-in response [:body :results]))]
        (is (map? result)
            "Union should return a single merged feature")))))

(deftest execute-model-missing-bindings-test
  (testing "Execute model with missing bindings returns error"
    (let [model (item-> :missing-data
                  (geo/buffer 100))
          bindings {}
          request (mock-request {:flow-graph model
                                 :bindings bindings})
          response (api/execute-model request)]
      (is (= 500 (:status response))
          "Should return error for missing bindings")
      (is (contains? (:body response) :error)))))

(deftest transform-geometry-buffer-test
  (testing "Direct geometry transformation - buffer"
    (let [request (mock-request {:geometry sample-point
                                 :operation :buffer
                                 :params {:distance 10}})
          response (api/transform-geometry request)]
      (is (= 200 (:status response)))
      (is (contains? (:body response) :result)
          "Should return transformed geometry"))))

(deftest transform-geometry-simplify-test
  (testing "Direct geometry transformation - simplify"
    (let [request (mock-request {:geometry sample-polygon
                                 :operation :simplify
                                 :params {:tolerance 0.5}})
          response (api/transform-geometry request)]
      (is (= 200 (:status response)))
      (is (contains? (:body response) :result)))))

(deftest transform-geometry-union-test
  (testing "Direct geometry transformation - union"
    (let [request (mock-request {:geometry sample-polygon
                                 :operation :union
                                 :params {:other sample-polygon}})
          response (api/transform-geometry request)]
      (is (= 200 (:status response)))
      (is (contains? (:body response) :result)))))

(deftest transform-geometry-unknown-operation-test
  (testing "Unknown operation returns error in result"
    (let [request (mock-request {:geometry sample-point
                                 :operation :unknown-op
                                 :params {}})
          response (api/transform-geometry request)]
      (is (= 200 (:status response)))
      (is (contains? (get-in response [:body :result]) :error)
          "Should return error for unknown operation"))))

(deftest project-geometry-not-implemented-test
  (testing "Projection endpoint returns 501 not implemented"
    (let [request (mock-request {:geometry sample-point
                                 :from-crs "EPSG:4326"
                                 :to-crs "EPSG:3857"})
          response (api/project-geometry request)]
      (is (= 501 (:status response))
          "Projection not yet implemented")
      (is (contains? (:body response) :error)))))

(deftest execute-code-def-ending-cg-file-test
  (testing "execute-code runs a def-ending .cg file with a trailing ^:cgproj overlay"
    (let [code (slurp "app/probes/fixtures/exec-buffer.cg")
          response (api/execute-code (mock-request {:code code}))]
      (is (= 200 (:status response))
          (str "body: " (:body response)))
      (is (= "success" (get-in response [:body :status])))
      (is (= 2 (get-in response [:body :count]))
          "two input points -> two buffered rows")
      (is (not (contains? (:body response) :results))
          "raw results are not wire-serializable (JTS) and have no consumers"))))

(def ^:private two-models-code
  "(require '[cg.feature :as f] '[cg.geo :as geo])
(require '[cg.macros :refer [item->]])
(def pts [(f/point 0 0) (f/point 1 1)])
(def pt [(f/point 5 5)])
(def first-model (item-> :pts (geo/centroid)))
(def last-model (item-> :pt (geo/buffer 1)))
(def unrelated 42)")

(deftest execute-code-runs-the-last-model-by-default-test
  (testing "the last threading-macro binding runs, though a plain def follows it"
    (let [response (api/execute-code (mock-request {:code two-models-code}))]
      (is (= 200 (:status response)) (str "body: " (:body response)))
      (is (= 1 (get-in response [:body :count])) "last-model reads pt, one point"))))

(deftest execute-code-runs-the-model-in-opts-test
  (let [response (api/execute-code (mock-request {:code two-models-code
                                                  :opts {:model "first-model"}}))]
    (is (= 200 (:status response)) (str "body: " (:body response)))
    (is (= 2 (get-in response [:body :count])) "first-model reads pts, two points")))

(defn- poll-result
  "Poll the result endpoint until the execution leaves :running (or ~20s)."
  [id]
  (loop [i 0]
    (let [r (api/execution-result {:path-params {:id id}})]
      (if (and (= "running" (get-in r [:body :status])) (< i 200))
        (do (Thread/sleep 100) (recur (inc i)))
        r))))

(deftest execution-start-returns-id-and-records-result-test
  (testing "start returns an execution-id immediately; summary lands in the registry"
    (let [code (slurp "app/probes/fixtures/exec-buffer.cg")
          response (api/execution-start (mock-request {:code code}))
          id (get-in response [:body :execution-id])]
      (is (= 200 (:status response)) (str "body: " (:body response)))
      (is (string? id))
      (let [result (poll-result id)]
        (is (= "complete" (get-in result [:body :status]))
            (str "body: " (:body result)))
        (is (= 2 (get-in result [:body :result :count]))
            "two input points -> two buffered rows")))))

(deftest execution-start-serializes-409-test
  (testing "409 while another execution is streaming (trace events are global)"
    (let [fake-id "test-live-execution"
          ch (async/chan 1)]
      (swap! api/executions assoc fake-id
             {:id fake-id :chan ch :status :running
              :started-at (System/currentTimeMillis)})
      (try
        (let [response (api/execution-start (mock-request {:code "(ignored)"}))]
          (is (= 409 (:status response)))
          (is (= fake-id (get-in response [:body :live-execution-id]))))
        (finally
          (swap! api/executions dissoc fake-id)
          (async/close! ch))))))

(deftest execution-start-failure-recorded-test
  (testing "a failing execution records :failed with the error, and ends the stream"
    (let [response (api/execution-start (mock-request {:code "(+ 1 1)"}))
          id (get-in response [:body :execution-id])
          result (poll-result id)]
      (is (= 200 (:status response)))
      (is (= "failed" (get-in result [:body :status])))
      (is (re-find #"model binding" (get-in result [:body :result :error]))))))

(deftest execution-trace-sse-stream-test
  (testing "SSE stream carries proc events and terminates with the execution-ended sentinel"
    (let [code (slurp "app/probes/fixtures/exec-buffer.cg")
          start (api/execution-start (mock-request {:code code}))
          id (get-in start [:body :execution-id])
          trace-response (api/execution-trace {:path-params {:id id}})
          baos (java.io.ByteArrayOutputStream.)]
      (is (= 200 (:status trace-response)))
      (is (= "text/event-stream" (get-in trace-response [:headers "Content-Type"])))
      ;; Blocks until the sentinel closes the stream.
      (protocols/write-body-to-stream (:body trace-response) trace-response baos)
      (let [out (.toString baos "UTF-8")
            frames (re-seq #"data: \{[^\n]*\}" out)]
        (is (seq frames) "at least one SSE data frame")
        (is (some #(re-find #"\"kind\":\"proc-start\"" %) frames) (str "frames: " (count frames)))
        (is (some #(re-find #"\"kind\":\"proc-complete\"" %) frames))
        (is (re-find #"\"kind\":\"execution-ended\"" (last frames))
            "sentinel is the final frame")
        (is (re-find #"\"outcome\":\"success\"" (last frames)))))))

(deftest execution-trace-unknown-id-test
  (testing "404 for an unknown execution id"
    (is (= 404 (:status (api/execution-trace {:path-params {:id "nope"}}))))
    (is (= 404 (:status (api/execution-result {:path-params {:id "nope"}}))))))

(deftest execution-tap-never-wedges-global-trace-test
  (testing "overflowing an unconsumed execution tap does not stall the trace mult"
    ;; A fixed-size tap buffer parks the mult on a full tap, and the global
    ;; trace stream then stops for the whole process. The tap must slide.
    (let [code (slurp "app/probes/fixtures/exec-buffer.cg")
          start (api/execution-start (mock-request {:code code}))
          id (get-in start [:body :execution-id])]
      (dotimes [i 5000]
        (trace/emit! "overflow-probe" {:i i}))
      (poll-result id)
      (let [probe (async/chan 8)]
        (trace/tap-trace probe)
        (trace/emit! "post-overflow-probe" {})
        (let [deadline (+ (System/currentTimeMillis) 5000)
              seen (loop []
                     (let [[evt _] (async/alts!! [probe (async/timeout 500)])]
                       (cond
                         (= "post-overflow-probe" (:kind evt)) :delivered
                         (> (System/currentTimeMillis) deadline) :timeout
                         :else (recur))))]
          (trace/untap-trace probe)
          (async/close! probe)
          (is (= :delivered seen)
              "trace mult wedged: fresh tap never received the probe event"))))))

(def ^:private projjson-4326
  "Trimmed EPSG:4326 PROJJSON. Top-level id is the 4326 code itself."
  (str "{\"type\":\"GeographicCRS\",\"name\":\"WGS 84\","
       "\"datum\":{\"type\":\"GeodeticReferenceFrame\",\"name\":\"World Geodetic System 1984\"},"
       "\"id\":{\"authority\":\"EPSG\",\"code\":4326}}"))

(def ^:private projjson-crs84
  "Trimmed OGC:CRS84 PROJJSON. Authority is OGC and the code is a string."
  (str "{\"type\":\"GeographicCRS\",\"name\":\"WGS 84 (CRS84)\","
       "\"id\":{\"authority\":\"OGC\",\"code\":\"CRS84\"}}"))

(def ^:private projjson-2249
  "Trimmed EPSG:2249 PROJJSON. Its base_crs embeds 4326, which defeats a substring match."
  (str "{\"type\":\"ProjectedCRS\",\"name\":\"NAD83 / Massachusetts Mainland (ftUS)\","
       "\"base_crs\":{\"type\":\"GeographicCRS\",\"name\":\"NAD83\","
       "\"id\":{\"authority\":\"EPSG\",\"code\":4326}},"
       "\"id\":{\"authority\":\"EPSG\",\"code\":2249}}"))

(deftest crs-4326?-reads-only-the-top-level-id
  (let [crs-4326? #'api/crs-4326?]
    (testing "compact authority strings"
      (is (true? (crs-4326? "EPSG:4326")))
      (is (true? (crs-4326? "OGC:CRS84")))
      (is (true? (crs-4326? "CRS84")))
      (is (false? (crs-4326? "EPSG:2249")))
      (is (false? (crs-4326? "EPSG:3857"))))

    (testing "PROJJSON whose own id is 4326 or CRS84"
      (is (true? (crs-4326? projjson-4326)))
      (is (true? (crs-4326? projjson-crs84)))
      (is (true? (crs-4326? (clojure.string/replace projjson-4326 "4326" "\"4326\""))))
      )

    (testing "a projected CRS is never 4326, even though its base_crs is"
      ;; A substring match finds the 4326 of the base_crs. The feed then skips
      ;; reprojection, and survey feet reach the map as degrees.
      (is (clojure.string/includes? projjson-2249 "4326")
          "fixture must contain 4326 somewhere, or it does not test the bug")
      (is (false? (crs-4326? projjson-2249))))

    (testing "non-strings and junk"
      (is (false? (crs-4326? nil)))
      (is (false? (crs-4326? 4326)))
      (is (false? (crs-4326? {:authority "EPSG" :code 4326}))))))
