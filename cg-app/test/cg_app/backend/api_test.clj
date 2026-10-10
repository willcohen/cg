;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.backend.api-test
  "Integration tests for the backend API endpoints."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [clojure.string]
            [cg-app.backend.api :as api]
            [cg.run :as run]
            [cg.trace :as trace]
            [cg.dataset :as ds]
            [charred.api :as charred]
            [nrepl.core :as nrepl]
            [ring.core.protocols :as protocols]))

(set! *warn-on-reflection* true)

(defn- with-session [f]
  (let [sid (get-in (api/session-create {}) [:body :session-id])]
    (try (f sid)
         (finally (api/session-delete {:headers {"x-session-id" sid}})))))

(defn- poll-result
  "The result of the execution `id` after it ends, or after 3 minutes. The
   first run in a JVM also starts the parser, and on a busy machine it can
   take more than a minute."
  [id]
  (loop [i 0]
    (let [r (api/execution-result {:path-params {:id id}})]
      (if (and (= "running" (get-in r [:body :status])) (< i 1800))
        (do (Thread/sleep 100) (recur (inc i)))
        r))))

(defn- session-execution [sid body]
  (let [start (api/execution-start {:headers {"x-session-id" sid} :body body})]
    (is (= 200 (:status start)) (str "body: " (:body start)))
    (poll-result (get-in start [:body :execution-id]))))

(deftest session-vars-list-only-user-vars-test
  (with-session
    (fn [sid]
      (let [r (api/session-load {:headers {"x-session-id" sid}
                                 :body {:code "(def my-model 42)" :filename "vars.cg"}})]
        (is (= 200 (:status r)))
        (is (= ["my-model"] (mapv :name (get-in r [:body :vars])))
            (pr-str (mapv :qualified (get-in r [:body :vars]))))))))

(deftest health-endpoint-test
  (is (= {:status 200 :body {:status "ok"}}
         (api/health {}))))

(def ^:private two-models-code
  "(require '[cg.feature :as f] '[cg.geo :as geo])
(require '[cg.macros :refer [item->]])
(def pts [(f/point 0 0) (f/point 1 1)])
(def pt [(f/point 5 5)])
(def first-model (item-> :pts (geo/centroid)))
(def last-model (item-> :pt (geo/buffer 1)))
(def unrelated 42)")

(deftest session-execution-runs-the-last-model-by-default-test
  (testing "the last threading-macro binding runs, though a plain def follows it"
    (with-session
      (fn [sid]
        (let [r (session-execution sid {:code two-models-code})]
          (is (= "complete" (get-in r [:body :status])) (str "body: " (:body r)))
          (is (= 1 (get-in r [:body :result :count])) "last-model reads pt, one point"))))))

(deftest session-execution-runs-the-model-in-opts-test
  (with-session
    (fn [sid]
      (let [r (session-execution sid {:code two-models-code
                                      :opts {:model "first-model"}})]
        (is (= "complete" (get-in r [:body :status])) (str "body: " (:body r)))
        (is (= 2 (get-in r [:body :result :count])) "first-model reads pts, two points")
        (is (= 1 (#'api/result-count {:a 1 :b 2})) "a map is one result, as on JS")))))

(deftest execution-start-serializes-409-test
  (with-session
    (fn [sid]
      (let [fake-id "test-live-execution"
            ch (async/chan 1)]
        (swap! api/executions assoc fake-id {:id fake-id :chan ch :status :running})
        (try
          (let [response (api/execution-start {:headers {"x-session-id" sid}
                                               :body {:code "(ignored)"}})]
            (is (= 409 (:status response)))
            (is (= fake-id (get-in response [:body :live-execution-id]))))
          (finally
            (swap! api/executions dissoc fake-id)
            (async/close! ch)))))))

(deftest execution-start-failure-recorded-test
  (with-session
    (fn [sid]
      (let [result (session-execution sid {:code "(+ 1 1)"})]
        (is (= "failed" (get-in result [:body :status])))
        (is (re-find #"model binding" (get-in result [:body :result :error])))))))

(def ^:private layer-models-code
  "(require '[cg.dataset :as ds])
(require '[cg.feature :as f])
(require '[cg.geo :as geo])
(require '[cg.macros :refer [table->]])
(def shapes (ds/->dataset {:geometry [(f/point 0 0) (f/point 1 1)] :name [\"a\" \"b\"]}))
(def rings (table-> :shapes (geo/buffer 1) (map-layer {:name \"rings\" :kind :fill})))
(def centers (table-> :shapes (geo/centroid)))
(def broken (table-> :missing (map-layer {:name \"broken\"})))
(def dots (table-> :shapes (map-layer {:name \"dots\" :kind :point})))")

(deftest session-execution-gives-the-map-layers-and-keeps-the-datasets-test
  (with-session
    (fn [sid]
      (let [r (session-execution sid {:code layer-models-code})
            res (get-in r [:body :result])
            results (run/eval-cg* (:sci-ctx (get @api/sessions sid)) "cg.session/results")]
        (is (= "complete" (get-in r [:body :status])) (str "body: " (:body r)))
        (is (= "dots" (:name res)))
        (is (= "dots" (:var (#'api/layer-sink (:results res)))) "the model of the run is a layer")
        (is (= ["rings" "broken"] (mapv :model (:layers res)))
            "the other map-layer models in source order, with no other model")
        (is (= "fill" (get-in (#'api/layer-sink (:result (first (:layers res)))) [:spec :kind])))
        (is (some? (:error (second (:layers res)))) "a failed layer does not fail the run")
        (is (string? (charred/write-json-str res)) "the result is JSON data")
        (is (= #{"dots" "rings"} (set (keys results))) "a failed layer keeps nothing")
        (is (ds/dataset? (get results "dots")) "the dataset that went into map-layer")
        (is (= 2 (ds/row-count (get results "rings"))))
        (is (not-any? #(= "cg.session" (:ns %))
                      (get-in (api/session-vars {:headers {"x-session-id" sid}}) [:body :vars]))
            "the results are not a user var")))))

(deftest session-execution-maps-a-dataset-result-test
  (with-session
    (fn [sid]
      (let [res (get-in (session-execution sid {:code layer-models-code
                                                :opts {:model "centers"}})
                        [:body :result])
            gj (charred/read-json (:geojson (:results res)))]
        (is (= "centers" (:var (:results res))) "a plain layer named after the model")
        (is (= 2 (count (get gj "features"))))
        (is (empty? (:layers res)) "only a map-layer model brings the other layers")))))

(deftest session-execution-unknown-session-test
  (let [r (api/execution-start {:headers {"x-session-id" "no-such-session"}
                                :body {:code layer-models-code}})]
    (is (= 404 (:status r)))))

(deftest execution-start-needs-a-session-and-code-test
  (is (= 400 (:status (api/execution-start {:body {:code "1"}}))))
  (with-session
    (fn [sid]
      (is (= 400 (:status (api/execution-start {:headers {"x-session-id" sid} :body {}})))))))

(defn- with-project [files f]
  (let [root (str (java.nio.file.Files/createTempDirectory
                   "cg-root" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (doseq [[path text] files]
        (io/make-parents root path)
        (spit (io/file root path) text))
      (f root)
      (finally
        (run! #(.delete ^java.io.File %) (reverse (file-seq (io/file root))))))))

(def ^:private project-files
  "A project outside the cg folder, which its main file reads by relative paths."
  {"lib/roads.cg" "(ns roads)\n(def lanes 3)\n"
   "data/note.txt" "hello"})

(def ^:private project-main-code
  "(require '[cg.feature :as f] '[cg.geo :as geo] '[cg.run :as run])
(require '[cg.macros :refer [item->]])
(run/load-module \"lib/roads.cg\")
(require '[roads])
(def note (slurp \"data/note.txt\"))
(def pts (mapv (fn [i] (f/point i i)) (range roads/lanes)))
(def model (item-> :pts (geo/buffer 1)))")

(deftest session-execution-resolves-paths-against-the-root-test
  (with-project project-files
    (fn [root]
      (with-session
        (fn [sid]
          (let [r (session-execution sid {:code project-main-code :root root})
                ctx (:sci-ctx (get @api/sessions sid))]
            (is (= "complete" (get-in r [:body :status])) (str "body: " (:body r)))
            (is (= 3 (get-in r [:body :result :count])) "the run sees the def of the module")
            (is (= "hello" (run/eval-cg* ctx "note")) "slurp reads a file under the root")
            (is (= (.getAbsolutePath (io/file root "lib/roads.cg"))
                   (:path (run/eval-cg* ctx "(cg.run/load-module \"lib/roads.cg\")")))
                "a later eval resolves like the last run")))))))

(deftest session-load-resolves-paths-against-the-root-test
  (with-project (assoc project-files "deps.edn" "{:root true}")
    (fn [root]
      (with-session
        (fn [sid]
          (let [ctx (:sci-ctx (get @api/sessions sid))
                read-deps #(run/eval-cg* ctx "(slurp \"deps.edn\")")]
            (is (= (slurp (io/file api/cg-project-root "deps.edn")) (read-deps))
                "with no root, the file of the cg project")
            (let [r (api/session-load {:headers {"x-session-id" sid}
                                       :body {:code project-main-code :filename "main.cg" :root root}})]
              (is (= 200 (:status r)) (str "body: " (:body r)))
              (is (= 3 (count (run/eval-cg* ctx "pts"))))
              (is (= "hello" (run/eval-cg* ctx "note")))
              (is (= "{:root true}" (read-deps)) "the root comes before the cg project"))))))))

(defn- repl-port [sid]
  (let [r (api/session-repl {:headers {"x-session-id" sid}})]
    (is (= 200 (:status r)) (str "body: " (:body r)))
    (get-in r [:body :port])))

(defn- repl-eval [port code]
  (with-open [conn (nrepl/connect :port port)]
    (doall (nrepl/message (nrepl/client conn 10000) {:op "eval" :code code}))))

(deftest session-repl-evaluates-in-the-session-test
  (with-session
    (fn [sid]
      (session-execution sid {:code layer-models-code})
      (let [port (repl-port sid)]
        (is (pos-int? port))
        (is (= port (repl-port sid)) "one server for each session")
        (is (= [2] (nrepl/response-values
                    (repl-eval port "(ds/row-count (get cg.session/results \"dots\"))")))
            "the datasets of the last run")
        (is (= (str ":hi" (System/lineSeparator))
               (apply str (keep :out (repl-eval port "(println :hi)"))))
            "the output goes to the REPL client")
        (repl-eval port "(def from-repl 7)")
        (is (= 7 (run/eval-cg* (:sci-ctx (get @api/sessions sid)) "from-repl"))
            "a REPL def goes into the session")))))

(deftest session-delete-stops-the-repl-test
  (let [sid (get-in (api/session-create {}) [:body :session-id])
        port (repl-port sid)]
    (api/session-delete {:headers {"x-session-id" sid}})
    (is (thrown? java.net.ConnectException (java.net.Socket. "127.0.0.1" (int port))))))

(deftest session-repl-unknown-session-test
  (is (= 404 (:status (api/session-repl {:headers {"x-session-id" "no-such-session"}})))))

(deftest execution-trace-sse-stream-test
  (with-session
    (fn [sid]
      (let [code (slurp "app/probes/fixtures/exec-buffer.cg")
            start (api/execution-start {:headers {"x-session-id" sid} :body {:code code}})
            id (get-in start [:body :execution-id])
            trace-response (api/execution-trace {:path-params {:id id}})
            baos (java.io.ByteArrayOutputStream.)]
        (is (= "text/event-stream" (get-in trace-response [:headers "Content-Type"])))
        ;; Blocks until the sentinel closes the stream.
        (protocols/write-body-to-stream (:body trace-response) trace-response baos)
        (let [out (.toString baos "UTF-8")
              frames (re-seq #"data: \{[^\n]*\}" out)]
          (is (some #(re-find #"\"kind\":\"proc-start\"" %) frames) (str "frames: " (count frames)))
          (is (some #(re-find #"\"kind\":\"proc-complete\"" %) frames))
          (is (re-find #"\"kind\":\"execution-ended\"" (last frames)))
          (is (re-find #"\"outcome\":\"success\"" (last frames)))
          (is (= 2 (get-in (api/execution-result {:path-params {:id id}}) [:body :result :count]))
              "two input points -> two buffered rows"))))))

(deftest session-execution-sends-its-output-test
  (with-session
    (fn [sid]
      ;; The REPL gives the session printers that write to sci/out.
      (api/session-repl {:headers {"x-session-id" sid}})
      (let [code (str layer-models-code "\n(println \"hello from the run\")")
            start (api/execution-start {:headers {"x-session-id" sid} :body {:code code}})
            id (get-in start [:body :execution-id])
            trace-response (api/execution-trace {:path-params {:id id}})
            baos (java.io.ByteArrayOutputStream.)]
        (protocols/write-body-to-stream (:body trace-response) trace-response baos)
        (is (re-find #"\"kind\":\"output\"[^\n]*hello from the run" (.toString baos "UTF-8")))))))

(deftest execution-trace-unknown-id-test
  (is (= 404 (:status (api/execution-trace {:path-params {:id "nope"}}))))
  (is (= 404 (:status (api/execution-result {:path-params {:id "nope"}})))))

(deftest execution-tap-never-wedges-global-trace-test
  ;; A fixed-size tap buffer parks the mult on a full tap, and the global
  ;; trace stream then stops for the whole process. The tap must slide.
  (with-session
    (fn [sid]
      (let [code (slurp "app/probes/fixtures/exec-buffer.cg")
            start (api/execution-start {:headers {"x-session-id" sid} :body {:code code}})
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
            (is (= :delivered seen))))))))

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
      (is (true? (crs-4326? (clojure.string/replace projjson-4326 "4326" "\"4326\"")))))

    (testing "a projected CRS is never 4326, even though its base_crs is"
      ;; A substring match finds the 4326 of the base_crs. The feed then skips
      ;; reprojection, and survey feet reach the map as degrees.
      (is (false? (crs-4326? projjson-2249))))

    (testing "non-strings and junk"
      (is (false? (crs-4326? nil)))
      (is (false? (crs-4326? 4326)))
      (is (false? (crs-4326? {:authority "EPSG" :code 4326}))))))

(deftest a-projjson-map-crs-gives-its-json
  (let [crs (charred/read-json projjson-2249 :key-fn keyword)
        crs-str (#'api/dataset-crs-str (vary-meta (ds/->dataset {:a [1]}) assoc :crs crs))]
    (is (= crs (charred/read-json crs-str :key-fn keyword)))))
