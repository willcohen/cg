;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.backend-jvm-test
  "jvm-client against a running cg-app backend: the map feed, the debug layers
   of parcel-debug-smoke.cg, a session run and the nREPL. Set CG_JVM_PORT to
   the bound port; with no reachable backend, each test skips."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../src/cg_vscode/runtime/jvm_client.mjs" :as jvm]
            ["../../../cg-app/shared/squint/src/cg_gui/run/jvm.mjs" :as api]
            ["../../../cg-app/shared/squint/src/cg_gui/run/js.mjs" :as run-js]
            ["fs" :as fs]
            ["net" :as net]))

(def ^:private jvm-port
  (let [p (aget (.-env js/process) "CG_JVM_PORT")]
    (when p (js/parseInt p 10))))

(def ^:private send (when jvm-port (jvm/send-to jvm-port nil)))

(def ^:private map-bridge-path "../cg-app/app/probes/fixtures/map-bridge.cg")

(defn- features [layer]
  (let [gj (aget layer "geojson")]
    (and gj (aget gj "features"))))

(defn- feature-count [layer] (or (some-> (features layer) .-length) 0))

(deftest ^:async jvm-map-feed-against-running-backend
  (if-not (and jvm-port (await (api/health? (jvm/send-to jvm-port 1000))))
    (println "[skip] no reachable backend (set CG_JVM_PORT=<bound port> after `bb backend:dev`)")
    (let [code (.toString (fs/readFileSync map-bridge-path))]
      (let [sid (await (api/create-session send))
            r (await (jvm/load-layers send sid code "map-bridge.cg"
                                      {:vars ["parcels"] :limit 100}))
            layers (:layers r)
            l0 (and layers (aget layers 0))]
        (is (nil? (:error r)) (str "load-layers errored: " (:error r)))
        (is (not (:expired r)) "unexpected expiry")
        (is (= 1 (and layers (.-length layers))) "one layer for one var")
        (is (= "parcels" (and l0 (aget l0 "var"))))
        (is (= "EPSG:4326" (and l0 (aget l0 "crs"))))
        (is (= 2 (feature-count l0)) "2 polygons in the parcels FC")
        (is (= "#e8590c" (and l0 (aget l0 "color"))) "run-layer-color parity"))

      (let [sid (await (api/create-session send))
            r (await (jvm/load-layers send sid code "map-bridge.cg"
                                      {:vars ["parcels" "does_not_exist"] :limit 100}))]
        (is (= 1 (.-length (:layers r))) "a missing var is not fatal"))

      (let [r (await (jvm/load-layers send "not-a-real-session" code "map-bridge.cg"
                                      {:vars ["parcels"] :limit 100}))]
        (is (:expired r) "a stale session load surfaces {:expired true}"))

      ;; With no :vars, the feed finds the dataset vars. `mapped` is a flow model.
      (let [sid (await (api/create-session send))
            r (await (jvm/load-layers send sid code "map-bridge.cg" {:limit 100}))
            layers (:layers r)]
        (is (= 1 (and layers (.-length layers))) "discovery finds + fetches the parcels dataset")
        (is (= "parcels" (and layers (aget (aget layers 0) "var"))))))))

(deftest ^:async refetch-reuses-loaded-session-without-rerun
  (if-not (and jvm-port (await (api/health? (jvm/send-to jvm-port 1000))))
    (println "[skip] no reachable backend (set CG_JVM_PORT after `bb backend:dev`)")
    (let [code (.toString (fs/readFileSync map-bridge-path))
          sid  (await (api/create-session send))]
      (let [r (await (jvm/load-layers send sid code "map-bridge.cg"
                                      {:vars ["parcels"] :limit 100}))]
        (is (= 2 (feature-count (aget (:layers r) 0))) "initial full load: 2 polygons"))
      (let [r  (await (jvm/fetch-layers send sid #js ["parcels"]
                                          {:bbox #js [-71.062 42.349 -71.053 42.356] :limit 100}))
            l0 (aget (:layers r) 0)]
        (is (not (:expired r)) "session still live after refetch")
        (is (= 1 (feature-count l0)) "refetch scopes to the viewport bbox (1 polygon)")
        (is (= "parcels" (aget l0 "var"))))
      (let [r (await (jvm/fetch-layers send sid #js ["parcels"] {:limit 100}))]
        (is (= 2 (feature-count (aget (:layers r) 0))) "no-bbox refetch = full dataset (RunTheRest)"))
      (let [r (await (jvm/fetch-layers send "not-a-real-session" #js ["parcels"] {:limit 100}))]
        (is (:expired r) "refetch on a stale session → {:expired true}")))))

(def ^:private parcel-debug-path "../cg/test/fixtures/parcel-debug-smoke.cg")

(defn- find-layer [layers var]
  (some (fn [l] (when (= var (aget l "var")) l)) layers))

(defn- categories [layer]
  (let [feats (features layer)]
    (when feats
      (set (map (fn [f] (aget (aget f "properties") "category")) feats)))))

(deftest ^:async parcel-debug-emit-fetchable-via-jvm-feed
  (if-not (and jvm-port (await (api/health? (jvm/send-to jvm-port 1000))))
    (println "[skip] no reachable backend (set CG_JVM_PORT=<bound port> after `bb backend:dev`)")
    (let [code (.toString (fs/readFileSync parcel-debug-path))
          sid (await (api/create-session send))
          r (await (jvm/load-layers send sid code "parcel-debug-smoke.cg"
                                    {:limit 100}))
          layers (:layers r)
          edges (find-layer layers "debug-edges")
          envelope (find-layer layers "debug-envelope")
          boundary (find-layer layers "debug-boundary")]
      (is (nil? (:error r)) (str "load-layers errored: " (:error r)))
      (is (not (:expired r)) "unexpected expiry")
      (is (= 3 (and layers (.-length layers))) "three debug geometry layers auto-discovered")

      (is (= "EPSG:4326" (and edges (aget edges "crs"))) "edges reprojected to 4326")
      (is (= 4 (and edges (.-length (features edges)))) "4 categorized edges")
      (is (= #{"front" "side" "rear"} (categories edges))
          "edges carry a category property the styler keys on")

      (is (= "EPSG:4326" (and envelope (aget envelope "crs"))))
      (is (= 1 (and envelope (.-length (features envelope)))) "one envelope polygon")
      (is (= "EPSG:4326" (and boundary (aget boundary "crs"))))
      (is (= 1 (and boundary (.-length (features boundary)))) "one boundary polygon"))))

;; The dataset is EPSG:2249 and the viewport bbox is 4326. The backend must
;; reproject the bbox before it filters, or a bbox fetch is empty. The test
;; boxes the 4326 coordinates of a whole fetch.
(deftest ^:async viewport-bbox-reprojects-to-dataset-crs
  (if-not (and jvm-port (await (api/health? (jvm/send-to jvm-port 1000))))
    (println "[skip] no reachable backend")
    (let [code (.toString (fs/readFileSync parcel-debug-path))
          sid  (await (api/create-session send))
          full (await (jvm/load-layers send sid code "parcel-debug-smoke.cg"
                                       {:vars ["debug-edges"] :limit 100}))
          feats (features (aget (:layers full) 0))
          v0    (aget (aget (aget (aget feats 0) "geometry") "coordinates") 0)
          lng   (aget v0 0)
          lat   (aget v0 1)
          pad   0.02
          bbox  #js [(- lng pad) (- lat pad) (+ lng pad) (+ lat pad)]
          scoped (await (jvm/fetch-layers send sid #js ["debug-edges"]
                                            {:bbox bbox :limit 100}))
          scoped-feats (features (aget (:layers scoped) 0))]
      (is (pos? (.-length scoped-feats))))))

(def ^:private layer-models
  "(require '[cg.dataset :as ds])
(require '[cg.feature :as f])
(require '[cg.geo :as geo])
(require '[cg.macros :refer [table->]])
(def shapes (ds/->dataset {:geometry [(f/point 0 0) (f/point 1 1)] :name [\"a\" \"b\"]}))
(def rings (table-> :shapes (geo/buffer 1) (map-layer {:name \"rings\" :kind :fill})))
(def dots (table-> :shapes (map-layer {:name \"dots\" :kind :point})))")

(deftest ^:async a-session-run-gives-the-trace-and-the-layers
  (if-not (and jvm-port (await (api/health? (jvm/send-to jvm-port 1000))))
    (println "[skip] no reachable backend (set CG_JVM_PORT=<bound port> after `bb backend:dev`)")
    (let [sid (await (api/create-session send))
          !events (atom [])
          r (await (jvm/run-in-session jvm-port sid #js {:code layer-models :filename "layers.cg"}
                                       (fn [evt] (swap! !events conj evt))))
          res (:result r)
          layers (when res (run-js/run-result->layers nil res))]
      (is (nil? (:error r)) (str "error: " (:error r)))
      (is (pos? (count @!events)) "trace events before the end")
      (is (= "dots" (when res (aget res "name"))))
      (is (= ["rings" "dots"] (when layers (mapv (fn [l] (aget l "var")) layers))))
      (is (= "fill" (when layers (aget (aget (first layers) "spec") "kind")))))))

(deftest ^:async an-unknown-session-is-expired
  (when (and jvm-port (await (api/health? (jvm/send-to jvm-port 1000))))
    (is (:expired (await (jvm/run-in-session jvm-port "no-such-session"
                                             #js {:code layer-models} (fn [_] nil)))))))

(defn- accepts? [port]
  (js/Promise.
   (fn [resolve _]
     (let [^js sock (net/connect #js {:host "127.0.0.1" :port port})]
       (.on sock "connect" (fn [] (.destroy sock) (resolve true)))
       (.on sock "error" (fn [_] (resolve false)))))))

(deftest ^:async the-repl-of-a-session-starts-one-time
  (when (and jvm-port (await (api/health? (jvm/send-to jvm-port 1000))))
    (let [sid (await (api/create-session send))
          r1 (await (api/start-repl send sid))
          r2 (await (api/start-repl send sid))]
      (is (pos? (:port r1)) (str "start-repl: " r1))
      (is (= (:port r1) (:port r2)))
      (is (await (accepts? (:port r1))) "the nREPL takes a connection")
      (is (:expired (await (api/start-repl send "no-such-session")))))))

(.then (js/Promise.resolve (t/run-tests)) #(js/process.exit (if (t/successful? %) 0 1)))
