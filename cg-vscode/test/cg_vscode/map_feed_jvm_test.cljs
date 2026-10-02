;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.map-feed-jvm-test
  "jvm-client/run->layers on map-bridge.cg against a running cg-app backend.
   Set CG_JVM_PORT to the bound port; with no reachable backend, the test skips."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../src/cg_vscode/runtime/jvm_client.mjs" :as jvm]
            ["fs" :as fs]
            ["http" :as http]))

(def ^:private jvm-port
  (let [p (aget (.-env js/process) "CG_JVM_PORT")]
    (when p (js/parseInt p 10))))

(def ^:private fixture-path "../cg-app/app/probes/fixtures/map-bridge.cg")

(defn- backend-up? [port]
  (js/Promise.
   (fn [resolve _reject]
     (-> (.request http
                   #js {:hostname "localhost" :port port :method "GET"
                        :path "/api/health" :timeout 1000}
                   (fn [^js res] (.resume res) (resolve (= 200 (.-statusCode res)))))
         (.on "error" (fn [_e] (resolve false)))
         (.on "timeout" (fn [] (resolve false)))
         (.end)))))

(defn- feature-count [layer]
  (let [gj (aget layer "geojson")
        feats (and gj (aget gj "features"))]
    (if feats (.-length feats) 0)))

(deftest ^:async jvm-map-feed-against-running-backend
  (try
    (let [up? (and jvm-port (await (backend-up? jvm-port)))]
      (if-not up?
        (do (println "[skip] no reachable backend (set CG_JVM_PORT=<bound port> after `bb backend:dev`)")
            (is true))
        (let [code (.toString (fs/readFileSync fixture-path))]
          (let [sid (await (jvm/create-session jvm-port))
                r (await (jvm/run->layers jvm-port sid code "map-bridge.cg"
                                          {:vars ["parcels"] :limit 100}))
                layers (:layers r)
                l0 (and layers (aget layers 0))]
            (is (nil? (:error r)) (str "run->layers errored: " (:error r)))
            (is (not (:expired r)) "unexpected expiry")
            (is (= 1 (and layers (.-length layers))) "one layer for one var")
            (is (= "parcels" (and l0 (aget l0 "var"))))
            (is (= "EPSG:4326" (and l0 (aget l0 "crs"))))
            (is (= 2 (feature-count l0)) "2 polygons in the parcels FC")
            (is (= "#e8590c" (and l0 (aget l0 "color"))) "run-layer-color parity"))

          (let [sid (await (jvm/create-session jvm-port))
                r (await (jvm/run->layers jvm-port sid code "map-bridge.cg"
                                          {:vars ["parcels"]
                                           :bbox #js [-71.062 42.349 -71.053 42.356]
                                           :limit 100}))
                l0 (aget (:layers r) 0)]
            (is (= 1 (feature-count l0)) "bbox filters to the 1 polygon on screen"))

          (let [sid (await (jvm/create-session jvm-port))
                r (await (jvm/run->layers jvm-port sid code "map-bridge.cg"
                                          {:vars ["parcels" "does_not_exist"] :limit 100}))]
            (is (= 1 (.-length (:layers r))) "the good var still renders")
            (is (= 1 (.-length (:warnings r))) "the missing var is warned, not fatal"))

          (let [r (await (jvm/run->layers jvm-port "not-a-real-session" code "map-bridge.cg"
                                          {:vars ["parcels"] :limit 100}))]
            (is (:expired r) "a stale session load surfaces {:expired true}"))

          ;; With no :vars, the feed finds the dataset vars. `mapped` is a flow model.
          (let [sid (await (jvm/create-session jvm-port))
                r (await (jvm/run->layers jvm-port sid code "map-bridge.cg" {:limit 100}))
                layers (:layers r)]
            (is (= 1 (and layers (.-length layers))) "discovery finds + fetches the parcels dataset")
            (is (= "parcels" (and layers (aget (aget layers 0) "var"))))))))
    (catch :default e
      (println "[jvm-map-feed] unexpected:" (.-message e))
      (is false (str "unexpected: " (.-message e))))))

(deftest ^:async refetch-reuses-loaded-session-without-rerun
  (try
    (let [up? (and jvm-port (await (backend-up? jvm-port)))]
      (if-not up?
        (do (println "[skip] no reachable backend (set CG_JVM_PORT after `bb backend:dev`)")
            (is true))
        (let [code (.toString (fs/readFileSync fixture-path))
              sid  (await (jvm/create-session jvm-port))]
          (let [r (await (jvm/run->layers jvm-port sid code "map-bridge.cg"
                                          {:vars ["parcels"] :limit 100}))]
            (is (= 2 (feature-count (aget (:layers r) 0))) "initial full load: 2 polygons"))
          (let [r  (await (jvm/refetch-layers jvm-port sid #js ["parcels"]
                                              {:bbox #js [-71.062 42.349 -71.053 42.356] :limit 100}))
                l0 (aget (:layers r) 0)]
            (is (not (:expired r)) "session still live after refetch")
            (is (= 1 (feature-count l0)) "refetch scopes to the viewport bbox (1 polygon)")
            (is (= "parcels" (aget l0 "var"))))
          (let [r (await (jvm/refetch-layers jvm-port sid #js ["parcels"] {:limit 100}))]
            (is (= 2 (feature-count (aget (:layers r) 0))) "no-bbox refetch = full dataset (RunTheRest)"))
          (let [r (await (jvm/refetch-layers jvm-port "not-a-real-session" #js ["parcels"] {:limit 100}))]
            (is (:expired r) "refetch on a stale session → {:expired true}")))))
    (catch :default e
      (println "[refetch] unexpected:" (.-message e))
      (is false (str "unexpected: " (.-message e))))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
