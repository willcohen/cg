;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.parcel-debug-feed-jvm-test
  "The debug layers of parcel-debug-smoke.cg through the generic JVM map feed.
   Set CG_JVM_PORT to the bound port; with no reachable backend, the test skips."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../src/cg_vscode/runtime/jvm_client.mjs" :as jvm]
            ["../../../cg-app/shared/squint/src/cg_gui/run/jvm.mjs" :as api]
            ["fs" :as fs]))

(def ^:private jvm-port
  (let [p (aget (.-env js/process) "CG_JVM_PORT")]
    (when p (js/parseInt p 10))))

(def ^:private send (when jvm-port (jvm/send-to jvm-port nil)))

(def ^:private fixture-path "../cg/test/fixtures/parcel-debug-smoke.cg")

(defn- find-layer [layers var]
  (some (fn [l] (when (= var (aget l "var")) l)) layers))

(defn- features [layer]
  (let [gj (aget layer "geojson")]
    (and gj (aget gj "features"))))

(defn- categories [layer]
  (let [feats (features layer)]
    (when feats
      (set (map (fn [f] (aget (aget f "properties") "category")) feats)))))

(deftest ^:async parcel-debug-emit-fetchable-via-jvm-feed
  (if-not (and jvm-port (await (api/health? (jvm/send-to jvm-port 1000))))
    (println "[skip] no reachable backend (set CG_JVM_PORT=<bound port> after `bb backend:dev`)")
    (let [code (.toString (fs/readFileSync fixture-path))
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

      (is (some? edges) "debug-edges layer present")
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
    (let [code (.toString (fs/readFileSync fixture-path))
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
      (is (pos? (.-length scoped-feats))
          "a 4326 viewport bbox over 2249 data returns features (bbox reprojected to dataset CRS)"))))

(.then (js/Promise.resolve (t/run-tests)) #(when-not (t/successful? %) (js/process.exit 1)))
