;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.projection
  "The projection engine, imported through the importmap of the host page.
   The importmap gives each part the same maplibre-gl instance as the app Map."
  (:require ["react" :refer [useSyncExternalStore]]))

(defonce proj-state (atom {:status "idle" :error nil :modules nil}))

(defn ready? [] (= "ready" (:status @proj-state)))

(defn ^:async read-crs-catalog
  "The projected CRS catalog of the PROJ database, with no duplicates and no
   deprecated entries. This fn filters by type, because proj-wasm cannot."
  [proj-wasm]
  (let [catalog (await (.projGetCrsInfoListFromDatabase
                        proj-wasm #js {:auth_name ""}))
        projected-type (or (aget proj-wasm "PJ_TYPE_PROJECTED_CRS") 15)
        seen (js/Set.)]
    (.filter catalog
             (fn [entry]
               (and (not (aget entry "deprecated"))
                    (= projected-type (aget entry "type"))
                    (let [k (str (aget entry "authName") ":" (aget entry "code"))]
                      (when-not (.has seen k)
                        (.add seen k)
                        true)))))))

(defn ^:async init-proj!
  "Initializes the engine one time and reads the CRS catalog into proj-state.
   It builds no transformer, because reprojectStyle first moves proj-wasm."
  []
  ;; A failed init runs again on the next call: a CDN fetch can fail once.
  (when (or (= "idle" (:status @proj-state)) (= "error" (:status @proj-state)))
    (swap! proj-state assoc :status "loading" :error nil)
    (try
      (let [modules (await (js/Promise.all
                            #js [(js* "import(~{})" "backproj")
                                 (js* "import(~{})" "maplibre-proj")
                                 (js* "import(~{})" "proj-wasm")]))
            backproj (aget modules 0)
            maplibre-proj (aget modules 1)
            proj-wasm (aget modules 2)]
        (await (.initProj backproj))
        (let [catalog (await (read-crs-catalog proj-wasm))]
          (js/console.log "[projection] proj-wasm initialized")
          (swap! proj-state assoc
                 :status "ready"
                 :catalog catalog
                 :modules {:backproj backproj
                           :maplibre-proj maplibre-proj
                           :proj-wasm proj-wasm})))
      (catch :default e
        (js/console.error "[projection] init failed:" e)
        (swap! proj-state assoc :status "error" :error (.-message e)))))
  nil)

(defn wait-for-init
  "A Promise of the running init: it resolves on ready and rejects on error."
  []
  (js/Promise. (fn [resolve reject]
                 (let [wk (str "proj-wait-" (js/Math.random))]
                   (add-watch proj-state wk
                              (fn [_ _ _ new-state]
                                (case (:status new-state)
                                  "ready" (do (remove-watch proj-state wk)
                                              (resolve true))
                                  "error" (do (remove-watch proj-state wk)
                                              (reject (js/Error. (str "projection init failed: "
                                                                      (:error new-state)))))
                                  nil)))))))

(defn ensure-init!
  "A Promise that resolves when the engine is ready, and rejects when its
   init fails. Starts the init when none is running."
  []
  (cond
    (ready?) (js/Promise.resolve true)
    (= "loading" (:status @proj-state)) (wait-for-init)
    :else (let [p (wait-for-init)]
            (init-proj!)
            p)))

;; backproj gives each tile worker 10 s to start wasmts. In WebKit, eight
;; workers at once (the default) took 32 s and two took 7.8 s.
(def ^:private tile-pool-size 2)

(defn ^:async reproject-style!
  "Reprojects a MapLibre style to a CRS. Resolves to the ReprojectResult.
   opts: #js {:style <js style> :crs \"EPSG:5070\" :transformer <optional>}."
  [opts]
  (await (ensure-init!))
  (let [bp  (get-in @proj-state [:modules :backproj])
        mlp (get-in @proj-state [:modules :maplibre-proj])]
    ;; Only the first createTileProcessor call sets the pool size, and
    ;; reprojectStyle reuses that pool.
    (await (.createTileProcessor bp #js {:poolSize tile-pool-size}))
    (await (.reprojectStyle mlp opts))))

(defn ^:async get-crs-list
  "The projected CRS catalog that init-proj! read (see read-crs-catalog)."
  []
  (await (ensure-init!))
  (:catalog @proj-state))

(defonce ^:private projected-geojson (js/WeakMap.))

(defn ^:async project-geojson
  "Projects a 4326 GeoJSON FeatureCollection into the CRS of a reprojected
   map. Caches the result for each GeoJSON object and transformer."
  [geojson transformer]
  (let [hit (.get projected-geojson geojson)]
    (if (and hit (identical? transformer (aget hit "transformer")))
      (aget hit "geojson")
      (do (await (ensure-init!))
          (let [bp (get-in @proj-state [:modules :backproj])
                out (await (.reprojectGeoJSON bp geojson transformer))]
            (.set projected-geojson geojson #js {:transformer transformer :geojson out})
            out)))))

(defn ^:async project-layers
  "The map layers with their GeoJSON in the coordinates of the reprojected map.
   A layer with no GeoJSON stays as it is."
  [layers transformer]
  (await (js/Promise.all
          (.map layers
                (fn ^:async project-layer [l]
                  (if-let [g (aget l "geojson")]
                    (js/Object.assign #js {} l
                                      #js {:geojson (await (project-geojson g transformer))})
                    l))))))

(defonce ^:private sub-n (atom 0))

(defn- subscribe [cb]
  (let [k (str "proj-sub-" (swap! sub-n inc))]
    (add-watch proj-state k (fn [_ _ _ _] (cb)))
    (fn [] (remove-watch proj-state k))))

(defn use-proj-status
  "React hook: the engine status string."
  []
  (useSyncExternalStore subscribe (fn [] (:status @proj-state))))
