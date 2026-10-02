;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.map-panel
  "The MapLibre map panel. maplibre-gl loads on first show, and the panel div
   and the Map instance live as long as the app."
  (:require ["react" :refer [useEffect useMemo useState]]
            [cg-app.ui.state :as state]
            ["../../../../shared/squint/src/cg_gui/map/core.mjs" :as map-core]
            [cg-app.ui.projection :as projection]
            [cg-app.ui.crs-picker :as crs-picker]))

(defn- run-retired-cleanups-when-idle!
  "Run the retired projection cleanups once `m` is idle on its new style,
   when no tile request can still use the old protocols."
  [m]
  (.once m "idle" (fn [] (doseq [f (state/take-retired-cleanups!)] (f)))))

(defn- apply-projection!
  "Stores the reprojected style of `crs` for the style effect, unless a later
   pick replaced this switch."
  [crs ^js result]
  (when (state/crs-switch-pending? crs)
    (state/set-map-projection!
     #js {:crs crs
          :style (.-style result)
          :bounds (.-bounds result)
          :maxBounds (.-maxBounds result)
          :transformer (.-transformer result)
          :cleanup (.-cleanup result)}))
  (state/end-crs-switch! crs nil))

(defn- fail-projection!
  "Logs the failed switch to `crs` and records it for the panel."
  [crs e]
  (js/console.error "[map] reprojection failed:" e)
  (state/end-crs-switch! crs (str (or (and e (.-message e)) e))))

(defn- select-crs!
  "Handles the CRS picker. nil selects Mercator and the base style. Another
   CRS stores a reprojected base style, and the style effect applies it."
  [crs]
  (state/start-crs-switch! crs)
  (if (nil? crs)
    (do
      (state/clear-map-projection!)
      (when-let [m @map-core/!map]
        (.setMaxBounds m nil)
        (when-let [bs @map-core/!base-style]
          (.setStyle m bs))
        (run-retired-cleanups-when-idle! m)))
    (-> (map-core/build-style)
        (.then (fn [style]
                 ;; maplibre-proj uses a given transformer for any CRS: use the
                 ;; current one again only when the CRS stays the same.
                 (projection/reproject-style!
                  #js {:style style
                       :crs crs
                       :transformer (when (= crs (:map-crs @state/app-state))
                                      (:map-transformer @state/app-state))})))
        (.then (fn [result] (apply-projection! crs result)))
        (.catch (fn [e] (fail-projection! crs e))))))

(defn- ProjStatus
  "The projection status box: the engine start, a pending switch, or a failed one."
  [^js props]
  (let [st (.-st props)
        pending (:map-crs-pending st)
        err (:map-crs-error st)]
    (cond
      err
      #jsx [:div {:className "cg-map-proj-status" :data-status "error"
                  :data-testid "crs-error" :role "alert"}
            [:div (str "Could not switch to " (aget err "crs")
                       ". The map stays in " (or (:map-crs st) "Mercator") ".")]
            [:div {:className "cg-map-proj-reason"} (aget err "message")]
            [:button {:className "cg-map-proj-dismiss" :aria-label "Dismiss"
                      :onClick (fn [_] (state/dismiss-crs-error!))}
             "×"]]

      (= "loading" (.-projStatus props))
      #jsx [:div {:className "cg-map-proj-status"} "Initializing projections..."]

      pending
      #jsx [:div {:className "cg-map-proj-status" :data-testid "crs-pending"}
            (str "Switching to " pending "…")]

      :else nil)))

(defn- count-text [n]
  (when (number? n) (.toLocaleString n "en-US")))

(defn- LegendRow [^js props]
  (let [row (.-row props)]
    #jsx [:li {:className "cg-map-legend-row" :data-value (aget row "value")}
          [:span {:className "cg-map-swatch" :data-kind (.-kind props)
                  :style #js {"--cg-swatch" (aget row "swatch-color")}}]
          [:span {:className "cg-map-legend-label"} (aget row "label")]
          [:span {:className "cg-map-legend-count"} (count-text (aget row "count"))]]))

(defn- LayerToggle
  "The name of a layer with a checkbox that shows or hides it."
  [^js props]
  (let [var-name (.-var props)
        shown (true? (.-shown props))]
    #jsx [:label {:className "cg-map-legend-name"}
          [:input {:type "checkbox" :checked shown
                   :aria-label (str (if shown "Hide " "Show ") var-name)
                   :onChange (fn [_] (state/set-layer-visible! var-name (not shown)))}]
          var-name]))

(defn- LayerLegend
  "The legend of one layer: its name with a hide checkbox, the column that
   gives the colors of a color-by layer, and one row for each value."
  [^js props]
  (let [legend (.-legend props)
        title (aget legend "title")]
    #jsx [:section {:className "cg-map-legend-layer" :data-layer (aget legend "var")}
          [LayerToggle {:var (aget legend "var") :shown true}]
          (when title
            #jsx [:div {:className "cg-map-legend-title"} title])
          [:ul {:className "cg-map-legend-rows"}
           (.map (aget legend "rows")
                 (fn [row i]
                   #jsx [LegendRow {:key i :row row :kind (aget legend "layer-type")}]))]]))

(defn- hidden-layers
  "The hidden layers that still hold data, which the legend can show again."
  [layers]
  (.filter layers (fn [l] (and (not (true? (aget l "visible?"))) (aget l "geojson")))))

;; The hidden sections carry data-visible, not data-hidden, because Carbon
;; sets display none on each [data-hidden] element.
(defn- MapLegend [^js props]
  (let [layers (.-layers props)
        legends (useMemo (fn [] (map-core/legends layers)) #js [layers])
        hidden (hidden-layers layers)]
    (when (pos? (+ (.-length legends) (.-length hidden)))
      #jsx [:div {:className "cg-map-legend" :data-testid "map-legend"}
            (.map legends
                  (fn [legend]
                    #jsx [LayerLegend {:key (aget legend "var") :legend legend}]))
            (.map hidden
                  (fn [l]
                    #jsx [:section {:key (aget l "var") :className "cg-map-legend-layer"
                                    :data-layer (aget l "var") :data-visible "false"}
                          [LayerToggle {:var (aget l "var") :shown false}]]))])))

(defonce ^:private sync-generation (atom 0))

(defn ^:async sync-data-layers!
  "Syncs the data layers onto `m` in the map CRS. Runs after each style
   change, because setStyle replaces all layers. Only the newest call applies."
  [m layers transformer]
  (let [gen (swap! sync-generation inc)
        shown (if (and transformer (pos? (.-length layers)))
                (await (projection/project-layers layers transformer))
                layers)
        apply! (fn [] (when (= gen @sync-generation)
                        (map-core/sync-layers! m shown)))]
    (if (.isStyleLoaded m)
      (apply!)
      (.once m "idle" apply!))))

(defn MapPanel [^js props]
  (let [st (state/use-app-state)
        visible (true? (.-visible props))
        [map-ready set-map-ready] (useState false)
        [picker-open set-picker-open] (useState false)
        proj-status (projection/use-proj-status)
        layers (:map-layers st)
        map-style (:map-style st)
        map-bounds (:map-bounds st)
        map-max-bounds (:map-max-bounds st)
        map-crs (:map-crs st)
        map-transformer (:map-transformer st)]
    (useEffect
     (fn []
       (when visible
         (map-core/ensure-map! nil (fn [_m] (set-map-ready true))))
       js/undefined)
     #js [visible])
    (useEffect
     (fn []
       (when-let [m @map-core/!map]
         (when map-style
           (.setMaxBounds m nil)
           (.setStyle m map-style)
           (run-retired-cleanups-when-idle! m)
           (.once m "styledata"
                  (fn []
                    (when map-bounds
                      (.fitBounds m map-bounds #js {:animate false :padding 20}))
                    (when map-max-bounds
                      (.setMaxBounds m map-max-bounds))))))
       js/undefined)
     #js [map-style])
    (useEffect
     (fn []
       (when-let [m @map-core/!map]
         (.catch (sync-data-layers! m layers map-transformer)
                 (fn [e] (js/console.error "[map] data layer sync failed:" e))))
       js/undefined)
     #js [layers map-ready map-style])
    #jsx [:div {:className "cg-map-panel" :data-testid "map-panel"}
          [:div {:id "cg-map"}]
          (when-not map-ready
            #jsx [:div {:className "cg-map-placeholder" :data-testid "map-placeholder"}
                  "Loading map…"])
          [:div {:className "cg-map-topbar"}
           [:button {:className "cg-map-crs-button" :data-testid "crs-button"
                     :onClick (fn [_] (set-picker-open true))}
            (or map-crs "Mercator")]
           [ProjStatus {:st st :projStatus proj-status}]]
          [MapLegend {:layers layers}]
          [crs-picker/CrsPickerModal {:open picker-open
                                      :onClose (fn [] (set-picker-open false))
                                      :currentCrs map-crs
                                      :onSelect select-crs!}]]))
