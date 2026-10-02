;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.map.core
  "MapLibre map code shared by the cg-app MapPanel and the cg-vscode webview. It
   needs no React or app state: only maplibre-gl, the basemap and the sync plan."
  (:require [cg-gui.map.popup :as popup]
            [cg-gui.map.sync :as ms]))

(def ^:private osm-us-tiles "https://tiles.openstreetmap.us/vector/openmaptiles/{z}/{x}/{y}.mvt")
(def ^:private osm-us-glyphs "https://tiles.openstreetmap.us/fonts/{fontstack}/{range}.pbf")
(def ^:private style-url "https://openmaptiles.github.io/maptiler-basic-gl-style/style-cdn.json")

(defonce ^:private cached-base-style (atom nil))

(defn- ^:async fetch-base-style
  "Fetch and cache the maptiler-basic style JSON."
  []
  (when (nil? @cached-base-style)
    (let [resp (await (js/fetch style-url))]
      (when-not (.-ok resp)
        (throw (js/Error. (str "Failed to fetch base style: " (.-status resp)))))
      (reset! cached-base-style (await (.json resp)))))
  @cached-base-style)

(defn- rewire-style
  "Returns a deep copy of `base-style` with the OSM US tiles and glyphs and
   with no layer zoom limits."
  [base-style]
  (let [style (.parse js/JSON (.stringify js/JSON base-style))]
    (set! (.-sources style)
          #js {:openmaptiles #js {:type "vector"
                                  :tiles #js [osm-us-tiles]
                                  :maxzoom 14}})
    (set! (.-glyphs style) osm-us-glyphs)
    (.forEach (.-layers style)
              (fn [layer]
                (js-delete layer "minzoom")
                (js-delete layer "maxzoom")
                ;; Keep js-delete out of tail position: squint emits
                ;; `return return delete ...` (a SyntaxError) for two
                ;; js-delete forms at the end of a fn.
                nil))
    style))

(defn ^:async build-style
  "The OSM US vector tile basemap style for MapLibre. It removes the per-layer
   zoom limits of maptiler-basic, because reprojected zooms differ."
  []
  (rewire-style (await (fetch-base-style))))

;; One Map instance for each page, in the one map div of the host.
(defonce !map (atom nil))
(defonce !base-style (atom nil))
(defonce !map-loading (atom false))

;; var -> #js {:spec :style :signature} of the layers on the map.
(defonce ^:private !layer-info (atom #js {}))

(defn layer-info
  "#js {:spec :style} of the var on the map, or nil."
  [var-name]
  (aget @!layer-info var-name))

(defn- cgdata-layer-ids
  "The ids of the data layers (cgdata-*) on the map."
  [m]
  (.filter (.getLayersOrder m) (fn [id] (.startsWith id "cgdata-"))))

(defn- hide-identify! [panel]
  (set! (.-display (.-style panel)) "none"))

(defn- clear-children! [el]
  (loop []
    (when (.-firstChild el)
      (.removeChild el (.-firstChild el))
      (recur))))

(defn- build-identify-panel!
  "Creates the hidden identify panel in the map container and returns
   #js {:panel :body}. `on-close` runs when the close button hides the panel."
  [container on-close]
  ;; The MapLibre CSS makes the container position:relative. If that CSS did
  ;; not load, the container is static and the absolute panel needs this.
  (when (= "static" (.-position (js/getComputedStyle container)))
    (set! (.-position (.-style container)) "relative"))
  (let [panel (js/document.createElement "div")
        header (js/document.createElement "div")
        title (js/document.createElement "span")
        close (js/document.createElement "span")
        body  (js/document.createElement "div")
        ps    (.-style panel)]
    (set! (.-className panel) "cg-identify")
    (set! (.-position ps) "absolute")
    (set! (.-right ps) "8px")
    (set! (.-top ps) "8px")
    (set! (.-zIndex ps) "3")
    (set! (.-width ps) "320px")
    (set! (.-maxHeight ps) "calc(100% - 16px)")
    (set! (.-overflowY ps) "auto")
    (set! (.-padding ps) "6px 8px")
    (set! (.-fontSize ps) "12px")
    (set! (.-borderRadius ps) "4px")
    (set! (.-background ps) "var(--cg-surface-raised, var(--vscode-editor-background, #ffffff))")
    (set! (.-color ps) "var(--cg-ink, var(--vscode-foreground, #1a1a1a))")
    (set! (.-border ps) "1px solid var(--cg-line-soft, var(--vscode-panel-border, #cccccc))")
    (set! (.-boxShadow ps) "0 1px 4px rgba(0,0,0,0.25)")
    (set! (.-display ps) "none")
    (set! (.-className header) "cg-identify-header")
    (set! (.-display (.-style header)) "flex")
    (set! (.-justifyContent (.-style header)) "space-between")
    (set! (.-alignItems (.-style header)) "center")
    (set! (.-marginBottom (.-style header)) "4px")
    (set! (.-textContent title) "Identify")
    (set! (.-fontWeight (.-style title)) "700")
    (set! (.-textContent close) "×")
    (set! (.-cursor (.-style close)) "pointer")
    (set! (.-padding (.-style close)) "0 4px")
    (.addEventListener close "click" (fn [_e] (hide-identify! panel) (on-close)))
    (.appendChild header title)
    (.appendChild header close)
    (set! (.-className body) "cg-identify-body")
    (.appendChild panel header)
    (.appendChild panel body)
    (.appendChild container panel)
    #js {:panel panel :body body}))

(defn- feature-card
  "The card of one clicked feature, from the :popup of its layer spec and the
   colors of its layer style."
  [var-name props]
  (let [info (layer-info var-name)]
    (popup/card-element
     (popup/card-model var-name props
                       (some-> info (aget "spec") (aget "popup"))
                       (some-> info (aget "style") (aget "category-colors"))))))

(defn- render-identify!
  "Fills the identify panel with one card for each clicked feature, at most 12
   and no duplicates (same var and properties), then shows the panel."
  [panel body feats]
  (clear-children! body)
  (let [seen #js {}
        cap  (js/Math.min (.-length feats) 12)]
    (loop [i 0]
      (when (< i cap)
        (let [f        (aget feats i)
              var-name (.substring (aget (aget f "layer") "id") 7)
              props    (aget f "properties")
              sig      (str var-name "|" (js/JSON.stringify props))]
          (when-not (aget seen sig)
            (aset seen sig true)
            (.appendChild body (feature-card var-name props))))
        (recur (inc i))))
    (set! (.-display (.-style panel)) "block")))

(defonce ^:private !hover (atom nil))
(defonce ^:private !selected (atom #js []))

(defn- feature-ref
  "The feature-state target of a rendered feature."
  [f]
  #js {:source (aget f "source") :id (aget f "id")})

(defn- same-ref? [a b]
  (and (some? a) (some? b)
       (= (aget a "source") (aget b "source"))
       (= (aget a "id") (aget b "id"))))

(defn- set-state!
  "Sets the feature state `k` of `ref`, when its source is still on the map."
  [m ref k v]
  (when (.getSource m (aget ref "source"))
    (let [state #js {}]
      (aset state k v)
      (.setFeatureState m ref state))))

(defn- hover!
  "Makes `f` the hover feature of the map. nil clears the hover."
  [m f]
  (let [prev @!hover
        nxt  (when f (feature-ref f))]
    (when-not (or (same-ref? prev nxt) (and (nil? prev) (nil? nxt)))
      (when prev (set-state! m prev "hover" false))
      (when nxt (set-state! m nxt "hover" true))
      (reset! !hover nxt))))

(defn- select!
  "Makes `feats` the selected features of the map. An empty array clears the
   selection."
  [m feats]
  (.forEach @!selected (fn [ref] (set-state! m ref "selected" false)))
  (let [refs (.map feats feature-ref)]
    (.forEach refs (fn [ref] (set-state! m ref "selected" true)))
    (reset! !selected refs)))

(defn- hits
  "The data features at the point `p`. With none there, the features in a box
   of ±4 px, because a thin line is hard to hit."
  [m p]
  (let [opts  #js {:layers (cgdata-layer-ids m)}
        exact (.queryRenderedFeatures m p opts)]
    (if (pos? (.-length exact))
      exact
      (.queryRenderedFeatures m #js [#js [(- (.-x p) 4) (- (.-y p) 4)]
                                     #js [(+ (.-x p) 4) (+ (.-y p) 4)]]
                              opts))))

(defn install-inspect!
  "Installs identify: a click on a feature selects it and shows its card, and a
   click on empty map clears it. Hover sets the feature state and the cursor."
  [m]
  (popup/install-css!)
  (let [ctx   (build-identify-panel! (.getContainer m) (fn [] (select! m #js [])))
        panel (aget ctx "panel")
        body  (aget ctx "body")]
    (.on m "click"
         (fn [e]
           (let [feats (hits m (.-point e))]
             (select! m feats)
             (if (pos? (.-length feats))
               (render-identify! panel body feats)
               (hide-identify! panel)))))
    (.on m "mousemove"
         (fn [e]
           (let [feats (.queryRenderedFeatures m (.-point e) #js {:layers (cgdata-layer-ids m)})]
             (hover! m (aget feats 0))
             (set! (.-cursor (.-style (.getCanvas m)))
                   (if (pos? (.-length feats)) "pointer" "")))))
    (.on m "mouseout" (fn [_e] (hover! m nil)))))

(defn viewport-bbox
  "The viewport of the map as #js [west south east north] in EPSG:4326, the
   order that the bbox filter of /api/data takes."
  [m]
  (let [b (.getBounds m)]
    #js [(.getWest b) (.getSouth b) (.getEast b) (.getNorth b)]))

(defn install-viewport-refetch!
  "Calls (on-settled (viewport-bbox m)) after a `moveend` and `debounce-ms` of
   quiet. A fast pan or zoom gives one call. Returns nil."
  [m on-settled debounce-ms]
  (let [!timer (atom nil)]
    (.on m "moveend"
         (fn on-moveend []
           (when-let [t @!timer] (js/clearTimeout t))
           (reset! !timer
                   (js/setTimeout #(on-settled (viewport-bbox m)) debounce-ms))))
    nil))

(defn ^:async ensure-map!
  "Creates the Map in the container one time and calls (on-ready map). It has one
   arity, because squint makes only the dispatcher of a multi-arity defn async."
  [opts on-ready]
  (cond
    (some? @!map) (on-ready @!map)
    @!map-loading nil
    :else
    (do
      (reset! !map-loading true)
      (try
        (let [o (or opts #js {})
              container (or (.-container o) "cg-map")
              center (or (.-center o) #js [-98.5 39.8])
              zoom (if (some? (.-zoom o)) (.-zoom o) 4)
              ml (await (js* "import(~{})" "maplibre-gl"))
              style (await (build-style))
              MapCtor (or (.-Map ml) (.-Map (.-default ml)))
              m (new MapCtor #js {:container container
                                  :style style
                                  :center center
                                  :zoom zoom})]
          (reset! !base-style style)
          (reset! !map m)
          (install-inspect! m)
          ;; A handle for acceptance probes, because page.evaluate cannot read
          ;; module atoms.
          (set! (.-__cgMap js/window) m)
          (on-ready m))
        (catch :default e
          (reset! !map-loading false)
          (js/console.error "[map] init failed:" e))))))

(def ^:private line-width-px 3)

;; A white casing under each colored line keeps the line visible over any fill
;; or basemap.
(def ^:private casing-color "#ffffff")
(def ^:private casing-opacity 0.9)
(def ^:private casing-extra-px 2)

(def ^:private label-color "#161616")
(def ^:private highlight-color "#161616")

;; Each color below is a Carbon color (@carbon/colors).

;; The color of a feature with no value in the color-by column: gray 50.
(def ^:private fallback-color "#8d8d8d")

;; The Carbon categorical palette in its order, without red 90, blue 80 and
;; cyan 90: a translucent fill in one of them reads as black.
(def ^:private categorical-palette
  #js ["#6929c4" "#1192e8" "#005d5d" "#9f1853" "#fa4d56" "#198038"
       "#ee5396" "#b28600" "#009d9a" "#8a3800" "#a56eff"])

;; Fill colors for a layer with one color. They leave out the first three
;; categorical colors, because a fill must not have the color of a line of a
;; small category set drawn on it, and green and teal, the colors of a park
;; and of water on the basemap.
(def ^:private single-palette
  #js ["#9f1853" "#a56eff" "#ee5396" "#0f62fe" "#8a3800" "#b28600" "#da1e28" "#8a3ffc"])

(defn- str-hash [s]
  (reduce (fn [h i] (bit-or (+ (* 31 h) (.charCodeAt s i)) 0))
          0 (range (.-length s))))

(defn stable-color
  "A palette color that is a pure function of the var name."
  [var]
  (aget single-palette (mod (js/Math.abs (str-hash var)) (.-length single-palette))))

(defn humanize
  "A column name or a value as display text. \"final-grade\" gives \"Final
   grade\"."
  [s]
  (popup/humanize s))

(defn shade
  "The #rrggbb color `c` mixed with black: `k` 0 gives `c` and 1 gives black.
   Any other color text comes back with no change."
  [c k]
  (if (.test (js/RegExp. "^#[0-9a-fA-F]{6}$") c)
    (let [part (fn [i]
                 (let [v (js/Math.round (* (- 1 k) (js/parseInt (.slice c i (+ i 2)) 16)))]
                   (.padStart (.toString v 16) 2 "0")))]
      (str "#" (part 1) (part 3) (part 5)))
    c))

(defn geometry-layer-type
  "Polygon/MultiPolygon -> \"fill\", Point/MultiPoint -> \"circle\", else
   \"line\" (from the first feature)."
  [fc]
  (let [g (some-> (aget fc "features") (aget 0) (aget "geometry") (aget "type"))]
    (cond
      (or (= g "Polygon") (= g "MultiPolygon")) "fill"
      (or (= g "Point") (= g "MultiPoint")) "circle"
      :else "line")))

(defn- value-counts
  "The number of features for each value of `prop` in a FeatureCollection, as
   #js {value count} with string keys. A feature with no value has no key."
  [fc prop]
  (let [counts #js {}]
    (.forEach (aget fc "features")
              (fn [f]
                (let [v (some-> f (aget "properties") (aget prop))]
                  (when (some? v)
                    (aset counts (str v) (inc (or (aget counts (str v)) 0)))))))
    counts))

(defn- natural-compare [a b]
  (.localeCompare a b js/undefined #js {:numeric true}))

(defn category-colors
  "Category -> color, in legend order. The values that `pairs` names come first;
   the others, sorted, take the categorical colors that `pairs` does not use."
  [cats pairs]
  (let [o      #js {}
        names  (js/Array.from cats)
        named  (.filter (or pairs #js []) (fn [p] (.includes names (str (aget p 0)))))
        used   (.map (or pairs #js []) (fn [p] (aget p 1)))
        free   (.filter categorical-palette (fn [c] (not (.includes used c))))
        free   (if (pos? (.-length free)) free categorical-palette)
        others (.sort (.filter names (fn [c] (not (.some named (fn [p] (= c (str (aget p 0))))))))
                      natural-compare)]
    (.forEach named (fn [p] (aset o (str (aget p 0)) (aget p 1))))
    (.forEach others (fn [c i] (aset o c (aget free (mod i (.-length free))))))
    o))

(defn spec->style
  "The layer style #js {layer-type mode ...} of a var from its map-layer spec, or
   the default. :color-by, or a `category` property with no :color, is categorical."
  [var fc spec]
  (let [by     (some-> spec (aget "color-by"))
        color  (some-> spec (aget "color"))
        offset (some-> spec (aget "offset"))
        opacity (some-> spec (aget "opacity"))
        shown-counts (some-> spec (aget "counts"))
        prop   (or by "category")
        counts (cond
                 (some? by) (value-counts fc by)
                 (some? color) #js {}
                 :else (value-counts fc "category"))
        cats   (js/Object.keys counts)
        layer-type (or (some-> spec (aget "kind")) (geometry-layer-type fc))
        style (if (pos? (.-length cats))
                #js {:layer-type layer-type
                     :mode "categorical"
                     :category-property prop
                     :category-colors (category-colors cats (some-> spec (aget "colors")))
                     :category-counts counts}
                #js {:layer-type layer-type
                     :mode "single"
                     :color (or color (stable-color var))})]
    (aset style "feature-count" (.-length (aget fc "features")))
    (when (some? offset) (aset style "offset" offset))
    (when (number? opacity) (aset style "opacity" opacity))
    (when (false? shown-counts) (aset style "counts" false))
    style))

(defn default-style
  "The layer style with no sink spec."
  [var fc]
  (spec->style var fc nil))

(defn- categorical? [style]
  (= "categorical" (aget style "mode")))

(defn- uncolored-count
  "The number of features of a categorical style with no category value."
  [style]
  (let [counts (aget style "category-counts")]
    (- (or (aget style "feature-count") 0)
       (reduce (fn [n k] (+ n (aget counts k))) 0 (js/Object.keys counts)))))

(defn- legend-rows [var style]
  (if (categorical? style)
    (let [cc     (aget style "category-colors")
          counts (or (aget style "category-counts") #js {})
          rows   (.map (js/Object.keys cc)
                       (fn [k]
                         #js {:layer-var var :label (humanize k) :value k
                              :swatch-color (aget cc k) :count (aget counts k)}))
          blank  (uncolored-count style)]
      (when (pos? blank)
        (.push rows #js {:layer-var var :label "No value" :value nil
                         :swatch-color fallback-color :count blank}))
      rows)
    #js [#js {:layer-var var :label var :value nil
              :swatch-color (aget style "color")
              :count (aget style "feature-count")}]))

(defn legend-entries
  "The legend rows of a styled layer as #js {layer-var label value swatch-color
   count}: one for each category in color order, then \"No value\" when needed.
   A style with counts false gives no count."
  [var style]
  (let [rows (legend-rows var style)]
    (when (false? (aget style "counts"))
      ;; squint emits `return return delete` for a js-delete in tail position.
      (.forEach rows (fn [r] (js-delete r "count") nil)))
    rows))

(defn layer-legend
  "The legend of a styled layer: #js {var, layer-type, title, rows}. The title
   is the color-by column as display text, or nil for a layer of one color."
  [var style]
  #js {:var var
       :layer-type (aget style "layer-type")
       :title (when (categorical? style) (humanize (aget style "category-property")))
       :rows (legend-entries var style)})

(defn legends
  "The legend (layer-legend) of each visible layer of a layer table that has
   data, top layer first."
  [layers]
  (-> (.filter layers (fn [l] (and (true? (aget l "visible?")) (aget l "geojson"))))
      (.map (fn [l]
              (let [var (aget l "var")]
                (layer-legend var (or (aget l "style")
                                      (spec->style var (aget l "geojson") (aget l "spec")))))))
      (.reverse)))

(defn- paint-color
  "The MapLibre color of a style, with each color through `f`: one color, or
   a match expression on the category property."
  [style f]
  (if (categorical? style)
    (let [cc   (aget style "category-colors")
          expr #js ["match" #js ["to-string" #js ["get" (aget style "category-property")]]]]
      (.forEach (js/Object.keys cc)
                (fn [k] (.push expr k) (.push expr (f (aget cc k)))))
      (.push expr (f fallback-color))
      expr)
    (f (aget style "color"))))

(defn- zoom-ramp
  "A linear interpolation on zoom over `stops`, #js [zoom value ...]."
  [stops]
  (.concat #js ["interpolate" #js ["linear"] #js ["zoom"]] stops))

(defn- state-case
  "A paint value for the selected, hover and plain state of a feature."
  [selected hover plain]
  #js ["case"
       #js ["boolean" #js ["feature-state" "selected"] false] selected
       #js ["boolean" #js ["feature-state" "hover"] false] hover
       plain])

(defn- outline-color [c] (shade c 0.35))

;; A fill is strong at district zoom, where a small polygon is a few pixels, and
;; weaker at street zoom, where the buildings of the basemap must show. An
;; :opacity of the spec is one value at each zoom.
(defn- fill-layer [layer-id source-id style]
  #js {:id layer-id :type "fill" :source source-id
       :paint #js {:fill-color (paint-color style identity)
                   :fill-opacity (let [o (aget style "opacity")]
                                   (if (number? o) o (zoom-ramp #js [12 0.5 16 0.3])))}})

(defn- fill-outline-layer [layer-id source-id style]
  #js {:id layer-id :type "line" :source source-id
       :layout #js {:line-join "round"}
       :paint #js {:line-color (paint-color style outline-color)
                   :line-width (zoom-ramp #js [12 0.25 15 0.75 18 1.5])
                   :line-opacity (zoom-ramp #js [12 0.4 15 0.85])}})

(defn- line-layer [layer-id source-id style]
  #js {:id layer-id :type "line" :source source-id
       :layout #js {:line-cap "round" :line-join "round"}
       :paint #js {:line-color (paint-color style identity)
                   :line-width line-width-px
                   ;; An offset draws the line beside its feature, not on it.
                   :line-offset (or (aget style "offset") 0)}})

(defn- casing-layer [layer-id source-id style]
  #js {:id layer-id :type "line" :source source-id
       :layout #js {:line-cap "round" :line-join "round"}
       :paint #js {:line-color casing-color
                   :line-width (+ line-width-px (* 2 casing-extra-px))
                   :line-opacity casing-opacity
                   :line-offset (or (aget style "offset") 0)}})

(defn- circle-layer [layer-id source-id style]
  #js {:id layer-id :type "circle" :source source-id
       :paint #js {:circle-color (paint-color style identity)
                   :circle-radius (state-case 8 7 6)
                   :circle-stroke-color (state-case highlight-color highlight-color casing-color)
                   :circle-stroke-width (state-case 3 2 1.5)}})

;; The outline of the hover and the selected feature of a fill. It is its own
;; layer, above each outline of the var, because a neighbor must not draw over
;; it.
(defn- highlight-layer [layer-id source-id]
  #js {:id layer-id :type "line" :source source-id
       :layout #js {:line-join "round"}
       :paint #js {:line-color highlight-color
                   :line-width (state-case 3 2 0)
                   :line-opacity (state-case 1 0.75 0)}})

;; The highlight of a line goes under the line, as a dark casing, because the
;; line must keep its color.
(defn- line-highlight-layer [layer-id source-id style]
  #js {:id layer-id :type "line" :source source-id
       :layout #js {:line-cap "round" :line-join "round"}
       :paint #js {:line-color highlight-color
                   :line-width (+ line-width-px (* 2 casing-extra-px))
                   :line-opacity (state-case 1 0.6 0)
                   :line-offset (or (aget style "offset") 0)}})

(defn- has-label?
  "True when a feature has a non-empty `label` property. Such a line layer gets
   distance labels."
  [fc]
  (let [feats (aget fc "features")
        n (.-length feats)]
    (loop [i 0]
      (if (>= i n)
        false
        (let [l (some-> (aget feats i) (aget "properties") (aget "label"))]
          (if (and l (not= l "")) true (recur (inc i))))))))

;; A symbol layer that puts the `label` of each line feature at the center of
;; its segment. MapLibre needs the glyphs of the style and a font that the
;; fontstack serves (Noto Sans, from the basemap).
(defn- label-layer [layer-id source-id]
  #js {:id layer-id :type "symbol" :source source-id
       :layout #js {:symbol-placement "line-center"
                    :text-field #js ["get" "label"]
                    :text-font #js ["Noto Sans Regular"]
                    :text-size 11
                    :text-allow-overlap false}
       :paint #js {:text-color label-color
                   :text-halo-color "#ffffff"
                   :text-halo-width 1.5}})

;; A layer whose fetch the row limit truncated is a misleading sample at low
;; zoom. It shows only from street zoom, where the viewport refetch makes it
;; complete. The limit reads the fetch metadata, not the layer names.
(def ^:private street-minzoom 14)

(defn- truncated-layer? [l]
  (let [t (aget l "total-rows")
        r (aget l "returned-rows")]
    (and (number? t) (number? r) (> t r))))

;; The layers of a var: the data layer, its casing or outline, the highlight
;; and the labels. map_sync sees only cgdata-*.
(def ^:private layer-prefixes #js ["cgdata-" "cgcase-" "cghi-" "cglabel-"])

(defn- var-layer-ids [var-name]
  (.map layer-prefixes (fn [p] (str p var-name))))

(defn- apply-min-zoom!
  "Sets or removes the street-zoom limit on the layers of a var. Runs on each
   upsert, because truncation belongs to the last fetch."
  [m var-name truncated?]
  (let [mz (if truncated? street-minzoom 0)]
    (.forEach (var-layer-ids var-name)
              (fn [id]
                (when (.getLayer m id)
                  (.setLayerZoomRange m id mz 24))))))

(defn- remove-var-layers! [m var-name]
  (.forEach (var-layer-ids var-name)
            (fn [id] (when (.getLayer m id) (.removeLayer m id)))))

(defn- first-layer-id
  "The id of the first layer in draw order for which (pred layer) is true."
  [m pred]
  (.find (.getLayersOrder m) (fn [id] (pred (.getLayer m id)))))

(defn- symbol-layer? [layer]
  (= "symbol" (aget layer "type")))

(defn- line-var-layer?
  "True for a layer of a var whose data layer is not a fill."
  [m layer]
  (let [id (aget layer "id")]
    (and (.startsWith id "cg")
         (let [data (.getLayer m (.replace id (js/RegExp. "^cg[a-z]+-") "cgdata-"))]
           (and data (not= "fill" (aget data "type")))))))

(defn- insert-before
  "The id of the layer under which a new data layer goes: under the basemap labels,
   and for a fill, under each data line and point too."
  [m fill?]
  (or (when fill? (first-layer-id m (fn [layer] (line-var-layer? m layer))))
      (first-layer-id m symbol-layer?)))

(defn- building-layer? [layer]
  (= "building" (or (aget layer "sourceLayer") (aget layer "source-layer"))))

;; The fill paint goes under the basemap buildings, because a blend mode does
;; not exist in MapLibre and a footprint must show through a parcel color.
;; Its outline and highlight stay above them.
(defn- fill-insert-before
  "The id of the layer under which the paint of a fill goes."
  [m]
  (first-layer-id m (fn [layer] (or (building-layer? layer)
                                    (line-var-layer? m layer)
                                    (symbol-layer? layer)))))

(defn- add-var-layers!
  "Adds the layers of a var for its style, in draw order."
  [m var-name style geojson]
  (let [source-id (str "cgsrc-" var-name)
        id        (fn [prefix] (str prefix var-name))
        kind      (aget style "layer-type")
        before    (insert-before m (= "fill" kind))
        add!      (fn [layer] (.addLayer m layer before))]
    (case kind
      "fill"   (do (.addLayer m (fill-layer (id "cgdata-") source-id style)
                              (fill-insert-before m))
                   (add! (fill-outline-layer (id "cgcase-") source-id style))
                   (add! (highlight-layer (id "cghi-") source-id)))
      "circle" (add! (circle-layer (id "cgdata-") source-id style))
      (do (add! (casing-layer (id "cgcase-") source-id style))
          (add! (line-highlight-layer (id "cghi-") source-id style))
          (add! (line-layer (id "cgdata-") source-id style))
          (when (has-label? geojson)
            (.addLayer m (label-layer (id "cglabel-") source-id)))))))

(defn- upsert-layer! [m added l]
  (let [var-name  (aget l "var")
        geojson   (aget l "geojson")
        ;; An inline :style wins over the :spec of the map-layer sink. A nil
        ;; spec gives the default style.
        style     (or (aget l "style") (spec->style var-name geojson (aget l "spec")))
        signature (js/JSON.stringify style)
        source-id (str "cgsrc-" var-name)
        source    (.getSource m source-id)
        drawn?    (and source (.getLayer m (str "cgdata-" var-name)))
        current?  (and drawn? (= signature (some-> (layer-info var-name) (aget "signature"))))]
    (if source
      (do (.removeFeatureState m #js {:source source-id})
          (.setData source geojson))
      (do
        ;; The hover and selected states need a feature id.
        (.addSource m source-id #js {:type "geojson" :data geojson :generateId true})
        (.push added geojson)))
    (when-not current?
      (remove-var-layers! m var-name)
      (add-var-layers! m var-name style geojson))
    (aset @!layer-info var-name #js {:spec (aget l "spec") :style style :signature signature})
    (apply-min-zoom! m var-name (truncated-layer? l))))

(defn- remove-var!
  "Removes the layers, the source and the record of a var."
  [m var-name]
  (remove-var-layers! m var-name)
  (when (.getSource m (str "cgsrc-" var-name))
    (.removeSource m (str "cgsrc-" var-name)))
  (js-delete @!layer-info var-name)
  nil)

;; 0 for fills, 1 for lines. Fills go first (bottom), because a line under a
;; polygon is not visible.
(defn- fill-first-rank [l]
  (if (= "fill" (geometry-layer-type (aget l "geojson"))) 0 1))

(defn sync-layers!
  "Applies a map_sync plan to `m` and fits the view to new data. The style must be
   loaded: the host waits for isStyleLoaded or the 'idle' event."
  [m layers]
  (let [plan (ms/plan-sync (.getLayersOrder m) layers)
        added #js []
        upserts (.sort (.slice (aget plan "upsert"))
                       (fn [a b] (- (fill-first-rank a) (fill-first-rank b))))]
    (.forEach (aget plan "remove")
              (fn [layer-id] (remove-var! m (.substring layer-id 7))))
    (.forEach upserts (fn [l] (upsert-layer! m added l)))
    (when (pos? (.-length added))
      (when-let [bounds (ms/bounds-of added)]
        (.fitBounds m bounds #js {:padding 50 :animate true})))))
