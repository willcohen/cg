;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.state-test
  "The theme toggle, the initial theme with no stored choice (node has no
   matchMedia), and the Carbon theme sync."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_app/ui/state.mjs" :as st]))

(deftest init-theme-defaults-without-storage-or-matchmedia
  (when-let [ls (.-localStorage js/globalThis)] (.removeItem ls "cg-theme"))
  (is (= "white" (st/init-theme!))))

(deftest toggle-theme-flips-and-returns
  (st/set-theme! "g100")
  (is (= "white" (st/toggle-theme!)))
  (is (= "g100" (st/toggle-theme!))))

;; The OS preference reads as light here, because node has no matchMedia.
(deftest toggle-theme-stores-only-a-choice-that-differs-from-the-os
  (st/set-theme! "white")
  (is (= "g100" (st/toggle-theme!)))
  (when-let [ls (.-localStorage js/globalThis)]
    (is (= "g100" (.getItem ls "cg-theme")) "dark on a light OS is a stored choice"))
  (is (= "white" (st/toggle-theme!)))
  (when-let [ls (.-localStorage js/globalThis)]
    (is (nil? (.getItem ls "cg-theme")) "back on the OS preference, no choice is stored")))

(deftest theme-mode-carbon-brand-sync
  (is (= "dark" (st/theme-mode "g100")))
  (is (= "dark" (st/theme-mode "g90")))
  (is (= "light" (st/theme-mode "white")))
  (is (= "light" (st/theme-mode "g10"))))

(deftest positions-round-trip
  (st/set-position! "m__buffer-1" #js [400 250])
  (is (= 400 (aget (aget (st/positions) "m__buffer-1") 0))))

(deftest subscribe-fires-on-swap
  (let [hits (atom 0)
        unsub (st/subscribe (fn [] (swap! hits inc)))]
    (st/set-theme! "g10")
    (unsub)
    (st/set-theme! "white")
    (is (= 1 @hits))))

(deftest default-panels-are-flow-and-code
  (is (true? (:flow (st/panels))))
  (is (true? (:code (st/panels))))
  (is (false? (:map (st/panels))))
  (is (false? (:results (st/panels)))))

(deftest toggle-panel-flips-only-its-key
  (is (false? (st/toggle-panel! "code")))
  (is (false? (:code (st/panels))))
  (is (true? (:flow (st/panels))))
  (is (false? (:map (st/panels))))
  (is (true? (st/toggle-panel! "code"))))

(deftest select-panel-is-exclusive
  (st/toggle-panel! "code")
  (st/select-panel! "map")
  (is (true? (:map (st/panels))))
  (is (false? (:flow (st/panels))))
  (is (false? (:code (st/panels))))
  (st/select-panel! "flow"))

(deftest run-model-clears-when-a-file-loads
  (st/set-run-model! "first")
  (is (= "first" (:run-model @st/app-state)))
  (st/set-loaded-flow! "a.cg" "(x)" #js {})
  (is (nil? (:run-model @st/app-state)) "a new file runs its last model until a pick")
  (st/set-run-model! "first")
  (st/clear-loaded!)
  (is (nil? (:run-model @st/app-state))))

(deftest a-new-file-drops-the-output-of-the-file-before
  (st/add-map-layer! "zoned" "#4589ff" "EPSG:4326" #js {:type "FeatureCollection"})
  (st/set-report-result! #js {:title "t" :columns #js [] :rows #js []})
  (st/set-scalar-result! #js {:text "3" :name "n"})
  (st/set-loaded-flow! "simple.cg" "(x)" #js {})
  (is (= 0 (.-length (:map-layers @st/app-state))))
  (is (nil? (:report-result @st/app-state)))
  (is (nil? (:scalar-result @st/app-state)))
  (st/add-map-layer! "buffered" "#4589ff" "EPSG:4326" #js {:type "FeatureCollection"})
  (st/clear-loaded!)
  (is (= 0 (.-length (:map-layers @st/app-state))) "File > New empties the map too"))

(deftest toggle-sidebar-round-trip
  (is (true? (st/toggle-sidebar!)))
  (is (false? (st/toggle-sidebar!))))

(deftest map-layers-add-remove-upsert
  (st/add-map-layer! "parcels" "#4589ff" "EPSG:4326" #js {:type "FeatureCollection"})
  (let [l (st/get-layer "parcels")]
    (is (true? (aget l "visible?")))
    (is (= "#4589ff" (aget l "color"))))
  (is (= 1 (.-length (st/visible-layers))))
  (st/remove-map-layer! "parcels")
  (let [l (st/get-layer "parcels")]
    (is (false? (aget l "visible?")))
    (is (nil? (aget l "geojson"))))
  (is (= 0 (.-length (st/visible-layers))))
  (st/add-map-layer! "parcels" "#ff832b" "EPSG:4326" #js {:type "FeatureCollection"})
  (let [l (st/get-layer "parcels")]
    (is (true? (aget l "visible?")))
    (is (= "#ff832b" (aget l "color"))))
  (is (= 1 (.-length (:map-layers @st/app-state)))))

(deftest a-hidden-layer-keeps-its-geojson
  (st/add-map-layer! "lines" "#4589ff" "EPSG:4326" #js {:type "FeatureCollection"})
  (st/set-layer-visible! "lines" false)
  (is (false? (aget (st/get-layer "lines") "visible?")))
  (is (some? (aget (st/get-layer "lines") "geojson")))
  (st/set-layer-visible! "lines" true)
  (is (true? (aget (st/get-layer "lines") "visible?"))))

(deftest session-vars-and-loading
  (st/set-session-vars! #js [#js {:name "a" :type "dataset"}])
  (is (= 1 (.-length (:session-vars @st/app-state))))
  (st/set-session-loading! true)
  (is (true? (:session-loading? @st/app-state)))
  (st/set-session-loading! false))

;; The map loads tiles through the old protocols until the next style is in.
;; The map panel runs the retired cleanup later.
(deftest map-projection-set-and-clear-retire-cleanup
  (st/take-retired-cleanups!)
  (let [cleaned (atom [])
        cleanup (fn [k] (fn [] (swap! cleaned conj k)))]
    (st/set-map-projection! #js {:crs "EPSG:5070" :style #js {:s 1}
                                 :bounds #js [#js [0 0] #js [1 1]]
                                 :maxBounds nil :transformer #js {}
                                 :cleanup (cleanup "5070")})
    (is (= "EPSG:5070" (:map-crs @st/app-state)))
    (is (= 0 (count (st/take-retired-cleanups!))))
    (st/set-map-projection! #js {:crs "EPSG:26986" :style #js {:s 2}
                                 :cleanup (cleanup "26986")})
    (is (= [] @cleaned) "a new projection does not run the old cleanup")
    (doseq [f (st/take-retired-cleanups!)] (f))
    (is (= ["5070"] @cleaned) "the old cleanup is retired for the panel")
    ;; The same CRS again reuses the tile protocols: its cleanup is dropped.
    (st/set-map-projection! #js {:crs "EPSG:26986" :style #js {:s 3}
                                 :cleanup (cleanup "26986 again")})
    (is (= 0 (count (st/take-retired-cleanups!))))
    (st/clear-map-projection!)
    (is (nil? (:map-crs @st/app-state)))
    (is (nil? (:map-style @st/app-state)))
    (doseq [f (st/take-retired-cleanups!)] (f))
    (is (= ["5070" "26986 again"] @cleaned) "clear retires the current cleanup")))

(deftest subscribe-fires-on-panel-toggle
  (let [hits (atom 0)
        unsub (st/subscribe (fn [] (swap! hits inc)))]
    (st/toggle-panel! "map")
    (st/toggle-panel! "map")
    (unsub)
    (is (= 2 @hits))))

(deftest an-edit-keeps-the-view-and-the-file-name
  (st/set-loaded-flow! "a.cg" "(def n 1)" #js {:diagnostics #js []})
  (let [load-n (:load-n @st/app-state)
        flow #js {:diagnostics #js [#js {:severity "warning" :message "w"}]}]
    (st/set-edited-text! "(def n 2)" flow nil)
    (is (= {:name "a.cg" :text "(def n 2)" :unparsed nil} (:loaded-file @st/app-state)))
    (is (identical? flow (:flow @st/app-state)))
    (is (= load-n (:load-n @st/app-state)) "the canvas fits again only on a new :load-n")
    (is (= 1 (.-length (st/diagnostics @st/app-state))))))

(deftest text-that-does-not-parse-keeps-the-last-flow-and-reports-one-error
  (let [flow #js {:diagnostics #js []}]
    (st/set-loaded-flow! "a.cg" "(def n 1)" flow)
    (st/set-edited-text! "(def n \"1)" nil "an open string")
    (is (= "(def n \"1)" (:text (:loaded-file @st/app-state))) "Run and Save read the text of the editor")
    (is (identical? flow (:flow @st/app-state)))
    (let [diags (st/diagnostics @st/app-state)]
      (is (= ["error"] (mapv (fn [d] (aget d "severity")) diags)))
      (is (.includes (aget (aget diags 0) "message") "an open string")))
    (st/set-loaded-flow! "b.cg" "(def n 1)" flow)
    (is (= 0 (.-length (st/diagnostics @st/app-state))) "a load clears the mark")))

(deftest url-bar-opens-empty-and-keeps-an-error
  (st/hide-url-bar!)
  (st/toggle-url-bar!)
  (is (= "" (:url (:url-bar @st/app-state))))
  (is (nil? (:error (:url-bar @st/app-state))))
  (st/toggle-url-bar!)
  (is (nil? (:url-bar @st/app-state)))
  (st/show-url-bar! "https://a.example/x.cg" "x.cg: HTTP 404. The server has no file at this URL.")
  (is (= "https://a.example/x.cg" (:url (:url-bar @st/app-state))))
  (is (.includes (:error (:url-bar @st/app-state)) "404"))
  (st/hide-url-bar!))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
