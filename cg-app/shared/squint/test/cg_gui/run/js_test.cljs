;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.run.js-test
  "The result-shape router: the one place that decides the shape of a JS
   backend result. Each value gets exactly one shape."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_gui/run/js.mjs" :as rc]))

(deftest empty-covers-nil-and-empty-arrays
  (is (= "empty" (rc/result-shape nil)))
  (is (= "empty" (rc/result-shape js/undefined)))
  (is (= "empty" (rc/result-shape #js []))))

(deftest report-shapes
  (is (= "report" (rc/result-shape #js [#js {:cells #js ["a" 1]}])))
  (is (= "report" (rc/result-shape #js {:cells #js ["a" 1]})))
  (is (= "report" (rc/result-shape #js {:counts #js {} :rows #js []})))
  (is (= "report" (rc/result-shape #js {:columns #js ["n"] :rows #js [#js [1]]}))))

(deftest dataset-is-numrows-bearing-and-not-an-array
  (is (= "dataset" (rc/result-shape #js {:numRows 12})))
  (is (= "dataset" (rc/result-shape #js {:numRows 0}))))

(deftest geometry-rows-are-drawable
  (is (= "geometry" (rc/result-shape #js [#js {:id 1 :geometry #js {}}]))))

(deftest collection-is-an-array-with-nothing-to-draw
  (is (= "collection" (rc/result-shape #js [#js {:id 1 :name "a"}])))
  (is (= "collection" (rc/result-shape #js [1 2 3])))
  (is (= "collection" (rc/result-shape #js [#js {:id 1}])))
  (is (= "collection" (rc/result-shape #js [#js [1 2]]))))

(deftest a-fold-arrives-as-its-sink-s-collected-vector
  ;; run-model returns the collected vector of its sink: a fold gives [9].
  (is (= "scalar" (rc/result-shape #js [9])))
  (is (= "scalar" (rc/result-shape #js ["west roxbury"])))
  (is (= "scalar" (rc/result-shape #js [false])))
  (is (= 9 (rc/scalar-value #js [9])))
  (is (= "9" (rc/scalar-text #js [9]))))

(deftest scalar-covers-a-bare-value-too
  (is (= "scalar" (rc/result-shape 42)))
  (is (= "scalar" (rc/result-shape 0)))
  (is (= "scalar" (rc/result-shape "west roxbury")))
  (is (= "scalar" (rc/result-shape "")))
  (is (= "scalar" (rc/result-shape false)))
  (is (= "scalar" (rc/result-shape #js {:ok true}))))

(def layer-sink
  #js {:layer true :var "parcels"
       :geojson "{\"type\":\"FeatureCollection\",\"features\":[]}"
       :spec #js {:kind "line" :color-by ":zoning" :offset 2}})

(deftest the-map-layer-sink-is-its-own-shape
  ;; The sink sets how the map draws it. It must not read as a dataset.
  (is (= "layer" (rc/result-shape layer-sink)))
  (is (= "layer" (rc/result-shape #js [layer-sink])))
  (is (= "parcels" (aget (rc/layer-sink-result #js [layer-sink]) "var")))
  (is (nil? (rc/layer-sink-result #js [#js {:id 1}])))
  (is (nil? (rc/layer-sink-result #js {:layer true})))
  (is (nil? (rc/layer-sink-result #js {:geojson "{}"}))))

(defn- sink-of [nm]
  #js {:layer true :var nm
       :geojson "{\"type\":\"FeatureCollection\",\"features\":[{\"type\":\"Feature\",\"geometry\":null,\"properties\":{}}]}"})

(deftest the-other-map-layer-models-come-before-the-run-s-own-layer
  (let [r #js {:name "main" :results #js [(sink-of "main")]
               :layers #js [#js {:model "a" :result #js [(sink-of "a")]}
                            #js {:model "b" :result #js [#js {:id 1}]}
                            #js {:model "c" :result #js [(sink-of "c")]}]}]
    (is (= ["a" "c" "main"] (vec (.map (rc/run-result->layers nil r) (fn [l] (aget l "var")))))
        "a model with no drawable result adds no layer")
    (is (= ["main"] (vec (.map (rc/run-result->layers nil #js {:name "main" :results #js [(sink-of "main")]})
                               (fn [l] (aget l "var"))))))))

(defn- recording-cg-module
  "A cg module for execute-js! that records its calls in `calls`. Its
   load_deps_BANG_ resolves one macrotask late, to expose a run that does not wait."
  [calls]
  #js {:metadata #js {:split_code_and_metadata (fn [text] #js {:code text})}
       :run #js {:run_file (fn [code opts]
                             (.push calls (str "run " code " " (aget opts "model")))
                             (js/Promise.resolve #js {:model (aget opts "model") :result #js [1]}))
                 :set_module_reader_BANG_ (fn [f] (.push calls ["reader" f]) nil)
                 :load_deps_BANG_ (fn [code]
                                    (.push calls (str "deps " code))
                                    (js/Promise. (fn [res _]
                                                   (js/setTimeout
                                                    (fn [] (.push calls "deps done") (res nil))
                                                    0))))}})

(def one-model-flow
  #js {:bindings #js [#js {:name "m" :body-shape "threading_macro"}]})

(def module-code "(run/load-module \"mod.cg\")")

(def two-model-flow
  #js {:bindings #js [#js {:name "first" :body-shape "threading_macro"}
                      #js {:name "data" :body-shape "function_call"}
                      #js {:name "last" :body-shape "threading_macro"}]})

(deftest execute-js-loads-the-modules-before-the-eval
  (let [calls #js []
        read-module (fn [_] (js/Promise.resolve ""))]
    (.then (rc/execute-js! (recording-cg-module calls) module-code one-model-flow nil read-module)
           (fn [_]
             (is (= ["reader" read-module] (vec (aget calls 0)))
                 "the host's reader goes to run/set-module-reader!")
             (is (= [(str "deps " module-code) "deps done" (str "run " module-code " m")]
                    (vec (.slice calls 1 4)))
                 "run/load-deps! resolves before run_file")))))

(deftest execute-js-with-no-reader-loads-no-modules
  (let [calls #js []]
    (.then (rc/execute-js! (recording-cg-module calls) module-code one-model-flow nil)
           (fn [_]
             (is (= (str "run " module-code " m") (aget calls 0))
                 "a host with no reader (the cg-vscode webview) runs at once")))))

(deftest execute-js-runs-the-last-model-by-default
  (let [calls #js []]
    (.then (rc/execute-js! (recording-cg-module calls) "(x)" two-model-flow nil nil)
           (fn [r]
             (is (= "run (x) last" (aget calls 0)))
             (is (= "last" (aget r "name")))))))

(deftest execute-js-asks-for-the-other-map-layer-models
  (let [opts (atom nil)
        mod #js {:metadata #js {:split_code_and_metadata (fn [text] #js {:code text})}
                 :run #js {:run_file (fn [_ o]
                                       (reset! opts o)
                                       (js/Promise.resolve #js {:result #js [1]
                                                                :layers #js ["x"]}))}}]
    (.then (rc/execute-js! mod "(x)" one-model-flow nil nil)
           (fn [r]
             (is (true? (aget @opts "map-layers")))
             (is (= ["x"] (vec (aget r "layers"))))))))

(deftest execute-js-runs-the-model-it-is-given
  (let [calls #js []]
    (.then (rc/execute-js! (recording-cg-module calls) "(x)" two-model-flow nil nil "first")
           (fn [r]
             (is (= "run (x) first" (aget calls 0)))
             (is (= "first" (aget r "name")))
             (is (= 1 (aget r "count")))))))

(deftest scalar-text-reads-as-the-value
  (is (= "42" (rc/scalar-text 42)))
  (is (= "0" (rc/scalar-text 0)))
  (is (= "west roxbury" (rc/scalar-text "west roxbury")))
  (is (= "false" (rc/scalar-text false)))
  (is (= "{\"ok\":true}" (rc/scalar-text #js {:ok true}))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
