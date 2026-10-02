;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.canvas-html-test
  "The page of the canvas webview against the sources of the shared canvas.
   Runs standalone: node test/cg_vscode/canvas_html_test.mjs."
  (:require [clojure.string :as str]
            [cljs.test :as t :refer [deftest is]]
            ["fs" :as fs]
            ["path" :as path]
            ["../../src/cg_vscode/canvas_html.mjs" :as canvas-html]))

(def ^:private canvas-dir "../cg-app/shared/squint/src/cg_gui/canvas")

(defn- canvas-sources
  "The text of each source file of the shared canvas, the style sheet too."
  []
  (->> (.readdirSync fs canvas-dir)
       (filter (fn [f] (or (str/ends-with? f ".cljs") (str/ends-with? f ".css"))))
       (map (fn [f] (.readFileSync fs (path/join canvas-dir f) "utf8")))
       (str/join "\n")))

(defn- page [mode]
  (canvas-html/build-html
   {:asset-uri  (fn [rel] (str "https://host.test/" rel))
    :csp-source "https://host.test"
    :nonce      "n0"
    :mode       mode
    :sprite     "<svg id=\"sprite\"></svg>"}))

(deftest theme-kinds
  (is (= "light" (canvas-html/theme-kind->mode 1)))
  (is (= "dark" (canvas-html/theme-kind->mode 2)))
  (is (= "dark" (canvas-html/theme-kind->mode 3)))
  (is (= "light" (canvas-html/theme-kind->mode 4))))

(deftest the-page-loads-the-canvas
  (let [html (page "dark")]
    (is (str/includes? html "data-theme=\"dark\""))
    (is (str/includes? html "href=\"https://host.test/cg.css?v=n0\""))
    (is (str/includes? html "href=\"https://host.test/react-flow.css\""))
    (is (str/includes? html "import { mount } from \"https://host.test/cg-canvas.mjs?v=n0\""))
    (is (str/includes? html "<svg id=\"sprite\"></svg>"))))

(deftest the-page-defines-each-token-that-the-canvas-reads
  ;; --cg-pulse-opacity is written by the canvas itself.
  (let [read (->> (re-seq #"var\(--cg-([a-z0-9-]+)" (canvas-sources))
                  (map second)
                  (remove (fn [k] (= k "pulse-opacity")))
                  distinct)
        html (page "light")]
    (is (pos? (count read)))
    (doseq [k read]
      (is (some? (aget canvas-html/theme-tokens k)) (str "--cg-" k " has no value"))
      (is (str/includes? html (str "--cg-" k ":")) (str "--cg-" k " is not on the page")))))

(deftest the-page-styles-each-carbon-component-of-the-canvas
  ;; The webview loads no Carbon style sheet. A component that the canvas
  ;; starts to use needs rules in webview-css and a name in carbon-components.
  (let [used (->> (re-seq #"\"@carbon/react\" :refer \[([^\]]+)\]" (canvas-sources))
                  (mapcat (fn [m] (str/split (second m) #"\s+")))
                  distinct)]
    (is (pos? (count used)))
    (doseq [c used]
      (is (contains? canvas-html/carbon-components c)
          (str "the canvas webview has no style for the Carbon component " c)))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err (or (get results "error") 0)]
           (js/process.exit (if (pos? (+ fail err)) 1 0)))))
