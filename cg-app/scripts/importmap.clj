;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns importmap
  "Does every bare specifier in the built browser bundle resolve through each
importmap? Reports the node bundle specifiers too, as information only."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def ^:private browser-bundle "shared/ui/public/cg-bundled-browser.mjs")
(def ^:private node-bundle "shared/ui/public/cg-bundled-node.mjs")
(def ^:private vendor-root "shared/ui/public/vendor")

(def ^:private importmaps
  ;; The one importmap of the npm packages, from importmap.mjs. The dev web
  ;; app, the demo and the cg-vscode map page each use all of it.
  ["shared/ui/public/importmap.json"])

;; The bundle is minified, and a loose scan also matches string content. The
;; lookbehind stops `fromArrow` and `.import` as heads. The charset allows no
;; spaces, commas, parens or colons, and excludes relative paths and node:*.
(def ^:private specifier-re
  #"(?<![\w$.])(?:from|import)\s*\(?\s*[\"']([@\w][@\w./-]*)[\"']")

(defn- specifiers
  "Every bare module specifier in a built bundle: static, side-effect and
   dynamic imports, with either quote and with or without a space."
  [path]
  (when (fs/exists? path)
    (->> (re-seq specifier-re (slurp path))
         (map second)
         set)))

;; A static node: import fails in a browser, and no importmap can fix it. The
;; browser config keeps node:* external only because those imports are dynamic
;; and sit in Node-guarded fns that never run in a webview.
(def ^:private static-node-re
  #"(?<![\w$.])(?:from|import)\s*[\"'](node:[^\"']+)[\"']")

(defn- static-node-imports [path]
  (when (fs/exists? path)
    (->> (re-seq static-node-re (slurp path)) (map second) set)))

(defn- importmap-of
  "The {specifier -> target} map out of the imports of an importmap JSON file."
  [path]
  (get (json/parse-string (slurp path)) "imports"))

(defn- covered?
  "An importmap resolves a specifier by exact key, or by a trailing-slash
   prefix key (\"squint-cljs/\" covers \"squint-cljs/core.js\")."
  [imports spec]
  (or (contains? imports spec)
      (some (fn [[k _]] (and (str/ends-with? k "/")
                             (str/starts-with? spec k)))
            imports)))

;; serve.mjs answers these /vendor paths from node_modules, not vendor-root.
(def ^:private node-module-routes
  {"/vendor/monaco/" "node_modules/monaco-editor/min/"
   "/vendor/tree-sitter-clojure.wasm"
   "../cg/node_modules/@yogthos/tree-sitter-clojure/tree-sitter-clojure.wasm"})

(defn- vendor-file [v]
  (or (some (fn [[route src]]
              (when (str/starts-with? v route)
                (str src (subs v (count route)))))
            node-module-routes)
      (str vendor-root (subs v (count "/vendor")))))

(defn- dangling
  "Map entries pointing at a /vendor path that is not on disk. Catches the
   reverse rot: an entry added for a file nobody vendored."
  [imports]
  (->> imports
       (keep (fn [[k v]]
               (when (and (str/starts-with? v "/vendor/")
                          (not (fs/exists? (vendor-file v))))
                 [k v])))
       (into {})))

(defn- report-map!
  "Check one importmap against the bundle's specifiers. Returns a failure
   count and prints what is wrong."
  [specs path]
  (if-not (fs/exists? path)
    (do (println "  MISSING importmap:" path) 1)
    (let [imports (importmap-of path)
          missing (sort (remove #(covered? imports %) specs))
          dead    (dangling imports)]
      (println (str "  " path))
      (if (seq missing)
        (doseq [m missing]
          (println (str "    UNMAPPED  " m)))
        (println "    all bundle specifiers mapped"))
      (doseq [[k v] (sort dead)]
        (println (str "    DANGLING  " k " -> " v " (not vendored)")))
      (+ (count missing) (count dead)))))

(defn check!
  "Fail loudly when the browser bundle needs a specifier no importmap
   resolves, or when a map points at a file nobody vendored."
  []
  (let [specs (specifiers browser-bundle)]
    (when-not specs
      (println "MISSING bundle:" browser-bundle)
      (println "  build it first: bb build:cg")
      (System/exit 1))
    (println "browser bundle bare specifiers:" (count specs))
    (doseq [s (sort specs)] (println "  " s))
    (println)
    (let [statics (static-node-imports browser-bundle)
          _ (when (seq statics)
              (println "STATIC node: imports in the BROWSER bundle:")
              (doseq [s (sort statics)] (println "    " s))
              (println "  A webview fetches these as URLs and fails on CORS;")
              (println "  no importmap entry can fix it. Find the :cljs require")
              (println "  and make it a guarded dynamic import instead.")
              (println))
          map-failures (reduce + (map #(report-map! specs %) importmaps))
          failures (+ (count statics) map-failures)]
      (println)
      (when-let [node-specs (specifiers node-bundle)]
        (println "node bundle bare specifiers (resolved from node_modules,"
                 "not an importmap):")
        (doseq [s (sort node-specs)]
          ;; Check the package root. A subpath such as "ffi-wasm/pool"
          ;; resolves through the exports map and is not a file on disk.
          (let [segs (str/split s #"/")
                pkg (if (str/starts-with? s "@")
                      (str/join "/" (take 2 segs))
                      (first segs))]
            (println (str "   " (if (fs/exists? (str "node_modules/" pkg)) " " "?")
                          " " s))))
        (println "  ? = package not under cg-app/node_modules; the extension"
                 "host may resolve it elsewhere")
        (println))
      (if (pos? failures)
        (do (println (str "importmap check: " failures " problem(s)"))
            (when (pos? map-failures)
              (println "  every bundle specifier must appear in each importmap"))
            (when (seq statics)
              (println "  a static node: import cannot be mapped; fix the require"))
            (System/exit 1))
        (println "importmap check: ok")))))
