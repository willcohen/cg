;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns appbundle
  "Is app/dist/cg-app.mjs newer than each input in its esbuild metafile? The
inputs are compiled outputs, and an edit to a .cljs file is not visible here."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]))

(def ^:private bundle "app/dist/cg-app.mjs")
(def ^:private metafile "app/dist/cg-app.meta.json")

(defn- mtime [f] (.toMillis (fs/last-modified-time f)))

(defn- stale-inputs
  "Metafile input paths (relative to app/) that are newer than the bundle,
   or gone from disk. Either way the bundle no longer reflects them."
  []
  (let [built (mtime bundle)]
    (->> (keys (get (json/parse-string (slurp metafile)) "inputs"))
         (keep (fn [p]
                 (let [f (fs/path "app" p)]
                   (cond
                     (not (fs/exists? f)) (str p " (missing)")
                     (> (mtime f) built) p
                     :else nil))))
         sort)))

(defn check!
  "Fail loudly when the shell bundle predates any of its inputs."
  []
  (when-not (and (fs/exists? bundle) (fs/exists? metafile))
    (println "MISSING" (if (fs/exists? bundle) metafile bundle))
    (println "  build it first: bb bundle:app")
    (System/exit 1))
  (let [stale (stale-inputs)]
    (if (seq stale)
      (do (println (str "app bundle check: " (count stale)
                        " input(s) newer than " bundle))
          (doseq [p stale] (println "  STALE " p))
          (println "  the served shell embeds these; rebuild: bb bundle:app")
          (System/exit 1))
      (println "app bundle check: ok"))))
