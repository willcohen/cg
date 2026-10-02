;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.cli
  "Command logic for the bb tasks and bin/cg.mjs. Each command takes a .cg
   file: show-model, generate, validate (a code-to-model-to-code round trip)
   and run."
  (:require [cg.introspect :as introspect]
            [cg.codegen :as codegen]
            [cg.metadata :as metadata]
            [cg.dataset :as dataset]
            [cg.feature :as feature]
            [cg.io :as io]
            [clojure.string :as str]
            [cg.util :as util]
            [cg.run :as run]
            #?(:clj [clojure.java.io :as jio])
            #?(:clj [clojure.pprint :as pprint])))

#?(:clj (set! *warn-on-reflection* true))

(defn- exit! [code]
  #?(:clj (System/exit code)
     :cljs (js/process.exit code)))

(defn- require-file! [file]
  (when-not (util/file-exists? file)
    (println "Error: File not found:" file)
    (exit! 1)))

(defn- print-error [e]
  (println "Error:" (ex-message e))
  #?(:clj (when-let [data (ex-data e)]
            (println "Details:")
            (pprint/pprint data))))

(defn- file-models
  "The models that the .cg file binds, as a map of name -> Flow Graph. On
   cljs it evaluates the code: squint's eval env holds a model as
   compiler-side data, and clj->js gives the plain object."
  [file]
  #?(:clj (:models (introspect/extract-models-from-file file))
     :cljs (let [code (util/read-file file)]
             (run/eval-cg code)
             (reduce (fn [models nm]
                       (let [m (run/eval-cg (str "(clj->js " nm ")"))]
                         (if m (assoc models nm m) models)))
                     {} (introspect/extract-model-names-from-code code)))))

(defn show-model
  "Print the Flow Graphs that the .cg file `file` builds, or only the model
  :binding in `opts`. Gives the exit code."
  [file & [opts]]
  (require-file! file)
  (println "=== Extracting Models from" file "===")
  (println)
  (try
    (let [models (file-models file)
          binding-filter (:binding opts)]
      (cond
        (empty? models)
        (do (println "No models found in file.")
            (println "Make sure the file contains model definitions using item->, coll->, or table->")
            1)

        binding-filter
        (do (println "Found" (count models) "model(s):")
            (println)
            (if-let [model (get models binding-filter)]
              (do (println "Model:" binding-filter)
                  (println)
                  #?(:clj (pprint/pprint model) :cljs (js/console.log model))
                  0)
              (do (println "Error: Model" binding-filter "not found")
                  (println "Available models:" (keys models))
                  1)))

        :else
        (do (println "Found" (count models) "model(s):")
            (println)
            (doseq [{:keys [name node-count edge-count operations]} (introspect/list-models models)]
              (println "•" name)
              (println "  Nodes:" node-count)
              (println "  Edges:" edge-count)
              (println "  Operations:" (vec operations))
              (println))
            0)))
    (catch #?(:clj Exception :cljs :default) e
      (print-error e)
      1)))

(defn generate
  "Write .cg source from the models of the .cg file `file`. `opts`: :output,
  a file path (default: stdout), and :binding, the one model to write. Gives
  the exit code."
  [file & [opts]]
  (require-file! file)
  (try
    (let [{:keys [metadata]} (metadata/split-code-and-metadata (util/read-file file))
          models (file-models file)
          binding-filter (:binding opts)
          models-to-gen (if binding-filter
                          (if-let [m (get models binding-filter)]
                            {binding-filter m}
                            (do (println "Error: Model" binding-filter "not found")
                                (exit! 1)))
                          models)
          generated-code (str/join "\n\n"
                                   (for [[name model] models-to-gen]
                                     (str "(def " name "\n"
                                          "  " (codegen/flow-graph->cg-code model) ")")))
          full-code (if (seq metadata)
                      (codegen/append-visual-metadata generated-code metadata)
                      generated-code)]
      (if-let [output (:output opts)]
        (do (util/write-file! output full-code)
            (println "✓ Generated code written to:" output))
        (println full-code))
      0)
    (catch #?(:clj Exception :cljs :default) e
      (print-error e)
      1)))

(defn validate
  "Check the round trip of the .cg file `file` (code to model to code) and
  print the result for each model. True when every model round-trips."
  [file]
  (require-file! file)
  (println "=== Validating" file "===")
  (println)
  (try
    (let [{:keys [code]} (metadata/split-code-and-metadata (util/read-file file))
          models (file-models file)
          original-normalized (codegen/normalize-code code)]
      (if (empty? models)
        (do (println "No models found in file.")
            false)
        (let [valid (doall
                     (for [[name model] models
                           :let [generated (codegen/normalize-code (codegen/flow-graph->cg-code model))
                                 ok? (str/includes? original-normalized generated)]]
                       (do (if ok?
                             (println "✓" name ": round-trip OK")
                             (do (println "✗" name ": round-trip FAILED")
                                 (println "  Generated code doesn't match original")))
                           ok?)))]
          (println)
          (if (every? true? valid)
            (do (println "✓ All models validate successfully!")
                true)
            (do (println "✗ Validation failed for some models")
                false)))))
    (catch #?(:clj Exception :cljs :default) e
      (print-error e)
      false)))

(defn- cell [v]
  (if (feature/is-geometry? v)
    (let [wkt (io/write-wkt v)]
      (if (> (count wkt) 72) (str (subs wkt 0 69) "...") wkt))
    (str v)))

(defn- print-result
  "Print a run result the same way on both runtimes. Columns print in name order
  with the geometry last as one-line WKT, because the JVM and arquero order
  them differently. A map-layer result prints its layer name and kind."
  [result]
  (cond
    (dataset/dataset? result)
    (let [cols (sort-by (fn [c] [(= "geometry" (name c)) (name c)])
                        (dataset/column-names result))]
      (println (dataset/row-count result) "rows")
      (doseq [row (dataset/rows result)]
        (println (str/join "  " (map (fn [c] (str (name c) " " (cell (get row c)))) cols)))))

    (and (map? result) (true? (:layer result)))
    (println "map layer" (:var result) (str "(" (name (or (:kind (:spec result)) "default")) ")"))

    (and (sequential? result) (every? feature/is-geometry? result))
    (do (println (count result) "geometries")
        (doseq [g result] (println (cell g))))

    :else
    #?(:clj (pprint/pprint result)
       :cljs (println result))))

(defn run-model-file
  "Run one model of a .cg file and print the result. Returns the exit code, 0
  or 1 (a Promise on cljs). `opts`: :model, the binding to run (default: the
  last threading-macro binding), and :timeout-ms for run-model."
  ([file] (run-model-file file nil))
  ([file opts]
   (if-not (util/file-exists? file)
     (do (println "Error: File not found:" file) 1)
     #?(:clj
        (try
          (let [{:keys [model result]}
                (binding [run/*script-file* (.getName (jio/file file))]
                  (run/run-file (util/read-file file) opts))]
            (println "===" model "===")
            (print-result result)
            0)
          (catch Exception e
            (println "Error:" (.getMessage e))
            1))
        :cljs
        (-> (js/Promise.resolve nil)
            (.then (fn [_] (run/run-file (util/read-file file) opts)))
            (.then (fn [r]
                     (println "===" (:model r) "===")
                     (print-result (:result r))
                     0))
            (.catch (fn [e]
                      (println "Error:" (.-message e))
                      1)))))))
