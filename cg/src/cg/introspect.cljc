;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; cg.introspect extracts the Flow Graph models that .cg code binds. On the JVM
;; it reads the bindings of the SCI environment after the eval. On JS the
;; caller evaluates each name that extract-model-names-from-code finds.

(ns cg.introspect
  (:require [cg.codegen :as codegen]
            #?(:clj [cg.model :as model])
            #?(:clj [cg.dsl :as dsl])
            #?(:clj [cg.run :as run])
            #?(:clj [sci.lang])))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (defn extract-models-from-context
     "The Flow Graph models bound in the `user` namespace of the SCI context
     `sci-ctx`, as a map of symbol -> model."
     [sci-ctx]
     (let [env (:env sci-ctx)]
       (->> (get-in @env [:namespaces 'user])
            (filter (fn [[k _v]] (symbol? k)))
            (map (fn [[k v]]
                   [k (if (instance? sci.lang.Var v)
                        @v
                        v)]))
            (filter (fn [[_k v]]
                      (model/model? v)))
            (into {})))))

#?(:clj
   (defn extract-models-from-string
     "Evaluate the CG code string in a new SCI context. Gives {:result :models
     :context}, where :models maps each symbol to its Flow Graph."
     [code-str]
     (let [ctx (dsl/create-cg-context)
           result (run/eval-cg* ctx code-str)]
       {:result result
        :models (extract-models-from-context ctx)
        :context ctx})))

#?(:clj
   (defn extract-models-from-file
     "As extract-models-from-string, for the .cg file at `file-path`. The
     result also has :file."
     [file-path]
     (assoc (extract-models-from-string (slurp file-path)) :file file-path)))

#?(:cljs
   (defn extract-model-names-from-code
     "The names of the (def <name> (item->|coll->|table-> ...)) bindings of
      `code`, in source order."
     [code]
     (mapv second (re-seq #"\(def\s+([^\s\)]+)\s+\((?:item->|coll->|table->)" code))))

(defn list-models
  "A summary of each model in `models` (a map of symbol -> Flow Graph):
  {:name :node-count :edge-count :operations}."
  [models]
  (for [[name model] models]
    {:name name
     :node-count (count (:nodes model))
     :edge-count (count (:edges model))
     :operations (->> (:nodes model)
                      (filter #(contains? #{:operation :group} (:type %)))
                      (map :fn)
                      (map #(keyword (codegen/denamespace-cg (str %))))
                      (vec))}))
