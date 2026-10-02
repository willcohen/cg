;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; cg.introspect extracts the Flow Graph models that .cg code binds. On the JVM
;; it reads the bindings of the SCI environment after the eval. On JS the
;; caller evaluates each name that extract-model-names-from-code finds.

(ns cg.introspect
  (:require #?(:clj [cg.model :as model])
            #?(:clj [cg.dsl :as dsl])
            #?(:clj [cg.run :as run])
            [clojure.string :as str]
            #?(:clj [sci.lang])))

#?(:clj (set! *warn-on-reflection* true))

(defn- denamespace-cg
  "Strip the 'cg.' prefix from a resolved operation name (\"cg.geo/buffer\"
   -> \"geo/buffer\"). cg.codegen has the same helper."
  [s]
  (if (str/starts-with? s "cg.")
    (let [slash (str/index-of s "/")]
      (if (and slash (> slash 3))
        (str (subs s 3 slash) (subs s slash))
        s))
    s))

#?(:clj
   (defn extract-models-from-context
     "The Flow Graph models bound in the `user` namespace of the SCI context
     `sci-ctx`, as a map of symbol -> model."
     [sci-ctx]
     (let [env (:env sci-ctx)]
       (->> (get-in @env [:namespaces 'user])
            (filter (fn [[k _v]]
                      (and (symbol? k)
                           (not (#{:obj :aliases :refers} k)))))
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

           result (run/eval-cg* ctx code-str)

           models (extract-models-from-context ctx)]

       {:result result
        :models models
        :context ctx})))

#?(:clj
   (defn extract-models-from-file
     "As extract-models-from-string, for the .cg file at `file-path`. The
     result also has :file."
     [file-path]
     (let [code (slurp file-path)

           {:keys [result models context]} (extract-models-from-string code)]

       {:result result
        :models models
        :file file-path
        :context context})))

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
                      (filter #(= (:type %) :operation))
                      (map :fn)
                      (map #(keyword (denamespace-cg (str %))))
                      (vec))}))

(defn get-model-by-name
  "The model `model-name` of an extraction result (from
  extract-models-from-file or extract-models-from-string), or nil."
  [extraction-result model-name]
  (get-in extraction-result [:models model-name]))

;; The runtime runs (when-row {pred+opts} body) as one :operation node (see
;; cg.run/when-row), and the model keeps that shape. explain-when-row gives
;; tooling the conceptual expansion as data. The executor does not use it.

(defn when-row-node?
  "True when `node` is an operation node whose :fn is cg.run/when-row, as a
   string or as a symbol."
  [node]
  (and (map? node)
       (= :operation (:type node))
       (let [f (:fn node)]
         (contains? #{"cg.run/when-row" 'cg.run/when-row} f))))

(defn explain-when-row
  "The expansion of a when-row op node as data for tooling, or nil for any
   other node. Returns {:node-kind :when-row :predicate :on-miss :body
   :expansion}."
  [node]
  (when (when-row-node? node)
    (let [args (let [params (:params node)]
                 ;; The params are the parsed map, or a vector of one element
                 ;; that the macro hook makes for apply-params. Read both shapes.
                 (cond
                   (and (vector? params) (= 1 (count params))) (first params)
                   (map? params) params
                   :else nil))
          {:keys [predicate options body]} args
          on-miss (get options "on-miss" "pass-through")
          merge-step (case on-miss
                       "drop" {:step :drop-merge
                               :description "predicate-false rows are suppressed"}
                       {:step :pass-through-merge
                        :description "predicate-false rows emit unchanged"})]
      {:node-kind :when-row
       :predicate predicate
       :on-miss on-miss
       :body body
       :expansion
       [{:step :predicate-split
         :predicate predicate
         :description (str "evaluate predicate '" (:kind predicate)
                           "' against the input dataset")}
        {:step :body-apply
         :body body
         :description (str "on predicate true, dispatch to body op '"
                           (:fn body) "'")}
        merge-step]})))
