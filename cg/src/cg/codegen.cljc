;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; Flow Graph -> .cg code (threading macro calls). The visual editor changes
;; a Flow Graph, and this namespace writes the code for it.

(ns cg.codegen
  (:require [cg.feature :as feature]
            [clojure.string :as str]
            #?(:clj [clojure.pprint :as pprint])))

#?(:clj (set! *warn-on-reflection* true))

(defn- get-prop
  "Get a property of `obj`: a map key on the JVM, a JS property or a key on
   squint."
  [obj prop-key]
  (when obj
    #?(:cljs (or (aget obj prop-key) (get obj prop-key))
       :clj (get obj prop-key))))

(defn- mode->macro-name
  "The threading macro name for a mode (a keyword or a string). The default
   is item->."
  [mode]
  (let [mode-str #?(:clj (if (keyword? mode) (name mode) (str mode))
                    :cljs (str mode))]
    (case mode-str
      "item" "item->"
      "coll" "coll->"
      "table" "table->"
      "item->")))

(defn- operation-node?
  "True for an operation node. :type can be a keyword or a string."
  [node]
  (let [type (get-prop node :type)]
    (or (= type :operation)
        (= type "operation"))))

(defn- source-node?
  [node]
  (let [type (get-prop node :type)]
    (or (= type :source)
        (= type "source"))))

(defn- sink-node?
  [node]
  (let [type (get-prop node :type)]
    (or (= type :sink)
        (= type "sink"))))

(defn- find-node-by-id
  [graph node-id]
  (let [nodes (get-prop graph :nodes)]
    (first (filter #(= (get-prop % :id) node-id) nodes))))

(defn- find-edges-from
  [graph node-id]
  (let [edges (get-prop graph :edges)]
    (filter #(= (get-prop % :from) node-id) edges)))

(defn- find-edges-to
  [graph node-id]
  (let [edges (get-prop graph :edges)]
    (filter #(= (get-prop % :to) node-id) edges)))

(defn- topological-sort
  "Sort nodes in topological order (dependencies first)"
  [graph]
  (let [nodes (get-prop graph :nodes)
        edges (get-prop graph :edges)
        in-degree (reduce (fn [acc edge]
                            (update acc (get-prop edge :to) (fnil inc 0)))
                          {}
                          edges)
        queue (into [] (filter #(zero? (get in-degree (get-prop % :id) 0)) nodes))
        result (atom [])]

    (loop [q queue
           visited #{}]
      (if (empty? q)
        @result
        (let [node (first q)
              node-id (get-prop node :id)]
          (swap! result conj node)
          (let [outgoing (find-edges-from graph node-id)
                next-nodes (mapv #(find-node-by-id graph (get-prop % :to)) outgoing)
                ready-nodes (filter (fn [n]
                                      (let [n-id (get-prop n :id)
                                            incoming (find-edges-to graph n-id)]
                                        (every? #(contains? visited (get-prop % :from))
                                                incoming)))
                                    next-nodes)]
            (recur (into (subvec q 1) ready-nodes)
                   (conj visited node-id))))))))

(defn- param-value->code
  [v]
  (cond
    #?@(:clj [(keyword? v) (str v)
              (symbol? v) (str v)])
    (string? v) (pr-str v)
    (number? v) (str v)
    ;; JVM str prints a collection readably. squint str gives
    ;; "[object Object]", and pr-str replaces it there: a string key prints
    ;; as a keyword, and a keyword value prints quoted.
    (vector? v) #?(:clj (str v) :cljs (pr-str v))
    (map? v) #?(:clj (str v) :cljs (pr-str v))
    :else (pr-str v)))

(defn- denamespace-cg
  "Remove the `cg.` prefix from an operation name, to give the alias form
   (\"cg.geo/buffer\" -> \"geo/buffer\"). The macro stores :fn in resolved
   form, and the generated code must match the source as written."
  [s]
  (if (str/starts-with? s "cg.")
    (let [slash (str/index-of s "/")]
      (if (and slash (> slash 3))
        (str (subs s 3 slash) (subs s slash))
        s))
    s))

(defn- registered-param-order
  "The declared :params names, in order, for the :fn of a node (for example
   \"cg.dataset/summarize\"). A bare head resolves through the bare-op
   namespace map. nil when the op is not registered or declares no :params."
  [raw-fn]
  (let [qualified (if (str/includes? raw-fn "/")
                    raw-fn
                    (when-let [ns-name (feature/get-bare-op-namespace raw-fn)]
                      (str ns-name "/" raw-fn)))
        info (when qualified (feature/get-operation-info qualified))]
    (seq (mapv :name (:params info)))))

(defn- node->operation-call
  [node]
  (let [fn-name (get-prop node :fn)
        raw #?(:clj (if (keyword? fn-name)
                      (subs (str fn-name) 1)
                      (str fn-name))
               :cljs (str fn-name))
        fn-str (denamespace-cg raw)
        params (get-prop node :params)
        param-order (registered-param-order raw)
        ordered (when param-order
                  (seq (keep (fn [k]
                               (let [v (get-prop params k)]
                                 (when (some? v) (param-value->code v))))
                             param-order)))
        primary-param (or (get-prop params :value)
                          (get-prop params :distance)
                          (get-prop params :tolerance))]

    (cond
      ;; Registered params print in their declared order:
      ;; (buffer 10 {:quad-segs 8}).
      ordered
      (str "(" fn-str " " (str/join " " ordered) ")")

      primary-param
      (str "(" fn-str " " (param-value->code primary-param) ")")

      :else
      (str "(" fn-str ")"))))

(defn- find-pipeline
  "The operation nodes that follow `source-id` in a chain. The chain stops at
   a node that is not an operation, or at a node without exactly one outgoing
   edge."
  [graph source-id]
  (loop [current-id source-id
         pipeline []]
    (let [outgoing (find-edges-from graph current-id)]
      (if (= 1 (count outgoing))
        (let [edge (first outgoing)
              next-node (find-node-by-id graph (get-prop edge :to))]
          (if (operation-node? next-node)
            (recur (get-prop next-node :id) (conj pipeline next-node))
            pipeline))
        pipeline))))

(defn- generate-threading-macro
  [source-node pipeline]
  (let [data-ref-raw (get-prop source-node :data-ref)
        data-ref #?(:clj (if (keyword? data-ref-raw)
                           (str data-ref-raw)
                           (str ":" data-ref-raw))
                    :cljs (str ":" data-ref-raw))
        mode (get-prop (first pipeline) :mode)
        macro-name (mode->macro-name mode)
        operation-calls (mapv node->operation-call pipeline)]

    (str "(" macro-name " " data-ref "\n"
         (str/join "\n" (map #(str "  " %) operation-calls))
         ")")))

(defn flow-graph->cg-code
  "Generate a .cg code string from a Flow Graph: one threading macro call for
  each source node, inside a def when the sink has an :output-binding."
  [graph]
  (let [sorted-nodes (topological-sort graph)
        sources (filter source-node? sorted-nodes)
        sinks (filter sink-node? sorted-nodes)
        edges-for-sink (get-prop graph :edges)
        pipelines (mapv (fn [source]
                          (let [pipeline (find-pipeline graph (get-prop source :id))
                                sink (first (filter #(some (fn [edge]
                                                             (and (= (get-prop edge :from) (get-prop (last pipeline) :id))
                                                                  (= (get-prop edge :to) (get-prop % :id))))
                                                           edges-for-sink)
                                                    sinks))]
                            {:source source
                             :pipeline pipeline
                             :sink sink}))
                        sources)

        code-parts (mapv (fn [{:keys [source pipeline sink]}]
                           (let [macro-call (generate-threading-macro source pipeline)
                                 binding (get-prop sink :output-binding)]
                             (if binding
                               (str "(def " binding "\n  " macro-call ")")
                               macro-call)))
                         pipelines)]

    (str/join "\n\n" code-parts)))

(defn extract-visual-metadata
  "The ^:cgproj metadata of a Flow Graph: the :visual map of each node, keyed
  by the node :id as a string, and the viewport."
  [graph]
  (let [nodes (get-prop graph :nodes)
        node-metadata (reduce (fn [acc node]
                                (let [node-id (get-prop node :id)
                                      visual (get-prop node :visual)]
                                  (if (and node-id visual)
                                    (assoc acc (str node-id) visual)
                                    acc)))
                              {}
                              nodes)
        config (get-prop graph :config)
        viewport (if config
                   (get-prop config :viewport)
                   nil)]

    {:nodes node-metadata
     :viewport (or viewport {:zoom 1.0 :pan [0 0]})}))

(defn append-visual-metadata
  "Append the ^:cgproj metadata to a code string. Returns the code unchanged
  when the metadata has no nodes."
  [code metadata]
  (if (empty? (:nodes metadata))
    code
    (str code "\n\n^:cgproj\n"
         #?(:clj (with-out-str (pprint/pprint metadata))
            :cljs (pr-str metadata)))))

(defn flow-graph->cg-code-with-metadata
  "Generate .cg code from a Flow Graph, with its ^:cgproj metadata."
  [graph]
  (let [code (flow-graph->cg-code graph)
        metadata (extract-visual-metadata graph)]
    (append-visual-metadata code metadata)))

(defn normalize-code
  "Normalize code for a round-trip comparison: collapse whitespace, and print
  a whole decimal as an integer (1.0 -> 1)."
  [code-str]
  (-> code-str
      (str/replace #"(\d+)\.0+(?!\d)" "$1")
      (str/replace #"\s+" " ")
      (str/replace #"\(\s+" "(")
      (str/replace #"\s+\)" ")")
      str/trim))
