;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.metadata
  "Parse the visual metadata of a .cg file (EDN after a ^:cgproj tag, read as
  data and never evaluated) and merge it into Flow Graphs. The keys under
  :nodes are not node ids: see find-node-key-for-metadata."
  (:require [clojure.string :as str]
            #?(:clj [clojure.edn :as edn]
               :cljs ["squint-cljs" :as squint-compiler])))

#?(:clj (set! *warn-on-reflection* true))

(defn split-code-and-metadata
  "Split the content of a .cg file at the ^:cgproj tag into {:code :metadata}.
  :metadata is {} when there is no tag."
  [file-content]
  (if-let [idx (str/index-of file-content "^:cgproj")]
    (let [code (subs file-content 0 idx)
          metadata-start (+ idx (count "^:cgproj"))
          metadata-str (str/trim (subs file-content metadata-start))
          ;; The JVM reads the metadata with the EDN reader, with no eval.
          ;; squint has no EDN reader: compile the text as an expression and
          ;; eval it. :elide-imports and :elide-exports give JS that eval can
          ;; run with no module errors.
          metadata (if (empty? metadata-str)
                    {}
                    #?(:clj (edn/read-string metadata-str)
                       :cljs (let [result-state ((.-compileStringEx squint-compiler)
                                                 metadata-str
                                                 #js {:context "expr"
                                                      :elide-imports true
                                                      :elide-exports true})
                                   compiled (.-javascript result-state)]
                               ;; An indirect eval runs in the global scope,
                               ;; where the squint_core import of this module
                               ;; is not visible. EDN compiles to plain JS
                               ;; literals and does not need it.
                               ((js* "(0, eval)") compiled))))]
      ;; A squint map is a plain JS object. No clj->js is necessary.
      {:code (str/trim code)
       :metadata metadata})
    {:code (str/trim file-content)
     :metadata {}}))

(defn merge-visual-into-node
  "Return node with visual-metadata under :visual. A nil visual-metadata
  gives node as it is."
  [node visual-metadata]
  (if visual-metadata
    (assoc node :visual visual-metadata)
    node))

(defn find-node-key-for-metadata
  "The key in metadata-nodes that matches node, or nil: the data ref name of a
  source node, the operation id of an operation node, or the output binding of
  a sink node."
  [node metadata-nodes]
  (let [node-type (:type node)
        ;; On the JVM a value can be a keyword. On JS it is a string. Try both
        ;; forms.
        to-candidates (fn [val]
                        (when val
                          #?(:clj (let [as-keyword (if (keyword? val) val (keyword val))
                                        as-string (if (keyword? val) (name val) (str val))]
                                    [as-keyword as-string])
                             :cljs (let [s (str val)] [s s]))))
        candidate-keys (case node-type
                         :source (to-candidates (:data-ref node))
                         "source" (to-candidates (:data-ref node))
                         :operation (to-candidates (:id node))
                         "operation" (to-candidates (:id node))
                         :sink (to-candidates (:output-binding node))
                         "sink" (to-candidates (:output-binding node))
                         [])]
    (some #(when (contains? metadata-nodes %) %) candidate-keys)))

(defn merge-metadata-into-graph
  "Merge the metadata map {:nodes {...} :viewport {...}} into a Flow Graph.
  Each matching node gets its :visual data, and :viewport goes into :config."
  [graph metadata]
  (let [graph-clj graph
        metadata-clj metadata

        metadata-nodes (:nodes metadata-clj {})
        viewport (:viewport metadata-clj)

        updated-nodes (mapv (fn [node]
                             (if-let [node-key (find-node-key-for-metadata node metadata-nodes)]
                               (merge-visual-into-node node (get metadata-nodes node-key))
                               node))
                           (:nodes graph-clj))

        updated-config (if viewport
                        (assoc-in (:config graph-clj) [:viewport] viewport)
                        (:config graph-clj))

        result (assoc graph-clj
                      :nodes updated-nodes
                      :config updated-config)]

    result))

(defn has-position?
  "Check if a node has a visual position"
  [node]
  #?(:clj (boolean (get-in node [:visual :pos]))
     :cljs (and node
                (.-visual node)
                (.-pos (.-visual node)))))

(defn default-node-spacing
  "Default spacing between nodes in pixels"
  []
  150)

(defn auto-layout-nodes
  "Give a position to each node that has none. A node with a position stays
  as it is. Each other node goes at x = index * spacing, y = 100."
  [graph]
  (let [graph-clj graph
        nodes (:nodes graph-clj)
        spacing (default-node-spacing)

        nodes-with-positions
        (reduce (fn [acc [idx node]]
                 (if (has-position? node)
                   (conj acc node)
                   (let [x (* idx spacing)
                         y 100
                         visual (assoc (:visual node {}) :pos [x y])]
                     (conj acc (assoc node :visual visual)))))
               []
               (map-indexed vector nodes))

        result (assoc graph-clj :nodes nodes-with-positions)]

    result))

(defn load-cg-with-metadata
  "Merge the visual metadata of a .cg file into each Flow Graph that
  extract-models-fn extracts. Returns {:models :metadata :code}."
  [file-content extract-models-fn]
  (let [{:keys [code metadata]} (split-code-and-metadata file-content)

        {:keys [models]} (extract-models-fn code)

        models-with-metadata (into {}
                                  (map (fn [[name graph]]
                                        [name (-> graph
                                                 (merge-metadata-into-graph metadata)
                                                 auto-layout-nodes)])
                                       models))]

    {:models models-with-metadata
     :metadata metadata
     :code code}))
