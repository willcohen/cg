;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.metadata
  "Parse the visual metadata of a .cg file (EDN after a ^:cgproj tag, read as
  data and never evaluated) and merge it into Flow Graphs. The keys under
  :nodes are not node ids: see find-node-key-for-metadata."
  (:require [cg.util :as util]
            [clojure.string :as str]))

#?(:clj (set! *warn-on-reflection* true))

(defn split-code-and-metadata
  "Split the content of a .cg file at the ^:cgproj tag into {:code :metadata}.
  :metadata is {} when there is no tag."
  [file-content]
  (if-let [idx (str/index-of file-content "^:cgproj")]
    (let [code (subs file-content 0 idx)
          metadata-start (+ idx (count "^:cgproj"))
          metadata-str (str/trim (subs file-content metadata-start))
          metadata (if (empty? metadata-str) {} (util/read-edn-string metadata-str))]
      ;; A squint map is a plain JS object. No clj->js is necessary.
      {:code (str/trim code)
       :metadata metadata})
    {:code (str/trim file-content)
     :metadata {}}))

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
                         (:source "source") (to-candidates (:data-ref node))
                         (:operation "operation") (to-candidates (:id node))
                         (:sink "sink") (to-candidates (:output-binding node))
                         [])]
    (some #(when (contains? metadata-nodes %) %) candidate-keys)))

(defn merge-metadata-into-graph
  "Merge the metadata map {:nodes {...} :viewport {...}} into a Flow Graph.
  Each matching node gets its :visual data, and :viewport goes into :config."
  [graph metadata]
  (let [metadata-nodes (:nodes metadata {})
        viewport (:viewport metadata)]
    (assoc graph
           :nodes (mapv (fn [node]
                          (if-let [visual (some->> (find-node-key-for-metadata node metadata-nodes)
                                                   (get metadata-nodes))]
                            (assoc node :visual visual)
                            node))
                        (:nodes graph))
           :config (if viewport
                     (assoc-in (:config graph) [:viewport] viewport)
                     (:config graph)))))

(defn has-position? [node]
  (boolean (get-in node [:visual :pos])))

(defn auto-layout-nodes
  "Give a position to each node that has none. A node with a position stays
  as it is. Each other node goes at x = index * spacing, y = 100."
  [graph]
  (let [spacing 150] ; px
    (assoc graph :nodes
           (vec (map-indexed (fn [idx node]
                               (if (has-position? node)
                                 node
                                 (assoc node :visual (assoc (:visual node {}) :pos [(* idx spacing) 100]))))
                             (:nodes graph))))))

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
