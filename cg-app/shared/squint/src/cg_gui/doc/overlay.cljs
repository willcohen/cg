;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.doc.overlay
  "The ^:cgproj overlay codec of both hosts. The read half places the cards of
   a parsed flow by their React Flow node ids. The write half keeps the form of
   a key that exists, and a first drag adds its key as the node id."
  (:require [clojure.string :as str]
            [cg-gui.doc.edn :as edn]))

(defn overlay-positions
  "{node-id [x y]} from the overlay of the parsed `flow`, with no entry for a
   node that has no :pos. {} when the file has no overlay."
  [flow]
  (if-let [o (aget flow "overlay")]
    (reduce (fn [acc [id-k props]]
              (if-let [pos (edn/kget props "pos")]
                (assoc acc (edn/kw-name id-k) (vec pos))
                acc))
            {}
            (js/Object.entries (or (edn/kget (edn/read-overlay (aget o "raw")) "nodes") {})))
    {}))

(defn- unq
  "The last `/` segment of a qualified name, as the canvas makes node ids."
  [qn]
  (last (str/split (str qn) #"/")))

(defn- thread-step-id
  "The node id of step `i` of a thread: `<model>__<unq-fn>-<i+1>`."
  [model i op]
  (str model "__" (unq (:qualified-name op)) "-" (inc i)))

(defn- body-node-id
  "The node id of a body-graph node of an op: `<op>-body__<raw>`."
  [op bn]
  (str (:name op) "-body__" (edn/kw-name (str (:id bn)))))

(defn- binding-node-ids [b]
  (case (:body-shape b)
    :function_call   [(str "binding-" (:name b))]
    :threading_macro (map-indexed (fn [i op] (thread-step-id (or (:name b) "anon") i op))
                                   (:ops b))
    []))

(defn overlay-node-ids
  "The node ids whose position merge-positions reads. A host writes no other
   position, because nothing reads it."
  [flow]
  (set (concat (mapcat binding-node-ids (:bindings flow))
               (mapcat (fn [op] (map (fn [bn] (body-node-id op bn)) (:nodes (:body-graph op))))
                       (:operations flow)))))

(defn- placed
  "`node` with :visual {:pos p} when `positions` has `id`."
  [node positions id]
  (if-let [p (get positions id)]
    (assoc node :visual {:pos p})
    node))

(defn- placed-binding [b positions]
  (case (:body-shape b)
    :function_call (placed b positions (str "binding-" (:name b)))
    :threading_macro (update b :ops
                              #(vec (map (fn [op id] (placed op positions id)) % (binding-node-ids b))))
    b))

(defn- placed-op [op positions]
  (if-let [bg (:body-graph op)]
    (assoc op :body-graph
           (update bg :nodes (fn [bns] (mapv (fn [bn] (placed bn positions (body-node-id op bn))) bns))))
    op))

(defn merge-positions
  "`flow` with :visual {:pos [x y]} on each binding, thread step and op body
   node that `positions` ({node-id [x y]}) places."
  [flow positions]
  (-> flow
      (update :bindings (fn [bs] (mapv (fn [b] (placed-binding b positions)) (or bs []))))
      (update :operations (fn [ops] (if (seq ops) (mapv (fn [op] (placed-op op positions)) ops) ops)))))

(defn- splice-overlay
  "Replace the overlay region of `text` with `rendered`, using the
   parser-supplied 1-indexed (start-line, end-line) span."
  [text start-line end-line rendered]
  (let [lines (str/split-lines text)
        before (str/join "\n" (take (dec start-line) lines))
        after  (let [after-lines (drop end-line lines)]
                 (if (seq after-lines)
                   (str "\n" (str/join "\n" after-lines))
                   ""))]
    (str before
         (when (pos? (count before)) "\n")
         rendered
         after)))

(defn- update-overlay-positions
  "Applies a {node-id-string -> [x y]} map to the :nodes of the parsed overlay.
   Updates existing entries and adds ids that are not present."
  [parsed positions]
  (let [existing (or (edn/kget parsed "nodes") {})
        entries  (js/Object.entries existing)
        updated  (reduce (fn [acc [id-k props]]
                           (let [pos (get positions (edn/kw-name id-k))]
                             (assoc acc id-k
                                    (if pos (assoc props ":pos" (vec pos)) props))))
                         {}
                         entries)
        present  (set (map (fn [[id-k _]] (edn/kw-name id-k)) entries))
        added    (reduce (fn [acc [id pos]]
                           (if (contains? present id)
                             acc
                             (assoc acc id {":pos" (vec pos)})))
                         {}
                         (js/Object.entries positions))]
    (assoc parsed ":nodes" (merge updated added))))

(defn update-visual-metadata
  "Writes `positions` (node id -> [x y]) into the overlay block of `text`, or
   appends a new block when `flow` (the parsed CgFlow) has no overlay."
  [text flow positions]
  (let [overlay (aget flow "overlay")]
    (if overlay
      (let [location (aget overlay "location")
            parsed   (edn/read-overlay (aget overlay "raw"))
            updated  (update-overlay-positions parsed positions)
            rendered (str (edn/render-overlay updated) "\n")]
        (splice-overlay text
                        (aget location "start-line")
                        (aget location "end-line")
                        rendered))
      (let [nodes    (reduce (fn [m [id pos]] (assoc m id {":pos" (vec pos)}))
                             {}
                             (js/Object.entries positions))
            rendered (str (edn/render-overlay {":nodes" nodes}) "\n")]
        (str (str/trimr text) "\n\n" rendered)))))
