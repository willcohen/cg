;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.extract
  "The parser of cg core and the overlay read for the extension. The overlay
   EDN goes through cg-gui.doc.edn, which keeps the leading colon of a key."
  (:require ["../../../cg/src/cg/parser/core.mjs" :as parser]
            ["node:module" :refer [createRequire]]
            ["../../../cg-app/shared/squint/src/cg_gui/doc/edn.mjs" :as edn]
            [clojure.string :as str]))

(defonce ^:private !grammar-wasm (atom nil))

(defn- grammar-wasm-path
  "The path of the tree-sitter-clojure grammar wasm. createRequire needs
   import.meta.url, which esbuild shims to a file URL in the CJS bundle."
  []
  ;; Deref after reset!, because squint reset! returns undefined.
  (when (nil? @!grammar-wasm)
    (let [req (createRequire (js* "import.meta.url"))]
      (reset! !grammar-wasm
              (.resolve req "@yogthos/tree-sitter-clojure/tree-sitter-clojure.wasm"))))
  @!grammar-wasm)

(defn ^:async extract-models
  "Promise of the CgFlow of `text`. A form that the parser rejects becomes a
   block, which the canvas shows as a read-only card."
  [text]
  (parser/extract-models text {:grammar-wasm (grammar-wasm-path)
                               :opaque-blocks true}))

(defn recheck
  "Runs the parser checks again after the merge of the operations of a followed
   module. The first parse marks each call to those operations as unresolved."
  [flow]
  (parser/recheck flow))

(defn- unq
  "The last `/` segment of a qualified name, as the canvas makes node ids."
  [qn]
  (let [s (str qn)]
    (or (last (str/split s #"/")) s)))

(defn decoded-overlay
  "{:positions [{:node-id :pos {:x :y} :collapsed}] :viewport} from the overlay
   of the parser, or nil when the file has no overlay."
  [{:keys [overlay]}]
  (when overlay
    (let [parsed    (edn/read-overlay (:raw overlay))
          nodes     (or (edn/kget parsed "nodes") {})
          positions (mapv (fn [[id-k props]]
                            (let [pos (or (edn/kget props "pos") [])]
                              {:node-id (edn/kw-name id-k)
                               :pos {:x (nth pos 0 nil) :y (nth pos 1 nil)}
                               :collapsed (boolean (edn/kget props "collapsed"))}))
                          (js/Object.entries nodes))
          viewport  (edn/kget parsed "viewport")]
      (cond-> {:positions positions}
        viewport (assoc :viewport (edn/strip-keys viewport))))))

(defn- decoded-id->pos
  "{node-id [x y]} from a decoded overlay."
  [overlay]
  (reduce (fn [acc p]
            (assoc acc (:node-id p)
                   [(get-in p [:pos :x]) (get-in p [:pos :y])]))
          {}
          (:positions overlay)))

(defn- strip-colon [s]
  (if (str/starts-with? s ":") (subs s 1) s))

(defn- thread-step-id
  "The React Flow id of step `i` of a thread: `<model>__<unq-fn>-<i+1>`."
  [model i op]
  (str model "__" (unq (:qualified-name op)) "-" (inc i)))

(defn- body-node-id
  "The React Flow id of a body-graph node of an op: `<op>-body__<raw>`."
  [op bn]
  (str (:name op) "-body__" (strip-colon (str (:id bn)))))

(defn- binding-node-ids [b]
  (case (:body-shape b)
    :function_call   [(str "binding-" (:name b))]
    :threading_macro (map-indexed (fn [i op] (thread-step-id (or (:name b) "anon") i op))
                                  (:ops b))
    []))

(defn overlay-node-ids
  "The canvas node ids whose overlay position merge-overlay-into-cg-flow reads.
   The host writes no other position, because nothing reads it."
  [cg-flow]
  (set (concat (mapcat binding-node-ids (:bindings cg-flow))
               (mapcat (fn [op]
                         (map (fn [bn] (body-node-id op bn))
                              (:nodes (:body-graph op))))
                       (:operations cg-flow)))))

(defn merge-overlay-into-cg-flow
  "Adds :visual {:pos [x y]} from a decoded overlay to each binding, thread step
   and op body node of `cg-flow`, by the node ids of the canvas."
  [cg-flow overlay]
  (if-not overlay
    cg-flow
    (let [id->pos (decoded-id->pos overlay)]
      (-> cg-flow
          (update :bindings
                  (fn [bindings]
                    (mapv
                     (fn [b]
                       (case (:body-shape b)
                         :function_call
                         (if-let [p (get id->pos (str "binding-" (:name b)))]
                           (assoc b :visual {:pos p})
                           b)
                         :threading_macro
                         (let [model (or (:name b) "anon")]
                           (update b :ops
                                   (fn [ops]
                                     (vec (map-indexed
                                           (fn [i op]
                                             (let [id (thread-step-id model i op)]
                                               (if-let [p (get id->pos id)]
                                                 (assoc op :visual {:pos p})
                                                 op)))
                                           ops)))))
                         b))
                     (or bindings []))))
          (update :operations
                  (fn [ops]
                    (if (seq ops)
                      (mapv
                       (fn [op]
                         (if-let [bg (:body-graph op)]
                           (assoc op :body-graph
                                  (update bg :nodes
                                          (fn [bnodes]
                                            (mapv
                                             (fn [bn]
                                               (if-let [p (get id->pos (body-node-id op bn))]
                                                 (assoc bn :visual {:pos p})
                                                 bn))
                                             bnodes))))
                           op))
                       ops)
                      ops)))))))
