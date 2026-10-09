;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.extract-test
  (:require [clojure.string :as str]
            [cljs.test :as t :refer [deftest is]]
            ["../../src/cg_vscode/extract.mjs" :as extract]
            ["../../../cg-app/shared/squint/src/cg_gui/doc/overlay.mjs" :as overlay]))

(deftest ^:async extract-models-returns-cg-flow
  (let [cg-flow (await (extract/extract-models
                        "(require '[cg.macros :refer [item->]])\n(def m (item-> :data))"))]
    (is (map? cg-flow))
    (is (= 1 (count (:requires cg-flow))))
    (is (vector? (:bindings cg-flow)))
    (is (= 1 (count (:bindings cg-flow))))
    (is (= :threading_macro
           (-> cg-flow :bindings first :body-shape)))
    (is (empty? (:diagnostics cg-flow)))))

(deftest ^:async extract-models-loader-binding-classifies-as-dataset-source
  (let [cg-flow (await (extract/extract-models
                        "(def parcels (ds/load-geojson \"parcels.geojson\"))"))]
    (is (= 1 (count (:bindings cg-flow))))
    (let [b (first (:bindings cg-flow))]
      (is (= :function_call (:body-shape b)))
      ;; A loader head has :kind :source in the built-in signatures.
      (is (true? (:is-dataset-source b))))))

(deftest ^:async extract-models-eager-threading-macro-binding-admits
  (let [cg-flow (await (extract/extract-models "(def t (item-> parcels (geo/buffer 10)))"))
        codes (set (map :code (:diagnostics cg-flow)))
        b (first (:bindings cg-flow))]
    (is (not (contains? codes :non-keyword-data-ref)))
    (is (= 1 (count (:bindings cg-flow))))
    (is (= :threading_macro (:body-shape b)))
    (is (false? (:is-template b)))
    (is (= :parameter_ref (-> b :source :kind)))))

(deftest ^:async defop-let-body-builds-body-graph
  ;; squint's (partition 2 2 nil coll) throws on a short last group, unlike
  ;; the JVM. An odd bindings vector must not crash the let-body path.
  (let [even-flow (await (extract/extract-models
                          (str "(defn ^:op prep [^:row row]\n"
                               "  (let [a (:g row) b (when a (inc a))] row))")))
        odd-flow  (await (extract/extract-models
                          (str "(defn ^:op weird [^:row x]\n"
                               "  (let [a (:g x) dangling] x))")))
        even-bg (-> even-flow :operations first :body-graph :nodes)
        odd-bg  (-> odd-flow  :operations first :body-graph :nodes)]
    (is (= ["a" "b"] (mapv :id even-bg)))
    (is (= ["a"] (mapv :id odd-bg)))))

(deftest ^:async update-visual-metadata-adds-new-id-to-existing-overlay
  (let [original (str "(def m (item-> :data))\n"
                      "^:cgproj\n"
                      "{:nodes {\"m__buffer-1\" {:pos [1 2]}}}\n")
        cg-flow (await (extract/extract-models original))
        updated (overlay/update-visual-metadata
                 original cg-flow {"m__simplify-2" [9 9]})]
    (is (str/includes? updated "(def m (item-> :data))"))
    (is (str/includes? updated "m__buffer-1"))
    (is (str/includes? updated "m__simplify-2"))))

(deftest ^:async drag-persist-disk-round-trip
  (let [original (str "(require '[cg.macros :refer [item->]])\n"
                      "(require '[cg.geo :as geo])\n"
                      "(def m (item-> :data (geo/buffer 10)))\n")
        cg-flow (await (extract/extract-models original))
        written (overlay/update-visual-metadata
                 original cg-flow {"m__buffer-1" [55 66]})
        cg-flow2 (await (extract/extract-models written))
        merged  (overlay/merge-positions cg-flow2 (overlay/overlay-positions cg-flow2))
        b       (first (:bindings merged))]
    (is (= [55 66] (get-in b [:ops 0 :visual :pos])))))

(deftest ^:async overlay-node-ids-are-the-ids-that-the-merge-reads
  ;; A drag of a thread frame (model-m, op-prep) or of a parameter chip
  ;; (param-width) has no reader. The host does not write it.
  (let [cg-flow (await (extract/extract-models
                        (str "(require '[cg.macros :refer [table->]])\n"
                             "(require '[cg.geo :as geo])\n"
                             "(def width 10)\n"
                             "(def parcels (ds/->dataset {:a [1]}))\n"
                             "(defn ^:op prep [^:table rows]\n"
                             "  (table-> rows (geo/buffer 1)))\n"
                             "(def m (table-> :parcels (geo/buffer width) (prep)))\n")))
        ids (overlay/overlay-node-ids cg-flow)]
    (is (contains? ids "binding-parcels"))
    (is (contains? ids "m__buffer-1"))
    (is (contains? ids "m__prep-2"))
    (is (some (fn [id] (str/starts-with? id "prep-body__")) ids))
    (is (not (contains? ids "param-width")))))

(deftest ^:async each-overlay-node-id-round-trips
  (let [text    (str "(require '[cg.macros :refer [table->]])\n"
                     "(require '[cg.geo :as geo])\n"
                     "(def parcels (ds/->dataset {:a [1]}))\n"
                     "(defn ^:op prep [^:table rows]\n"
                     "  (table-> rows (geo/buffer 1)))\n"
                     "(def m (table-> :parcels (geo/buffer 2) (prep)))\n")
        cg-flow (await (extract/extract-models text))
        ids     (vec (overlay/overlay-node-ids cg-flow))
        written (overlay/update-visual-metadata
                 text cg-flow
                 (into {} (map-indexed (fn [i id] [id [(* 10 (inc i)) 7]]) ids)))
        flow2   (await (extract/extract-models written))
        merged  (overlay/merge-positions flow2 (overlay/overlay-positions flow2))
        found   (concat (keep (fn [b] (get-in b [:visual :pos])) (:bindings merged))
                        (keep (fn [op] (get-in op [:visual :pos]))
                              (mapcat :ops (:bindings merged)))
                        (keep (fn [bn] (get-in bn [:visual :pos]))
                              (mapcat (fn [op] (:nodes (:body-graph op)))
                                      (:operations merged))))]
    (is (pos? (count ids)))
    (is (= (count ids) (count found)))
    (is (= (count (:diagnostics cg-flow)) (count (:diagnostics flow2))))))

;; The runner reads the exit code.
(.then (t/run-tests) #(when-not (t/successful? %) (js/process.exit 1)))
