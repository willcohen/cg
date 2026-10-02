;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.extract-test
  "Runs standalone: node test/cg_vscode/extract_test.mjs."
  (:require [clojure.string :as str]
            [cljs.test :as t :refer [deftest is]]
            ["../../src/cg_vscode/extract.mjs" :as extract]
            ["../../../cg-app/shared/squint/src/cg_gui/doc/overlay.mjs" :as overlay]))

(deftest ^:async extract-models-returns-cg-flow
  (let [cg-flow (await (extract/extract-models
                        "(require '[cg.macros :refer [item->]])\n(def m (item-> :data))"))]
    (is (map? cg-flow))
    (is (vector? (:bindings cg-flow)))
    (is (= 1 (count (:bindings cg-flow))))
    (is (= :threading_macro
           (-> cg-flow :bindings first :body-shape)))
    (is (empty? (:diagnostics cg-flow)))))

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
    (is (= 2 (count even-bg)))
    (is (= ["a" "b"] (mapv :id even-bg)))
    (is (= 1 (count odd-bg)))
    (is (= ["a"] (mapv :id odd-bg)))))

(deftest ^:async update-visual-metadata-mutates-only-overlay
  (let [original (str "(require '[cg.macros :refer [item->]])\n"
                      "(def m (item-> :data))\n"
                      "^:cgproj\n"
                      "{:nodes {:item-> {:pos [10 20]}}}\n")
        cg-flow (await (extract/extract-models original))
        updated (overlay/update-visual-metadata
                 original cg-flow {"item->" [99 99]})]
    (is (str/includes? updated "(def m (item-> :data))"))
    (is (str/includes? updated "99"))))

(deftest ^:async decoded-overlay-shape
  (let [text (str "(def m (item-> :data))\n"
                  "^:cgproj\n"
                  "{:nodes {:item-> {:pos [10 20]}}}\n")
        cg-flow (await (extract/extract-models text))
        decoded (extract/decoded-overlay cg-flow)
        p (first (:positions decoded))]
    (is (= "item->" (:node-id p)))
    (is (= {:x 10 :y 20} (:pos p)))))

(deftest ^:async update-visual-metadata-creates-overlay-when-absent
  (let [original (str "(require '[cg.macros :refer [item->]])\n"
                      "(def m (item-> :data))\n")
        cg-flow (await (extract/extract-models original))
        updated (overlay/update-visual-metadata
                 original cg-flow {"m__foo-1" [12 34]})]
    (is (str/includes? updated "^:cgproj"))
    (is (str/includes? updated "m__foo-1"))
    (is (str/includes? updated "12"))
    (is (str/includes? updated "(def m (item-> :data))"))))

(deftest ^:async update-visual-metadata-adds-new-id-to-existing-overlay
  (let [original (str "(def m (item-> :data))\n"
                      "^:cgproj\n"
                      "{:nodes {\"m__buffer-1\" {:pos [1 2]}}}\n")
        cg-flow (await (extract/extract-models original))
        updated (overlay/update-visual-metadata
                 original cg-flow {"m__simplify-2" [9 9]})]
    (is (str/includes? updated "m__buffer-1"))
    (is (str/includes? updated "m__simplify-2"))))

(deftest ^:async merge-overlay-into-cg-flow-attaches-visual
  (let [original (str "(require '[cg.macros :refer [item->]])\n"
                      "(require '[cg.geo :as geo])\n"
                      "(def m (item-> :data (geo/buffer 10)))\n"
                      "^:cgproj\n"
                      "{:nodes {\"m__buffer-1\" {:pos [7 8]}}}\n")
        cg-flow (await (extract/extract-models original))
        overlay (extract/decoded-overlay cg-flow)
        merged  (extract/merge-overlay-into-cg-flow cg-flow overlay)
        b       (first (:bindings merged))]
    (is (= :threading_macro (:body-shape b)))
    (is (= [7 8] (get-in b [:ops 0 :visual :pos])))))

(deftest ^:async drag-persist-disk-round-trip
  (let [original (str "(require '[cg.macros :refer [item->]])\n"
                      "(require '[cg.geo :as geo])\n"
                      "(def m (item-> :data (geo/buffer 10)))\n")
        cg-flow (await (extract/extract-models original))
        written (overlay/update-visual-metadata
                 original cg-flow {"m__buffer-1" [55 66]})
        cg-flow2 (await (extract/extract-models written))
        overlay (extract/decoded-overlay cg-flow2)
        merged  (extract/merge-overlay-into-cg-flow cg-flow2 overlay)
        b       (first (:bindings merged))]
    (is (= [55 66] (get-in b [:ops 0 :visual :pos])))))

(deftest merge-overlay-into-cg-flow-loader-branch
  ;; Pure data: the test does not depend on the body-shape choice of the parser.
  (let [cg-flow {:bindings [{:name "parcels" :body-shape :function_call
                             :head-sym "load-shp"}]}
        overlay {:positions [{:node-id "binding-parcels" :pos {:x 3 :y 4}}]}
        merged  (extract/merge-overlay-into-cg-flow cg-flow overlay)
        b       (first (:bindings merged))]
    (is (= [3 4] (get-in b [:visual :pos])))))

(deftest merge-overlay-into-cg-flow-body-step
  ;; The position is relative to the call site.
  (let [cg-flow {:bindings []
                 :operations [{:name "analyze-parcel"
                               :body-graph {:nodes [{:id :buffer-1 :fn "geo/buffer"}]
                                            :edges []}}]}
        overlay {:positions [{:node-id "analyze-parcel-body__buffer-1"
                              :pos {:x 41 :y 191}}]}
        merged  (extract/merge-overlay-into-cg-flow cg-flow overlay)
        bn      (get-in merged [:operations 0 :body-graph :nodes 0])]
    (is (= [41 191] (get-in bn [:visual :pos])))))

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
        ids (extract/overlay-node-ids cg-flow)]
    (is (contains? ids "binding-parcels"))
    (is (contains? ids "m__buffer-1"))
    (is (contains? ids "m__prep-2"))
    (is (some (fn [id] (str/starts-with? id "prep-body__")) ids))
    (is (not (contains? ids "model-m")))
    (is (not (contains? ids "op-prep")))
    (is (not (contains? ids "param-width")))))

(deftest ^:async each-overlay-node-id-round-trips
  (let [text    (str "(require '[cg.macros :refer [table->]])\n"
                     "(require '[cg.geo :as geo])\n"
                     "(def parcels (ds/->dataset {:a [1]}))\n"
                     "(defn ^:op prep [^:table rows]\n"
                     "  (table-> rows (geo/buffer 1)))\n"
                     "(def m (table-> :parcels (geo/buffer 2) (prep)))\n")
        cg-flow (await (extract/extract-models text))
        ids     (vec (extract/overlay-node-ids cg-flow))
        written (overlay/update-visual-metadata
                 text cg-flow
                 (into {} (map-indexed (fn [i id] [id [(* 10 (inc i)) 7]]) ids)))
        flow2   (await (extract/extract-models written))
        merged  (extract/merge-overlay-into-cg-flow flow2 (extract/decoded-overlay flow2))
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
(.then (t/run-tests)
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
