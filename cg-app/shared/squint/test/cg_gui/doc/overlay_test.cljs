;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.doc.overlay-test
  "The ^:cgproj overlay. The read half places the cards by their full node ids.
   The write half updates the block in place."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_gui/doc/edn.mjs" :as edn]
            ["../../../src/cg_gui/doc/overlay.mjs" :as ov]))

(def text-with-overlay
  "(def buffered\n  (table-> parcels\n           (geo/buffer 10)))\n\n^:cgproj\n{:nodes {:buffer-1 {:pos [400 250]}}}")

(def flow-with-overlay
  #js {:overlay #js {:raw "^:cgproj\n{:nodes {:buffer-1 {:pos [400 250]}}}"
                     :location #js {:start-line 5 :start-column 1
                                    :end-line 6 :end-column 38}}})

(def flow-no-overlay #js {:overlay nil})

(deftest edn-overlay-round-trip
  (let [parsed (edn/read-overlay "^:cgproj\n{:nodes {:buffer-1 {:pos [400 250]}}}")
        nodes (edn/kget parsed "nodes")]
    (is (= 400 (nth (edn/kget (edn/kget nodes "buffer-1") "pos") 0)))
    (let [rendered (edn/render-overlay parsed)
          reparsed (edn/read-overlay rendered)]
      (is (.startsWith rendered "^:cgproj"))
      (is (= 250 (nth (edn/kget (edn/kget (edn/kget reparsed "nodes") "buffer-1") "pos") 1))))))

(deftest update-existing-overlay-in-place
  (let [updated (ov/update-visual-metadata text-with-overlay flow-with-overlay
                                           {"buffer-1" #js [111 222]})]
    (is (.includes updated "(geo/buffer 10)"))
    (is (.includes updated "[111 222]"))
    (is (not (.includes updated "[400 250]")))))

(deftest drag-of-unknown-id-adds-key
  (let [updated (ov/update-visual-metadata text-with-overlay flow-with-overlay
                                           {"buffered__simplify-2" #js [7 8]})
        parsed (edn/read-overlay (subs updated (.indexOf updated "^:cgproj")))
        nodes (edn/kget parsed "nodes")]
    (is (= 400 (nth (edn/kget (edn/kget nodes "buffer-1") "pos") 0)))
    (is (= 7 (nth (edn/kget (get nodes "buffered__simplify-2") "pos") 0)))))

(deftest overlay-positions-read-the-node-ids
  (is (= {"buffer-1" [400 250]} (ov/overlay-positions flow-with-overlay)))
  (is (= {} (ov/overlay-positions flow-no-overlay)))
  (is (= {} (ov/overlay-positions #js {:overlay #js {:raw "^:cgproj\n{:nodes {:a {:collapsed true}}}"}}))
      "a node with no :pos gives no entry"))

(deftest an-overlay-the-reader-rejects-places-nothing
  (is (= {} (ov/overlay-positions #js {:overlay #js {:raw "^:cgproj\n{:nodes {:a {:pos [1 2]}} :tags #{:x}}"}}))))

(def flow
  #js {:bindings #js [#js {:name "parcels" :body-shape "function_call"}
                      #js {:name "buffered" :body-shape "threading_macro"
                           :ops #js [#js {:qualified-name "geo/buffer"}
                                     #js {:qualified-name "geo/simplify"}]}]
       :operations #js [#js {:name "prep"
                             :body-graph #js {:nodes #js [#js {:id "buffer-1"}]}}]})

(defn- pos-of [node] (some-> node (aget "visual") (aget "pos") vec))

(deftest merge-positions-places-each-kind-of-node
  (let [merged (ov/merge-positions flow {"binding-parcels" [7 8]
                                         "buffered__simplify-2" [10 20]
                                         "prep-body__buffer-1" [41 191]})
        bs (aget merged "bindings")]
    (is (= [7 8] (pos-of (aget bs 0))))
    (is (nil? (pos-of (aget (aget (aget bs 1) "ops") 0))))
    (is (= [10 20] (pos-of (aget (aget (aget bs 1) "ops") 1))))
    (is (= [41 191] (pos-of (-> merged (aget "operations") (aget 0) (aget "body-graph") (aget "nodes") (aget 0)))))
    (is (nil? (pos-of (aget (aget flow "bindings") 0))) "the flow is not changed")))

(deftest a-bare-key-places-nothing
  ;; buffer-1 matches step 1 of each model whose first step is a buffer.
  (let [merged (ov/merge-positions flow {"buffer-1" [1 2] "parcels" [3 4]})]
    (is (nil? (pos-of (aget (aget merged "bindings") 0))))
    (is (nil? (pos-of (aget (aget (aget (aget merged "bindings") 1) "ops") 0))))))

(deftest overlay-node-ids-are-the-ids-that-merge-positions-reads
  (is (= #{"binding-parcels" "buffered__buffer-1" "buffered__simplify-2" "prep-body__buffer-1"}
         (ov/overlay-node-ids flow))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
