;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.doc.overlay-test
  "The write half of the ^:cgproj overlay: the block is updated in place,
   or appended when the source has none."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_gui/doc/edn.mjs" :as edn]
            ["../../../src/cg_gui/doc/overlay.mjs" :as ov]))

(def text-with-overlay
  "(def buffered\n  (table-> parcels\n           (geo/buffer 10)))\n\n^:cgproj\n{:nodes {:buffer-1 {:pos [400 250]}}}")

(def flow-with-overlay
  #js {:overlay #js {:raw "^:cgproj\n{:nodes {:buffer-1 {:pos [400 250]}}}"
                     :location #js {:start-line 5 :start-column 1
                                    :end-line 6 :end-column 38}}})

(def text-no-overlay "(def buffered\n  (table-> parcels\n           (geo/buffer 10)))\n")
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

(deftest append-overlay-when-absent
  (let [updated (ov/update-visual-metadata text-no-overlay flow-no-overlay
                                           {"buffered__buffer-1" #js [10 20]})]
    (is (.includes updated "(geo/buffer 10)"))
    (is (.includes updated "^:cgproj"))
    (is (.includes updated "[10 20]"))))

(deftest drag-of-unknown-id-adds-key
  (let [updated (ov/update-visual-metadata text-with-overlay flow-with-overlay
                                           {"buffered__simplify-2" #js [7 8]})
        parsed (edn/read-overlay (subs updated (.indexOf updated "^:cgproj")))
        nodes (edn/kget parsed "nodes")]
    (is (= 400 (nth (edn/kget (edn/kget nodes "buffer-1") "pos") 0)))
    (is (= 7 (nth (edn/kget (get nodes "buffered__simplify-2") "pos") 0)))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
