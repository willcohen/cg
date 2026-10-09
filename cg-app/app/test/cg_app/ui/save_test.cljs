;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.save-test
  "The save serializer: the saved text decodes back through the real cg
   metadata module. Also the shape of the cg/file-save-request message."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../../../cg/src/cg/metadata.mjs" :as md]
            ["../../../src/cg_app/ui/save.mjs" :as save]))

(defn- decoded-pos
  "The [x y] of the node `id` in the overlay of `text`, as the cg metadata
   module reads it."
  [text id]
  (some-> (md/split_code_and_metadata text) (aget "metadata") (aget "nodes") (aget id) (aget "pos")))

(defn- saved-text [text flow positions]
  (aget (save/request-for "x.cg" {:text text} flow positions) "content"))

(def code "(def buffered\n  (table-> parcels\n           (geo/buffer 10)))")

(def text-with-overlay
  (str code "\n\n^:cgproj\n{:nodes {:buffer-1 {:pos [400 250]}}}"))

(def flow-with-overlay
  #js {:overlay #js {:raw "^:cgproj\n{:nodes {:buffer-1 {:pos [400 250]}}}"
                     :location #js {:start-line 5 :start-column 1
                                    :end-line 6 :end-column 38}}})

(deftest saved-text-roundtrips-through-decode
  (let [txt (saved-text text-with-overlay flow-with-overlay
                        {"buffer-1" #js [111 222]})]
    (is (= [111 222] (vec (decoded-pos txt "buffer-1"))))))

(deftest saved-text-appends-overlay-when-absent
  (let [txt (saved-text (str code "\n") #js {:overlay nil}
                        {"buffered__buffer-1" #js [10 20]})]
    (is (.includes txt "(geo/buffer 10)"))
    (is (= [10 20] (vec (decoded-pos txt "buffered__buffer-1"))))))

(deftest save-request-shape
  (let [req (save/request-for "x.cg" {:text "(def x 1)\n"} #js {:overlay nil}
                              {"a" #js [1 2]})]
    (is (= "cg/file-save-request" (aget req "type")))
    (is (= "x.cg" (aget req "name")))
    (is (.includes (aget req "content") "^:cgproj"))))

(deftest text-that-does-not-parse-is-saved-as-typed
  (let [flow #js {:overlay #js {:raw "^:cgproj {:nodes {}}"
                                :location #js {:start-line 1 :end-line 1}}}
        typed "(def x \"1)\n^:cgproj {:nodes {}}\n"
        req (save/request-for "x.cg" {:text typed :unparsed "an open string"}
                              flow {"a" #js [1 2]})]
    (is (= typed (aget req "content")))
    (is (= "x.cg" (aget req "name")))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
