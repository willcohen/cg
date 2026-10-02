;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.doc.modules-test
  "The module follower with a fake host: the modules are JSON texts, so
   the test does not need the parser."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_gui/doc/modules.mjs" :as modules]))

(defn- module-text [ns-name op-names loads]
  (js/JSON.stringify
   #js {:module-ns #js {:name ns-name}
        :operations (into-array (map (fn [n] #js {:name n}) op-names))
        :loaded-modules (into-array (map (fn [p] #js {:path p}) loads))}))

(def ^:private files
  {"/abs/src/cg/parcel.cg" (module-text "cg.parcel" ["prepare-parcel"] ["src/cg/zoning-rules.cg"])
   "/abs/src/cg/zoning-rules.cg" (module-text "cg.zoning" ["zone-tables" "district-stats"] ["src/cg/parcel.cg"])
   "/abs/src/cg/broken.cg" "not json"})

(defn- fake-host [reads]
  {:resolve (fn [p] (when-not (= p "src/cg/missing.cg") (str "/abs/" p)))
   :read (fn [id]
           (.push reads id)
           (js/Promise.resolve (get files id)))
   :parse (fn [text] (js/Promise.resolve (js/JSON.parse text)))})

(defn- flow-loading [& paths]
  #js {:loaded-modules (into-array (map (fn [p] #js {:path p}) paths))})

(deftest module-label-from-path
  (is (= "Parcel" (modules/module-label-from-path "src/cg/parcel.cg")))
  (is (= "Zoning Rules" (modules/module-label-from-path "src/cg/zoning-rules.cg")))
  (is (= "A" (modules/module-label-from-path "a.cg"))))

(deftest ^:async follows-the-chain-and-tags-each-op
  (let [reads #js []
        ops (await (modules/follow-loaded-modules!
                    (flow-loading "src/cg/parcel.cg") (fake-host reads)))]
    (is (= ["prepare-parcel" "zone-tables" "district-stats"]
           (mapv (fn [op] (get op "name")) ops)))
    (is (= {"path" "/abs/src/cg/parcel.cg" "label" "Parcel" "ns" "cg.parcel"}
           (get (first ops) "source-module")))
    (is (= "Zoning Rules" (get-in (second ops) ["source-module" "label"])))
    (is (= ["/abs/src/cg/parcel.cg" "/abs/src/cg/zoning-rules.cg"] (vec reads))
        "the load cycle ends, and each module is read once")))

(deftest ^:async skips-a-module-that-does-not-resolve-read-or-parse
  (let [reads #js []
        ops (await (modules/follow-loaded-modules!
                    (flow-loading "src/cg/missing.cg" "src/cg/none.cg"
                                  "src/cg/broken.cg" "src/cg/parcel.cg")
                    (fake-host reads)))]
    (is (= ["prepare-parcel" "zone-tables" "district-stats"]
           (mapv (fn [op] (get op "name")) ops)))
    (is (not (some (fn [id] (= id "/abs/src/cg/missing.cg")) reads))
        "an unresolved module is not read")))

(deftest ^:async no-loaded-modules-gives-no-ops
  (is (= [] (vec (await (modules/follow-loaded-modules! #js {} (fake-host #js [])))))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
