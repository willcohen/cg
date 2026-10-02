;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.map.core-test
  "The viewport fns of the shared map core, which drive the refetch of a JVM
   session. They run under node with a fake map."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_gui/map/core.mjs" :as mc]))

(defn- fake-map [on-spy]
  #js {:on (fn [ev h] (aset on-spy ev h))
       :getBounds (fn [] #js {:getWest (fn [] 10.2) :getSouth (fn [] 20.2)
                              :getEast (fn [] 10.4) :getNorth (fn [] 20.6)})})

(deftest viewport-bbox-is-west-south-east-north
  (let [bbox (mc/viewport-bbox (fake-map #js {}))]
    (is (= [10.2 20.2 10.4 20.6] (vec bbox)))))

(deftest install-viewport-refetch-registers-moveend
  (let [spy #js {}]
    (mc/install-viewport-refetch! (fake-map spy) (fn [_bbox] nil) 300)
    (is (fn? (aget spy "moveend")))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
