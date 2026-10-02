;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.splitter-test
  "The width math and the memory of the panel borders. The drag needs the DOM
   and runs in the demo check."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_app/ui/splitter.mjs" :as sp]))

(deftest set-key-and-neighbors
  (is (= "flow+code+map" (sp/set-key #js ["flow" "code" "map"])))
  (is (= "code" (sp/next-shown #js ["flow" "code" "map"] "flow")))
  (is (= "map" (sp/next-shown #js ["flow" "code" "map"] "code")))
  (is (nil? (sp/next-shown #js ["flow" "code" "map"] "map")))
  (is (nil? (sp/next-shown #js ["flow" "map"] "code"))))

(def ^:private five #js ["flow" "code" "status" "map" "results"])

(defn- widths [m] (mapv (fn [k] (aget m k)) five))

(deftest moved-takes-from-the-nearest-neighbor-first
  (let [base #js {:flow 300 :code 300 :status 300 :map 300 :results 300}]
    (is (= [400 200 300 300 300] (widths (sp/moved base five "flow" 100))))
    (is (= [600 200 200 200 300] (widths (sp/moved base five "flow" 300)))
        "past the minimum of code, status and then map give room")
    (is (= [700 200 200 200 200] (widths (sp/moved base five "flow" 5000)))
        "no panel goes under min-width")
    (is (= [300 200 200 500 300] (widths (sp/moved base five "status" -200)))
        "a move to the left shrinks status, then code")
    (is (= 300 (aget base "flow")) "the base is not changed")))

(deftest moved-keeps-the-sum
  (let [base #js {:flow 250 :code 410 :status 360 :map 330 :results 250}
        sum (fn [m] (reduce + (widths m)))]
    (doseq [[k dx] [["flow" 77] ["code" -300] ["map" 900] ["status" -9000]]]
      (is (= (sum base) (sum (sp/moved base five k dx))) (str k " " dx)))))

(deftest moved-with-a-panel-under-the-minimum
  (let [base #js {:flow 150 :code 150}
        two #js ["flow" "code"]]
    (is (= 150 (aget (sp/moved base two "flow" 50) "flow"))
        "a narrow neighbor gives nothing")))

(deftest style-and-percent
  (is (nil? (sp/panel-style nil "flow")))
  (is (nil? (sp/panel-style #js {:code 3} "flow")))
  (is (= "520 1 0px" (aget (sp/panel-style #js {:flow 520} "flow") "flex")))
  (is (= 60 (sp/percent-of-pair 600 400)))
  (is (= 50 (sp/percent-of-pair 0 0))))

(deftest sizes-stay-per-set-without-storage
  (sp/forget-cache!)
  (sp/set-sizes! "flow+code" #js {:flow 700 :code 300} true)
  (sp/set-sizes! "flow+code+map" #js {:flow 1 :code 1 :map 2} true)
  (is (= 700 (aget (sp/sizes-for "flow+code") "flow")))
  (is (= 2 (aget (sp/sizes-for "flow+code+map") "map")))
  (sp/set-sizes! "flow+code" nil true)
  (is (nil? (sp/sizes-for "flow+code")) "nil gives back the default shares")
  (is (some? (sp/sizes-for "flow+code+map"))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
