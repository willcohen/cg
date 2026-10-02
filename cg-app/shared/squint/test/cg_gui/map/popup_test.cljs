;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.map.popup-test
  "When the card of a clicked feature marks a rule as not met."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_gui/map/popup.mjs" :as pp]))

(deftest a-rule-fails-by-its-status-column-or-past-its-limit
  (let [by-status #js {:highlight "bad"
                       :rules #js [#js {:actual "width" :min "min-width" :status "width-check"}]}
        by-limit  #js {:highlight "bad" :rules #js [#js {:actual "depth" :max "limit"}]}
        rule (fn [props popup] (aget (aget (pp/card-model "a" props popup nil) "rules") 0))]
    (is (true? (aget (rule #js {"width" 41 "min-width" 50 "width-check" "bad"} by-status) "failed")))
    (is (false? (aget (rule #js {"width" 60 "min-width" 50 "width-check" "good"} by-status) "failed")))
    (is (true? (aget (rule #js {"depth" 9 "limit" 8} by-limit) "failed")))
    (is (false? (aget (rule #js {"depth" 7 "limit" 8} by-limit) "failed")))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
