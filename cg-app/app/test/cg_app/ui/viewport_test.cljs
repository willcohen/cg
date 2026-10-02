;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.viewport-test
  "Breakpoint mapping: under 768 is phone, under 1024 is tablet, else desktop.
   The hook half needs the DOM and has no test."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_app/ui/viewport.mjs" :as vp]))

(deftest breakpoint-boundaries
  (is (= "phone" (vp/breakpoint 320)))
  (is (= "phone" (vp/breakpoint 767)))
  (is (= "tablet" (vp/breakpoint 768)))
  (is (= "tablet" (vp/breakpoint 1023)))
  (is (= "desktop" (vp/breakpoint 1024)))
  (is (= "desktop" (vp/breakpoint 1920))))

(deftest phone-predicate
  (is (true? (vp/phone? "phone")))
  (is (false? (vp/phone? "tablet")))
  (is (false? (vp/phone? "desktop"))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
