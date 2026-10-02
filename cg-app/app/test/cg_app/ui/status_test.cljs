;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.status-test
  "The header status line: errors and warnings are counted apart, and only an
   error gives the error state."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_app/ui/status.mjs" :as status]))

(defn- diag [severity code]
  #js {:severity severity :code code :message (str code " here")})

(deftest counts-split-errors-from-warnings
  (let [c (status/diagnostic-counts #js [(diag "warning" "control-form-binding")
                                         (diag "error" "unresolved-op")
                                         (diag "warning" "control-form-binding")])]
    (is (= 1 (aget c "errors")))
    (is (= 2 (aget c "warnings")))))

(deftest counts-text-is-plain-and-plural
  (is (= "2 warnings" (status/counts-text #js {:errors 0 :warnings 2})))
  (is (= "1 error, 1 warning" (status/counts-text #js {:errors 1 :warnings 1})))
  (is (nil? (status/counts-text #js {:errors 0 :warnings 0}))))

(deftest warnings-only-is-not-an-error
  (let [l (status/status-line nil "boston.cg" #js [(diag "warning" "control-form-binding")
                                                   (diag "warning" "control-form-binding")])]
    (is (= "boston.cg · 2 warnings" (.-text l)))
    (is (= "warning" (.-kind l)))
    (is (= "warning: control-form-binding here\nwarning: control-form-binding here"
           (.-title l)))))

(deftest an-error-gives-the-error-state
  (let [l (status/status-line nil "a.cg" #js [(diag "error" "unresolved-op")])]
    (is (= "a.cg · 1 error" (.-text l)))
    (is (= "error" (.-kind l)))))

(deftest a-clean-file-shows-its-name-only
  (let [l (status/status-line nil "simple.cg" #js [])]
    (is (= "simple.cg" (.-text l)))
    (is (= "ok" (.-kind l)))
    (is (nil? (.-title l)))))

(deftest no-file-and-load-error
  (is (= "no file loaded" (.-text (status/status-line nil nil nil))))
  (let [l (status/status-line "x.cg: HTTP 404" "a.cg" #js [])]
    (is (= "error: x.cg: HTTP 404" (.-text l)))
    (is (= "error" (.-kind l)))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
