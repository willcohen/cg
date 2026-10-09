;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.calva-test
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../src/cg_vscode/repl/calva.mjs" :as calva]))

(deftest the-ns-of-a-file-is-the-name-in-its-ns-form
  (is (= "boston" (calva/file-ns "(ns boston\n  \"Doc.\"\n  (:require [cg.dataset :as ds]))")))
  (is (= "cg.parcel" (calva/file-ns ";; a comment\n\n(ns ^:no-doc cg.parcel)")))
  (is (nil? (calva/file-ns "(require '[cg.dataset :as ds])\n(def x 1)")))
  (is (nil? (calva/file-ns "(def x 1)\n(ns late)"))))

(deftest the-jvm-repl-is-named-claims-cg-files-and-starts-in-the-file-ns
  (let [s (calva/connect-sequence "jvm" "/w/proj" "boston")]
    (is (= "generic" (:projectType s)))
    (is (= "none" (:cljsType s)))
    (is (= "cg-jvm" (get-in s [:replSessionNames :primary])))
    (is (= ["/w/proj"] (vec (:projectRootPath s))))
    (is (= ["*.cg" "*.clj" "*.edn"]
           (vec (get-in s [:replSessionFilePatterns :primary "always-claim"]))))
    (is (= "(when (find-ns 'boston) (in-ns 'boston))" (:afterPrimaryReplConnectedCode s)))))

(deftest the-js-repl-takes-cg-files-only-as-a-fallback-and-starts-in-user
  (let [s (calva/connect-sequence "js" "/w/proj" "boston")
        files (get-in s [:replSessionFilePatterns :primary])]
    (is (= "squint" (:projectType s)))
    (is (= "ClojureScript nREPL" (:cljsType s)))
    (is (= "cg-js" (get-in s [:replSessionNames :primary])))
    (is (= ["*.cljs"] (vec (get files "always-claim"))))
    (is (= ["*.cg"] (vec (get files "is-fallback-for"))))
    (is (nil? (:afterPrimaryReplConnectedCode s)))))

(deftest a-file-with-no-ns-or-no-root-gives-no-key
  (let [s (calva/connect-sequence "jvm" nil nil)]
    (is (not (contains? s :projectRootPath)))
    (is (not (contains? s :afterPrimaryReplConnectedCode)))))

(.then (js/Promise.resolve (t/run-tests)) #(js/process.exit (if (t/successful? %) 0 1)))
