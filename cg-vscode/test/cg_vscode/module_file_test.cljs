;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.module-file-test
  "The file of a module path: under the project root, then in cg."
  (:require [cljs.test :as t :refer [deftest is]]
            ["path" :as path]
            ["../../src/cg_vscode/module_file.mjs" :as mf]))

(def ^:private proj (path/resolve "/w/proj"))
(def ^:private cg (path/resolve "/w/cg/cg"))

(defn- on-disk [& files] #(contains? (set files) %))

(deftest the-project-file-comes-first
  (let [own (path/join proj "examples" "boston.cg")]
    (is (= own (mf/find-under [proj cg] "examples/boston.cg"
                              (on-disk own (path/join cg "examples" "boston.cg")))))))

(deftest a-path-that-the-project-lacks-comes-from-cg
  (let [lib (path/join cg "src" "cg" "parcel.cg")]
    (is (= lib (mf/find-under [proj cg] "src/cg/parcel.cg" (on-disk lib))))))

(deftest a-nil-dir-is-skipped-and-no-file-gives-nil
  (is (nil? (mf/find-under [nil cg] "lib/roads.cg" (on-disk))))
  (is (= (path/join proj "lib" "roads.cg")
         (mf/find-under [nil proj] "lib/roads.cg" (on-disk (path/join proj "lib" "roads.cg"))))))

(.then (js/Promise.resolve (t/run-tests)) #(js/process.exit (if (t/successful? %) 0 1)))
