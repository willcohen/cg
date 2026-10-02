;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.util-fs-test
  "The cross-platform file primitives that the .cg modules use: make-dirs!,
   list-file-names, delete-file!, file-exists?, read-file, write-file!,
   read-edn and write-edn!.

   The cljs suite loads cg.util as an ES module and sets no
   globalThis.nodeFs, which is how an npm consumer loads it. An ES module has
   no require, so cg.util must find node:fs another way."
  #?(:clj (:require [clojure.test :refer [deftest is]]
                    [cg.util :as util])
     :cljs (:require [cljs.test :refer [deftest is]]
                     ["../../src/cg/util.mjs" :as util]
                     ["node:fs" :as fs]
                     ["node:os" :as os]
                     ["node:path" :as node-path]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]]))
  #?(:clj (:import (java.nio.file Files)
                   (java.nio.file.attribute FileAttribute))))

#?(:clj (set! *warn-on-reflection* true))

(defn- temp-dir []
  #?(:clj (str (Files/createTempDirectory "cg-util-fs" (make-array FileAttribute 0)))
     :cljs (fs/mkdtempSync (node-path/join (os/tmpdir) "cg-util-fs"))))

(defn- write-text! [path text]
  #?(:clj (spit path text)
     :cljs (fs/writeFileSync path text)))

(deftest make-dirs-creates-nested-directories
  (let [root (temp-dir)
        nested (str root "/a/b/c")]
    (is (false? (util/file-exists? nested)))
    (is (true? (util/make-dirs! nested)))
    (is (true? (util/file-exists? nested)))
    (is (false? (util/make-dirs! nested)) "already present: no new directory")))

(deftest list-file-names-gives-names-not-paths
  (let [root (temp-dir)]
    (write-text! (str root "/one.parquet") "x")
    (write-text! (str root "/two.parquet") "y")
    (is (= ["one.parquet" "two.parquet"] (vec (sort (util/list-file-names root)))))))

(deftest list-file-names-on-a-missing-directory-is-empty
  (is (= [] (util/list-file-names "/nonexistent/cg-util-fs-test"))))

(deftest delete-file-removes-and-reports
  (let [root (temp-dir)
        f (str root "/gone.txt")]
    (write-text! f "x")
    (is (true? (util/delete-file! f)))
    (is (false? (util/file-exists? f)))
    (is (false? (util/delete-file! f)) "second delete: nothing to remove")))

(deftest write-file-writes-what-read-file-reads
  (let [f (str (temp-dir) "/notes.md")]
    (util/write-file! f "one\ntwo")
    (is (= "one\ntwo" (util/read-file f)))
    (util/write-file! f "three")
    (is (= "three" (util/read-file f)) "a second write replaces the file")))

(deftest write-edn-writes-what-read-edn-reads
  (let [f (str (temp-dir) "/kinds.edn")]
    (util/write-edn! f {:keyword-cols #{"a" "b"} :n 3 :v ["x" nil true]})
    (let [v (util/read-edn f)]
      (is (contains? (:keyword-cols v) "a"))
      (is (= 2 (count (:keyword-cols v))))
      (is (= 3 (:n v)))
      (is (= "x" (first (:v v))))
      (is (nil? (second (:v v))))
      (is (true? (nth (:v v) 2))))))

;; A cache sidecar that the JVM wrote must read on the JS runtime too.
(deftest read-edn-reads-the-edn-that-the-jvm-writes
  (let [f (str (temp-dir) "/sidecar.edn")]
    (util/write-file! f "{:keyword-cols #{\"lot-type\"} :three-valued-cols #{} :crs \"EPSG:2249\"}")
    (let [v (util/read-edn f)]
      (is (contains? (:keyword-cols v) "lot-type"))
      (is (zero? (count (:three-valued-cols v))))
      (is (= "EPSG:2249" (:crs v))))))

#?(:cljs (run_tests_and_exit_BANG_ "cg.util-fs-test"))
