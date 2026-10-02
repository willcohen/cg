;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.trace-file-test
  "The JVM trace file writer writes only into a folder that the cg.trace.dir
   system property or the CG_TRACE_DIR environment variable names. The event
   stream itself stays on: see trace_test."
  (:require [clojure.test :refer [deftest is]]
            [cg.trace :as trace]
            [clojure.java.io :as io])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(set! *warn-on-reflection* true)

(defn- with-trace-dir-property
  "Call f with the cg.trace.dir system property set to v (nil clears it), then
   put the old value back."
  [v f]
  (let [old (System/getProperty "cg.trace.dir")]
    (try
      (if v
        (System/setProperty "cg.trace.dir" v)
        (System/clearProperty "cg.trace.dir"))
      (f)
      (finally
        (if old
          (System/setProperty "cg.trace.dir" old)
          (System/clearProperty "cg.trace.dir"))))))

(defn- file-names [dir]
  (set (.list (io/file dir))))

(deftest no-folder-named-writes-no-file
  (when-not (System/getenv "CG_TRACE_DIR")
    (with-trace-dir-property nil
      (fn []
        (let [before  (file-names "tmp/cg-traces")
              session (trace/open-file-writer! "trace-file-test")]
          (trace/close-file-writer! session)
          (is (nil? (:path session)))
          (is (nil? (trace/traces-dir)))
          (is (= before (file-names "tmp/cg-traces"))))))))

(deftest the-property-names-the-folder
  (let [dir (.toFile (Files/createTempDirectory "cg-trace-file-test"
                                                (make-array FileAttribute 0)))]
    (with-trace-dir-property (.getPath dir)
      (fn []
        (let [session (trace/open-file-writer! "trace-file-test")]
          (trace/close-file-writer! session)
          (is (some? (:path session)))
          (is (= (.getCanonicalPath dir)
                 (.getCanonicalPath (.toFile (.getParent ^java.nio.file.Path (:path session))))))
          (is (contains? (file-names dir) "latest.jsonl")))))))
