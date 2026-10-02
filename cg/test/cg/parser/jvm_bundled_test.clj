;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.jvm-bundled-test
  "The parser files that bb jar puts on the classpath. JVM-only."
  (:require [cg.parser.jvm]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.net URLClassLoader URL]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private files
  {"node_modules/web-tree-sitter/web-tree-sitter.cjs" "web-tree-sitter.cjs"
   "node_modules/web-tree-sitter/web-tree-sitter.wasm" "web-tree-sitter.wasm"
   "node_modules/@yogthos/tree-sitter-clojure/tree-sitter-clojure.wasm" "tree-sitter-clojure.wasm"})

(defn- bytes-of [f]
  (Files/readAllBytes (.toPath (io/file f))))

(defn- with-context-classpath
  "Call `f` with the folder `dir` on the context class loader, as the jar
   puts cg/parser/tree-sitter/ on the classpath."
  [dir f]
  (let [t (Thread/currentThread)
        old (.getContextClassLoader t)]
    (.setContextClassLoader t (URLClassLoader. (into-array URL [(.toURL (.toURI (io/file dir)))]) old))
    (try (f) (finally (.setContextClassLoader t old)))))

(deftest unpack-bundled-test
  (testing "the repo classpath has no bundled files"
    (is (nil? (#'cg.parser.jvm/unpack-bundled))))
  (testing "the bundled files go to a temp folder in the node_modules layout"
    (let [dir (.toFile (Files/createTempDirectory "cg-bundled-test" (make-array FileAttribute 0)))]
      (try
        (doseq [[src jar-name] files
                :let [target (io/file dir "cg/parser/tree-sitter" jar-name)]]
          (io/make-parents target)
          (io/copy (io/file src) target))
        (let [root (with-context-classpath dir #(#'cg.parser.jvm/unpack-bundled))]
          (is (some? root))
          (doseq [src (keys files)]
            (is (java.util.Arrays/equals ^bytes (bytes-of src) ^bytes (bytes-of (io/file root src)))
                src)))
        (finally
          (run! #(io/delete-file % true) (reverse (file-seq dir))))))))
