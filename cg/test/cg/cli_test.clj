;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.cli-test
  (:require [clojure.test :refer [deftest is]]
            [cg.cli :as cli]))

(set! *warn-on-reflection* true)

(deftest run-model-file-runs-a-model-of-a-file-with-an-ns-form
  ;; The defs of such a file are in its namespace, and run-file runs after
  ;; the eval, in the user namespace.
  (let [f (java.io.File/createTempFile "ns-probe" ".cg")]
    (try
      (spit f (str "(ns cli-probe.entry\n"
                   "  (:require [cg.feature :as f] [cg.geo :as geo]\n"
                   "            [cg.macros :refer [item->]]))\n"
                   "(def pts [(f/point 0 0)])\n"
                   "(def centers (item-> :pts (geo/centroid)))\n"))
      (is (= 0 (cli/run-model-file (.getPath f))))
      (finally (.delete f)))))

(deftest run-model-file-fails-without-a-model
  (let [f (java.io.File/createTempFile "no-model" ".cg")]
    (try
      (spit f "(def x 1)")
      (is (= 1 (cli/run-model-file (.getPath f))))
      (finally (.delete f)))))
