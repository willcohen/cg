;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.load-module-cache-test
  "load-module's content-hash cache: an unchanged
   module that two files both load costs one SCI evaluation; an edited
   module re-evaluates on its next load; reset-context! forgets every
   load. The observable is a rand-seeded def — re-evaluation moves it,
   a skipped evaluation cannot."
  (:require [clojure.test :refer [deftest is testing]]
            [cg.run :as run])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(set! *warn-on-reflection* true)

(def ^:private module-v1
  "(ns test.load-cache)\n(def stamp (rand))\n")

(def ^:private module-v2
  "(ns test.load-cache)\n(def stamp (rand))\n(def marker 2)\n")

(defn- temp-module [content]
  (let [dir (str (Files/createTempDirectory "cg-load-cache" (make-array FileAttribute 0)))
        path (str dir "/mod.cg")]
    (spit path content)
    path))

(deftest load-module-evaluates-once-per-content
  (let [path (temp-module module-v1)]
    (run/reset-context!)
    (testing "an unchanged file does not re-evaluate"
      (let [r1 (run/load-module path)
            s1 (run/eval-cg "test.load-cache/stamp")
            r2 (run/load-module path)
            s2 (run/eval-cg "test.load-cache/stamp")]
        (is (= 'test.load-cache (:ns-name r1)))
        (is (= r1 r2) "a cache hit still returns the module map")
        (is (= s1 s2))))
    (testing "an edited file re-evaluates on the next load"
      (let [s1 (run/eval-cg "test.load-cache/stamp")]
        (spit path module-v2)
        (run/load-module path)
        (is (not= s1 (run/eval-cg "test.load-cache/stamp")))
        (is (= 2 (run/eval-cg "test.load-cache/marker")))))
    (testing "reset-context! forgets the load"
      (let [s1 (run/eval-cg "test.load-cache/stamp")]
        (run/reset-context!)
        (run/load-module path)
        (is (not= s1 (run/eval-cg "test.load-cache/stamp"))
            "a fresh context re-evaluates the same content")))))
