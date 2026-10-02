;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parcel-lot-type-smoke-test
  "Runs test/fixtures/parcel-lot-type-smoke.cg: the checks of the lot-type
   rules (corner, through, mid-block, end-cap, frontage groups, the
   pointed-lot Rear) on synthetic edges. The program throws when a check
   fails and prints one PASS or FAIL line for each check."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [cg.run :as run]))

(set! *warn-on-reflection* true)

(deftest parcel-lot-type-smoke
  (let [out     (with-out-str
                  (run/eval-cg-file "test/fixtures/parcel-lot-type-smoke.cg"))
        summary (last (str/split-lines out))]
    (is (re-find #"parcel lot-type smoke: \d+ passed, 0 failed" summary) summary)
    (is (not (str/includes? out "FAIL -")))
    (is (<= 80 (count (re-seq #"(?m)^PASS - " out)))
        "the program still runs every check")))
