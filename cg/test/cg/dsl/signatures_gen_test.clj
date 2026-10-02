;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.dsl.signatures-gen-test
  "JVM-only staleness guard for the generated parser built-ins. Loads the
   gen-signatures script, rebuilds the snapshot from the live op registry,
   and asserts it matches the committed signatures_generated.cljc. Fails
   when someone edits a register-operation! parser slice without running
   `bb gen:signatures`."
  (:require [clojure.test :refer [deftest is]]
            [cg.dsl.signatures-generated :as gen]))

(set! *warn-on-reflection* true)

(deftest generated-built-ins-not-stale
  (load-file "scripts/gen_signatures.clj")
  (let [fresh ((resolve 'gen-signatures/build-built-ins))]
    (is (= fresh gen/built-ins)
        "src/cg/dsl/signatures_generated.cljc is stale — run `bb gen:signatures`")))
