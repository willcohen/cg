;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.cli-file-test
  "cli/run-model-file gives the name of the file to the run on both runtimes,
   because a .cg file reads it with run/script-file. boston.cg runs its
   analysis only when run/script-file names it."
  #?(:clj (:require [clojure.test :refer [deftest is]]
                    [cg.cli :as cli]
                    [cg.run :as run])
     :cljs (:require [cljs.test :refer [deftest is]]
                     ["node:fs" :as fs]
                     ["node:os" :as os]
                     ["node:path" :as node-path]
                     ["../../src/cg/cli.mjs" :as cli]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]]))
  #?(:clj (:import (java.nio.file Files)
                   (java.nio.file.attribute FileAttribute))))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private checked-name "script-file-check.cg")

(def ^:private checked-text
  (str "(require '[cg.run :as run])
(require '[cg.feature :as f])
(require '[cg.geo :as geo])
(require '[cg.macros :refer [item->]])
(def named (when-not (= \"" checked-name "\" (run/script-file))
             (throw (ex-info \"run/script-file does not name the file\" {}))))
(def pts [(f/point 0 0)])
(def buffered (item-> :pts (geo/buffer 1)))"))

(defn- checked-file
  "Write the check to a new folder, and give its path."
  []
  #?(:clj (let [dir (Files/createTempDirectory "cg-cli-file-test" (make-array FileAttribute 0))
                f (.toFile (.resolve dir ^String checked-name))]
            (spit f checked-text)
            (.getPath f))
     :cljs (let [dir (fs/mkdtempSync (node-path/join (os/tmpdir) "cg-cli-file-test-"))
                 f (node-path/join dir checked-name)]
             (fs/writeFileSync f checked-text)
             f)))

#?(:cljs
   (defn ^:async run-checked! []
     (is (= 0 (await (cli/run-model-file (checked-file)))))))

(deftest run-model-file-names-the-file-for-the-run
  #?(:clj (do (is (= 0 (cli/run-model-file (checked-file))))
              (is (nil? (run/script-file)) "nil outside a run"))
     :cljs (run-checked!)))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.cli-file-test")))))
