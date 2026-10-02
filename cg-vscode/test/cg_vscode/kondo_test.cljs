;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.kondo-test
  "The clj-kondo diagnostics: line numbers and output reading. Imports the
   compiled .mjs directly: no vscode, and no process is started."
  (:require [cljs.test :as t :refer [deftest is testing]]
            ["../../src/cg_vscode/diagnostics/kondo.mjs" :as kondo]))

(defn- finding [row col & [level]]
  #js {:type "unresolved-symbol"
       :filename "examples/x.cg"
       :row row :col col
       :end-row row :end-col (+ col 3)
       :level (or level "error")
       :message "Unresolved symbol: nope"})

(deftest line-numbers-count-from-zero
  (testing "clj-kondo line 1 is file line 1, which VS Code counts as 0"
    (let [[d] (kondo/findings->diagnostic-data #js [(finding 1 1)])]
      (is (= [0 0] (:start (:range d))))))

  (testing "a later line"
    (let [[d] (kondo/findings->diagnostic-data #js [(finding 42 5)])]
      ;; Line 42 of the file is line 41 to VS Code. Column 5 is 4.
      (is (= [41 4] (:start (:range d))))
      (is (= [41 7] (:end (:range d)))))))

(deftest finding-fields-map-across
  (testing "type becomes the code, level becomes the severity, source is clj-kondo"
    (let [[d] (kondo/findings->diagnostic-data #js [(finding 2 1)])]
      (is (= "unresolved-symbol" (:code d)))
      (is (= :error (:severity d)))
      (is (= "clj-kondo" (:source d)))
      (is (= "Unresolved symbol: nope" (:message d)))))

  (testing "every level clj-kondo reports has a severity"
    (let [sev-of (fn [level]
                   (:severity (first (kondo/findings->diagnostic-data
                                      #js [(finding 2 1 level)]))))]
      (is (= :error (sev-of "error")))
      (is (= :warning (sev-of "warning")))
      (is (= :info (sev-of "info")))
      (is (= :error (sev-of "something-new"))
          "an unknown level reads as an error rather than vanishing"))))

(deftest output-reading-survives-a-bad-run
  (testing "the JSON clj-kondo prints becomes diagnostic data"
    (let [out (js/JSON.stringify #js {:findings #js [(finding 3 2)]})
          ds (kondo/parse-output out)]
      (is (= 1 (count ds)))
      (is (= [2 1] (:start (:range (first ds)))))))

  (testing "empty, absent and non-JSON output all read as no diagnostics"
    (is (= 0 (count (kondo/parse-output ""))))
    (is (= 0 (count (kondo/parse-output nil))))
    (is (= 0 (count (kondo/parse-output "clj-kondo: unrecognised option")))))

  (testing "JSON with no findings reads as no diagnostics"
    (is (= 0 (count (kondo/parse-output (js/JSON.stringify #js {:findings #js []})))))))

(deftest command-names-the-file-and-turns-the-cache-off
  (let [args (kondo/argv "examples/boston.cg")
        after (fn [flag] (aget args (inc (.indexOf args flag))))]
    (testing "the buffer arrives on standard input under the file's own name"
      ;; clj-kondo reads no .cg file from a directory scan; it does read
      ;; standard input under any name.
      (is (= "-" (after "--lint")))
      (is (= "examples/boston.cg" (after "--filename"))))

    (testing "the cache is off, so one .cg program cannot resolve another's names"
      (is (= "false" (after "--cache"))))

    (testing "the config asks for JSON"
      (is (re-find #":format :json" (after "--config"))))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err (or (get results "error") 0)]
           (js/process.exit (if (pos? (+ fail err)) 1 0)))))
