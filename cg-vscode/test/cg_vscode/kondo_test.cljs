;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.kondo-test
  "The clj-kondo diagnostics: line numbers and output reading."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../src/cg_vscode/diagnostics/kondo.mjs" :as kondo]))

(defn- finding [row col & [level]]
  #js {:type "unresolved-symbol"
       :filename "examples/x.cg"
       :row row :col col
       :end-row row :end-col (+ col 3)
       :level (or level "error")
       :message "Unresolved symbol: nope"})

(deftest line-numbers-count-from-zero
  (let [[d] (kondo/findings->diagnostic-data #js [(finding 42 5)])]
    (is (= [41 4] (:start (:range d))))
    (is (= [41 7] (:end (:range d))))))

(deftest finding-fields-map-across
  (let [[d] (kondo/findings->diagnostic-data #js [(finding 2 1)])]
    (is (= "unresolved-symbol" (:code d)))
    (is (= :error (:severity d)))
    (is (= "clj-kondo" (:source d)))
    (is (= "Unresolved symbol: nope" (:message d))))
  (let [sev-of (fn [level]
                 (:severity (first (kondo/findings->diagnostic-data
                                    #js [(finding 2 1 level)]))))]
    (is (= :warning (sev-of "warning")))
    (is (= :info (sev-of "info")))
    (is (= :error (sev-of "something-new"))
        "an unknown level reads as an error rather than vanishing")))

(deftest output-reading-survives-a-bad-run
  (let [out (js/JSON.stringify #js {:findings #js [(finding 3 2)]})
        ds (kondo/parse-output out)]
    (is (= 1 (count ds)))
    (is (= [2 1] (:start (:range (first ds))))))
  (is (= 0 (count (kondo/parse-output ""))))
  (is (= 0 (count (kondo/parse-output nil))))
  (is (= 0 (count (kondo/parse-output "clj-kondo: unrecognised option"))))
  (is (= 0 (count (kondo/parse-output (js/JSON.stringify #js {:findings #js []}))))))

(deftest command-names-the-file-and-turns-the-cache-off
  (let [args (kondo/argv "examples/boston.cg")
        after (fn [flag] (aget args (inc (.indexOf args flag))))]
    (is (= "-" (after "--lint")))
    (is (= "examples/boston.cg" (after "--filename")))
    (is (= "false" (after "--cache")))
    (is (re-find #":format :json" (after "--config")))))

(.then (js/Promise.resolve (t/run-tests)) #(js/process.exit (if (t/successful? %) 0 1)))
