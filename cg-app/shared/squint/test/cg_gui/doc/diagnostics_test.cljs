;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.doc.diagnostics-test
  "Pure CgFlow -> diagnostic-data conversion. Imports the
   compiled data .mjs directly. With no vscode coupling, it runs standalone."
  (:require [cljs.test :as t :refer [deftest is testing]]
            ["../../../src/cg_gui/doc/diagnostics.mjs" :as data]))

(deftest cg-flow->diagnostic-data-conversion
  (testing "1-indexed source spans become 0-indexed range pairs"
    (let [cg-flow {:diagnostics
                   [{:code :forbidden-form
                     :severity :error
                     :message "(defn ...) is not allowed in a flow file"
                     :location {:start-line 7 :start-column 1
                                :end-line 7 :end-column 32}}]}
          ds (data/cg-flow->diagnostic-data cg-flow)]
      (is (= 1 (count ds)))
      (let [d (first ds)]
        (is (= "forbidden-form" (:code d)))
        (is (= :info (:severity d))
            "the parser calls it an error; the editor grades it down (see below)")
        (is (= "(defn ...) is not allowed in a flow file" (:message d)))
        ;; there is no plain "cg" source
        (is (= "cg-parser" (:source d)))
        (let [r (:range d)]
          (is (= [6 0] (:start r)))
          (is (= [6 31] (:end r)))))))

  (testing "a form the canvas cannot draw is information, not an error"
    ;; The parser calls these errors because it checks against a closed
    ;; vocabulary. The file runs.
    (let [sev (fn [code]
                (:severity (first (data/cg-flow->diagnostic-data
                                   {:diagnostics
                                    [{:code code :severity :error :message "m"
                                      :location {:start-line 1 :start-column 1
                                                 :end-line 1 :end-column 1}}]}))))]
      (is (= :info (sev :forbidden-form)))
      (is (= :info (sev :unclassified-top-level-form)))
      (is (= :warning (sev :unresolved-op))
          "a name the parser cannot resolve is worth a mark, not a red one")
      (is (= :warning (sev :model-inside-defn)))
      (is (= :error (sev :defop-malformed))
          "a malformed defop really is an error")
      (is (= :error (sev :pipeline-type-mismatch)))
      (is (= :error (sev :single-overlay)))))

  (testing "severity keywords pass through (VSCode enum mapping at boundary)"
    (let [mk (fn [sev] {:diagnostics [{:code :x :severity sev :message "m"
                                       :location {:start-line 1 :start-column 1
                                                  :end-line 1 :end-column 1}}]})
          sev-of (fn [d] (:severity (first (data/cg-flow->diagnostic-data d))))]
      (is (= :error   (sev-of (mk :error))))
      (is (= :warning (sev-of (mk :warning))))
      (is (= :info    (sev-of (mk :info)))))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
