;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.macros-test
  (:require [clojure.test :refer [deftest is testing]]
            cg.dataset
            cg.run
            [cg.macros :as macros]))

(set! *warn-on-reflection* true)

;; table-> records the level shift as node metadata with the parser's keys
;; (:emits-level, :component, :operand-level). :component is the op name as
;; a string, as the parser gives it.
(deftest table-macro-tracks-group-unit-shift
  (let [m (macroexpand-1 '(cg.macros/table-> :rows
                            (group-by :zone) (map analyze) (into {})))
        model (eval m)
        node-of (fn [f] (first (filter #(= f (some-> (:fn %) name)) (:nodes model))))]
    (testing "group-by node marks the unit shift to :groups"
      (is (= :groups (:emits-level (node-of "group-by"))))
      (is (= :dataset (:operand-level (node-of "group-by")))))
    (testing "map node carries its component name and runs at group granularity"
      (is (= "analyze" (:component (node-of "map"))))
      (is (= :groups (:operand-level (node-of "map"))))
      (is (= :groups (:emits-level (node-of "map")))))
    (testing "into demotes back to a keyed map"
      (is (= :map (:emits-level (node-of "into"))))
      (is (= :groups (:operand-level (node-of "into")))))))

(deftest resolve-alias-reads-registered-and-clojure-aliases
  (try
    (testing "an alias that a require in a .cg module registered"
      (macros/register-alias! 'macros-test-registered 'cg.geo)
      (is (= 'cg.geo (macros/resolve-alias 'macros-test-registered))))
    (testing "an alias of the Clojure namespace in *ns*"
      (binding [*ns* (the-ns 'cg.macros-test)]
        (is (= 'cg.macros (macros/resolve-alias 'macros)))))
    (testing "an unknown symbol comes back as itself"
      (is (= 'macros-test-unknown (macros/resolve-alias 'macros-test-unknown))))
    (finally
      (swap! @#'cg.macros/namespace-aliases dissoc 'macros-test-registered))))
