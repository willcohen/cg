;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.run.op-parity-test
  "The parser and cg.run read the same op data from a defn with ^:op. The
   parser reads it from the text. On the JVM, cg.run reads it from the SCI
   var. Because the JS runtime registers ops from the parser decl, the two must
   agree. This test reads each op of a module both ways and compares the
   results."
  (:require [clojure.test :refer [deftest is testing]]
            [cg.parser.core :as parser]
            [cg.run :as run]))

(set! *warn-on-reflection* true)

(defn- comparable
  "The part of a decl that both readers give. Row keys compare as sets: the
   parser keeps their source order, and a set in var metadata has none."
  [decl]
  {:name (:name decl)
   :inputs (mapv #(select-keys % [:name :type :role :optional :options]) (:inputs decl))
   :outputs (mapv #(select-keys % [:name :type]) (:outputs decl))
   :row-reads (some-> (:row-reads decl) set)
   :row-writes (some-> (:row-writes decl) set)
   :op? (boolean (some #{"op"} (:flags decl)))})

(defn- both-decls
  "[op-name var-decl parser-decl] for each ^:op var of the module at
   `path`, whose namespace is `ns-name`, and the parser diagnostics."
  [path ns-name]
  (run/load-module path)
  (let [flow (parser/extract-models (slurp path))
        parsed (into {} (map (juxt :name comparable)) (:operations flow))
        vars (run/eval-cg (str "(->> (ns-interns '" ns-name ") vals"
                               " (filter (comp :op meta)) (sort-by (comp str symbol)) vec)"))]
    {:diagnostics (:diagnostics flow)
     :ops (mapv (fn [v]
                  (let [d (comparable (run/var-op-decl v))]
                    [(:name d) d (get parsed (:name d))]))
                vars)}))

(deftest the-parser-and-the-var-give-the-same-decl
  (let [{:keys [diagnostics ops]} (both-decls "test/fixtures/op-parity.cg" "test.op-parity")]
    (is (empty? diagnostics))
    (testing "each op shape of the fixture"
      (is (= #{"tag" "whole" "arity" "with-opts" "many-opts" "destructured" "private-op"}
             (set (map first ops)))))
    (doseq [[op-name from-var from-text] ops]
      (is (= from-var from-text) op-name))))

(deftest the-modules-give-the-same-decls
  (doseq [[path ns-name] [["src/cg/cache.cg" "cg.cache"]
                          ["src/cg/zoning.cg" "cg.zoning"]
                          ["src/cg/parcel.cg" "cg.parcel"]
                          ["examples/boston.cg" "boston"]]]
    (testing path
      (let [{:keys [ops]} (both-decls path ns-name)]
        (is (seq ops))
        (doseq [[op-name from-var from-text] ops]
          (is (= from-var from-text) op-name))))))
