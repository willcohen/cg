;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.op-defn-test
  "A defn with ^:op on its name is an operation in each .cg file. The
   parser reads the op data from the metadata: the name
   flags, the attr-map, and a role flag or a type map on each param. A rung
   must name an op, and a row op must return a value.

   Run with: clojure -M:test-parser"
  (:require [cg.parser.core :as parser]
            [clojure.test :refer [deftest is testing]]))

(set! *warn-on-reflection* true)

(defn- diagnostic-codes [flow]
  (into #{} (map :code) (:diagnostics flow)))

(defn- op-named [flow op-name]
  (first (filter #(= op-name (:name %)) (:operations flow))))

(defn- inputs [op]
  (mapv (juxt :name :type :role :optional) (:inputs op)))

(def ^:private tag-src
  (str "(defn ^:op ^:idempotent tag\n"
       "  \"Tags a row.\"\n"
       "  {:row-reads #{:geometry} :row-writes #{:tag}\n"
       "   :outputs {:row {:type :map}}}\n"
       "  [^:row row ^{:type :number} size label]\n"
       "  (assoc row :tag [size label]))\n"))

(deftest an-op-defn-is-an-operation-in-a-flow-file
  (let [flow (parser/extract-models (str "(ns city)\n" tag-src))
        op (op-named flow "tag")]
    (is (empty? (:diagnostics flow)))
    (is (= [["row" "any" :row false]
            ["size" "number" nil false]
            ["label" "any" nil false]]
           (inputs op)))
    (is (= [["row" "map"]] (mapv (juxt :name :type) (:outputs op))))
    (is (= ["geometry"] (:row-reads op)))
    (is (= ["tag"] (:row-writes op)))
    (is (= "Tags a row." (:doc op)))
    (is (every? (set (:flags op)) ["op" "idempotent" "defn"]))))

(deftest an-op-defn-in-a-module-ns-reads-its-metadata
  (let [flow (parser/extract-models (str "(ns cg.m)\n" tag-src))]
    (is (empty? (:diagnostics flow)))
    (is (= [:row nil nil] (mapv :role (:inputs (op-named flow "tag")))))))

(deftest an-op-defn-without-an-attr-map-has-one-output
  (let [flow (parser/extract-models "(ns city)\n(defn ^:op one [^:table rows] rows)\n")
        op (op-named flow "one")]
    (is (empty? (:diagnostics flow)))
    (is (= [["result" "any"]] (mapv (juxt :name :type) (:outputs op))))
    (is (nil? (:row-reads op)))))

(deftest a-table-op-body-keeps-its-pipeline
  (let [flow (parser/extract-models
              (str "(ns city)\n"
                   "(defn ^:op run-all [^:table rows d]\n"
                   "  (table-> :rows (geo/buffer d) (geo/centroid)))\n"))
        op (op-named flow "run-all")]
    (is (empty? (:diagnostics flow)))
    (is (= :pipeline (:kind (first (:body op)))))
    (is (some? (:body-graph op)))))

(deftest a-row-op-that-returns-a-model-is-an-error
  (testing "a threading form as the body"
    (is (contains? (diagnostic-codes
                    (parser/extract-models
                     "(ns city)\n(defn ^:op area [^:row p] (item-> :p (geo/area)))\n"))
                   :row-op-returns-model)))
  (testing "a threading form as the tail of a let"
    (is (contains? (diagnostic-codes
                    (parser/extract-models
                     (str "(ns city)\n"
                          "(defn ^:op area2 [^:row p] (let [d 1] (item-> :p (geo/buffer d))))\n")))
                   :row-op-returns-model)))
  (testing "a table op can return a model"
    (is (not (contains? (diagnostic-codes
                         (parser/extract-models
                          "(ns city)\n(defn ^:op t [^:table rows] (table-> :rows (geo/centroid)))\n"))
                        :row-op-returns-model)))))

(deftest a-role-param-comes-first
  (is (contains? (diagnostic-codes
                  (parser/extract-models "(ns city)\n(defn ^:op late [n ^:row row] row)\n"))
                 :op-malformed))
  (is (contains? (diagnostic-codes
                  (parser/extract-models "(ns city)\n(defn ^:op two [^:row a ^:table b] a)\n"))
                 :op-malformed)))

(deftest an-op-rung-counts-its-row-param-as-implicit
  (let [flow (parser/extract-models
              (str "(ns city)\n" tag-src
                   "(def m (table-> :rows (tag 1 \"a\")))\n"))]
    (is (empty? (:diagnostics flow)))))

(deftest a-rung-must-name-an-op
  (testing "a plain defn as a rung gets :rung-not-op, not an arity error"
    (let [codes (diagnostic-codes
                 (parser/extract-models
                  (str "(ns cg.m)\n"
                       "(defn helper [row x] row)\n"
                       "(def m (table-> :rows (helper 1)))\n")))]
      (is (contains? codes :rung-not-op))
      (is (not (contains? codes :op-arity-mismatch)))))
  (testing "a plain defn called as a fn is not a rung"
    (is (empty? (:diagnostics
                 (parser/extract-models
                  (str "(ns cg.m)\n"
                       "(defn helper [row x] row)\n"
                       "(defn caller [r] (helper r 1))\n")))))))

;; D14: an option map is the rest param, `& {:keys! [...] :keys [...]
;; :or {...}}`. 9 keys of each kind and 9 defaults: more than an array map
;; keeps in order on the JVM.
(def ^:private option-map-src
  (str "(ns cg.m)\n"
       "(defn ^:op opts-op\n"
       "  [^:table rows & {:keys! [a b c d e f g h i]\n"
       "                   :keys [j k l m n o p q r]\n"
       "                   :or {j 10 k 11 l 12 m 13 n 14 o 15 p 2.5 q \"x\" r :y}}]\n"
       "  rows)\n"))

(deftest an-op-reads-its-option-map
  (let [flow (parser/extract-models option-map-src)
        in (last (:inputs (op-named flow "opts-op")))]
    (is (empty? (:diagnostics flow)))
    (testing "the keys in source order, and the source text of each default"
      (is (= {:required ["a" "b" "c" "d" "e" "f" "g" "h" "i"]
              :optional ["j" "k" "l" "m" "n" "o" "p" "q" "r"]
              :defaults {"j" "10" "k" "11" "l" "12" "m" "13" "n" "14"
                         "o" "15" "p" "2.5" "q" "\"x\"" "r" ":y"}}
             (:options in))))
    (testing "the input keeps its name, and a required key makes it required"
      (is (= ["arg1" false] ((juxt :name :optional) in))))))

(deftest an-option-map-with-no-required-key-is-optional
  (let [flow (parser/extract-models
              (str "(ns cg.m)\n"
                   "(defn ^:op only-optional [^:row x & {:keys [b]}] x)\n"
                   "(defn ^:op two-arities ([^:row x] x) ([^:row x & {:keys! [a]}] x))\n"))]
    (is (true? (:optional (last (:inputs (op-named flow "only-optional"))))))
    (testing "an arity that takes no option map makes it optional"
      (is (true? (:optional (last (:inputs (op-named flow "two-arities")))))))))

(deftest a-rung-must-give-a-required-option-map
  (let [codes (fn [rung]
                (diagnostic-codes
                 (parser/extract-models
                  (str "(ns cg.m)\n"
                       "(defn ^:op o [^:row x & {:keys! [a] :keys [b]}] x)\n"
                       "(def m (item-> :xs " rung "))\n"))))]
    (is (contains? (codes "(o)") :op-arity-mismatch))
    (is (empty? (codes "(o {:a 1})")))
    (is (empty? (codes "(o :a 1 :b 2)")))))

(deftest only-a-map-rest-param-is-an-option-map
  (let [flow (parser/extract-models
              (str "(ns cg.m)\n"
                   "(defn ^:op more [^:row x & more] x)\n"
                   "(defn ^:op fixed [^:row x {:keys [d]}] x)\n"))]
    (is (nil? (:options (last (:inputs (op-named flow "more"))))))
    (is (nil? (:options (last (:inputs (op-named flow "fixed"))))))))
