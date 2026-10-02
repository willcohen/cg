;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.run.op-registry-test
  "A defn with ^:op on its name is a flow op. On the JVM, cg.run/eval-cg
   reads the op data from the metadata of the SCI var and registers the op
   before the next top-level form expands. The arglist gives the param
   order, a role flag on a param gives its role, and the attr-map gives the
   row reads and writes."
  (:require [clojure.test :refer [deftest is testing]]
            [cg.run :as run]
            [cg.feature :as feature]
            [cg.model :as model]))

(set! *warn-on-reflection* true)

(def ^:private prelude
  "(require '[cg.macros :refer [item->]])")

(defn- op-info [qualified-name]
  (feature/get-operation-info qualified-name))

(defn- op-params [m]
  (mapv :params (filter #(= :operation (:type %)) (:nodes m))))

(deftest an-op-defn-registers-its-schema
  (run/eval-cg "(defn ^:op reg-tag
                  {:row-reads #{:geometry} :row-writes #{:tag}}
                  [^:row row ^{:type :number} size label]
                  (assoc row :tag [size label]))")
  (testing "the schema has the params after the row param, in arglist order"
    (is (= {:type :transformer
            :row-shape :per-row
            :reads #{:geometry}
            :writes #{:tag}
            :params [{:name :size :type :number} {:name :label}]}
           (op-info "user/reg-tag"))))
  (testing "the fn resolves under the qualified name and the bare name"
    (is (= {:tag [1 "a"]} ((run/resolve-operation "user/reg-tag") {} 1 "a")))
    (is (= "user" (feature/get-bare-op-namespace "reg-tag")))))

(deftest the-decl-has-the-parser-shape
  (run/eval-cg "(defn ^:op ^:idempotent reg-decl
                  {:row-reads #{:geometry}}
                  [^:row row ^{:type :number} size]
                  row)")
  (is (= {:name "reg-decl"
          :inputs [{:name "row" :type "any" :role :row :optional false}
                   {:name "size" :type "number" :role nil :optional false}]
          :outputs [{:name "result" :type "any"}]
          :row-reads ["geometry"]
          :row-writes nil}
         (dissoc (run/var-op-decl (run/eval-cg "#'user/reg-decl")) :flags)))
  (is (= #{"op" "idempotent"}
         (set (:flags (run/var-op-decl (run/eval-cg "#'user/reg-decl")))))))

(deftest a-defn-without-op-is-not-an-op
  (run/eval-cg "(defn reg-plain [x] x)")
  (is (nil? (op-info "user/reg-plain"))))

(deftest a-table-op-keeps-its-table-param
  (run/eval-cg "(defn ^:op reg-whole [^:table rows ^{:type :component} step n] rows)")
  (is (= {:type :transformer
          :row-shape :whole-dataset
          :reads #{}
          :writes #{}
          :params [{:name :rows :role :table}
                   {:name :step :type :component}
                   {:name :n}]}
         (op-info "user/reg-whole"))))

(deftest a-rung-in-the-same-eval-gets-its-params-in-arglist-order
  (testing "9 params, more than an array map keeps in order"
    (let [m (run/eval-cg (str prelude
                              " (defn ^:op reg-nine [^:row x a b c d e f g h i]
                                  [x a b c d e f g h i])
                                (item-> :xs (reg-nine 1 2 3 4 5 6 7 8 9))"))]
      (is (model/model? m))
      (is (= [(zipmap [:a :b :c :d :e :f :g :h :i] (range 1 10))] (op-params m)))
      (is (= [[0 1 2 3 4 5 6 7 8 9]] (vec (run/run-model m {:xs [0]} {})))))))

(deftest the-forms-of-a-top-level-do-are-top-level-forms
  (let [m (run/eval-cg (str prelude
                            " (do (defn ^:op reg-in-do [^:row x a b] [x a b])
                                  (item-> :xs (reg-in-do 1 2)))"))]
    (is (= [{:a 1 :b 2}] (op-params m)))))

(deftest a-module-op-registers-under-its-namespace
  (run/eval-cg "(ns test.op-registry-mod) (defn ^:op reg-mod [^:row x a] [x a])")
  (is (= [{:name :a}] (:params (op-info "test.op-registry-mod/reg-mod"))))
  (testing "the eval does not change the namespace of the next eval"
    (is (= "user" (run/eval-cg "(str *ns*)")))))

(deftest the-widest-arity-gives-the-params
  (run/eval-cg "(defn ^:op reg-arity
                  ([^:row x a] (reg-arity x a 1))
                  ([^:row x a b] [x a b]))")
  (is (= [{:name :a} {:name :b}] (:params (op-info "user/reg-arity"))))
  (is (= [false false true]
         (mapv :optional (:inputs (run/var-op-decl (run/eval-cg "#'user/reg-arity")))))))

(deftest a-role-param-comes-first
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"reg-late.*first param"
                        (run/eval-cg "(defn ^:op reg-late [n ^:row row] row)")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"reg-two.*one role param"
                        (run/eval-cg "(defn ^:op reg-two [^:row a ^:table b] a)"))))

(deftest an-op-reads-its-option-map
  (testing "9 keys of each kind and 9 defaults, more than an array map keeps in order"
    (run/eval-cg "(defn ^:op reg-opts
                    [^:table rows & {:keys! [a b c d e f g h i]
                                     :keys [j k l m n o p q r]
                                     :or {j 10 k 11 l 12 m 13 n 14 o 15 p 2.5 q \"x\" r :y}}]
                    rows)")
    (is (= {:required ["a" "b" "c" "d" "e" "f" "g" "h" "i"]
            :optional ["j" "k" "l" "m" "n" "o" "p" "q" "r"]
            :defaults {"j" "10" "k" "11" "l" "12" "m" "13" "n" "14"
                       "o" "15" "p" "2.5" "q" "\"x\"" "r" ":y"}}
           (:options (last (:inputs (run/var-op-decl (run/eval-cg "#'user/reg-opts")))))))
    (is (false? (:optional (last (:inputs (run/var-op-decl (run/eval-cg "#'user/reg-opts"))))))
        "a required key makes the option map a required input"))
  (testing "an option map with no required key is optional"
    (run/eval-cg "(defn ^:op reg-only-optional [^:row x & {:keys [b]}] x)")
    (is (true? (:optional (last (:inputs (run/var-op-decl (run/eval-cg "#'user/reg-only-optional"))))))))
  (testing "a rest param that is not a map has no options"
    (run/eval-cg "(defn ^:op reg-more [^:row x & more] x)")
    (is (nil? (:options (last (:inputs (run/var-op-decl (run/eval-cg "#'user/reg-more")))))))))
