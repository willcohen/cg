;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.types-test
  "Point tests for the cg.parser.types static checker."
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [cg.dsl.signatures :as sigs]
                    [cg.parser.core :as parser]
                    [cg.parser.setup :as setup]
                    [cg.util :as util])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../../src/cg/dsl/signatures.mjs" :as sigs]
                     ["../../../src/cg/parser/core.mjs" :as parser]
                     ["./setup.mjs" :as setup]
                     ["../../../src/cg/util.mjs" :as util]
                     ["../test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(defn- diag-codes [flow]
  (into #{} (map :code) (:diagnostics flow)))

(defn- count-code [flow code]
  (count (filter #(= code (:code %)) (:diagnostics flow))))

(deftest a-fully-qualified-built-in-head-resolves
  (testing "cg.dataset/transform-crs is ds/transform-crs (a module can
            write the full name: the threading macro resolves an alias
            only through a top-level require)"
    (let [flow (setup/extract-models
                "(def m (table-> :input (cg.dataset/transform-crs \"EPSG:3857\" \"EPSG:4326\")))")]
      (is (zero? (count-code flow :unresolved-op)))
      (is (zero? (count-code flow :op-arity-mismatch))))))

(deftest a-merge-of-maps-resolves
  (testing "(merge ...) combines spec maps"
    (let [flow (setup/extract-models
                "(def a {:x 1})\n(def b (merge {:y 2} a))\n(def c (merge {:y 2} a {:z 3}))")]
      (is (zero? (count-code flow :unresolved-op)))
      (is (zero? (count-code flow :op-arity-mismatch))))))

(deftest pass-2-threading-fills-a-table-input
  (testing "in a table-> rung the threaded table fills a :role :table input,
            as the rung macro does (cg.cache/checkpoint, for example)"
    (let [src  (str "(defn ^:op my-read [^:table rows ^{:type :map} spec]\n"
                    "  (count rows))\n"
                    "(def m (table-> :rows (my-read {:a 1})))")
          flow (setup/extract-models src)]
      (is (zero? (count-code flow :op-arity-mismatch))))))

(deftest a-rung-fills-the-first-input-of-a-user-op
  ;; cg.run gives the threaded value to the first param of an op, also when
  ;; no param has a role.
  (let [op "(defn ^:op k2 [n] n)\n"]
    (is (zero? (count-code (setup/extract-models (str op "(def m (table-> :ds (k2)))"))
                           :op-arity-mismatch)))
    (is (= 1 (count-code (setup/extract-models (str op "(def m (table-> :ds (k2 5)))"))
                         :op-arity-mismatch)))))

(deftest a-module-defn-call-resolves
  (testing "a call to a ^:module defn resolves; & more takes any count"
    (let [src  (str "(ns ^:module cg.m)\n"
                    "(defn f [a & more] a)\n"
                    "(def x (f 1 2 3 4))\n")
          flow (setup/extract-models src)]
      (is (zero? (count-code flow :unresolved-op)))
      (is (zero? (count-code flow :op-arity-mismatch))))
    (is (pos? (count-code (setup/extract-models
                           "(ns ^:module cg.m)\n(defn f [a & more] a)\n(def x (f))\n")
                          :op-arity-mismatch))
        "a required param is still required")))

(deftest pass-2-arity
  ;; geo/buffer has a :row input and two others. A call binding gives all
  ;; three, where a rung gives only the other two.
  (doseq [[n src] [[1 "(def m (item-> :parcels (geo/buffer)))"]
                   [1 "(def m (item-> :parcels (geo/buffer 1 2 3)))"]
                   [0 "(def b (geo/buffer g 10 {}))"]]]
    (is (= n (count-code (setup/extract-models src) :op-arity-mismatch)) src)))

(deftest pass-3-arg-types
  (doseq [[n src] [[1 "(def m (item-> :parcels (geo/buffer \"ten\")))"]
                   [1 "(def p (ds/load-geojson 42))"]
                   [1 "(def label \"abc\")\n(def m (item-> :parcels (geo/buffer label)))"]
                   ;; A symbol with a namespace is any.
                   [0 "(def m (item-> :parcels (geo/buffer other/foo)))"]
                   ;; The input of ds/->dataset is any.
                   [0 "(def x (ds/->dataset \"not-a-map\"))"]]]
    (is (= n (count-code (setup/extract-models src) :type-mismatch)) src)))

(deftest pass-3-a-binding-arg-has-the-output-type-of-the-binding
  (let [flow (setup/extract-models
              (str "(def area-metrics (ds/load-csv \"a.csv\"))\n"
                   "(def m (item-> :parcels (geo/buffer area-metrics)))"))]
    (is (= ["operation 'geo/buffer' argument 'distance' expects type number but got dataset"]
           (mapv :message (filter #(= :type-mismatch (:code %)) (:diagnostics flow)))))))

(deftest sig-type-kind-static-lookup
  (is (= :boolean (sigs/type-kind "boolean")))
  (is (= :boolean (sigs/type-kind "bool")))
  (is (= :any     (sigs/type-kind "ridiculous-unknown-type"))))

(deftest sig-from-operation-decl-projection
  (let [decl {:name "my-op"
              :inputs [{:name "parcel" :type "feature" :role :row :optional false}
                       {:name "k"      :type "number"  :role nil  :optional false}]
              :outputs [{:name "n" :type "number"}]
              :flags ["idempotent"]
              :location {:start [1 1] :end [1 10]}}
        sig (sigs/from-operation-decl decl)]
    (is (= "my-op" (:qualified-name sig)))
    (is (= :feature (-> sig :inputs first :type)))
    (is (= :row (-> sig :inputs first :role)))
    (is (= :number (-> sig :inputs second :type)))
    (is (= :number (-> sig :outputs first :type)))
    (is (= #{:idempotent} (:flags sig)))
    (is (= :user (:source sig)))))

(deftest sig-merge-signatures-overlay
  (let [base {"a" {:qualified-name "a" :source :built-in}}
        extra {"a" {:qualified-name "a" :source :user}
               "b" {:qualified-name "b" :source :user}}
        merged (sigs/merge-signatures base extra)]
    (is (= :user (-> merged (get "a") :source)))
    (is (contains? merged "b"))))

(deftest typed-clean-fixture
  (let [flow (setup/extract-models (util/read-file "test/fixtures/typed-clean.cg"))]
    (is (empty? (:diagnostics flow)))))

(deftest typed-mismatch-fixture
  (let [flow (setup/extract-models (util/read-file "test/fixtures/typed-mismatch.cg"))]
    (is (pos? (count-code flow :type-mismatch)))
    (is (pos? (count-code flow :op-arity-mismatch)))
    (is (pos? (count-code flow :unresolved-op)))
    (is (pos? (count-code flow :pipeline-type-mismatch))
        "ds/transform-crs feeding geo/buffer must fire pipeline-type-mismatch")
    (is (pos? (count-code flow :op-output-mismatch))
        "an op declaring :number output but body terminal :feature must fire op-output-mismatch")
    (is (pos? (count-code flow :unknown-predicate))
        "a filter-by-column predicate absent from the registry must fire unknown-predicate")))

(deftest pass-4-pipeline-flow
  ;; A binding, an op body and a sink. The first two give a :dataset to the
  ;; :feature row input of geo/buffer, the sink a :feature to a :dataset one.
  (doseq [src ["(def bad (table-> :ds (ds/transform-crs \"EPSG:3857\") (geo/buffer 10)))"
               "(defn ^:op bad [^:table ds] (table-> :ds (ds/transform-crs \"EPSG:3857\") (geo/buffer 10)))"
               "(table-> :ds (geo/buffer 10) (ds/write-geojson \"out.geojson\"))"]]
    (is (= 1 (count-code (setup/extract-models src) :pipeline-type-mismatch)) src)))

(deftest pass-5-mismatch-fires
  (testing "body returns :feature but output declared :number — op-output-mismatch"
    (let [flow (setup/extract-models
                (str "(defn ^:op bad {:outputs {:n {:type :number}}} [^:table parcel]\n"
                     "  (item-> :parcel (geo/centroid)))"))]
      (is (contains? (diag-codes flow) :op-output-mismatch)))))

(deftest pass-5-multi-output-op-skipped
  (testing "multi-output ops are not pass-5 checked (column-mapping convention TBD)"
    (let [flow (setup/extract-models
                (str "(defn ^:op multi\n"
                     "  {:outputs {:area {:type :number}\n"
                     "             :error {:type :string :optional true}}}\n"
                     "  [^:table parcel]\n"
                     "  (item-> :parcel (geo/centroid)))"))]
      (is (empty? (:diagnostics flow))))))

(deftest pass-1-walks-op-body-ops
  (let [flow (setup/extract-models
              (str "(defn ^:op bad [^:table parcel]\n"
                   "  (item-> :parcel (geo/totally-bogus)))"))]
    (is (contains? (diag-codes flow) :unresolved-op))))

(deftest sink-ops-walked-by-pass-1-3
  (let [flow (setup/extract-models
              (str "(table-> :ds\n"
                   "  (geo/nope 1)\n"
                   "  (ds/write-geojson \"out.geojson\"))"))]
    (is (= 1 (count (:sinks flow))))
    (is (= 1 (count-code flow :unresolved-op)))))

(deftest unsinked-top-level-pipeline-doesnt-block-other-passes
  ;; The unsinked form comes first, so a walk that stops at it loses the def.
  (let [flow (setup/extract-models
              (str "(table-> :ds (ds/transform-crs \"EPSG:3857\"))\n"
                   "(def m (item-> :parcels (geo/nope 100)))"))]
    (is (= 1 (count-code flow :unsinked-top-level-pipeline)))
    (is (= 1 (count-code flow :unresolved-op)))))

(defn- follow-merge
  "Simulates a module follower: tags the module flow's ops with
   :source-module (carrying the module's declared :ns) and merges
   them into the main flow's :operations — the same shape cg-app /
   cg-vscode produce before calling recheck."
  [main-flow module-flow path label]
  (let [mod-ns (-> module-flow :module-ns :name)
        tagged (mapv #(assoc % :source-module {:path path :label label :ns mod-ns})
                     (:operations module-flow))]
    (update main-flow :operations (fnil into []) tagged)))

(deftest recheck-resolves-a-module-own-namespace-head
  (testing "post-follow recheck resolves a fully-qualified cross-file op call"
    (let [main (setup/extract-models
                (str "(require '[cg.mymod :as mm])\n"
                     "(run/load-module \"src/cg/mymod.cg\")\n"
                     "(def a (item-> :parcels (cg.mymod/my-thing)))\n"
                     "(def b (item-> :parcels (mm/my-thing)))"))
          module (setup/extract-models
                  (str "(ns cg.mymod)\n"
                       "(defn ^:op my-thing [^:row parcel]\n"
                       "  (geo/area parcel))"))]
      (is (= 2 (count-code main :unresolved-op))
          "pre-follow, neither spelling resolves")
      (let [re (parser/recheck (follow-merge main module "src/cg/mymod.cg" "Mymod"))]
        (is (zero? (count-code re :unresolved-op))
            "the module's own namespace resolves, and so does the alias")))))

(deftest recheck-strips-only-checker-codes-and-is-idempotent
  (let [flow (setup/extract-models
              (str "(defmacro helper [x] x)\n"
                   "(def x (totally/missing 1))"))
        re   (parser/recheck flow)
        re2  (parser/recheck re)]
    (is (= 1 (count-code re :forbidden-form))
        "non-checker diagnostics survive recheck")
    (is (= 1 (count-code re :unresolved-op)))
    (is (= (:diagnostics re) (:diagnostics re2)))))

#?(:cljs (-> (setup/init!)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.parser.types-test")))))
