;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.types-test
  "Point tests for the cg.parser.types static checker (passes 1-3).

   Run with: clojure -M:test-parser"
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

(deftest pass-1-known-built-in-resolves
  (testing "a binding whose head is a built-in produces no :unresolved-op"
    (let [flow (setup/extract-models
                "(def parcels (ds/load-geojson \"parcels.geojson\"))")]
      (is (zero? (count-code flow :unresolved-op))))))

(deftest pass-1-unknown-head-fires
  (testing "a binding with a head absent from the registry fires :unresolved-op"
    (let [flow (setup/extract-models
                "(def x (totally/missing 1 2))")]
      (is (contains? (diag-codes flow) :unresolved-op))
      (is (= 1 (count-code flow :unresolved-op))))))

(deftest pass-1-threading-op-resolves
  (testing "a threading-body OpCall resolves through the same registry"
    (let [flow (setup/extract-models
                "(def m (item-> :parcels (geo/buffer 100)))")]
      (is (zero? (count-code flow :unresolved-op))))))

(deftest pass-1-threading-unknown-op-fires
  (testing "an unknown threading-body op fires :unresolved-op"
    (let [flow (setup/extract-models
                "(def m (item-> :parcels (geo/nope 100)))")]
      (is (contains? (diag-codes flow) :unresolved-op)))))

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

(deftest pass-1-user-op-registers
  (testing "a user op's name is admitted to the registry when a model calls it"
    (let [src (str "(defn ^:op my-thing [^:row parcel]\n"
                   "  (geo/area parcel))\n"
                   "(def m (item-> :parcels (my-thing)))")
          flow (setup/extract-models src)]
      (is (zero? (count-code flow :unresolved-op))
          "user op 'my-thing' must resolve via from-operation-decl projection"))))

(deftest pass-2-threading-arity-clean
  (testing "geo/buffer with the right arg count in threading context is clean"
    (let [flow (setup/extract-models
                "(def m (item-> :parcels (geo/buffer 100)))")]
      (is (zero? (count-code flow :op-arity-mismatch))))))

(deftest pass-2-threading-too-few-args
  (testing "geo/buffer with zero args in threading context fires :op-arity-mismatch"
    (let [flow (setup/extract-models
                "(def m (item-> :parcels (geo/buffer)))")]
      (is (contains? (diag-codes flow) :op-arity-mismatch)))))

(deftest pass-2-threading-too-many-args
  (testing "geo/buffer with three args in threading context fires :op-arity-mismatch"
    (let [flow (setup/extract-models
                "(def m (item-> :parcels (geo/buffer 1 2 3)))")]
      (is (contains? (diag-codes flow) :op-arity-mismatch)))))

(deftest pass-2-optional-arg-accepted
  (testing "geo/buffer with its optional opts arg is clean"
    (let [flow (setup/extract-models
                "(def m (item-> :parcels (geo/buffer 10 {:quad-segs 8})))")]
      (is (zero? (count-code flow :op-arity-mismatch))))))

(deftest pass-2-binding-context-includes-row-input
  ;; geo/buffer has a :row input and two others. A call binding gives all
  ;; three, where a rung gives only the other two.
  (let [flow (setup/extract-models "(def b (geo/buffer g 10 {}))")]
    (is (zero? (count-code flow :op-arity-mismatch)))))

(deftest pass-3-clean-types
  (testing "geo/buffer with a numeric distance is clean"
    (let [flow (setup/extract-models
                "(def m (item-> :parcels (geo/buffer 100)))")]
      (is (zero? (count-code flow :type-mismatch))))))

(deftest pass-3-string-where-number-expected
  (testing "geo/buffer with a string distance fires :type-mismatch"
    (let [flow (setup/extract-models
                "(def m (item-> :parcels (geo/buffer \"ten\")))")]
      (is (contains? (diag-codes flow) :type-mismatch))
      (is (= 1 (count-code flow :type-mismatch))))))

(deftest pass-3-number-where-string-expected
  (testing "ds/load-geojson with a numeric path fires :type-mismatch"
    (let [flow (setup/extract-models
                "(def p (ds/load-geojson 42))")]
      (is (contains? (diag-codes flow) :type-mismatch)))))

(deftest pass-3-parameter-ref-with-matching-type
  (testing "a parameter_ref of the right type passes through"
    (let [src (str "(def threshold 10)\n"
                   "(def m (item-> :parcels (geo/buffer threshold)))")
          flow (setup/extract-models src)]
      (is (zero? (count-code flow :type-mismatch))))))

(deftest pass-3-parameter-ref-with-mismatched-type
  (testing "a parameter_ref of the wrong type fires :type-mismatch"
    (let [src (str "(def label \"abc\")\n"
                   "(def m (item-> :parcels (geo/buffer label)))")
          flow (setup/extract-models src)]
      (is (contains? (diag-codes flow) :type-mismatch)))))

(deftest pass-3-a-binding-arg-has-the-output-type-of-the-binding
  (let [flow (setup/extract-models
              (str "(def area-metrics (ds/load-csv \"a.csv\"))\n"
                   "(def m (item-> :parcels (geo/buffer area-metrics)))"))]
    (is (= ["operation 'geo/buffer' argument 'distance' expects type number but got dataset"]
           (mapv :message (filter #(= :type-mismatch (:code %)) (:diagnostics flow)))))))

(deftest pass-3-cross-namespace-symbol-treated-as-any
  (let [flow (setup/extract-models "(def m (item-> :parcels (geo/buffer other/foo)))")]
    (is (zero? (count-code flow :type-mismatch)))))

(deftest pass-3-any-typed-input-accepts-anything
  (testing "a signature input typed :any accepts mismatched argument types"
    (let [flow (setup/extract-models
                "(def x (ds/->dataset \"not-a-map\"))")]
      (is (zero? (count-code flow :type-mismatch))))))

(deftest sig-type-kind-static-lookup
  (testing "type-kind maps known type-name strings to keywords"
    (is (= :feature (sigs/type-kind "feature")))
    (is (= :number  (sigs/type-kind "number")))
    (is (= :boolean (sigs/type-kind "boolean")))
    (is (= :boolean (sigs/type-kind "bool")))
    (is (= :any     (sigs/type-kind "ridiculous-unknown-type"))
        "unknown type strings degrade gracefully to :any")))

(deftest sig-from-operation-decl-projection
  (testing "from-operation-decl maps an OperationDecl to a signature"
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
      (is (= :user (:source sig))))))

(deftest sig-merge-signatures-overlay
  (testing "merge-signatures has later-wins semantics"
    (let [base {"a" {:qualified-name "a" :source :built-in}}
          extra {"a" {:qualified-name "a" :source :user}
                 "b" {:qualified-name "b" :source :user}}
          merged (sigs/merge-signatures base extra)]
      (is (= :user (-> merged (get "a") :source)))
      (is (contains? merged "b")))))

(deftest typed-clean-fixture
  (testing "test/fixtures/typed-clean.cg produces zero checker diagnostics"
    (let [flow (setup/extract-models (util/read-file "test/fixtures/typed-clean.cg"))]
      (is (zero? (count-code flow :unresolved-op)))
      (is (zero? (count-code flow :op-arity-mismatch)))
      (is (zero? (count-code flow :type-mismatch))))))

(deftest typed-mismatch-fixture
  (testing "test/fixtures/typed-mismatch.cg surfaces the planted errors"
    (let [flow (setup/extract-models (util/read-file "test/fixtures/typed-mismatch.cg"))]
      (is (pos? (count-code flow :type-mismatch)))
      (is (pos? (count-code flow :op-arity-mismatch)))
      (is (pos? (count-code flow :unresolved-op)))
      (is (pos? (count-code flow :pipeline-type-mismatch))
          "ds/transform-crs feeding geo/buffer must fire pipeline-type-mismatch")
      (is (pos? (count-code flow :op-output-mismatch))
          "an op declaring :number output but body terminal :feature must fire op-output-mismatch")
      (is (pos? (count-code flow :unknown-predicate))
          "a filter-by-column predicate absent from the registry must fire unknown-predicate"))))

(deftest pass-4-pipeline-clean
  (testing "a chain of geo ops all returning :feature passes pipeline flow"
    (let [flow (setup/extract-models
                (str "(def m\n"
                     "  (item-> :parcels\n"
                     "    (geo/buffer 10)\n"
                     "    (geo/simplify 1.0)\n"
                     "    (geo/centroid)))"))]
      (is (zero? (count-code flow :pipeline-type-mismatch))))))

(deftest pass-4-dataset-feeding-feature-fires
  (testing "ds/transform-crs (:dataset out) → geo/buffer (:feature in) fires pipeline-type-mismatch"
    (let [flow (setup/extract-models
                (str "(def bad\n"
                     "  (table-> :ds\n"
                     "    (ds/transform-crs \"EPSG:3857\")\n"
                     "    (geo/buffer 10)))"))]
      (is (contains? (diag-codes flow) :pipeline-type-mismatch)))))

(deftest pass-4-single-op-pipeline-clean
  (testing "a one-op pipeline has no adjacent pairs and never fires pipeline-type-mismatch"
    (let [flow (setup/extract-models
                "(def one (item-> :parcels (geo/buffer 10)))")]
      (is (zero? (count-code flow :pipeline-type-mismatch))))))

(deftest pass-4-source-not-checked
  (testing "the source (keyword) is not checked against op[0] — only adjacent op pairs are"
    (let [;; :parcels is a bare keyword, no type info; geo/buffer row-in :feature
          ;; would mismatch under strict source checking. With pass 4's
          ;; source-to-op[0] skip, this is clean.
          flow (setup/extract-models
                "(def m (item-> :parcels (geo/buffer 10) (geo/simplify 1.0)))")]
      (is (zero? (count-code flow :pipeline-type-mismatch))))))

(deftest pass-4-runs-inside-an-op-body-pipeline
  (testing "an op body pipeline gets pass-4 checking like a top-level model"
    (let [flow (setup/extract-models
                (str "(defn ^:op bad [^:table ds]\n"
                     "  (table-> :ds (ds/transform-crs \"EPSG:3857\") (geo/buffer 10)))"))]
      (is (contains? (diag-codes flow) :pipeline-type-mismatch)))))

(deftest pass-5-clean-op
  (testing "single-output op whose body terminal matches declared type is clean"
    (let [flow (setup/extract-models
                (str "(defn ^:op ok {:outputs {:area {:type :number}}} [^:table parcel]\n"
                     "  (item-> :parcel (geo/area)))"))]
      (is (empty? (:diagnostics flow))))))

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
      (is (empty? (:diagnostics flow))
          "multi-output ops sidestep pass 5 until output convention is pinned down"))))

(deftest pass-1-walks-op-body-ops
  (testing "pass 1 resolves the ops of an op body pipeline and surfaces :unresolved-op when missing"
    (let [flow (setup/extract-models
                (str "(defn ^:op bad [^:table parcel]\n"
                     "  (item-> :parcel (geo/totally-bogus)))"))]
      (is (contains? (diag-codes flow) :unresolved-op)))))

(deftest partition-by-admitted
  (testing "partition-by resolves through built-ins in a table-> body"
    (let [flow (setup/extract-models
                "(def g (table-> :ds (partition-by :col)))")]
      (is (zero? (count-code flow :unresolved-op)))
      (is (zero? (count-code flow :op-arity-mismatch)))
      (is (zero? (count-code flow :type-mismatch))))))

(deftest aggregate-admitted
  (testing "aggregate resolves through built-ins with an inline-map config"
    (let [flow (setup/extract-models
                (str "(def g (table-> :ds\n"
                     "  (aggregate {:total {:op :sum :over :area}})))"))]
      (is (zero? (count-code flow :unresolved-op)))
      (is (zero? (count-code flow :op-arity-mismatch))))))

(deftest coalesce-admitted
  (testing "coalesce resolves through built-ins with one explicit default arg in item-> body"
    (let [flow (setup/extract-models
                (str "(defn ^:op fill [^:table n]\n"
                     "  (item-> :n (coalesce 0)))"))]
      (is (zero? (count-code flow :unresolved-op)))
      (is (zero? (count-code flow :op-arity-mismatch)))
      (is (zero? (count-code flow :type-mismatch))))))

(deftest partition-by-then-aggregate-chains-cleanly
  (testing "partition-by → aggregate chain passes pass 4 (both produce :dataset under Option C)"
    (let [flow (setup/extract-models
                (str "(def g (table-> :parcels\n"
                     "  (partition-by :neighborhood)\n"
                     "  (aggregate {:total {:op :sum :over :area}})))"))]
      (is (zero? (count-code flow :pipeline-type-mismatch))))))

(deftest filter-by-column-clean-predicate-admitted
  (testing "filter-by-column with a registered predicate passes pass 1-3 and parse-time predicate validation"
    (let [flow (setup/extract-models
                (str "(def big (table-> :ds\n"
                     "  (ds/filter-by-column :area :gt 1000)))"))]
      (is (zero? (count-code flow :unresolved-op)))
      (is (zero? (count-code flow :unknown-predicate))))))

(deftest sink-ops-walked-by-pass-1-3
  (let [flow (setup/extract-models
              (str "(table-> :ds\n"
                   "  (geo/nope 1)\n"
                   "  (ds/write-geojson \"out.geojson\"))"))]
    (is (= 1 (count (:sinks flow))))
    (is (= 1 (count-code flow :unresolved-op)))))

(deftest sink-pipeline-pass-4-catches-bad-flow
  (testing "pass 4 walks sink pipelines: dataset-from-transform feeding feature-row sink would fire"
    ;; ds/write-geojson takes :dataset, so a clean predecessor chain
    ;; passes. Inject a geo/buffer (-> :feature) before the sink and
    ;; pass 4 should fire :pipeline-type-mismatch.
    (let [flow (setup/extract-models
                (str "(table-> :ds\n"
                     "  (geo/buffer 10)\n"
                     "  (ds/write-geojson \"out.geojson\"))"))]
      (is (contains? (diag-codes flow) :pipeline-type-mismatch)))))

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

(deftest recheck-resolves-aliased-cross-file-ops
  (testing "post-follow recheck resolves an aliased cross-file op call"
    (let [main (setup/extract-models
                (str "(require '[cg.mymod :as mm])\n"
                     "(run/load-module \"src/cg/mymod.cg\")\n"
                     "(def m (item-> :parcels (mm/my-thing)))"))
          module (setup/extract-models
                  (str "(ns cg.mymod)\n"
                       "(defn ^:op my-thing [^:row parcel]\n"
                       "  (geo/area parcel))"))]
      (is (= 1 (count-code main :unresolved-op))
          "pre-follow, mm/my-thing is a resolution miss")
      (let [re (parser/recheck (follow-merge main module "src/cg/mymod.cg" "Mymod"))]
        (is (zero? (count-code re :unresolved-op))
            "the alias-qualified spelling resolves through :source-module :ns")))))

(deftest recheck-resolves-a-module-own-namespace-head
  ;; A module can call another module's op by its full namespace
  ;; (cg.parcel/prepare-parcel) or by its alias. Both resolve.
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
  (testing "recheck re-fires still-unknown heads once, keeps non-checker codes"
    (let [flow (setup/extract-models
                (str "(defmacro helper [x] x)\n"
                     "(def x (totally/missing 1))"))
          re   (parser/recheck flow)
          re2  (parser/recheck re)]
      (is (= 1 (count-code flow :forbidden-form)))
      (is (= 1 (count-code re :forbidden-form))
          "non-checker diagnostics survive recheck")
      (is (= 1 (count-code re :unresolved-op))
          "a genuinely unknown head re-fires exactly once")
      (is (= (:diagnostics re) (:diagnostics re2))
          "recheck is idempotent"))))

#?(:cljs (-> (setup/init!)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.parser.types-test")))))
