;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.types-test
  "Point tests for the cg.parser.types static checker (passes 1-3).

   Run with: clojure -M:test-parser"
  (:require [cg.parser.core :as parser]
            [cg.dsl.signatures :as sigs]
            [clojure.java.io :as io]
            #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (defn- fixture-text [rel-path]
     (slurp (io/file rel-path))))

#?(:clj
   (defn- diag-codes [flow]
     (into #{} (map :code) (:diagnostics flow))))

#?(:clj
   (defn- count-code [flow code]
     (count (filter #(= code (:code %)) (:diagnostics flow)))))

#?(:clj
   (deftest pass-1-known-built-in-resolves
     (testing "a binding whose head is a built-in produces no :unresolved-op"
       (let [flow (parser/extract-models
                   "(def parcels (ds/load-geojson \"parcels.geojson\"))")]
         (is (zero? (count-code flow :unresolved-op)))))))

#?(:clj
   (deftest pass-1-unknown-head-fires
     (testing "a binding with a head absent from the registry fires :unresolved-op"
       (let [flow (parser/extract-models
                   "(def x (totally/missing 1 2))")]
         (is (contains? (diag-codes flow) :unresolved-op))
         (is (= 1 (count-code flow :unresolved-op)))))))

#?(:clj
   (deftest pass-1-threading-op-resolves
     (testing "a threading-body OpCall resolves through the same registry"
       (let [flow (parser/extract-models
                   "(def m (item-> :parcels (geo/buffer 100)))")]
         (is (zero? (count-code flow :unresolved-op)))))))

#?(:clj
   (deftest pass-1-threading-unknown-op-fires
     (testing "an unknown threading-body op fires :unresolved-op"
       (let [flow (parser/extract-models
                   "(def m (item-> :parcels (geo/nope 100)))")]
         (is (contains? (diag-codes flow) :unresolved-op))))))

#?(:clj
   (deftest a-fully-qualified-built-in-head-resolves
     (testing "cg.dataset/transform-crs is ds/transform-crs (a module can
               write the full name: the threading macro resolves an alias
               only through a top-level require)"
       (let [flow (parser/extract-models
                   "(def m (table-> :input (cg.dataset/transform-crs \"EPSG:3857\" \"EPSG:4326\")))")]
         (is (zero? (count-code flow :unresolved-op)))
         (is (zero? (count-code flow :op-arity-mismatch)))))))

#?(:clj
   (deftest a-merge-of-maps-resolves
     (testing "(merge ...) combines spec maps"
       (let [flow (parser/extract-models
                   "(def a {:x 1})\n(def b (merge {:y 2} a))\n(def c (merge {:y 2} a {:z 3}))")]
         (is (zero? (count-code flow :unresolved-op)))
         (is (zero? (count-code flow :op-arity-mismatch)))))))

#?(:clj
   (deftest pass-2-threading-fills-a-table-input
     (testing "in a table-> rung the threaded table fills a :role :table input,
               as the rung macro does (cg.cache/checkpoint, for example)"
       (let [src  (str "(defn ^:op my-read [^:table rows ^{:type :map} spec]\n"
                       "  (count rows))\n"
                       "(def m (table-> :rows (my-read {:a 1})))")
             flow (parser/extract-models src)]
         (is (zero? (count-code flow :op-arity-mismatch)))))))

#?(:clj
   (deftest a-module-defn-call-resolves
     (testing "a call to a ^:module defn resolves; & more takes any count"
       (let [src  (str "(ns ^:module cg.m)\n"
                       "(defn f [a & more] a)\n"
                       "(def x (f 1 2 3 4))\n")
             flow (parser/extract-models src)]
         (is (zero? (count-code flow :unresolved-op)))
         (is (zero? (count-code flow :op-arity-mismatch))))
       (is (pos? (count-code (parser/extract-models
                              "(ns ^:module cg.m)\n(defn f [a & more] a)\n(def x (f))\n")
                             :op-arity-mismatch))
           "a required param is still required"))))

#?(:clj
   (deftest pass-1-user-op-registers
     (testing "a user op's name is admitted to the registry when a model calls it"
       (let [src (str "(defn ^:op my-thing [^:row parcel]\n"
                      "  (geo/area parcel))\n"
                      "(def m (item-> :parcels (my-thing)))")
             flow (parser/extract-models src)]
         (is (zero? (count-code flow :unresolved-op))
             "user op 'my-thing' must resolve via from-operation-decl projection")))))

#?(:clj
   (deftest pass-2-threading-arity-clean
     (testing "geo/buffer with the right arg count in threading context is clean"
       (let [flow (parser/extract-models
                   "(def m (item-> :parcels (geo/buffer 100)))")]
         (is (zero? (count-code flow :op-arity-mismatch)))))))

#?(:clj
   (deftest pass-2-threading-too-few-args
     (testing "geo/buffer with zero args in threading context fires :op-arity-mismatch"
       (let [flow (parser/extract-models
                   "(def m (item-> :parcels (geo/buffer)))")]
         (is (contains? (diag-codes flow) :op-arity-mismatch))))))

#?(:clj
   (deftest pass-2-threading-too-many-args
     (testing "geo/buffer with three args in threading context fires :op-arity-mismatch"
       (let [flow (parser/extract-models
                   "(def m (item-> :parcels (geo/buffer 1 2 3)))")]
         (is (contains? (diag-codes flow) :op-arity-mismatch))))))

#?(:clj
   (deftest pass-2-optional-arg-accepted
     (testing "geo/buffer with its optional opts arg is clean"
       (let [flow (parser/extract-models
                   "(def m (item-> :parcels (geo/buffer 10 {:quad-segs 8})))")]
         (is (zero? (count-code flow :op-arity-mismatch)))))))

#?(:clj
   (deftest pass-2-binding-context-includes-row-input
     (testing "a Binding head includes :row inputs in the explicit-arg slots"
       (let [;; ds/load-geojson has zero :row inputs but exactly one
             ;; required string arg.
             flow (parser/extract-models "(def p (ds/load-geojson))")]
         (is (contains? (diag-codes flow) :op-arity-mismatch)
             "missing required path arg should still fire arity")))))

#?(:clj
   (deftest pass-3-clean-types
     (testing "geo/buffer with a numeric distance is clean"
       (let [flow (parser/extract-models
                   "(def m (item-> :parcels (geo/buffer 100)))")]
         (is (zero? (count-code flow :type-mismatch)))))))

#?(:clj
   (deftest pass-3-string-where-number-expected
     (testing "geo/buffer with a string distance fires :type-mismatch"
       (let [flow (parser/extract-models
                   "(def m (item-> :parcels (geo/buffer \"ten\")))")]
         (is (contains? (diag-codes flow) :type-mismatch))
         (is (= 1 (count-code flow :type-mismatch)))))))

#?(:clj
   (deftest pass-3-number-where-string-expected
     (testing "ds/load-geojson with a numeric path fires :type-mismatch"
       (let [flow (parser/extract-models
                   "(def p (ds/load-geojson 42))")]
         (is (contains? (diag-codes flow) :type-mismatch))))))

#?(:clj
   (deftest pass-3-parameter-ref-with-matching-type
     (testing "a parameter_ref of the right type passes through"
       (let [src (str "(def threshold 10)\n"
                      "(def m (item-> :parcels (geo/buffer threshold)))")
             flow (parser/extract-models src)]
         (is (zero? (count-code flow :type-mismatch)))))))

#?(:clj
   (deftest pass-3-parameter-ref-with-mismatched-type
     (testing "a parameter_ref of the wrong type fires :type-mismatch"
       (let [src (str "(def label \"abc\")\n"
                      "(def m (item-> :parcels (geo/buffer label)))")
             flow (parser/extract-models src)]
         (is (contains? (diag-codes flow) :type-mismatch))))))

#?(:clj
   (deftest pass-3-symbol-ref-binding-output-resolves
     (testing "a symbol_ref pointing at a binding inherits the binding's output type"
       (let [;; area-metrics is a binding with output type :dataset (via
             ;; ds/load-csv). ds/concat accepts :any so this is clean.
             src (str "(def area-metrics (ds/load-csv \"a.csv\"))\n"
                      "(def combined (ds/concat area-metrics))")
             flow (parser/extract-models src)]
         (is (zero? (count-code flow :type-mismatch)))))))

#?(:clj
   (deftest pass-3-cross-namespace-symbol-treated-as-any
     (testing "a cross-namespace symbol_ref (contains '/') resolves to :any"
       (let [;; ds/concat input is :any so this is trivially clean,
             ;; but the assertion is that other/foo does not itself
             ;; cause a :type-mismatch (which would happen if a
             ;; future tightening changed the rule to anything
             ;; stricter).
             flow (parser/extract-models
                   "(def x (ds/concat other/foo))")]
         (is (zero? (count-code flow :type-mismatch)))))))

#?(:clj
   (deftest pass-3-any-typed-input-accepts-anything
     (testing "a signature input typed :any accepts mismatched argument types"
       (let [flow (parser/extract-models
                   "(def x (ds/->dataset \"not-a-map\"))")]
         (is (zero? (count-code flow :type-mismatch)))))))

#?(:clj
   (deftest sig-type-kind-static-lookup
     (testing "type-kind maps known type-name strings to keywords"
       (is (= :feature (sigs/type-kind "feature")))
       (is (= :number  (sigs/type-kind "number")))
       (is (= :boolean (sigs/type-kind "boolean")))
       (is (= :boolean (sigs/type-kind "bool")))
       (is (= :any     (sigs/type-kind "ridiculous-unknown-type"))
           "unknown type strings degrade gracefully to :any"))))

#?(:clj
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
         (is (= :user (:source sig)))))))

#?(:clj
   (deftest sig-merge-signatures-overlay
     (testing "merge-signatures has later-wins semantics"
       (let [base {"a" {:qualified-name "a" :source :built-in}}
             extra {"a" {:qualified-name "a" :source :user}
                    "b" {:qualified-name "b" :source :user}}
             merged (sigs/merge-signatures base extra)]
         (is (= :user (-> merged (get "a") :source)))
         (is (contains? merged "b"))))))

#?(:clj
   (deftest typed-clean-fixture
     (testing "test/fixtures/typed-clean.cg produces zero checker diagnostics"
       (let [flow (parser/extract-models (fixture-text "test/fixtures/typed-clean.cg"))]
         (is (zero? (count-code flow :unresolved-op)))
         (is (zero? (count-code flow :op-arity-mismatch)))
         (is (zero? (count-code flow :type-mismatch)))))))

#?(:clj
   (deftest typed-mismatch-fixture
     (testing "test/fixtures/typed-mismatch.cg surfaces the planted errors"
       (let [flow (parser/extract-models (fixture-text "test/fixtures/typed-mismatch.cg"))]
         (is (pos? (count-code flow :type-mismatch)))
         (is (pos? (count-code flow :op-arity-mismatch)))
         (is (pos? (count-code flow :unresolved-op)))
         (is (pos? (count-code flow :pipeline-type-mismatch))
             "ds/transform-crs feeding geo/buffer must fire pipeline-type-mismatch")
         (is (pos? (count-code flow :op-output-mismatch))
             "an op declaring :number output but body terminal :feature must fire op-output-mismatch")
         (is (pos? (count-code flow :unknown-predicate))
             "a filter-by-column predicate absent from the registry must fire unknown-predicate")))))

#?(:clj
   (deftest pass-4-pipeline-clean
     (testing "a chain of geo ops all returning :feature passes pipeline flow"
       (let [flow (parser/extract-models
                   (str "(def m\n"
                        "  (item-> :parcels\n"
                        "    (geo/buffer 10)\n"
                        "    (geo/simplify 1.0)\n"
                        "    (geo/centroid)))"))]
         (is (zero? (count-code flow :pipeline-type-mismatch)))))))

#?(:clj
   (deftest pass-4-dataset-feeding-feature-fires
     (testing "ds/transform-crs (:dataset out) → geo/buffer (:feature in) fires pipeline-type-mismatch"
       (let [flow (parser/extract-models
                   (str "(def bad\n"
                        "  (table-> :ds\n"
                        "    (ds/transform-crs \"EPSG:3857\")\n"
                        "    (geo/buffer 10)))"))]
         (is (contains? (diag-codes flow) :pipeline-type-mismatch))))))

#?(:clj
   (deftest pass-4-single-op-pipeline-clean
     (testing "a one-op pipeline has no adjacent pairs and never fires pipeline-type-mismatch"
       (let [flow (parser/extract-models
                   "(def one (item-> :parcels (geo/buffer 10)))")]
         (is (zero? (count-code flow :pipeline-type-mismatch)))))))

#?(:clj
   (deftest pass-4-source-not-checked
     (testing "the source (keyword) is not checked against op[0] — only adjacent op pairs are"
       (let [;; :parcels is a bare keyword, no type info; geo/buffer row-in :feature
             ;; would mismatch under strict source checking. With pass 4's
             ;; source-to-op[0] skip, this is clean.
             flow (parser/extract-models
                   "(def m (item-> :parcels (geo/buffer 10) (geo/simplify 1.0)))")]
         (is (zero? (count-code flow :pipeline-type-mismatch)))))))

#?(:clj
   (deftest pass-4-runs-inside-an-op-body-pipeline
     (testing "an op body pipeline gets pass-4 checking like a top-level model"
       (let [flow (parser/extract-models
                   (str "(defn ^:op bad [^:table ds]\n"
                        "  (table-> :ds (ds/transform-crs \"EPSG:3857\") (geo/buffer 10)))"))]
         (is (contains? (diag-codes flow) :pipeline-type-mismatch))))))

#?(:clj
   (deftest pass-5-clean-op
     (testing "single-output op whose body terminal matches declared type is clean"
       (let [flow (parser/extract-models
                   (str "(defn ^:op ok {:outputs {:area {:type :number}}} [^:table parcel]\n"
                        "  (item-> :parcel (geo/area)))"))]
         (is (empty? (:diagnostics flow)))))))

#?(:clj
   (deftest pass-5-mismatch-fires
     (testing "body returns :feature but output declared :number — op-output-mismatch"
       (let [flow (parser/extract-models
                   (str "(defn ^:op bad {:outputs {:n {:type :number}}} [^:table parcel]\n"
                        "  (item-> :parcel (geo/centroid)))"))]
         (is (contains? (diag-codes flow) :op-output-mismatch))))))

#?(:clj
   (deftest pass-5-multi-output-op-skipped
     (testing "multi-output ops are not pass-5 checked (column-mapping convention TBD)"
       (let [flow (parser/extract-models
                   (str "(defn ^:op multi\n"
                        "  {:outputs {:area {:type :number}\n"
                        "             :error {:type :string :optional true}}}\n"
                        "  [^:table parcel]\n"
                        "  (item-> :parcel (geo/centroid)))"))]
         (is (empty? (:diagnostics flow))
             "multi-output ops sidestep pass 5 until output convention is pinned down")))))

#?(:clj
   (deftest pass-1-walks-op-body-ops
     (testing "pass 1 resolves the ops of an op body pipeline and surfaces :unresolved-op when missing"
       (let [flow (parser/extract-models
                   (str "(defn ^:op bad [^:table parcel]\n"
                        "  (item-> :parcel (geo/totally-bogus)))"))]
         (is (contains? (diag-codes flow) :unresolved-op))))))

#?(:clj
   (deftest partition-by-admitted
     (testing "partition-by resolves through built-ins in a table-> body"
       (let [flow (parser/extract-models
                   "(def g (table-> :ds (partition-by :col)))")]
         (is (zero? (count-code flow :unresolved-op)))
         (is (zero? (count-code flow :op-arity-mismatch)))
         (is (zero? (count-code flow :type-mismatch)))))))

#?(:clj
   (deftest aggregate-admitted
     (testing "aggregate resolves through built-ins with an inline-map config"
       (let [flow (parser/extract-models
                   (str "(def g (table-> :ds\n"
                        "  (aggregate {:total {:op :sum :over :area}})))"))]
         (is (zero? (count-code flow :unresolved-op)))
         (is (zero? (count-code flow :op-arity-mismatch)))))))

#?(:clj
   (deftest coalesce-admitted
     (testing "coalesce resolves through built-ins with one explicit default arg in item-> body"
       (let [flow (parser/extract-models
                   (str "(defn ^:op fill [^:table n]\n"
                        "  (item-> :n (coalesce 0)))"))]
         (is (zero? (count-code flow :unresolved-op)))
         (is (zero? (count-code flow :op-arity-mismatch)))
         (is (zero? (count-code flow :type-mismatch)))))))

#?(:clj
   (deftest partition-by-then-aggregate-chains-cleanly
     (testing "partition-by → aggregate chain passes pass 4 (both produce :dataset under Option C)"
       (let [flow (parser/extract-models
                   (str "(def g (table-> :parcels\n"
                        "  (partition-by :neighborhood)\n"
                        "  (aggregate {:total {:op :sum :over :area}})))"))]
         (is (zero? (count-code flow :pipeline-type-mismatch)))))))

#?(:clj
   (deftest filter-by-column-clean-predicate-admitted
     (testing "filter-by-column with a registered predicate passes pass 1-3 and parse-time predicate validation"
       (let [flow (parser/extract-models
                   (str "(def big (table-> :ds\n"
                        "  (ds/filter-by-column :area :gt 1000)))"))]
         (is (zero? (count-code flow :unresolved-op)))
         (is (zero? (count-code flow :unknown-predicate)))))))

#?(:clj
   (deftest sink-ops-walked-by-pass-1-3
     (testing "sink pipelines feed call-sites; pass 1-3 resolve their ops the same as Models"
       (let [flow (parser/extract-models
                   (str "(table-> :ds\n"
                        "  (ds/write-geojson \"out.geojson\"))"))]
         (is (zero? (count-code flow :unresolved-op)))
         (is (zero? (count-code flow :op-arity-mismatch)))
         (is (zero? (count-code flow :type-mismatch)))))))

#?(:clj
   (deftest sink-pipeline-pass-4-catches-bad-flow
     (testing "pass 4 walks sink pipelines: dataset-from-transform feeding feature-row sink would fire"
       ;; ds/write-geojson takes :dataset, so a clean predecessor chain
       ;; passes. Inject a geo/buffer (-> :feature) before the sink and
       ;; pass 4 should fire :pipeline-type-mismatch.
       (let [flow (parser/extract-models
                   (str "(table-> :ds\n"
                        "  (geo/buffer 10)\n"
                        "  (ds/write-geojson \"out.geojson\"))"))]
         (is (contains? (diag-codes flow) :pipeline-type-mismatch))))))

#?(:clj
   (deftest unsinked-top-level-pipeline-doesnt-block-other-passes
     (testing "an unsinked bare top-level pipeline still walks ops through passes 1-3"
       (let [flow (parser/extract-models
                   (str "(table-> :ds\n"
                        "  (ds/transform-crs \"EPSG:3857\"))"))]
         (is (contains? (diag-codes flow) :unsinked-top-level-pipeline))
         ;; ds/transform-crs is in the registry, so no :unresolved-op.
         (is (zero? (count-code flow :unresolved-op)))))))

#?(:clj
   (defn- follow-merge
     "Simulates a module follower: tags the module flow's ops with
      :source-module (carrying the module's declared :ns) and merges
      them into the main flow's :operations — the same shape cg-app /
      cg-vscode produce before calling recheck."
     [main-flow module-flow path label]
     (let [mod-ns (-> module-flow :module-ns :name)
           tagged (mapv #(assoc % :source-module {:path path :label label :ns mod-ns})
                        (:operations module-flow))]
       (update main-flow :operations (fnil into []) tagged))))

#?(:clj
   (deftest recheck-resolves-aliased-cross-file-ops
     (testing "post-follow recheck resolves an aliased cross-file op call"
       (let [main (parser/extract-models
                   (str "(require '[cg.mymod :as mm])\n"
                        "(run/load-module \"src/cg/mymod.cg\")\n"
                        "(def m (item-> :parcels (mm/my-thing)))"))
             module (parser/extract-models
                     (str "(ns cg.mymod)\n"
                          "(defn ^:op my-thing [^:row parcel]\n"
                          "  (geo/area parcel))"))]
         (is (= 1 (count-code main :unresolved-op))
             "pre-follow, mm/my-thing is a resolution miss")
         (let [re (parser/recheck (follow-merge main module "src/cg/mymod.cg" "Mymod"))]
           (is (zero? (count-code re :unresolved-op))
               "the alias-qualified spelling resolves through :source-module :ns"))))))

#?(:clj
   (deftest recheck-resolves-a-module-own-namespace-head
     ;; A module can call another module's op by its full namespace
     ;; (cg.parcel/prepare-parcel) or by its alias. Both resolve.
     (testing "post-follow recheck resolves a fully-qualified cross-file op call"
       (let [main (parser/extract-models
                   (str "(require '[cg.mymod :as mm])\n"
                        "(run/load-module \"src/cg/mymod.cg\")\n"
                        "(def a (item-> :parcels (cg.mymod/my-thing)))\n"
                        "(def b (item-> :parcels (mm/my-thing)))"))
             module (parser/extract-models
                     (str "(ns cg.mymod)\n"
                          "(defn ^:op my-thing [^:row parcel]\n"
                          "  (geo/area parcel))"))]
         (is (= 2 (count-code main :unresolved-op))
             "pre-follow, neither spelling resolves")
         (let [re (parser/recheck (follow-merge main module "src/cg/mymod.cg" "Mymod"))]
           (is (zero? (count-code re :unresolved-op))
               "the module's own namespace resolves, and so does the alias"))))))

#?(:clj
   (deftest recheck-strips-only-checker-codes-and-is-idempotent
     (testing "recheck re-fires still-unknown heads once, keeps non-checker codes"
       (let [flow (parser/extract-models
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
             "recheck is idempotent")))))
