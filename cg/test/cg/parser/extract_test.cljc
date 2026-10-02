;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.extract-test
  "End-to-end shape assertions for cg.parser.core/extract-models against
   the five fixture files. Strict-conforming fixtures must yield zero
   diagnostics; the non-conforming script-style.cg fixture must surface the
   expected set of diagnostic codes.

   Run with: clojure -M:test-parser"
  (:require [cg.parser.core :as parser]
            [clojure.java.io :as io]
            #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (defn- fixture-text [rel-path]
     (slurp (io/file rel-path))))

#?(:clj
   (defn- models
     "The models of a flow: the `:bindings` whose `:body-shape` is
      `:threading_macro`."
     [flow]
     (filterv #(= :threading_macro (:body-shape %)) (:bindings flow))))

#?(:clj
   (defn- model-by-name [flow name]
     (first (filter #(= (:name %) name) (models flow)))))

#?(:clj
   (defn- diagnostic-codes [flow]
     (into #{} (map :code) (:diagnostics flow))))

#?(:clj
   (deftest model-inspection-fixture
     (testing "test/fixtures/model-inspection.cg parses cleanly"
       (let [flow (parser/extract-models (fixture-text "test/fixtures/model-inspection.cg"))]
         (is (= 0 (count (:diagnostics flow)))
             "strict-conforming fixture must yield zero diagnostics")
         (is (= 2 (count (:requires flow))))
         (is (= #{"cg.macros" "cg.geo"}
                (into #{} (map :namespace) (:requires flow))))
         (is (= 0 (count (:parameters flow))))
         (is (= 1 (count (models flow))))
         (let [m (model-by-name flow "my-model")]
           (is (some? m))
           (is (= :item (:mode m)))
           (is (= "parcels" (:input m)))
           (is (= 3 (count (:ops m))))
           (is (= ["geo/buffer" "geo/simplify" "geo/centroid"]
                  (mapv :qualified-name (:ops m))))
           (is (nil? (:config m))))
         (is (nil? (:overlay flow)))))))

#?(:clj
   (deftest run-simple-fixture
     (testing "test/fixtures/run-simple.cg parses cleanly"
       (let [flow (parser/extract-models (fixture-text "test/fixtures/run-simple.cg"))]
         (is (= 0 (count (:diagnostics flow))))
         (is (= 2 (count (:requires flow))))
         (is (= 1 (count (models flow))))
         (let [m (model-by-name flow "buffer-model")]
           (is (some? m))
           (is (= :item (:mode m)))
           (is (= "parcels" (:input m)))
           (is (= 1 (count (:ops m))))
           (is (= "geo/buffer" (-> m :ops first :qualified-name))))
         (is (nil? (:overlay flow)))))))

#?(:clj
   (deftest threading-macros-fixture
     (testing "test/fixtures/threading-macros.cg parses all four models cleanly"
       (let [flow (parser/extract-models (fixture-text "test/fixtures/threading-macros.cg"))]
         (is (= 0 (count (:diagnostics flow))))
         (is (= 2 (count (:requires flow))))
         (is (= 4 (count (models flow))))
         (is (= #{"buffer-simplify-model" "union-model"
                  "centroid-model" "configured-model"}
                (into #{} (map :name) (models flow))))
         (testing "buffer-simplify-model"
           (let [m (model-by-name flow "buffer-simplify-model")]
             (is (= :item (:mode m)))
             (is (= 2 (count (:ops m))))
             (is (nil? (:config m)))))
         (testing "union-model is :coll mode"
           (let [m (model-by-name flow "union-model")]
             (is (= :coll (:mode m)))
             (is (= 1 (count (:ops m))))))
         (testing "centroid-model has three ops"
           (let [m (model-by-name flow "centroid-model")]
             (is (= 3 (count (:ops m))))))
         (testing "configured-model has trailing config map"
           (let [m (model-by-name flow "configured-model")]
             (is (= 1 (count (:ops m)))
                 "trailing map must be classified as config, not as an op")
             (is (some? (:config m))
                 "trailing map_lit on threading body must be detected")))))))

#?(:clj
   (deftest test-load-model-fixture
     (testing "test/fixtures/test-load-model.cg has a ^:cgproj overlay"
       (let [flow (parser/extract-models (fixture-text "test/fixtures/test-load-model.cg"))]
         (is (= 0 (count (:diagnostics flow))))
         (is (= 2 (count (:requires flow))))
         (is (= 1 (count (models flow))))
         (let [m (model-by-name flow "simple-buffer-model")]
           (is (= :item (:mode m)))
           (is (= 2 (count (:ops m)))))
         (is (some? (:overlay flow))
             "^:cgproj map_lit must be captured as overlay")
         (is (some? (-> flow :overlay :raw)))
         (is (some? (-> flow :overlay :location)))))))

#?(:clj
   (deftest script-style-fixture-negative
     (testing "test/fixtures/script-style.cg surfaces the expected diagnostic codes"
       (let [flow (parser/extract-models (fixture-text "test/fixtures/script-style.cg"))
             codes (diagnostic-codes flow)]
         (is (pos? (count (:diagnostics flow)))
             "a script with top-level prints must produce diagnostics under the strict format")
         (is (not (contains? codes :forbidden-form))
             "a defn at the top level is an operation")
         (is (seq (:operations flow)))
         (is (contains? codes :unclassified-top-level-form)
             "non-flow top-level forms must emit :unclassified-top-level-form")
         (is (= 2 (count (models flow)))
             "the two top-level models are buffer-flow and export-flow; the table-> in the body of reproject-to is not a model")
         (is (pos? (count (:bindings flow)))
             "(def lots-raw (ds/load-geojson ...)) must classify as a binding")))))

#?(:clj
   (deftest binding-simple-loader
     (testing "(def parcels (ds/load-geojson \"parcels.geojson\")) classifies as binding"
       (let [flow (parser/extract-models
                   "(def parcels (ds/load-geojson \"parcels.geojson\"))")]
         (is (= 0 (count (:diagnostics flow))))
         (is (= 0 (count (:parameters flow))))
         (is (= 0 (count (models flow))))
         (is (= 1 (count (:bindings flow))))
         (let [b (first (:bindings flow))]
           (is (= "parcels" (:name b)))
           (is (= "ds/load-geojson" (:head-sym b)))
           (is (= 1 (count (:args b))))
           (is (= :literal (-> b :args first :kind))))))))

#?(:clj
   (deftest layers-threading-fence
     (testing "layers-> is admitted by the threading vocabulary but has no
               implementation yet; decoding it must emit a
               :threading-mode-unimplemented diagnostic instead of throwing, and
               still extract a partial model with nil mode"
       (let [flow (parser/extract-models
                   "(def scoped (layers-> :parcels (geo/buffer 100)))")
             codes (diagnostic-codes flow)
             m (model-by-name flow "scoped")]
         (is (contains? codes :threading-mode-unimplemented)
             "decoding layers-> emits the unimplemented diagnostic, not a throw")
         (is (some? m) "the layers-> binding still extracts as a partial model")
         (is (nil? (:mode m)) "mode stays nil until :scope has an implementation")))))

#?(:clj
   (deftest binding-with-symbol-ref-args
     (testing "binding args admit parameter_ref and symbol_ref shapes"
       (let [flow (parser/extract-models
                   "(def combined (ds/concat area-metrics other/base-metrics))")]
         (is (= 0 (count (:diagnostics flow))))
         (is (= 1 (count (:bindings flow))))
         (let [b (first (:bindings flow))]
           (is (= "combined" (:name b)))
           (is (= "ds/concat" (:head-sym b)))
           (is (= 2 (count (:args b))))
           (is (= [:parameter_ref :symbol_ref]
                  (mapv :kind (:args b)))))))))

#?(:clj
   (deftest binding-with-parameter-ref-and-literals
     (testing "binding mixes parameter-ref, string and number literals"
       (let [flow (parser/extract-models
                   "(def t (gcrs/create-transform 4326 2249 input-ds))")]
         (is (= 0 (count (:diagnostics flow))))
         (is (= 1 (count (:bindings flow))))
         (let [b (first (:bindings flow))]
           (is (= "gcrs/create-transform" (:head-sym b)))
           (is (= [:literal :literal :parameter_ref]
                  (mapv :kind (:args b)))))))))

#?(:clj
   (deftest binding-accepts-nested-call-arg
     (testing "Stage E: a binding with a nested (call) arg is accepted, with the nested call captured as a recursive :call OpArg"
       (let [flow (parser/extract-models
                   "(def good (foo (nested 1 :k)))")
             codes (diagnostic-codes flow)]
         (is (= 1 (count (:bindings flow))))
         (is (not (contains? codes :binding-malformed)))
         (let [b (first (:bindings flow))
               nested (first (:args b))]
           (is (= "foo" (:head-sym b)))
           (is (= :call (:kind nested)))
           (is (= "nested" (:head nested)))
           (is (= [:literal :literal] (mapv :kind (:args nested)))))))))

#?(:clj
   (deftest binding-malformed-quoted-arg
     (testing "a binding with a quoted arg emits :binding-malformed and drops the form"
       (let [flow (parser/extract-models
                   "(def bad (foo 'quoted))")
             codes (diagnostic-codes flow)]
         (is (= 0 (count (:bindings flow))))
         (is (contains? codes :binding-malformed))))))

#?(:clj
   (deftest a-def-rhs-outside-the-vocabulary-is-unclassified
     (testing "a quoted RHS reaches none of the branches"
       (let [flow (parser/extract-models "(def x '(a b))")
             codes (diagnostic-codes flow)]
         (is (= 0 (count (:bindings flow))))
         (is (contains? codes :unclassified-top-level-form))))))

#?(:clj
   (deftest ops-carry-emits-level
     (testing "each spine op is annotated with the level it emits (the converter colors the outgoing edge from this)"
       (let [flow (parser/extract-models
                   "(def out (table-> parcels (filter :zone :in zs) (group-by :zone) (map f) (into {})))")
             out-b (first (filter #(= "out" (:name %)) (:bindings flow)))
             ops (:ops out-b)
             level-of (fn [qn] (:emits-level (first (filter #(= qn (:qualified-name %)) ops))))]
         (is (= :dataset (level-of "filter")) "filter preserves the dataset unit")
         (is (= :groups (level-of "group-by")) "group-by promotes dataset -> groups")
         (is (= :groups (level-of "map")) "map over groups stays at groups")
         (is (= :map (level-of "into")) "into demotes groups -> a keyed map")))))

#?(:clj
   (deftest table-role-on-an-op-param
     (testing "^:table on an op param gives the input the :table role"
       (let [flow (parser/extract-models
                   "(defn ^:op f [^:table rows] (table-> :rows (parcel/prepare-parcel)))")
             op  (first (:operations flow))
             in  (first (:inputs op))]
         (is (= :table (:role in)))
         (is (not-any? #(= :op-malformed (:code %)) (:diagnostics flow)))))))

#?(:clj
   (deftest map-over-component-carries-component-name
     (testing "(map <op-name>) lifts the referenced name onto :component (eval-free)"
       (let [flow (parser/extract-models
                   "(def out (table-> parcels (group-by :zone) (map analyze-nbhd) (into {})))")
             out-b (first (filter #(= "out" (:name %)) (:bindings flow)))
             ops (:ops out-b)
             mapn (first (filter #(= "map" (:qualified-name %)) ops))]
         (is (some? mapn) "map op present in the pipeline")
         (is (= "analyze-nbhd" (:component mapn))
             "component name carried by reference, not resolved")
         (testing "non-map spine ops carry no :component"
           (is (nil? (:component (first (filter #(= "group-by" (:qualified-name %)) ops)))))
           (is (nil? (:component (first (filter #(= "into" (:qualified-name %)) ops))))))))))

#?(:clj
   (deftest checkpoint-carries-component-name
     (testing "(cg.cache/checkpoint <spec> <op-name>) lifts its second argument onto :component"
       (let [flow (parser/extract-models
                   (str "(def out (table-> rows (cg.cache/checkpoint"
                        " {:dir \"d\" :prefix \"p\" :layer \"l\" :key [1]} impervious-join)))"))
             out-b (first (filter #(= "out" (:name %)) (:bindings flow)))
             cp (first (filter #(= "cg.cache/checkpoint" (:qualified-name %)) (:ops out-b)))]
         (is (some? cp) "checkpoint op present in the pipeline")
         (is (= "impervious-join" (:component cp)))))))

#?(:clj
   (deftest binding-is-dataset-source-classification
     (testing "loader head sets :is-dataset-source true on function-call Binding"
       (let [flow (parser/extract-models
                   "(def parcels (ds/load-geojson \"parcels.geojson\"))")
             b (first (:bindings flow))]
         (is (= :function_call (:body-shape b)))
         (is (true? (:is-dataset-source b)))))
     (testing "non-loader head sets :is-dataset-source false on function-call Binding"
       (let [flow (parser/extract-models
                   "(def t (gcrs/create-transform 4326 2249 input-ds))")
             b (first (:bindings flow))]
         (is (= :function_call (:body-shape b)))
         (is (false? (:is-dataset-source b)))))
     (testing "threading-macro Binding has no :is-dataset-source (function-call only)"
       (let [flow (parser/extract-models
                   "(def my-model (item-> :input (geo/buffer 10)))")
             b (first (:bindings flow))]
         (is (= :threading_macro (:body-shape b)))
         (is (not (contains? b :is-dataset-source)))))))

#?(:clj
   (deftest eager-threading-macro-binding-admission
     (testing "keyword first-arg admits as template Binding"
       (let [flow (parser/extract-models
                   "(def t (item-> :parcels (geo/buffer 10)))")
             b (first (:bindings flow))]
         (is (= 0 (count (:diagnostics flow))))
         (is (= :threading_macro (:body-shape b)))
         (is (true? (:is-template b)))
         (is (= :literal (-> b :source :kind)))))
     (testing "parameter-ref first-arg admits as eager Binding (no :non-keyword-data-ref)"
       (let [flow (parser/extract-models
                   "(def t (item-> parcels (geo/buffer 10)))")
             codes (diagnostic-codes flow)
             b (first (:bindings flow))]
         (is (not (contains? codes :non-keyword-data-ref)))
         (is (= 1 (count (:bindings flow))))
         (is (= :threading_macro (:body-shape b)))
         (is (false? (:is-template b)))
         (is (= :parameter_ref (-> b :source :kind)))
         (is (nil? (:input b)))))
     (testing "symbol-ref first-arg admits as eager Binding"
       (let [flow (parser/extract-models
                   "(def t (item-> other/parcels (geo/buffer 10)))")
             codes (diagnostic-codes flow)
             b (first (:bindings flow))]
         (is (not (contains? codes :non-keyword-data-ref)))
         (is (= :threading_macro (:body-shape b)))
         (is (false? (:is-template b)))
         (is (= :symbol_ref (-> b :source :kind)))))
     (testing "list-form first-arg still fires :non-keyword-data-ref (not OpArg-shaped)"
       (let [flow (parser/extract-models
                   "(def t (item-> (foo bar) (geo/buffer 10)))")
             codes (diagnostic-codes flow)]
         (is (contains? codes :non-keyword-data-ref))
         (is (= 0 (count (:bindings flow))))))))

#?(:clj
   (defn- operation-by-name [flow name]
     (first (filter #(= (:name %) name) (:operations flow)))))

#?(:clj
   (deftest an-op-with-a-pipeline-body
     (testing "a table op whose body is a threading form parses cleanly"
       (let [flow (parser/extract-models
                   (str "(defn ^:op area\n"
                        "  {:outputs {:area {:type :number}}}\n"
                        "  [^:table ^{:type :feature} parcel]\n"
                        "  (item-> :parcel (geo/area)))"))]
         (is (= 0 (count (:diagnostics flow)))
             "a minimal op must produce zero diagnostics")
         (is (= 1 (count (:operations flow))))
         (let [op (operation-by-name flow "area")]
           (is (some? op))
           (is (= ["defn" "op"] (:flags op)))
           (is (= 1 (count (:inputs op))))
           (let [i (first (:inputs op))]
             (is (= "parcel" (:name i)))
             (is (= "feature" (:type i)))
             (is (= :table (:role i)))
             (is (false? (:optional i))))
           (is (= 1 (count (:outputs op))))
           (let [o (first (:outputs op))]
             (is (= "area" (:name o)))
             (is (= "number" (:type o))))
           (is (= 1 (count (:body op))))
           (let [b (first (:body op))]
             (is (= :pipeline (:kind b)))
             (is (some? (:raw b)))
             (is (some? (:location b))))
           (let [g (:body-graph op)]
             (is (some? g)
                 ":body-graph derived from the single pipeline body form")
             (is (= [:area-1] (mapv :id (:nodes g)))
                 ":id matches runtime generate-node-id contract :<unq-name>-<counter>")
             (is (= "geo/area" (:fn (first (:nodes g))))
                 ":fn preserves the qualified-name as a string")
             (is (= [] (:edges g))
                 "single-op body produces a node with no edges")
             (is (= :item (:mode g))
                 ":body-graph carries the threading mode")
             (is (= "parcel" (:input g))
                 ":body-graph carries the input name from the threading form")))))))

#?(:clj
   (deftest op-body-graph-multi-step
     (testing "multi-op pipeline body produces ids :<unq-name>-<counter> with edges chaining sequentially"
       (let [flow (parser/extract-models
                   (str "(defn ^:op get-primary-zoning-info [^:table zone-info]\n"
                        "  (item-> :zone-info\n"
                        "    (score-zoning-intersections :zoning-dataset)\n"
                        "    (ds/max-by :intersection-area)\n"
                        "    (zone-to-primary-attrs)))"))]
         (let [op (operation-by-name flow "get-primary-zoning-info")
               g  (:body-graph op)]
           (is (= [:score-zoning-intersections-1
                   :max-by-2
                   :zone-to-primary-attrs-3]
                  (mapv :id (:nodes g)))
               "counter is 1-based positional within the pipeline; ds/max-by strips to max-by")
           (is (= ["score-zoning-intersections" "ds/max-by" "zone-to-primary-attrs"]
                  (mapv :fn (:nodes g))))
           (is (= [{:from :score-zoning-intersections-1 :to :max-by-2 :type :data-flow}
                   {:from :max-by-2 :to :zone-to-primary-attrs-3 :type :data-flow}]
                  (:edges g))))))))

#?(:clj
   (deftest op-param-without-a-role-flag
     (testing "a param without a role flag parses with role=nil (a broadcast arg at runtime)"
       (let [flow (parser/extract-models
                   (str "(defn ^:op analyze-parcel\n"
                        "  [^:row ^{:type :feature} parcel ^{:type :dataset} buildings]\n"
                        "  (geo/centroid parcel))"))]
         (is (= 0 (count (:diagnostics flow))))
         (let [op (operation-by-name flow "analyze-parcel")
               by-name (into {} (map (juxt :name identity)) (:inputs op))]
           (is (= :row (:role (get by-name "parcel"))))
           (is (nil? (:role (get by-name "buildings")))
               "a param without a role flag parses as nil; runtime treats it as broadcast"))))))

#?(:clj
   (deftest op-with-idempotent-flag
     (testing "^:idempotent metadata on the op name populates :flags"
       (let [flow (parser/extract-models
                   (str "(defn ^:op ^:idempotent transform-crs [^:table ds]\n"
                        "  (table-> :ds (ds/transform-crs \"EPSG:2249\")))"))]
         (is (= 0 (count (:diagnostics flow))))
         (let [op (operation-by-name flow "transform-crs")]
           (is (some? op))
           (is (= ["defn" "op" "idempotent"] (:flags op))))))))

#?(:clj
   (deftest op-body-pipeline-decoded-structurally
     (testing ":pipeline body forms decode like top-level Models"
       (let [flow (parser/extract-models
                   (str "(defn ^:op pipe [^:table parcel]\n"
                        "  (item-> :parcel (geo/buffer 10) (geo/centroid)))"))]
         (is (= 0 (count (:diagnostics flow))))
         (let [op (operation-by-name flow "pipe")
               b (first (:body op))]
           (is (= :pipeline (:kind b)))
           (is (= :item (:mode b)))
           (is (= "parcel" (:input b)))
           (is (= ["geo/buffer" "geo/centroid"]
                  (mapv :qualified-name (:ops b)))))))))

#?(:clj
   (deftest a-when-in-an-op-body-is-an-expr
     (testing "a when in an op body is plain Clojure, whatever its test,
               because an op body runs as written"
       (let [flow (parser/extract-models
                   (str "(defn ^:op g [^:row xs] (when (seq xs) (count xs)))\n"
                        "(defn ^:op h [^:row p]\n"
                        "  (when {:has-column? :geometry} (geo/centroid p)))\n"
                        "(defn ^:op k [^:row xs]\n"
                        "  (let [ys (filter number? xs)]\n"
                        "    (when (seq ys) (count ys))))"))]
         (is (empty? (:diagnostics flow)))
         (is (= :expr (-> (operation-by-name flow "g") :body first :kind)))
         (is (= :expr (-> (operation-by-name flow "h") :body first :kind))
             "a map as the test is a map, not a row predicate")
         (is (= :expr (-> (operation-by-name flow "k") :body first :tail first :kind)))))))

#?(:clj
   (deftest op-admits-reducer-role
     (testing "^:reducer joins ^:row and ^:table as a role flag"
       (let [flow (parser/extract-models
                   (str "(defn ^:op reduce-areas [^:reducer group]\n"
                        "  (reduce + 0 (map geo/area group)))"))]
         (is (= 0 (count (:diagnostics flow))))
         (let [op (operation-by-name flow "reduce-areas")
               inp (first (:inputs op))]
           (is (= "group" (:name inp)))
           (is (= :reducer (:role inp))))))))

;; A probe script narrates. The parser knows exactly what a println is, so
;; it gets its own node kind rather than an opaque card and a diagnostic
;; that inflates the residue measure.

#?(:clj
   (deftest println-classifies-as-output
     (testing "a bare top-level println is an OutputDecl, not a diagnostic"
       (let [flow (parser/extract-models "(println \"hello\")")]
         (is (= 0 (count (:diagnostics flow))))
         (is (= 1 (count (:outputs flow))))
         (let [o (first (:outputs flow))]
           (is (= "println" (:head o)))
           (is (= 1 (:arity o)))
           (is (= "hello" (:text o)))
           (is (= ["\"hello\""] (:args o)))
           (is (= "(println \"hello\")" (:raw o)))
           (is (some? (:location o))))))))

#?(:clj
   (deftest println-no-longer-lands-as-an-opaque-block
     (testing "the whole point: it stops counting against the residue measure"
       (let [flow (parser/extract-models
                   "(println \"a\")\n(println \"b\")\n(defmacro m [x] x)"
                   {:opaque-blocks true})]
         (is (= 2 (count (:outputs flow))))
         ;; the defmacro still is one, so this asserts a change, not an empty set
         (is (= 1 (count (:opaque-blocks flow))))
         (is (= #{:forbidden-form} (diagnostic-codes flow)))))))

#?(:clj
   (deftest println-carries-a-literal-only-when-it-has-one
     (testing "a string literal rides :text so the canvas can draw a rule or a spacer"
       (let [flow (parser/extract-models "(println \"\")\n(println \"=====\")")
             [spacer rule] (:outputs flow)]
         (is (= "" (:text spacer)))
         (is (= "=====" (:text rule)))))

     (testing "a computed argument leaves :text nil and falls back to :label"
       (let [flow (parser/extract-models
                   "(println (str \"HOUSING STOCK — \" title))")
             o (first (:outputs flow))]
         (is (nil? (:text o)))
         (is (= 1 (:arity o)))
         (is (= ["(str \"HOUSING STOCK — \" title)"] (:args o)))
         (is (= "(println (str \"HOUSING STOCK — \" title))" (:label o)))))

     (testing "more than one argument is not a literal line either"
       (let [flow (parser/extract-models "(println \"n =\" n)")
             o (first (:outputs flow))]
         (is (= 2 (:arity o)))
         (is (nil? (:text o)))
         (is (= ["\"n =\"" "n"] (:args o)))))

     (testing "(println) with no arguments prints a newline and is still output"
       (let [flow (parser/extract-models "(println)")]
         (is (= 0 (count (:diagnostics flow))))
         (is (= 0 (:arity (first (:outputs flow)))))
         (is (nil? (:text (first (:outputs flow)))))))))

#?(:clj
   (deftest println-label-is-one-truncated-line
     (testing "a multi-line println gives the canvas a single-line label"
       (let [src (str "(println (str \"a\"\n"
                      "              \"b\"))")
             o (first (:outputs (parser/extract-models src)))]
         (is (= "(println (str \"a\"" (:label o)))
         (is (= src (:raw o)))))))

#?(:clj
   (deftest println-inside-a-form-is-not-a-top-level-output
     (testing "only the top level classifies; a nested println rides its parent"
       ;; The doseq is a block constructor now, so the println inside it is a
       ;; body form of that block and not a member of :outputs.
       (let [flow (parser/extract-models
                   "(doseq [x xs] (println x))"
                   {:opaque-blocks true})]
         (is (= 0 (count (:outputs flow))))
         (is (= 1 (count (:blocks flow))))
         (is (= [:output] (mapv :kind (-> flow :blocks first :body)))))

     (testing "a println inside a form the vocabulary does not reach stays hidden"
       (let [flow (parser/extract-models
                   "(with-open [w (writer f)] (println w))"
                   {:opaque-blocks true})]
         (is (= 0 (count (:outputs flow))))
         (is (= 1 (count (:opaque-blocks flow))))
         (is (contains? (diagnostic-codes flow) :unclassified-top-level-form)))))))

#?(:clj
   (deftest println-does-not-shadow-an-op-named-println
     (testing "the classifier keys on the head symbol only at the top level"
       (let [flow (parser/extract-models
                   "(defn ^:op println [^:row p] (geo/area p))")]
         (is (= 0 (count (:outputs flow))))
         (is (some? (operation-by-name flow "println")))))))

;; `literal-deep?` knew about numbers, strings, keywords, booleans, vectors
;; and maps. A set or a literal nil anywhere inside dropped the whole def
;; to :unclassified-top-level-form — three of the live probe's nine.

#?(:clj
   (deftest literal-nil-classifies-as-a-parameter
     (testing "(def x nil) is a literal, and its :data is a present nil"
       (let [flow (parser/extract-models "(def missing nil)")
             p (first (:parameters flow))]
         (is (= 0 (count (:diagnostics flow))))
         (is (= "missing" (:name p)))
         (is (= :nil (-> p :value :kind)))
         (is (contains? (:value p) :data))
         (is (nil? (-> p :value :data)))))))

#?(:clj
   (deftest a-def-name-drops-its-metadata
     (testing "^:private and a ^{...} map are not part of the bound name"
       (let [flow (parser/extract-models
                   (str "(def ^:private limits [:a :b])\n"
                        "(def ^{:doc \"d\"} spec {:k (f limits)})\n"
                        "(def ^:private loader (f limits))\n"
                        "(def ^:private m (table-> :src (g)))"))]
         (is (= ["limits"] (mapv :name (:parameters flow))))
         (is (= #{"spec" "loader" "m"} (into #{} (map :name) (:bindings flow))))))))

#?(:clj
   (deftest a-nil-inside-a-collection-no-longer-kills-the-decode
     (testing "nil is a value, not a decode failure — the probe's crosstab specs"
       (let [flow (parser/extract-models
                   "(def spec {:dimensions [{:label \"extend\" :cols [:a nil]}]})")
             p (first (:parameters flow))]
         (is (= 0 (count (:diagnostics flow))))
         (is (= :mapv (-> p :value :kind)))
         (is (= {:dimensions [{:label "extend" :cols [:a nil]}]}
                (-> p :value :data)))))))

#?(:clj
   (deftest literal-sets-classify-but-do-not-decode
     (testing "a set is a literal, so the def is a parameter"
       (let [flow (parser/extract-models "(def sources #{:rs-2 :rs-3})")
             p (first (:parameters flow))]
         (is (= 0 (count (:diagnostics flow))))
         (is (= :set (-> p :value :kind)))
         (is (= "#{:rs-2 :rs-3}" (-> p :value :raw)))))

     (testing ":data stays absent rather than lying about set-ness"
       ;; A set has no JSON form, and quietly handing a consumer a vector
       ;; would change what the source means. :raw is authoritative.
       (let [p (first (:parameters (parser/extract-models "(def s #{:a})")))]
         (is (not (contains? (:value p) :data)))))

     (testing "a set nested inside a map suppresses :data for the whole map"
       (let [flow (parser/extract-models
                   "(def target-sources {:rs-4 #{:rs-2 :rs-3}})")
             p (first (:parameters flow))]
         (is (= 0 (count (:diagnostics flow))))
         (is (= :mapv (-> p :value :kind)))
         (is (not (contains? (:value p) :data)))
         (is (= "{:rs-4 #{:rs-2 :rs-3}}" (-> p :value :raw)))))))

;; (def rows (:analyzed-rows analysis)) is a projection off another
;; binding. Clojure calls the keyword, and so does the parser now — six of
;; the live probe's nine def blocks.

#?(:clj
   (deftest keyword-access-def-classifies-as-a-binding
     (testing "(def x (:key y)) is a function_call binding with a keyword head"
       (let [flow (parser/extract-models "(def analyzed-rows (:analyzed-rows analysis))")
             b (first (:bindings flow))]
         (is (= 0 (count (:diagnostics flow))))
         (is (= "analyzed-rows" (:name b)))
         (is (= :function_call (:body-shape b)))
         (is (= ":analyzed-rows" (:head-sym b)))
         (is (true? (:is-accessor b)))
         (is (false? (:is-dataset-source b)))))

     (testing "the source rides as a parameter_ref arg, which is what draws the edge"
       ;; convert.cljs/param-refs keys on parameter_ref + parameter-name,
       ;; so a flat accessor gets its `analysis -> rows` edge for free.
       (let [flow (parser/extract-models "(def rows (:analyzed-rows analysis))")
             arg (-> flow :bindings first :args first)]
         (is (= :parameter_ref (:kind arg)))
         (is (= "analysis" (:parameter-name arg)))))

     (testing "a nested accessor reaches through two levels"
       ;; The source ref sits inside a :call arg. convert.cljs/param-refs
       ;; does not follow those, so this binding draws as a node without
       ;; an incoming edge — pre-existing canvas behavior for every
       ;; nested-call binding, not specific to accessors.
       (let [flow (parser/extract-models
                   "(def debug-edges (:debug-edges (:debug-layers analysis)))")
             b (first (:bindings flow))
             arg (first (:args b))]
         (is (= 0 (count (:diagnostics flow))))
         (is (= ":debug-edges" (:head-sym b)))
         (is (true? (:is-accessor b)))
         (is (= :call (:kind arg)))
         (is (= ":debug-layers" (:head arg)))
         (is (= "analysis" (-> arg :args first :parameter-name)))))))

#?(:clj
   (deftest keyword-access-is-not-an-unresolved-op
     (testing "a keyword accessor has no signature and is never checked for one"
       ;; The point of :is-accessor. Without it the type checker would
       ;; trade an opaque block for an :unresolved-op diagnostic, which is
       ;; not an improvement.
       (let [flow (parser/extract-models
                   (str "(def analyzed-rows (:analyzed-rows analysis))\n"
                        "(def summary (:zone-summary analysis))"))]
         (is (= 0 (count (:diagnostics flow))))
         (is (= 2 (count (:bindings flow))))))

     (testing "an ordinary unknown head still fires :unresolved-op"
       (let [flow (parser/extract-models "(def x (no-such-op parcels))")]
         (is (contains? (diagnostic-codes flow) :unresolved-op))))))

;; (def census-blocks (or ...)) and (def cards (try ...)) reported
;; "operation 'or' is not in the built-in registry or any user op",
;; which tells the reader to go register an op named `or`. Three of the
;; live probe's four :unresolved-op diagnostics were this.

#?(:clj
   (deftest control-form-binding-is-not-an-unresolved-op
     (testing "(def x (or a b)) says what is true, not that `or` is missing"
       (let [flow (parser/extract-models
                   "(def census-blocks (or (:census-blocks analysis) :from-cache))")
             codes (diagnostic-codes flow)]
         (is (not (contains? codes :unresolved-op)))
         (is (contains? codes :control-form-binding))))

     (testing "(def x (try ...)) too"
       (let [flow (parser/extract-models
                   (str "(def cards (try (zoning/zone-tables rows)\n"
                        "                (catch Exception t nil)))"))
             codes (diagnostic-codes flow)]
         (is (not (contains? codes :unresolved-op)))
         (is (contains? codes :control-form-binding))))

     (testing "(def x (await ...)) too, as in examples/parcel.cg"
       (let [flow (parser/extract-models
                   "(def city-hall (await (ds/ensure-dataset query)))")
             codes (diagnostic-codes flow)]
         (is (not (contains? codes :unresolved-op)))
         (is (contains? codes :control-form-binding))))

     (testing "the diagnostic is a warning: unmodelable is not malformed"
       (let [flow (parser/extract-models "(def x (or a b))")
             d (first (filter #(= :control-form-binding (:code %)) (:diagnostics flow)))]
         (is (= :warning (:severity d)))
         (is (some? (:location d)))))

     (testing "the binding still exists, flagged, so the canvas keeps showing it"
       (let [flow (parser/extract-models "(def x (or a b))")
             b (first (:bindings flow))]
         (is (= "x" (:name b)))
         (is (= "or" (:head-sym b)))
         (is (true? (:is-control b)))))

     (testing "an ordinary unknown head is still an unresolved op"
       ;; The message is correct there: no-such-op could be registered.
       (let [flow (parser/extract-models "(def x (no-such-op parcels))")]
         (is (contains? (diagnostic-codes flow) :unresolved-op))
         (is (not (contains? (diagnostic-codes flow) :control-form-binding)))))

     (testing "a module entry point stays an unresolved op, which is the finding"
       ;; nh/ensure-analysis! is a plain defn in the followed module. That
       ;; diagnostic is absorption evidence and must not be muffled.
       (let [flow (parser/extract-models "(def analysis (nh/ensure-analysis!))")]
         (is (contains? (diagnostic-codes flow) :unresolved-op))))))

#?(:clj
   (deftest op-body-admits-plain-expressions
     (testing "a body form the parser does not model decodes to :expr, not a diagnostic"
       (let [flow (parser/extract-models
                   "(defn ^:op plain [^:row p] (reduce + 0 [1 2 3]))")]
         (is (empty? (:diagnostics flow))
             "an op body is ordinary Clojure")
         (let [op (operation-by-name flow "plain")
               bf (first (:body op))]
           (is (= 1 (count (:body op))))
           (is (= :expr (:kind bf)))
           (is (= "reduce" (:head bf)))
           (is (= "(reduce + 0 [1 2 3])" (:raw bf))))))))

#?(:clj
   (deftest op-comments-do-not-shift-the-walk
     (testing "comments inside an op are dropped, not read as its parts"
       ;; tree-sitter makes a comment a NAMED child. Before cg.parser.walk
       ;; filtered them, a comment took the place of the declaration after
       ;; it, and each later part moved by one: 86 diagnostics on parcel.cg
       ;; alone.
       (let [flow (parser/extract-models
                   (str "(defn ^:op commented\n"
                        "  ;; what this op returns\n"
                        "  {:outputs {:o {:type :number}}}\n"
                        "  ;; and what it reads\n"
                        "  [^:row p]\n"
                        "  (let [a 1\n"
                        "        ;; a note between bindings\n"
                        "        b 2]\n"
                        "    (+ a b)))"))]
         (is (= 0 (count (:diagnostics flow))))
         (let [op (operation-by-name flow "commented")
               bf (first (:body op))]
           (is (= "p" (-> op :inputs first :name)))
           (is (= "o" (-> op :outputs first :name)))
           (is (= :let_body (:kind bf)))
           (is (= ["a" "b"] (mapv :name (:bindings bf)))
               "a comment between binding pairs must not desynchronise them"))))))

#?(:clj
   (deftest let-body-binding-carries-the-component-run-component-names
     (testing "run-component names its component in a string argument, not in its head"
       ;; The head is `run-component` at every such call site, so a head-based
       ;; lookup finds nothing and the heavy flow it runs stays unreachable from
       ;; the canvas. A city module calls each of its parcel flows this way.
       (let [flow (parser/extract-models
                   (str "(defn ^:op analyze-parcels-rd [^:table rows]\n"
                        "  (table-> :rows (cg.parcel/prepare-parcel)))\n"
                        "(defn compute! [scope]\n"
                        "  (let [plain    (ds/->dataset scope)\n"
                        "        analyzed (run/run-component\n"
                        "                   \"analyze-parcels-rd\"\n"
                        "                   {:rows plain}\n"
                        "                   {:timeout-ms 100})]\n"
                        "    analyzed))"))]
         (is (not (contains? (diagnostic-codes flow) :unknown-component))
             "the component is declared in this file, so the check passes")
         (let [op    (operation-by-name flow "compute!")
               bf    (first (:body op))
               by-nm (into {} (map (juxt :name identity)) (:bindings bf))]
           (is (= "analyze-parcels-rd" (:component (get by-nm "analyzed")))
               "the component rides the binding")
           (is (nil? (:component (get by-nm "plain")))
               "an ordinary call carries none")
           (let [nodes (into {} (map (juxt :bound-name identity))
                             (:nodes (:body-graph op)))]
             (is (= "analyze-parcels-rd" (:component (get nodes "analyzed")))
                 "and reaches the body-graph node the canvas reads")
             (is (= "run/run-component" (:rhs-fn (get nodes "analyzed")))
                 "beside the head, which stays what the source says")))))))

#?(:clj
   (deftest let-body-component-needs-a-string-literal
     (testing "a computed component name carries nothing: the parser stays eval-free"
       (let [flow (parser/extract-models
                   (str "(defn compute! [scope]\n"
                        "  (let [analyzed (run/run-component (name scope) {})]\n"
                        "    analyzed))"))
             op   (operation-by-name flow "compute!")]
         (is (nil? (-> op :body first :bindings first :component)))))))

#?(:clj
   (deftest a-branching-binding-carries-what-its-branches-call
     (testing "the head names the control form, so the branches name themselves"
       ;; ensure-analysis! picks one of three compute functions by scope. A
       ;; reader who cannot see those three names on the card cannot see that
       ;; the program branches at all.
       (let [flow (parser/extract-models
                   (str "(defn dispatch [scope]\n"
                        "  (let [plain  (ds/->dataset scope)\n"
                        "        picked (cond\n"
                        "                 (:city scope)     (compute-city! scope)\n"
                        "                 (:district scope) (compute-district! scope)\n"
                        "                 :else             (compute-full! scope))]\n"
                        "    picked))"))
             op   (operation-by-name flow "dispatch")
             by-nm (into {} (map (juxt :name identity))
                         (:bindings (first (:body op))))]
         (is (= ["compute-city!" "compute-district!" "compute-full!"]
                (:rhs-calls (get by-nm "picked")))
             "every branch's call head, in source order, with `cond` itself dropped")
         (is (nil? (:rhs-calls (get by-nm "plain")))
             "an ordinary call keeps the single-callee reading :rhs-fn gives it")
         (let [nodes (into {} (map (juxt :bound-name identity))
                           (:nodes (:body-graph op)))]
           (is (= 3 (count (:rhs-calls (get nodes "picked"))))
               "and it reaches the body-graph node the canvas reads"))))))

#?(:clj
   (deftest a-branching-binding-drops-duplicate-and-nested-control-heads
     (testing "names are deduped; when/if/let name no callee a reader can follow"
       (let [flow (parser/extract-models
                   (str "(defn guarded [scope]\n"
                        "  (let [done (when-not (:cached scope)\n"
                        "               (let [r (compute! scope)\n"
                        "                     s (compute! r)]\n"
                        "                 (merge r s)))]\n"
                        "    done))"))
             op   (operation-by-name flow "guarded")]
         (is (= ["compute!" "merge"]
                (-> op :body first :bindings first :rhs-calls))
             "the nested let and the second compute! call add nothing new")))))

#?(:clj
   (deftest a-fold-binding-carries-what-its-lambda-calls
     (testing "a reduce's value comes from its lambda, so the lambda names it"
       ;; A module can hold reduce bindings whose lambdas call a walk fn. Without the carry the canvas sees
       ;; `verdicts · reduce` and can descend into nothing.
       (let [flow (parser/extract-models
                   (str "(defn walk [tables]\n"
                        "  (let [verdicts (reduce (fn [acc [suffix tbl]]\n"
                        "                           (merge acc (rule-verdicts suffix tbl)))\n"
                        "                         {} tables)]\n"
                        "    verdicts))"))
             op   (operation-by-name flow "walk")]
         (is (= ["fn" "merge" "rule-verdicts"]
                (-> op :body first :bindings first :rhs-calls))
             "every call head inside the fold, with `reduce` itself dropped")))))

#?(:clj
   (deftest a-run-component-naming-nothing-is-an-error
     (testing "the component is a STRING, so nothing else would catch a typo"
       (let [flow (parser/extract-models
                   (str "(defn compute! [scope]\n"
                        "  (let [analyzed (run/run-component \"analyze-parcles-rd\" {})]\n"
                        "    analyzed))"))]
         (is (contains? (diagnostic-codes flow) :unknown-component))))))

#?(:clj
   (deftest an-op-row-contract-must-be-a-set
     (testing "a :row-reads that is not a set literal fires :op-malformed"
       (let [flow (parser/extract-models
                   "(defn ^:op prepare {:row-reads [:geometry]} [^:row parcel-row] parcel-row)")]
         (is (contains? (diagnostic-codes flow) :op-malformed))))))

#?(:clj
   (deftest an-op-output-without-type-emits-diagnostic
     (testing "an output config missing :type fires :op-malformed but still emits the op"
       (let [flow (parser/extract-models
                   "(defn ^:op bad {:outputs {:o {:role :row}}} [^:row p] p)")]
         (is (contains? (diagnostic-codes flow) :op-malformed))
         (let [op (operation-by-name flow "bad")]
           (is (some? op))
           (is (nil? (-> op :outputs first :type))))))))

#?(:clj
   (deftest op-duplicate-names-detected
     (testing "two ops with the same name yield :duplicate-operation-name"
       (let [flow (parser/extract-models
                   (str "(defn ^:op dup [^:row p] p)\n"
                        "(defn ^:op dup [^:row p] p)"))]
         (is (= 2 (count (:operations flow))))
         (is (contains? (diagnostic-codes flow) :duplicate-operation-name))))))

#?(:clj
   (deftest op-collides-with-parameter
     (testing "operation name colliding with a parameter yields :name-collision"
       (let [flow (parser/extract-models
                   (str "(def threshold 100)\n"
                        "(defn ^:op threshold [^:row p] p)"))]
         (is (contains? (diagnostic-codes flow) :name-collision))))))

#?(:clj
   (deftest deftemplate-parses-as-model
     (testing "(deftemplate NAME (threading-> :input ops...)) yields a Model entry"
       (let [flow (parser/extract-models
                   (str "(deftemplate buffer-template\n"
                        "  (item-> :parcels (geo/buffer 100)))"))]
         (is (= 0 (count (:diagnostics flow))))
         (is (= 1 (count (models flow))))
         (let [m (model-by-name flow "buffer-template")]
           (is (some? m))
           (is (= :item (:mode m)))
           (is (= "parcels" (:input m))))))))

#?(:clj
   (deftest deftemplate-needs-threading-body
     (testing "deftemplate with non-threading body fires :def-malformed"
       (let [flow (parser/extract-models
                   "(deftemplate bad (foo/bar :input))")]
         (is (contains? (diagnostic-codes flow) :def-malformed))))))

#?(:clj
   (deftest filter-by-column-threading-clean-predicate
     (testing "registered predicate in a threading-body filter-by-column is silent"
       (let [flow (parser/extract-models
                   (str "(def big\n"
                        "  (table-> :parcels\n"
                        "    (ds/filter-by-column :area :gt 1000)))"))]
         (is (not (contains? (diagnostic-codes flow) :unknown-predicate)))))))

#?(:clj
   (deftest filter-by-column-threading-bad-predicate-fires
     (testing "unregistered predicate keyword in a threading body fires :unknown-predicate"
       (let [flow (parser/extract-models
                   (str "(def bad\n"
                        "  (table-> :ds\n"
                        "    (ds/filter-by-column :area :starts-with? 10)))"))]
         (is (contains? (diagnostic-codes flow) :unknown-predicate))))))

#?(:clj
   (deftest filter-by-column-binding-bad-predicate-fires
     (testing "unregistered predicate in a function-call Binding (dataset explicit, pos 2) fires"
       (let [flow (parser/extract-models
                   (str "(def bad\n"
                        "  (ds/filter-by-column ds :area :ends-with? 10))"))]
         (is (contains? (diagnostic-codes flow) :unknown-predicate))))))

#?(:clj
   (deftest filter-by-column-bare-head-also-checked
     (testing "unprefixed filter-by-column head is matched the same way as ds/filter-by-column"
       (let [flow (parser/extract-models
                   (str "(def bad\n"
                        "  (table-> :ds\n"
                        "    (filter-by-column :area :weirdo 10)))"))]
         (is (contains? (diagnostic-codes flow) :unknown-predicate))))))

#?(:clj
   (deftest filter-by-column-non-literal-predicate-passes
     (testing "non-literal predicate arg (parameter ref) is silent — type checker territory"
       (let [flow (parser/extract-models
                   (str "(def pred-key :gt)\n"
                        "(def filtered\n"
                        "  (table-> :ds\n"
                        "    (ds/filter-by-column :area pred-key 10)))"))]
         (is (not (contains? (diagnostic-codes flow) :unknown-predicate)))))))

#?(:clj
   (deftest sink-terminated-bare-pipeline-promotes-to-sinks
     (testing "bare top-level threading-macro form whose terminal is @sink-kind lands in CgFlow.sinks"
       (let [flow (parser/extract-models
                   (str "(table-> :reprojected\n"
                        "  (ds/write-geojson \"out.geojson\"))"))]
         (is (= 1 (count (:sinks flow))))
         (is (zero? (count (models flow))))
         (is (not (contains? (diagnostic-codes flow) :bare-model-expression)))
         (is (not (contains? (diagnostic-codes flow) :unsinked-top-level-pipeline)))
         (let [s (first (:sinks flow))]
           (is (= :table (:mode s)))
           (is (= :literal (-> s :source :kind))
               "a SinkPipeline's :source is an OpArg")
           (is (= :keyword (-> s :source :literal :kind))
               "first-arg keyword promotes to an OpArg literal of kind :keyword")
           (is (= ":reprojected" (-> s :source :literal :raw)))
           (is (= 1 (count (:ops s))))
           (is (= "ds/write-geojson" (-> s :ops first :qualified-name)))
           (is (nil? (:name s))
               "a SinkPipeline has no :name field; a sink is not a binding"))))))

#?(:clj
   (deftest unsinked-bare-pipeline-fires
     (testing "bare top-level threading-macro whose terminal is :transformer-kind fires :unsinked-top-level-pipeline"
       (let [flow (parser/extract-models
                   (str "(table-> :ds\n"
                        "  (ds/transform-crs \"EPSG:2249\"))"))]
         (is (zero? (count (:sinks flow))))
         (is (zero? (count (models flow))))
         (is (contains? (diagnostic-codes flow) :unsinked-top-level-pipeline))
         (is (not (contains? (diagnostic-codes flow) :bare-model-expression))
             "Stage C collapses the two-phase model: only :unsinked-top-level-pipeline fires for non-sink termination")))))

#?(:clj
   (deftest unsinked-pipeline-with-unknown-terminal-fires
     (testing "bare top-level threading-macro whose terminal op isn't in the registry also fires :unsinked-top-level-pipeline"
       (let [flow (parser/extract-models
                   (str "(table-> :ds\n"
                        "  (totally/missing 1))"))]
         (is (contains? (diagnostic-codes flow) :unsinked-top-level-pipeline))))))

#?(:clj
   (deftest sink-pipeline-with-write-csv-also-classifies
     (testing "ds/write-csv (the other built-in sink) also triggers promotion"
       (let [flow (parser/extract-models
                   (str "(table-> :ds\n"
                        "  (ds/write-csv \"out.csv\"))"))]
         (is (= 1 (count (:sinks flow))))
         (is (zero? (count (diagnostic-codes flow))))))))

#?(:clj
   (deftest sink-pipeline-malformed-source-still-surfaces
     (testing "shape errors in the bare threading-macro form surface their own diagnostics, not :unsinked-top-level-pipeline"
       ;; Literal/symbol/parameter-ref first-args are all admissible. A
       ;; list-form first-arg remains a true shape error (no OpArg promotion
       ;; possible) and exercises this path.
       (let [flow (parser/extract-models
                   (str "(table-> (nested call)\n"
                        "  (ds/write-geojson \"out.geojson\"))"))]
         (is (contains? (diagnostic-codes flow) :non-keyword-data-ref))
         (is (not (contains? (diagnostic-codes flow) :unsinked-top-level-pipeline)))))))

#?(:clj
   (deftest module-ns-form-recorded
     (testing "a leading (ns ...) form is admitted and recorded as :module-ns"
       (let [flow (parser/extract-models
                   (str "(ns cg.parcel\n"
                        "  \"Module docstring.\")\n"
                        "(require '[cg.geo :as geo])\n"))]
         (is (= "cg.parcel" (-> flow :module-ns :name)))
         (is (some? (-> flow :module-ns :location)))
         (is (not (contains? (diagnostic-codes flow) :unclassified-top-level-form))
             "the ns form no longer fires :unclassified-top-level-form")))))

#?(:clj
   (deftest qualified-require-is-a-require
     (testing "(clojure.core/require ...) after run/load-module, the form the
               .cg modules use, records its alias as (require ...) does"
       (let [flow (parser/extract-models
                   (str "(ns city (:require [cg.run :as run]))\n"
                        "(run/load-module \"src/cg/parcel.cg\")\n"
                        "(clojure.core/require '[cg.parcel :as parcel])\n"))]
         (is (not (contains? (diagnostic-codes flow) :unclassified-top-level-form)))
         (is (= [["cg.parcel" "parcel"]]
                (mapv (juxt :namespace :alias) (:requires flow))))))))

#?(:clj
   (deftest a-module-defn-is-an-operation
     (testing "a defn is an operation whose card the canvas can draw and open"
       (let [flow (parser/extract-models
                   (str "(ns cg.m \"Doc.\")\n"
                        "(defn- helper \"Adds.\" [a b] (when a (+ a b)))\n"
                        "(defn multi ([x] (multi x 1)) ([x y & more] (let [z (+ x y)] z)))\n"
                        "(declare later)\n"
                        "module-info\n"))
             ops  (into {} (map (juxt :name identity)) (:operations flow))]
         (is (empty? (:diagnostics flow)) "a when head is plain Clojure in a defn")
         (is (= "cg.m" (:name (:module-ns flow))))
         (is (= [["a" false] ["b" false]]
                (mapv (juxt :name :optional) (:inputs (get ops "helper")))))
         (is (= "Adds." (:doc (get ops "helper"))))
         (is (= [["x" false] ["y" true] ["more" true]]
                (mapv (juxt :name :optional) (:inputs (get ops "multi")))))
         (is (= :let_body (:kind (first (:body (get ops "multi"))))))))))

#?(:clj
   (deftest a-flow-file-defn-is-an-operation
     (let [flow (parser/extract-models "(ns city)\n(defn f [x] x)\n")]
       (is (empty? (:diagnostics flow)))
       (is (= ["f"] (mapv :name (:operations flow)))))))

#?(:clj
   (deftest module-ns-first-wins
     (testing "only the first (ns ...) is recorded"
       (let [flow (parser/extract-models
                   (str "(ns cg.first)\n"
                        "(ns cg.second)\n"))]
         (is (= "cg.first" (-> flow :module-ns :name)))))))

#?(:clj
   (deftest module-ns-malformed-fires
     (testing "(ns \"not-a-symbol\") fires :module-ns-malformed"
       (let [flow (parser/extract-models "(ns \"cg.parcel\")")]
         (is (nil? (:module-ns flow)))
         (is (contains? (diagnostic-codes flow) :module-ns-malformed))))))

#?(:clj
   (deftest literal-data-decoded-on-op-args
     (testing "fully-literal op args carry :data alongside :raw"
       (let [flow (parser/extract-models
                   (str "(def s (table-> :rows"
                        " (summarize {:value :units :as-int true :weight :rows"
                        " :scopes [:rs-2] :buckets [{:label \"4-6\" :range [4 6]}]})))"))
             arg (-> flow :bindings first :ops first :args first)]
         (is (= :inline_map (:kind arg)))
         (is (= {:value :units :as-int true :weight :rows
                 :scopes [:rs-2] :buckets [{:label "4-6" :range [4 6]}]}
                (-> arg :inline-map :data))
             "the decoded spec is plain data (JVM keywords; squint strings)")))))

#?(:clj
   (deftest boolean-literals-classify
     (testing "true/false parse as bool_lit and decode (the sym_lit check never fired)"
       (let [flow (parser/extract-models "(def flag true)")]
         (is (= :boolean (-> flow :parameters first :value :kind)))
         (is (true? (-> flow :parameters first :value :data)))))))

#?(:clj
   (deftest opaque-blocks-flag-gated
     (testing "rejected forms record OpaqueBlocks only when :opaque-blocks is set"
       ;; Neither a println nor a doseq is an example here any more: the
       ;; first classifies as an OutputDecl and the second as a BlockDecl.
       ;; The unclassified specimen is a bare top-level call.
       (let [src (str "(defmacro helper [x] x)\n"
                      "(helper 1)\n"
                      "(def m (item-> :parcels (geo/buffer 10)))")
             strict (parser/extract-models src)
             opq (parser/extract-models src {:opaque-blocks true})]
         (is (= [] (:opaque-blocks strict))
             "strict mode stays diagnostics-only")
         (is (= 2 (count (:opaque-blocks opq)))
             "the defmacro (forbidden) + the bare call (unclassified) become blocks")
         (is (= "(defmacro helper [x] x)" (-> opq :opaque-blocks first :label)))
         (is (= "(defmacro helper [x] x)" (-> opq :opaque-blocks first :raw)))
         (is (some? (-> opq :opaque-blocks first :location)))
         (is (= (diagnostic-codes strict) (diagnostic-codes opq))
             "diagnostics fire identically in both modes")
         (is (= 1 (count (filterv #(= :threading_macro (:body-shape %)) (:bindings opq))))
             "modelable forms still classify normally")))))

;; A top-level control form is a block constructor: a recognized outer shape
;; around a decoded body.

#?(:clj
   (deftest doseq-classifies-as-a-block
     (testing "a top-level doseq is a BlockDecl and not residue"
       (let [flow (parser/extract-models
                   "(doseq [card cards] (println card))"
                   {:opaque-blocks true})
             b (first (:blocks flow))]
         (is (= 0 (count (:diagnostics flow))))
         (is (= [] (:opaque-blocks flow)))
         (is (= :doseq (:kind b)))
         (is (= "doseq" (:head b)))
         (is (= [{:name "card" :rhs-raw "cards"}]
                (mapv #(select-keys % [:name :rhs-raw]) (:bindings b))))
         (is (= "(doseq [card cards] (println card))" (:raw b)))
         (is (some? (:location b)))))))

#?(:clj
   (deftest a-doseq-binding-keeps-its-destructuring-form
     (testing "the name is source text, so [title rows] survives"
       (let [b (-> (parser/extract-models
                    "(doseq [[title rows] scopes] (println title))")
                   :blocks first :bindings first)]
         (is (= "[title rows]" (:name b)))
         (is (true? (:destructured? b)))
         (is (= "scopes" (:rhs-raw b)))))))

#?(:clj
   (deftest a-block-body-decodes-recursively
     (testing "iteration over narration decodes all the way down"
       ;; Every leaf here is a form the parser already knows, which is why the
       ;; decode is deep rather than shallow.
       (let [src (str "(doseq [card cards]\n"
                      "  (println \"\")\n"
                      "  (doseq [line (lines card)]\n"
                      "    (println line)))")
             outer (-> (parser/extract-models src) :blocks first)
             [spacer inner] (:body outer)]
         (is (= :output (:kind spacer)))
         (is (= "" (-> spacer :output :text)))
         (is (= :block (:kind inner)))
         (is (= :doseq (-> inner :block :kind)))
         (is (= [:output] (mapv :kind (-> inner :block :body))))
         (is (= "line" (-> inner :block :body first :output :args first)))))))

#?(:clj
   (deftest a-body-form-outside-the-vocabulary-is-an-expr-leaf
     (testing "the honest leaf, carrying what an opaque card draws from"
       (let [flow (parser/extract-models
                   "(doseq [[title rows] scopes] (district-report title rows))")
             leaf (-> flow :blocks first :body first)]
         (is (= :expr (:kind leaf)))
         (is (= "district-report" (:head leaf)))
         (is (= "(district-report title rows)" (:raw leaf)))
         (is (= "(district-report title rows)" (:label leaf)))
         (is (some? (:location leaf))))
       (testing "and reports nothing, the same way an op body :expr does"
         (is (= 0 (count (:diagnostics
                          (parser/extract-models
                           "(doseq [x xs] (f x))")))))))))

#?(:clj
   (deftest a-nested-block-is-not-a-top-level-block
     (testing "only the top level lands in :blocks; the rest ride their parent"
       (let [flow (parser/extract-models
                   "(try (doseq [x xs] (println x)) (catch Exception e nil))")]
         (is (= 1 (count (:blocks flow))))
         (is (= :try (-> flow :blocks first :kind)))))))

#?(:clj
   (deftest try-carries-its-catch-clause
     (testing "the failure path is modeled, which is not the same as run"
       (let [src (str "(try\n"
                      "  (doseq [c cards] (println c))\n"
                      "  (catch Exception t\n"
                      "    (println (str \"ERROR: \" (ex-message t)))))")
             b (-> (parser/extract-models src) :blocks first)
             c (first (:catches b))]
         (is (= :try (:kind b)))
         (is (= [:block] (mapv :kind (:body b))) "the catch is not in the body")
         (is (= 1 (count (:catches b))))
         (is (= "Exception" (:class c)))
         (is (= "t" (:binding c)))
         (is (= [:output] (mapv :kind (:body c))))
         (is (nil? (:finally b)))))))

#?(:clj
   (deftest try-carries-its-finally-clause
     (testing "finally decodes its own body too"
       (let [b (-> (parser/extract-models
                    "(try (println \"a\") (finally (println \"b\")))")
                   :blocks first)]
         (is (= [:output] (mapv :kind (:body b))))
         (is (= [] (:catches b)))
         (is (= [:output] (mapv :kind (-> b :finally :body))))))))

#?(:clj
   (deftest when-carries-its-test-as-raw-source
     (testing "whether the test holds is a runtime question"
       (let [b (-> (parser/extract-models
                    "(when (:validation analysis) (println \"validating\"))")
                   :blocks first)]
         (is (= :when (:kind b)))
         (is (= "(:validation analysis)" (-> b :test :raw)))
         (is (= ":validation" (-> b :test :head)))
         (is (some? (-> b :test :location)))
         (is (nil? (:bindings b)))))))

#?(:clj
   (deftest if-let-and-let-classify-with-their-bindings
     (testing "if-let"
       (let [b (-> (parser/extract-models
                    "(if-let [ds (:census-blocks analysis)] (println ds))")
                   :blocks first)]
         (is (= :if-let (:kind b)))
         (is (= ["ds"] (mapv :name (:bindings b))))
         (is (= "(:census-blocks analysis)" (-> b :bindings first :rhs-raw)))))

     (testing "let, and its RHS head rides the binding"
       (let [b (-> (parser/extract-models
                    "(let [rows (filter :rd analyzed)] (println rows))")
                   :blocks first)]
         (is (= :let (:kind b)))
         (is (= "rows" (-> b :bindings first :name)))
         (is (= "filter" (-> b :bindings first :rhs-fn)))
         (is (false? (-> b :bindings first :destructured?)))))))

#?(:clj
   (deftest a-block-missing-its-binder-is-malformed-and-still-draws
     (testing "no bindings vector"
       (let [flow (parser/extract-models "(doseq (println 1))" {:opaque-blocks true})]
         (is (= 0 (count (:blocks flow))))
         (is (contains? (diagnostic-codes flow) :block-malformed))
         (is (= 1 (count (:opaque-blocks flow)))
             "a malformed block still draws, rather than vanishing")))

     (testing "an odd bindings vector warns but still classifies"
       (let [flow (parser/extract-models "(doseq [x] (println x))")]
         (is (= 1 (count (:blocks flow))))
         (is (= [:warning] (mapv :severity (:diagnostics flow))))
         (is (contains? (diagnostic-codes flow) :block-malformed))))))

#?(:clj
   (deftest blocks-no-longer-land-as-opaque-blocks
     (testing "the whole point: they stop counting against the residue measure"
       (let [src (str "(doseq [x xs] (println x))\n"
                      "(when p (println \"y\"))\n"
                      "(try (println \"z\") (catch Exception e nil))\n"
                      "(defmacro m [x] x)")
             flow (parser/extract-models src {:opaque-blocks true})]
         (is (= 3 (count (:blocks flow))))
         ;; the defmacro still is one, so this asserts a change, not an empty set
         (is (= 1 (count (:opaque-blocks flow))))
         (is (= #{:forbidden-form} (diagnostic-codes flow)))))))

#?(:clj
   (deftest a-block-head-does-not-shadow-an-op
     (testing "the classifier keys on the head symbol only at the top level"
       (let [flow (parser/extract-models
                   "(defn ^:op when [^:row p] (geo/area p))")]
         (is (= 0 (count (:blocks flow))))
         (is (some? (operation-by-name flow "when")))))))

;; A non-literal inline map keeps its entries as OpArgs. `{:rows parcels}`
;; is how a run/run-model call names its input, and without the entries the
;; reference lives only in the raw text.

#?(:clj
   (deftest a-computed-inline-map-keeps-its-entries
     (testing "the model-input idiom exposes the binding it references"
       (let [flow (parser/extract-models
                   "(def analyzed (time (run/run-model flow {:rows parcels} {:timeout-ms 10})))")
             call (-> flow :bindings first :args first)
             [_flow-ref in-map opts-map] (:args call)]
         (is (= :call (:kind call)))
         (is (= "run/run-model" (:head call)))
         (is (= :inline_map (:kind in-map)))
         (is (= [[:literal :parameter_ref]]
                (mapv (fn [[k v]] [(:kind k) (:kind v)]) (:entries in-map))))
         (is (= "parcels" (-> in-map :entries first second :parameter-name)))
         (is (some? (-> in-map :entries first second :location)))
         (testing "a fully literal map keeps :data and grows no entries"
           (is (nil? (:entries opts-map)))
           (is (= {:timeout-ms 10} (-> opts-map :inline-map :data))))))

     (testing "an odd map still pairs what it can rather than throwing"
       ;; `{:a}` alone is all-literal and so keeps the :data path; it takes a
       ;; non-literal value to reach the entries branch at all.
       (let [m (-> (parser/extract-models "(def x (f {:a b :c}))")
                   :bindings first :args first)]
         (is (= 2 (count (:entries m))))
         (is (= "b" (-> m :entries first second :parameter-name)))
         (is (nil? (-> m :entries second second)))))))

;; A collection literal that is not fully literal is a binding, not residue.
;; 21 of the corpus's 24 unclassified top-level defs were this shape.

#?(:clj
   (deftest a-config-map-def-is-a-binding
     (testing "(def opts {:thresholds t}) classifies and exposes what it names"
       (let [flow (parser/extract-models
                   "(def city-opts {:thresholds city-thresholds})"
                   {:opaque-blocks true})
             b (first (:bindings flow))]
         (is (= 0 (count (:diagnostics flow))))
         (is (= [] (:opaque-blocks flow)))
         (is (= "city-opts" (:name b)))
         (is (= :function_call (:body-shape b)))
         (is (= "{}" (:head-sym b)))
         (is (true? (:is-collection b)))
         (is (= ["city-thresholds"]
                (mapv :parameter-name
                      (filter #(= :parameter_ref (:kind %)) (:args b))))
             "the reference is reachable, which is what draws the edge")))))

#?(:clj
   (deftest a-computed-vector-def-is-a-binding
     (testing "(def points [(f/point 0 0) ...]) is data built by calls"
       (let [b (-> (parser/extract-models
                    "(def points [(f/point 0 0) (f/point 10 10)])")
                   :bindings first)]
         (is (= "[]" (:head-sym b)))
         (is (true? (:is-collection b)))
         (is (= [:call :call] (mapv :kind (:args b))))
         (is (= ["f/point" "f/point"] (mapv :head (:args b))))))

     (testing "a set keeps its items too"
       (let [b (-> (parser/extract-models "(def s #{a b})") :bindings first)]
         (is (= "#{}" (:head-sym b)))
         (is (= #{"a" "b"} (into #{} (map :parameter-name) (:args b))))))))

#?(:clj
   (deftest a-fully-literal-collection-stays-a-parameter
     (testing "nothing changed for the literal case: it decodes to :data"
       (let [flow (parser/extract-models "(def thresholds {:far 2.0 :units [4 6]})")
             p (first (:parameters flow))]
         (is (= 0 (count (:bindings flow))))
         (is (= "thresholds" (:name p)))
         (is (= {:far 2.0 :units [4 6]} (-> p :value :data)))))))

#?(:clj
   (deftest a-collection-binding-is-not-hunted-for-a-signature
     (testing "the type checker skips it, as it skips accessors and control forms"
       (let [flow (parser/extract-models
                   "(def opts {:thresholds t})"
                   {:opaque-blocks true})]
         (is (not (contains? (diagnostic-codes flow) :unresolved-op))
             "a map literal has no signature and never will")))))

;; An alias def. The three in the corpus were the last unclassified defs.

#?(:clj
   (deftest an-alias-def-is-a-binding
     (testing "(def output-dir data-dir) names what it aliases"
       (let [flow (parser/extract-models "(def output-dir data-dir)"
                                         {:opaque-blocks true})
             b (first (:bindings flow))]
         (is (= 0 (count (:diagnostics flow))))
         (is (= [] (:opaque-blocks flow)))
         (is (= "output-dir" (:name b)))
         (is (= "data-dir" (:head-sym b)) "the card reads output-dir <- data-dir")
         (is (true? (:is-alias b)))
         (is (= [:parameter_ref] (mapv :kind (:args b)))
             "the reference is reachable, which is what draws the edge")
         (is (= "data-dir" (-> b :args first :parameter-name)))))

     (testing "a namespaced alias is a symbol ref, so it draws no edge"
       (let [b (-> (parser/extract-models "(def x other/y)") :bindings first)]
         (is (= "other/y" (:head-sym b)))
         (is (= [:symbol_ref] (mapv :kind (:args b))))))

     (testing "and it is not hunted for a signature"
       (is (not (contains? (diagnostic-codes
                            (parser/extract-models "(def output-dir data-dir)"))
                           :unresolved-op))))))

#?(:clj
   (deftest a-binding-carries-the-whole-def-span
     (testing ":form-location is the def form; :location stays the RHS"
       ;; A caret on `(def study-analyzed` matched no node at all while the only
       ;; span recorded was the RHS's, which starts on the next line.
       (let [src "(def study-analyzed\n  (ds/rows parcels :as-maps))"
             b (-> (parser/extract-models src) :bindings first)]
         (is (= 1 (-> b :form-location :start-line)))
         (is (= 2 (-> b :form-location :end-line)))
         (is (= 2 (-> b :location :start-line))
             "the RHS span is where a diagnostic about the call belongs")))

     (testing "a threading binding carries it too"
       (let [src "(def m\n  (item-> :parcels (geo/buffer 10)))"
             b (-> (parser/extract-models src) :bindings first)]
         (is (= 1 (-> b :form-location :start-line)))))))

#?(:clj
   (deftest a-parameter-carries-its-editable-fields
     (testing "a scalar is one field, with the span of the literal"
       (let [p (-> (parser/extract-models "(def crs \"EPSG:2249\")") :parameters first)]
         (is (= [{:kind :string :raw "\"EPSG:2249\""
                  :location {:start-line 1 :start-column 10 :end-line 1 :end-column 21}}]
                (:fields p)))))

     (testing "each value of a flat map is a field under the text of its key"
       (let [src "(def scope {:district \"Jamaica Plain Neighborhood\"\n            :limit 40 :dry? true :mode :fast :pad nil})"
             p (-> (parser/extract-models src) :parameters first)]
         (is (= [":district" ":limit" ":dry?" ":mode" ":pad"] (mapv :key (:fields p))))
         (is (= [:string :number :boolean :keyword :nil] (mapv :kind (:fields p))))
         (is (= ["\"Jamaica Plain Neighborhood\"" "40" "true" ":fast" "nil"]
                (mapv :raw (:fields p))))
         (is (= {:start-line 1 :start-column 23 :end-line 1 :end-column 51}
                (:location (first (:fields p)))))
         (is (= {:start-line 2 :start-column 20 :end-line 2 :end-column 22}
                (:location (second (:fields p)))))))

     (testing "the columns count characters: a span after a non-ASCII character is exact"
       (let [f (-> (parser/extract-models "(def m {:name \"Säo\" :n 2})") :parameters first :fields second)]
         (is (= {:start-line 1 :start-column 24 :end-line 1 :end-column 25} (:location f)))))

     (testing "the members of a flat vector are fields with no key"
       (let [p (-> (parser/extract-models "(def ks [:MAP_PAR_ID :id])") :parameters first)]
         (is (= [":MAP_PAR_ID" ":id"] (mapv :raw (:fields p))))
         (is (every? nil? (map :key (:fields p))))))

     (testing "a nested, long, empty or set literal has no fields"
       (doseq [src ["(def a {:k [1 2]})"
                    "(def b [1 2 3 4 5 6 7 8 9])"
                    "(def c {})"
                    "(def d #{:a :b})"]]
         (let [p (-> (parser/extract-models src) :parameters first)]
           (is (some? p) src)
           (is (not (contains? p :fields)) src))))))
