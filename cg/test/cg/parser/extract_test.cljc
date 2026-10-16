;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.extract-test
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [cg.parser.extract :as extract]
                    [cg.parser.setup :as setup]
                    [cg.util :as util])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["web-tree-sitter" :refer [Parser Tree]]
                     ["../../../src/cg/parser/extract.mjs" :as extract]
                     ["./setup.mjs" :as setup]
                     ["../../../src/cg/parser/core.mjs" :as parser]
                     ["../../../src/cg/parser/squint.mjs" :as backend]
                     ["../../../src/cg/util.mjs" :as util]
                     ["../test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(defn- model-by-name [flow name]
  (first (filter #(= (:name %) name) (extract/threading-macro-bindings flow))))

(defn- diagnostic-codes [flow]
  (into #{} (map :code) (:diagnostics flow)))

(deftest model-inspection-fixture
  (let [flow (setup/extract-models (util/read-file "test/fixtures/model-inspection.cg"))]
    (is (= 0 (count (:diagnostics flow))))
    (is (= ["cg.macros" "cg.geo"] (mapv :namespace (:requires flow))))
    (is (= 1 (count (extract/threading-macro-bindings flow))))
    (let [m (model-by-name flow "my-model")]
      (is (= :item (:mode m)))
      (is (= "parcels" (:input m)))
      (is (= ["geo/buffer" "geo/simplify" "geo/centroid"]
             (mapv :qualified-name (:ops m))))
      (is (nil? (:config m))))
    (is (nil? (:overlay flow)))))

(deftest threading-macros-fixture
  (let [flow (setup/extract-models (util/read-file "test/fixtures/threading-macros.cg"))]
    (is (= 0 (count (:diagnostics flow))))
    (is (= ["union-model" "configured-model"]
           (mapv :name (extract/threading-macro-bindings flow))))
    (testing "union-model is :coll mode"
      (let [m (model-by-name flow "union-model")]
        (is (= :coll (:mode m)))))
    (testing "configured-model has trailing config map"
      (let [m (model-by-name flow "configured-model")]
        (is (= 1 (count (:ops m)))
            "trailing map must be classified as config, not as an op")
        (is (some? (:config m)))))))

(deftest test-load-model-fixture
  (let [flow (setup/extract-models (util/read-file "test/fixtures/test-load-model.cg"))]
    (is (= 0 (count (:diagnostics flow))))
    (is (some? (-> flow :overlay :raw)))
    (is (some? (-> flow :overlay :location)))))

(deftest script-style-fixture-negative
  (let [flow (setup/extract-models (util/read-file "test/fixtures/script-style.cg"))
        codes (diagnostic-codes flow)]
    (is (contains? codes :unclassified-top-level-form))
    (is (= 2 (count (extract/threading-macro-bindings flow)))
        "the two top-level models are buffer-flow and export-flow; the table-> in the body of reproject-to is not a model")))

(deftest a-call-def-is-a-binding
  (doseq [[src binding]
          [["(def parcels (ds/load-geojson \"parcels.geojson\"))"
            ["parcels" "ds/load-geojson" :function_call true [:literal]]]
           ["(def combined (ds/concat area-metrics other/base-metrics))"
            ["combined" "ds/concat" :function_call false [:parameter_ref :symbol_ref]]]
           ["(def t (gcrs/create-transform 4326 2249 input-ds))"
            ["t" "gcrs/create-transform" :function_call false [:literal :literal :parameter_ref]]]]]
    (let [flow (setup/extract-models src)]
      (is (= 0 (count (:diagnostics flow))) src)
      (is (= [binding]
             (mapv (juxt :name :head-sym :body-shape :is-dataset-source #(mapv :kind (:args %)))
                   (:bindings flow)))
          src))))

(deftest layers-threading-fence
  (testing "layers-> has no implementation yet"
    (let [flow (setup/extract-models
                "(def scoped (layers-> :parcels (geo/buffer 100)))")
          codes (diagnostic-codes flow)
          m (model-by-name flow "scoped")]
      (is (contains? codes :threading-mode-unimplemented)
          "decoding layers-> emits the unimplemented diagnostic, not a throw")
      (is (some? m) "the layers-> binding still extracts as a partial model")
      (is (nil? (:mode m)) "mode stays nil until :scope has an implementation"))))

(deftest binding-accepts-nested-call-arg
  (testing "a binding with a nested (call) arg is accepted, with the nested call captured as a recursive :call OpArg"
    (let [flow (setup/extract-models
                "(def good (foo (nested 1 :k)))")]
      (is (= ["foo"] (mapv :head-sym (:bindings flow))))
      (let [b (first (:bindings flow))
            nested (first (:args b))]
        (is (= [[:call "nested"]] (mapv (juxt :kind :head) (:args b))))
        (is (= [:literal :literal] (mapv :kind (:args nested))))))))

(deftest binding-malformed-quoted-arg
  (let [flow (setup/extract-models
              "(def bad (foo 'quoted))")
        codes (diagnostic-codes flow)]
    (is (= 0 (count (:bindings flow))))
    (is (contains? codes :binding-malformed))))

(deftest a-def-rhs-outside-the-vocabulary-is-unclassified
  (testing "a quoted RHS reaches none of the branches"
    (let [flow (setup/extract-models "(def x '(a b))")
          codes (diagnostic-codes flow)]
      (is (contains? codes :unclassified-top-level-form)))))

(deftest ops-carry-emits-level-and-component
  (let [flow (setup/extract-models
              "(def out (table-> parcels (filter :zone :in zs) (group-by :zone) (map analyze-nbhd) (into {})))")
        ops (:ops (first (:bindings flow)))]
    (testing "each spine op is annotated with the level it emits (the converter colors the outgoing edge from this)"
      (is (= [["filter" :dataset] ["group-by" :groups] ["map" :groups] ["into" :map]]
             (mapv (juxt :qualified-name :emits-level) ops))))
    (testing "(map <op-name>) lifts the referenced name onto :component (eval-free)"
      (is (= [nil nil "analyze-nbhd" nil] (mapv :component ops))
          "component name carried by reference, not resolved; other spine ops carry none"))))

(deftest checkpoint-carries-component-name
  (let [flow (setup/extract-models
              (str "(def out (table-> rows (cg.cache/checkpoint"
                   " {:dir \"d\" :prefix \"p\" :layer \"l\" :key [1]} impervious-join)))"))
        out-b (first (:bindings flow))
        cp (first (filter #(= "cg.cache/checkpoint" (:qualified-name %)) (:ops out-b)))]
    (is (= "impervious-join" (:component cp)))))

(deftest binding-is-dataset-source-classification
  (testing "threading-macro Binding has no :is-dataset-source (function-call only)"
    (let [flow (setup/extract-models
                "(def my-model (item-> :input (geo/buffer 10)))")
          b (first (:bindings flow))]
      (is (= :threading_macro (:body-shape b)))
      (is (not (contains? b :is-dataset-source))))))

(deftest eager-threading-macro-binding-admission
  (testing "a keyword, parameter-ref or symbol-ref first arg admits (no :non-keyword-data-ref); only a keyword makes a template"
    (doseq [[src binding]
            [["(def t (item-> :parcels (geo/buffer 10)))" [:threading_macro true :literal "parcels"]]
             ["(def t (item-> parcels (geo/buffer 10)))" [:threading_macro false :parameter_ref nil]]
             ["(def t (item-> other/parcels (geo/buffer 10)))" [:threading_macro false :symbol_ref nil]]]]
      (let [flow (setup/extract-models src)]
        (is (= 0 (count (:diagnostics flow))) src)
        (is (= [binding] (mapv (juxt :body-shape :is-template #(-> % :source :kind) :input) (:bindings flow)))
            src))))
  (testing "list-form first-arg still fires :non-keyword-data-ref (not OpArg-shaped)"
    (is (contains? (diagnostic-codes (setup/extract-models "(def t (item-> (foo bar) (geo/buffer 10)))"))
                   :non-keyword-data-ref))))

(defn- operation-by-name [flow name]
  (first (filter #(= (:name %) name) (:operations flow))))

(deftest an-op-with-a-pipeline-body
  (let [flow (setup/extract-models
              (str "(defn ^:op area\n"
                   "  {:outputs {:area {:type :number}}}\n"
                   "  [^:table ^{:type :feature} parcel]\n"
                   "  (item-> :parcel (geo/area)))"))]
    (is (= 0 (count (:diagnostics flow))))
    (let [op (operation-by-name flow "area")]
      (is (= [["parcel" "feature" :table false]]
             (mapv (juxt :name :type :role :optional) (:inputs op))))
      (is (= [["area" "number"]] (mapv (juxt :name :type) (:outputs op))))
      (is (= [:pipeline] (mapv :kind (:body op))))
      (let [b (first (:body op))]
        (is (some? (:raw b)))
        (is (some? (:location b))))
      (let [g (:body-graph op)]
        (is (= [:area-1] (mapv :id (:nodes g)))
            ":id matches runtime generate-node-id contract :<unq-name>-<counter>")
        (is (= "geo/area" (:fn (first (:nodes g))))
            ":fn preserves the qualified-name as a string")
        (is (= [] (:edges g)))
        (is (= :item (:mode g)))
        (is (= "parcel" (:input g)))))))

(deftest op-body-graph-multi-step
  (let [flow (setup/extract-models
              (str "(defn ^:op get-primary-zoning-info [^:table zone-info]\n"
                   "  (item-> :zone-info\n"
                   "    (score-zoning-intersections :zoning-dataset)\n"
                   "    (ds/max-by :intersection-area)\n"
                   "    (zone-to-primary-attrs)))"))
        op (operation-by-name flow "get-primary-zoning-info")
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
           (:edges g)))))

(deftest op-body-pipeline-decoded-structurally
  (testing ":pipeline body forms decode like top-level Models"
    (let [flow (setup/extract-models
                (str "(defn ^:op pipe [^:table parcel]\n"
                     "  (item-> :parcel (geo/buffer 10) (geo/centroid)))"))]
      (is (= 0 (count (:diagnostics flow))))
      (let [op (operation-by-name flow "pipe")
            b (first (:body op))]
        (is (= :pipeline (:kind b)))
        (is (= :item (:mode b)))
        (is (= "parcel" (:input b)))
        (is (= ["geo/buffer" "geo/centroid"]
               (mapv :qualified-name (:ops b))))))))

(deftest a-when-in-an-op-body-is-an-expr
  (testing "a when in an op body is plain Clojure, because an op body runs as written"
    (let [flow (setup/extract-models
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
      (is (= :expr (-> (operation-by-name flow "k") :body first :tail first :kind))))))

(deftest op-admits-reducer-role
  (testing "^:reducer joins ^:row and ^:table as a role flag"
    (let [flow (setup/extract-models
                (str "(defn ^:op reduce-areas [^:reducer group]\n"
                     "  (reduce + 0 (map geo/area group)))"))]
      (is (= 0 (count (:diagnostics flow))))
      (is (= [["group" :reducer]]
             (mapv (juxt :name :role) (:inputs (operation-by-name flow "reduce-areas"))))))))

(deftest println-classifies-as-output
  (testing "a bare top-level println is an OutputDecl, not a diagnostic"
    (let [flow (setup/extract-models "(println \"hello\")")]
      (is (= 0 (count (:diagnostics flow))))
      (is (= ["println"] (mapv :head (:outputs flow))))
      (let [o (first (:outputs flow))]
        (is (= "(println \"hello\")" (:raw o)))
        (is (some? (:location o)))))))

(deftest println-carries-a-literal-only-when-it-has-one
  (testing "a string literal rides :text so the canvas can draw a rule or a spacer; other arguments leave :text nil"
    (doseq [[src outputs]
            [["(println \"\")\n(println \"=====\")" [[1 "" ["\"\""]] [1 "=====" ["\"=====\""]]]]
             ["(println (str \"HOUSING STOCK — \" title))" [[1 nil ["(str \"HOUSING STOCK — \" title)"]]]]
             ["(println \"n =\" n)" [[2 nil ["\"n =\"" "n"]]]]
             ["(println)" [[0 nil []]]]]]
      (let [flow (setup/extract-models src)]
        (is (= 0 (count (:diagnostics flow))) src)
        (is (= outputs (mapv (juxt :arity :text :args) (:outputs flow))) src))))

  (testing "a computed argument falls back to :label"
    (is (= "(println (str \"HOUSING STOCK — \" title))"
           (-> (setup/extract-models "(println (str \"HOUSING STOCK — \" title))")
               :outputs first :label)))))

(deftest println-label-is-one-truncated-line
  (testing "a multi-line println gives the canvas a single-line label"
    (let [src (str "(println (str \"a\"\n"
                   "              \"b\"))")
          o (first (:outputs (setup/extract-models src)))]
      (is (= "(println (str \"a\"" (:label o)))
      (is (= src (:raw o)))
      (let [long-src (str "(println \"" (apply str (repeat 60 "a")) "\")")]
        (is (= (str (subs long-src 0 60) " …")
               (-> (setup/extract-models long-src) :outputs first :label)))))))

(deftest println-inside-a-form-is-not-a-top-level-output
  (testing "only the top level classifies; a nested println rides its parent"
    (let [flow (setup/extract-models "(doseq [x xs] (println x))")]
      (is (= 0 (count (:outputs flow))))
      (is (= [:output] (mapv :kind (-> flow :blocks first :body)))))

  (testing "a println inside a form the vocabulary does not reach stays hidden"
    (let [flow (setup/extract-models
                "(with-open [w (writer f)] (println w))")]
      (is (= 0 (count (:outputs flow))))
      (is (contains? (diagnostic-codes flow) :unclassified-top-level-form))))))

(deftest println-does-not-shadow-an-op-named-println
  (testing "the classifier keys on the head symbol only at the top level"
    (let [flow (setup/extract-models
                "(defn ^:op println [^:row p] (geo/area p))")]
      (is (some? (operation-by-name flow "println"))))))

(deftest literal-nil-classifies-as-a-parameter
  (testing "(def x nil) is a literal, and its :data is a present nil"
    (let [flow (setup/extract-models "(def missing nil)")
          p (first (:parameters flow))]
      (is (= 0 (count (:diagnostics flow))))
      (is (= "missing" (:name p)))
      (is (= :nil (-> p :value :kind)))
      (is (contains? (:value p) :data))
      (is (nil? (-> p :value :data))))))

;; On JS a keyword is a string, so both literals spell the old marker.
(deftest a-literal-that-spells-the-undecodable-marker-keeps-its-data
  (let [flow (setup/extract-models
              (str "(def s \"cg.parser.extract/undecodable\")\n"
                   "(def k :cg.parser.extract/undecodable)"))]
    (is (= ["cg.parser.extract/undecodable" :cg.parser.extract/undecodable]
           (mapv #(-> % :value :data) (:parameters flow))))))

(deftest a-def-name-drops-its-metadata
  (testing "^:private and a ^{...} map are not part of the bound name"
    (let [flow (setup/extract-models
                (str "(def ^:private limits [:a :b])\n"
                     "(def ^{:doc \"d\"} spec {:k (f limits)})\n"
                     "(def ^:private loader (f limits))\n"
                     "(def ^:private m (table-> :src (g)))"))]
      (is (= ["limits"] (mapv :name (:parameters flow))))
      (is (= #{"spec" "loader" "m"} (into #{} (map :name) (:bindings flow)))))))

(deftest a-nil-inside-a-collection-no-longer-kills-the-decode
  (testing "nil is a value, not a decode failure"
    (let [flow (setup/extract-models
                "(def spec {:dimensions [{:label \"extend\" :cols [:a nil]}]})")
          p (first (:parameters flow))]
      (is (= 0 (count (:diagnostics flow))))
      (is (= :mapv (-> p :value :kind)))
      (is (= {:dimensions [{:label "extend" :cols [:a nil]}]}
             (-> p :value :data))))))

(deftest a-string-literal-decodes-its-escapes
  (let [flow (setup/extract-models
              (str "(def sep \"a\\tb\\u0041\\101\\400\\\"\\\\\")\n"
                   "(run/load-module \"C:\\\\data\\\\m.cg\")"))]
    (is (= "a\tbAA\\400\"\\" (-> flow :parameters first :value :data)))
    (is (= "C:\\data\\m.cg" (-> flow :loaded-modules first :path)))))

(deftest a-def-with-a-docstring-takes-the-value-after-it
  (let [p (-> (setup/extract-models "(def threshold \"Cut-off in meters.\" 10)") :parameters first)]
    (is (= "threshold" (:name p)))
    (is (= 10 (-> p :value :data)))))

(deftest a-number-decodes-the-same-on-each-runtime
  (doseq [[t data] [["10" 10] ["-1.5" -1.5] ["0x10" 16] ["010" :none] ["08" :none]
                    ["1N" :none] ["1M" :none] ["1/2" :none] ["2r101" :none] ["-0x10" :none]]]
    (is (= data (-> (setup/extract-models (str "(def p " t ")")) :parameters first :value (get :data :none)))
        t)))

(deftest a-short-defn-or-def-is-malformed
  (doseq [[src code] [["(defn)" :defn-malformed] ["(defn f)" :defn-malformed]
                      ["(defn f \"doc\")" :defn-malformed] ["(defn f {:a 1})" :defn-malformed]
                      ["(def x ())" :unclassified-top-level-form]]]
    (is (= [code] (mapv :code (:diagnostics (setup/extract-models src)))) src)))

(deftest a-discarded-form-is-not-read
  (let [flow (setup/extract-models
              (str "#_(def x 1)\n"
                   "(def y #_old 2)\n"
                   "(def m (item-> :parcels #_(geo/buffer 1) (geo/area)))"))]
    (is (empty? (:diagnostics flow)))
    (is (= [["y" 2]] (mapv (juxt :name #(-> % :value :data)) (:parameters flow))))
    (is (= [["geo/area"]] (mapv #(mapv :qualified-name (:ops %)) (:bindings flow))))))

(deftest a-layers-sink-keeps-its-warning
  (let [sink (setup/extract-models "(layers-> :parcels (ds/write-csv \"out.csv\"))")
        bare (setup/extract-models "(layers-> :parcels (ds/transform-crs \"EPSG:2249\"))")]
    (is (= 1 (count (:sinks sink))))
    (is (= #{:threading-mode-unimplemented} (diagnostic-codes sink)))
    (is (= #{:threading-mode-unimplemented :unsinked-top-level-pipeline}
           (diagnostic-codes bare)))))

(deftest a-param-type-can-be-a-string-or-a-symbol
  (let [flow (setup/extract-models "(defn ^:op f [^{:type \"table\"} t ^{:type table} u] t)")]
    (is (= ["table" "table"] (mapv :type (:inputs (operation-by-name flow "f")))))))

(deftest a-require-reads-each-libspec
  (is (= [["a.b" "x"] ["c.d" "y"]]
         (mapv (juxt :namespace :alias)
               (:requires (setup/extract-models "(require '[a.b :as x] '[c.d :as y])")))))
  (is (= [:require-malformed]
         (mapv :code (:diagnostics (setup/extract-models "(require '[a.b :as x] c.d)"))))))

(deftest literal-sets-classify-but-do-not-decode
  (testing "a set is a literal, so the def is a parameter; a set anywhere suppresses :data for the whole value"
    (doseq [[src value] [["(def sources #{:rs-2 :rs-3})" {:kind :set :raw "#{:rs-2 :rs-3}"}]
                         ["(def target-sources {:rs-4 #{:rs-2 :rs-3}})" {:kind :mapv :raw "{:rs-4 #{:rs-2 :rs-3}}"}]]]
      (let [flow (setup/extract-models src)]
        (is (= 0 (count (:diagnostics flow))) src)
        (is (= [value] (mapv :value (:parameters flow))) src)))))

(deftest keyword-access-def-classifies-as-a-binding
  (testing "(def x (:key y)) is a function_call binding with a keyword head"
    (let [flow (setup/extract-models "(def analyzed-rows (:analyzed-rows analysis))")
          b (first (:bindings flow))]
      (is (= 0 (count (:diagnostics flow))))
      (is (= "analyzed-rows" (:name b)))
      (is (= :function_call (:body-shape b)))
      (is (= ":analyzed-rows" (:head-sym b)))
      (is (true? (:is-accessor b)))
      (is (false? (:is-dataset-source b)))
      ;; convert.cljs/param-refs keys on parameter_ref + parameter-name,
      ;; so a flat accessor gets its `analysis -> analyzed-rows` edge for free.
      (is (= [[:parameter_ref "analysis"]] (mapv (juxt :kind :parameter-name) (:args b)))
          "the source rides as a parameter_ref arg, which is what draws the edge")))

  (testing "a nested accessor reaches through two levels"
    ;; The source ref sits inside a :call arg. convert.cljs/param-refs
    ;; does not follow those, so this binding draws as a node without
    ;; an incoming edge — pre-existing canvas behavior for every
    ;; nested-call binding, not specific to accessors.
    (let [flow (setup/extract-models
                "(def debug-edges (:debug-edges (:debug-layers analysis)))")
          b (first (:bindings flow))]
      (is (= 0 (count (:diagnostics flow))))
      (is (= ":debug-edges" (:head-sym b)))
      (is (true? (:is-accessor b)))
      (is (= [[:call ":debug-layers"]] (mapv (juxt :kind :head) (:args b))))
      (is (= "analysis" (-> b :args first :args first :parameter-name))))))

;; (def census-blocks (or ...)) and (def cards (try ...)) reported
;; "operation 'or' is not in the built-in registry or any user op",
;; which tells the reader to go register an op named `or`.

(deftest control-form-binding-is-not-an-unresolved-op
  (testing "an or, try or await RHS says what is true, not that its head is missing"
    (doseq [src ["(def census-blocks (or (:census-blocks analysis) :from-cache))"
                 (str "(def cards (try (zoning/zone-tables rows)\n"
                      "                (catch Exception t nil)))")
                 "(def city-hall (await (ds/ensure-dataset query)))"]]
      (let [codes (diagnostic-codes (setup/extract-models src))]
        (is (not (contains? codes :unresolved-op)) src)
        (is (contains? codes :control-form-binding) src))))

  (testing "the diagnostic is a warning, and the binding still exists, flagged"
    (let [flow (setup/extract-models "(def x (or a b))")
          d (first (filter #(= :control-form-binding (:code %)) (:diagnostics flow)))]
      (is (= :warning (:severity d)) "unmodelable is not malformed")
      (is (some? (:location d)))
      (is (= [["x" "or" true]] (mapv (juxt :name :head-sym :is-control) (:bindings flow)))
          "so the canvas keeps showing it")))

  (testing "an ordinary unknown head is still an unresolved op"
    ;; The message is correct there: no-such-op could be registered.
    (let [flow (setup/extract-models "(def x (no-such-op parcels))")]
      (is (contains? (diagnostic-codes flow) :unresolved-op))
      (is (not (contains? (diagnostic-codes flow) :control-form-binding)))))

  (testing "a module entry point stays an unresolved op, which is the finding"
    ;; nh/ensure-analysis! is a plain defn in the followed module. That
    ;; diagnostic is absorption evidence and must not be muffled.
    (let [flow (setup/extract-models "(def analysis (nh/ensure-analysis!))")]
      (is (contains? (diagnostic-codes flow) :unresolved-op)))))

(deftest op-body-admits-plain-expressions
  (testing "a body form the parser does not model decodes to :expr, not a diagnostic"
    (let [flow (setup/extract-models
                "(defn ^:op plain [^:row p] (reduce + 0 [1 2 3]))")]
      (is (empty? (:diagnostics flow))
          "an op body is ordinary Clojure")
      (let [op (operation-by-name flow "plain")
            bf (first (:body op))]
        (is (= [[:expr "reduce"]] (mapv (juxt :kind :head) (:body op))))
        (is (= "(reduce + 0 [1 2 3])" (:raw bf)))))))

(deftest op-comments-do-not-shift-the-walk
  (testing "comments inside an op are dropped, not read as its parts"
    ;; tree-sitter makes a comment a NAMED child. Before cg.parser.walk
    ;; filtered them, a comment took the place of the declaration after
    ;; it, and each later part moved by one: 86 diagnostics on parcel.cg
    ;; alone.
    (let [flow (setup/extract-models
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
            "a comment between binding pairs must not desynchronize them")))))

(deftest let-body-binding-carries-the-component-run-component-names
  (testing "run-component names its component in a string argument, not in its head"
    ;; The head is `run-component` at every such call site, so a head-based
    ;; lookup finds nothing and the heavy flow it runs stays unreachable from
    ;; the canvas. A city module calls each of its parcel flows this way.
    (let [flow (setup/extract-models
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
            bf    (first (:body op))]
        (is (= [["plain" nil] ["analyzed" "analyze-parcels-rd"]]
               (mapv (juxt :name :component) (:bindings bf))))
        (let [nodes (into {} (map (juxt :bound-name identity))
                          (:nodes (:body-graph op)))]
          (is (= "analyze-parcels-rd" (:component (get nodes "analyzed")))
              "and reaches the body-graph node the canvas reads")
          (is (= "run/run-component" (:rhs-fn (get nodes "analyzed")))
              "beside the head, which stays what the source says"))))))

(deftest let-body-component-needs-a-string-literal
  (testing "a computed component name carries nothing: the parser stays eval-free"
    (let [flow (setup/extract-models
                (str "(defn compute! [scope]\n"
                     "  (let [analyzed (run/run-component (name scope) {})]\n"
                     "    analyzed))"))
          op   (operation-by-name flow "compute!")]
      (is (= "run/run-component" (-> op :body first :bindings first :rhs-fn)))
      (is (nil? (-> op :body first :bindings first :component))))))

(deftest a-branching-binding-carries-what-its-branches-call
  (testing "the head names the control form, so the branches name themselves"
    ;; ensure-analysis! picks one of three compute functions by scope. A
    ;; reader who cannot see those three names on the card cannot see that
    ;; the program branches at all.
    (let [flow (setup/extract-models
                (str "(defn dispatch [scope]\n"
                     "  (let [plain  (ds/->dataset scope)\n"
                     "        picked (cond\n"
                     "                 (:city scope)     (compute-city! scope)\n"
                     "                 (:district scope) (compute-district! scope)\n"
                     "                 :else             (compute-full! scope))]\n"
                     "    picked))"))
          op   (operation-by-name flow "dispatch")]
      (is (= [["plain" nil]
              ["picked" ["compute-city!" "compute-district!" "compute-full!"]]]
             (mapv (juxt :name :rhs-calls) (:bindings (first (:body op)))))
          "every branch's call head, in source order, with `cond` itself dropped; an ordinary call has no :rhs-calls")
      (let [nodes (into {} (map (juxt :bound-name identity))
                        (:nodes (:body-graph op)))]
        (is (= 3 (count (:rhs-calls (get nodes "picked"))))
            "and it reaches the body-graph node the canvas reads")))))

(deftest a-branching-binding-drops-duplicate-and-nested-control-heads
  (testing "names are deduped; when/if/let name no callee a reader can follow"
    (let [flow (setup/extract-models
                (str "(defn guarded [scope]\n"
                     "  (let [done (when-not (:cached scope)\n"
                     "               (let [r (compute! scope)\n"
                     "                     s (compute! r)]\n"
                     "                 (merge r s)))]\n"
                     "    done))"))
          op   (operation-by-name flow "guarded")]
      (is (= ["compute!" "merge"]
             (-> op :body first :bindings first :rhs-calls))
          "the nested let and the second compute! call add nothing new"))))

(deftest a-fold-binding-carries-what-its-lambda-calls
  (testing "a reduce's value comes from its lambda, so the lambda names it"
    ;; A module can hold reduce bindings whose lambdas call a walk fn. Without the carry the canvas sees
    ;; `verdicts · reduce` and can descend into nothing.
    (let [flow (setup/extract-models
                (str "(defn walk [tables]\n"
                     "  (let [verdicts (reduce (fn [acc [suffix tbl]]\n"
                     "                           (merge acc (rule-verdicts suffix tbl)))\n"
                     "                         {} tables)]\n"
                     "    verdicts))"))
          op   (operation-by-name flow "walk")]
      (is (= ["fn" "merge" "rule-verdicts"]
             (-> op :body first :bindings first :rhs-calls))
          "every call head inside the fold, with `reduce` itself dropped"))))

(deftest a-run-component-naming-nothing-is-an-error
  (testing "the component is a STRING, so nothing else would catch a typo"
    (let [flow (setup/extract-models
                (str "(defn compute! [scope]\n"
                     "  (let [analyzed (run/run-component \"analyze-parcles-rd\" {})]\n"
                     "    analyzed))"))]
      (is (contains? (diagnostic-codes flow) :unknown-component)))))

(deftest an-op-row-contract-must-be-a-set
  (let [flow (setup/extract-models
              "(defn ^:op prepare {:row-reads [:geometry]} [^:row parcel-row] parcel-row)")]
    (is (contains? (diagnostic-codes flow) :op-malformed))))

(deftest an-op-output-without-type-emits-diagnostic
  (let [flow (setup/extract-models
              "(defn ^:op bad {:outputs {:o {:role :row}}} [^:row p] p)")]
    (is (contains? (diagnostic-codes flow) :op-malformed))
    (is (= [["o" nil]] (mapv (juxt :name :type) (:outputs (operation-by-name flow "bad")))))))

(deftest an-op-output-without-a-value-emits-diagnostic
  (let [flow (setup/extract-models "(defn ^:op f {:outputs {:a}} [x] x)")
        d (first (filter #(= :op-malformed (:code %)) (:diagnostics flow)))]
    (is (re-find #"entry is missing a value" (:message d)))
    (is (= {:start-line 1 :start-column 25 :end-line 1 :end-column 27}
           (:location d)))))

(deftest op-duplicate-names-detected
  (let [flow (setup/extract-models
              (str "(defn ^:op dup [^:row p] p)\n"
                   "(defn ^:op dup [^:row p] p)"))]
    (is (contains? (diagnostic-codes flow) :duplicate-operation-name))))

(deftest op-collides-with-parameter
  (let [flow (setup/extract-models
              (str "(def threshold 100)\n"
                   "(defn ^:op threshold [^:row p] p)"))]
    (is (contains? (diagnostic-codes flow) :name-collision))))

(deftest filter-by-column-checks-a-literal-predicate
  (doseq [[fires? src why]
          [[true "(def bad (table-> :ds (ds/filter-by-column :area :starts-with? 10)))"
            "unregistered predicate keyword in a threading body fires :unknown-predicate"]
           [true "(def bad (ds/filter-by-column ds :area :ends-with? 10))"
            "unregistered predicate in a function-call Binding (dataset explicit, pos 2) fires"]
           [true "(def bad (table-> :ds (filter-by-column :area :weirdo 10)))"
            "unprefixed filter-by-column head is matched the same way as ds/filter-by-column"]
           [false (str "(def pred-key :gt)\n"
                       "(def filtered (table-> :ds (ds/filter-by-column :area pred-key 10)))")
            "a parameter ref as the predicate is silent: the type checker owns it"]
           [false "(def big (table-> :ds (ds/filter-by-column :area :gt 1000)))"
            "a registered predicate is silent"]]]
    (is (= fires? (contains? (diagnostic-codes (setup/extract-models src)) :unknown-predicate))
        why)))

(deftest sink-terminated-bare-pipeline-promotes-to-sinks
  (testing "bare top-level threading-macro form whose terminal is @sink-kind lands in CgFlow.sinks"
    (let [flow (setup/extract-models
                (str "(table-> :reprojected\n"
                     "  (ds/write-geojson \"out.geojson\"))"))]
      (is (= [:table] (mapv :mode (:sinks flow))))
      (let [s (first (:sinks flow))]
        (is (= :keyword (-> s :source :literal :kind)))
        (is (= ":reprojected" (-> s :source :literal :raw)))
        (is (= ["ds/write-geojson"] (mapv :qualified-name (:ops s))))
        (is (nil? (:name s))
            "a SinkPipeline has no :name field; a sink is not a binding")))))

(deftest unsinked-bare-pipeline-fires
  (testing "bare top-level threading-macro whose terminal is :transformer-kind fires :unsinked-top-level-pipeline"
    (let [flow (setup/extract-models
                (str "(table-> :ds\n"
                     "  (ds/transform-crs \"EPSG:2249\"))"))]
      (is (zero? (count (:sinks flow))))
      (is (zero? (count (extract/threading-macro-bindings flow))))
      (is (= #{:unsinked-top-level-pipeline} (diagnostic-codes flow))))))

(deftest unsinked-pipeline-with-unknown-terminal-fires
  (let [flow (setup/extract-models
              (str "(table-> :ds\n"
                   "  (totally/missing 1))"))]
    (is (contains? (diagnostic-codes flow) :unsinked-top-level-pipeline))))

(deftest sink-pipeline-with-write-csv-also-classifies
  (testing "ds/write-csv (the other built-in sink) also triggers promotion"
    (let [flow (setup/extract-models
                (str "(table-> :ds\n"
                     "  (ds/write-csv \"out.csv\"))"))]
      (is (= 1 (count (:sinks flow))))
      (is (zero? (count (diagnostic-codes flow)))))))

(deftest sink-pipeline-malformed-source-still-surfaces
  (testing "shape errors in the bare threading-macro form surface their own diagnostics, not :unsinked-top-level-pipeline"
    ;; Literal/symbol/parameter-ref first-args are all admissible. A
    ;; list-form first-arg remains a true shape error (no OpArg promotion
    ;; possible) and exercises this path.
    (let [flow (setup/extract-models
                (str "(table-> (nested call)\n"
                     "  (ds/write-geojson \"out.geojson\"))"))]
      (is (contains? (diagnostic-codes flow) :non-keyword-data-ref))
      (is (not (contains? (diagnostic-codes flow) :unsinked-top-level-pipeline))))))

(deftest module-ns-form-recorded
  (let [flow (setup/extract-models
              (str "(ns cg.parcel\n"
                   "  \"Module docstring.\")\n"
                   "(require '[cg.geo :as geo])\n"))]
    (is (= "cg.parcel" (-> flow :module-ns :name)))
    (is (some? (-> flow :module-ns :location)))))

(deftest qualified-require-is-a-require
  (testing "(clojure.core/require ...) records its alias as (require ...) does"
    (let [flow (setup/extract-models
                (str "(ns city (:require [cg.run :as run]))\n"
                     "(run/load-module \"src/cg/parcel.cg\")\n"
                     "(clojure.core/require '[cg.parcel :as parcel])\n"))]
      (is (= [["cg.parcel" "parcel"]]
             (mapv (juxt :namespace :alias) (:requires flow))))
      (is (= ["src/cg/parcel.cg"] (mapv :path (:loaded-modules flow)))))))

(deftest a-module-defn-is-an-operation
  (testing "a defn is an operation whose card the canvas can draw and open"
    (let [flow (setup/extract-models
                (str "(ns cg.m \"Doc.\")\n"
                     "(defn- helper \"Adds.\" [a b] (when a (+ a b)))\n"
                     "(defn multi ([x] (multi x 1)) ([x y & more] (let [z (+ x y)] z)))\n"
                     "(declare later)\n"
                     "module-info\n"))
          ops  (into {} (map (juxt :name identity)) (:operations flow))]
      (is (empty? (:diagnostics flow)) "a when head is plain Clojure in a defn")
      (is (= [["a" false] ["b" false]]
             (mapv (juxt :name :optional) (:inputs (get ops "helper")))))
      (is (= "Adds." (:doc (get ops "helper"))))
      (is (= [["x" false] ["y" true] ["more" true]]
             (mapv (juxt :name :optional) (:inputs (get ops "multi")))))
      (is (= :let_body (:kind (first (:body (get ops "multi")))))))))

(deftest module-ns-first-wins
  (let [flow (setup/extract-models
              (str "(ns cg.first)\n"
                   "(ns cg.second)\n"))]
    (is (= "cg.first" (-> flow :module-ns :name)))))

(deftest module-ns-malformed-fires
  (let [flow (setup/extract-models "(ns \"cg.parcel\")")]
    (is (nil? (:module-ns flow)))
    (is (contains? (diagnostic-codes flow) :module-ns-malformed))))

(deftest literal-data-decoded-on-op-args
  (let [flow (setup/extract-models
              (str "(def s (table-> :rows"
                   " (summarize {:value :units :as-int true :weight :rows"
                   " :scopes [:rs-2] :buckets [{:label \"4-6\" :range [4 6]}]})))"))
        arg (-> flow :bindings first :ops first :args first)]
    (is (= :inline_map (:kind arg)))
    (is (= {:value :units :as-int true :weight :rows
            :scopes [:rs-2] :buckets [{:label "4-6" :range [4 6]}]}
           (-> arg :inline-map :data))
        "the decoded spec is plain data (JVM keywords; squint strings)")))

(deftest boolean-literals-classify
  (testing "true/false parse as bool_lit and decode (the sym_lit check never fired)"
    (let [flow (setup/extract-models "(def yes true)\n(def no false)")]
      (is (= [{:kind :boolean :raw "true" :data true} {:kind :boolean :raw "false" :data false}]
             (mapv :value (:parameters flow)))))))

(deftest opaque-blocks-flag-gated
  (let [src (str "(defmacro helper [x] x)\n"
                 "(helper 1)\n"
                 "(def m (item-> :parcels (geo/buffer 10)))")
        strict (setup/extract-models src)
        opq (setup/extract-models src {:opaque-blocks true})]
    (is (= [] (:opaque-blocks strict))
        "strict mode stays diagnostics-only")
    (is (= ["(defmacro helper [x] x)" "(helper 1)"] (mapv :label (:opaque-blocks opq)))
        "the defmacro (forbidden) + the bare call (unclassified) become blocks")
    (is (= "(defmacro helper [x] x)" (-> opq :opaque-blocks first :raw)))
    (is (some? (-> opq :opaque-blocks first :location)))
    (is (= (diagnostic-codes strict) (diagnostic-codes opq)))
    (is (= 1 (count (extract/threading-macro-bindings opq)))
        "modelable forms still classify normally")))

(deftest doseq-classifies-as-a-block
  (testing "a top-level doseq is a BlockDecl and not residue"
    (let [flow (setup/extract-models "(doseq [card cards] (println card))")
          b (first (:blocks flow))]
      (is (= 0 (count (:diagnostics flow))))
      (is (= :doseq (:kind b)))
      (is (= "doseq" (:head b)))
      (is (= [{:name "card" :rhs-raw "cards"}]
             (mapv #(select-keys % [:name :rhs-raw]) (:bindings b))))
      (is (= "(doseq [card cards] (println card))" (:raw b)))
      (is (some? (:location b))))))

(deftest a-doseq-binding-keeps-its-destructuring-form
  (testing "the name is source text, so [title rows] survives"
    (let [bs (-> (setup/extract-models
                  "(doseq [[title rows] scopes] (println title))")
                 :blocks first :bindings)]
      (is (= [["[title rows]" true "scopes"]] (mapv (juxt :name :destructured? :rhs-raw) bs))))))

(deftest a-block-body-decodes-recursively
  (testing "iteration over narration decodes all the way down"
    ;; Every leaf here is a form the parser already knows, which is why the
    ;; decode is deep rather than shallow.
    (let [src (str "(doseq [card cards]\n"
                   "  (println \"\")\n"
                   "  (doseq [line (lines card)]\n"
                   "    (println line)))")
          outer (-> (setup/extract-models src) :blocks first)
          [spacer inner] (:body outer)]
      (is (= [:output :block] (mapv :kind (:body outer))))
      (is (= "" (-> spacer :output :text)))
      (is (= :doseq (-> inner :block :kind)))
      (is (= [:output] (mapv :kind (-> inner :block :body))))
      (is (= "line" (-> inner :block :body first :output :args first))))))

(deftest a-body-form-outside-the-vocabulary-is-an-expr-leaf
  (testing "the honest leaf, carrying what an opaque card draws from"
    (let [flow (setup/extract-models
                "(doseq [[title rows] scopes] (district-report title rows))")
          leaf (-> flow :blocks first :body first)]
      (is (= :expr (:kind leaf)))
      (is (= "district-report" (:head leaf)))
      (is (= "(district-report title rows)" (:raw leaf)))
      (is (= "(district-report title rows)" (:label leaf)))
      (is (some? (:location leaf)))
      (is (= 0 (count (:diagnostics flow)))
          "it reports nothing, the same way an op body :expr does"))))

(deftest try-carries-its-catch-clause
  (testing "the failure path is modeled, which is not the same as run"
    (let [src (str "(try\n"
                   "  (doseq [c cards] (println c))\n"
                   "  (catch Exception t\n"
                   "    (println (str \"ERROR: \" (ex-message t)))))")
          b (-> (setup/extract-models src) :blocks first)
          c (first (:catches b))]
      (is (= :try (:kind b)))
      (is (= [:block] (mapv :kind (:body b))) "the catch is not in the body")
      (is (= [["Exception" "t"]] (mapv (juxt :class :binding) (:catches b))))
      (is (= [:output] (mapv :kind (:body c))))
      (is (nil? (:finally b))))))

(deftest try-carries-its-finally-clause
  (testing "finally decodes its own body too"
    (let [b (-> (setup/extract-models
                 "(try (println \"a\") (finally (println \"b\")))")
                :blocks first)]
      (is (= [:output] (mapv :kind (:body b))))
      (is (= [] (:catches b)))
      (is (= [:output] (mapv :kind (-> b :finally :body)))))))

(deftest when-carries-its-test-as-raw-source
  (testing "whether the test holds is a runtime question"
    (let [b (-> (setup/extract-models
                 "(when (:validation analysis) (println \"validating\"))")
                :blocks first)]
      (is (= :when (:kind b)))
      (is (= "(:validation analysis)" (-> b :test :raw)))
      (is (= ":validation" (-> b :test :head)))
      (is (some? (-> b :test :location)))
      (is (nil? (:bindings b))))))

(deftest if-let-and-let-classify-with-their-bindings
  (doseq [[src kind binding]
          [["(if-let [ds (:census-blocks analysis)] (println ds))"
            :if-let ["ds" ":census-blocks" "(:census-blocks analysis)" false]]
           ["(let [rows (filter :rd analyzed)] (println rows))"
            :let ["rows" "filter" "(filter :rd analyzed)" false]]]]
    (let [b (-> (setup/extract-models src) :blocks first)]
      (is (= kind (:kind b)) src)
      (is (= [binding] (mapv (juxt :name :rhs-fn :rhs-raw :destructured?) (:bindings b))) src))))

(deftest a-block-missing-its-binder-is-malformed-and-still-draws
  (testing "no bindings vector"
    (let [flow (setup/extract-models "(doseq (println 1))" {:opaque-blocks true})]
      (is (contains? (diagnostic-codes flow) :block-malformed))
      (is (= 1 (count (:opaque-blocks flow)))
          "a malformed block still draws, rather than vanishing")))

  (testing "an odd bindings vector warns but still classifies"
    (let [flow (setup/extract-models "(doseq [x] (println x))")]
      (is (= 1 (count (:blocks flow))))
      (is (= [[:block-malformed :warning]] (mapv (juxt :code :severity) (:diagnostics flow)))))))

(deftest outputs-and-blocks-no-longer-land-as-opaque-blocks
  (testing "the whole point: they stop counting against the residue measure"
    (let [src (str "(println \"a\")\n(println \"b\")\n"
                   "(doseq [x xs] (println x))\n"
                   "(when p (println \"y\"))\n"
                   "(try (println \"z\") (catch Exception e nil))\n"
                   "(defmacro m [x] x)")
          flow (setup/extract-models src {:opaque-blocks true})]
      (is (= 2 (count (:outputs flow))))
      (is (= 3 (count (:blocks flow))))
      ;; the defmacro still is one, so this asserts a change, not an empty set
      (is (= 1 (count (:opaque-blocks flow))))
      (is (= #{:forbidden-form} (diagnostic-codes flow))))))

(deftest a-computed-inline-map-keeps-its-entries
  (testing "the model-input idiom exposes the binding it references"
    (let [flow (setup/extract-models
                "(def analyzed (time (run/run-model flow {:rows parcels} {:timeout-ms 10})))")
          call (-> flow :bindings first :args first)
          [_flow-ref in-map opts-map] (:args call)]
      (is (= :call (:kind call)))
      (is (= "run/run-model" (:head call)))
      (is (= :inline_map (:kind in-map)))
      (is (= [[:literal :parameter_ref "parcels"]]
             (mapv (fn [[k v]] [(:kind k) (:kind v) (:parameter-name v)]) (:entries in-map))))
      (is (some? (-> in-map :entries first second :location)))
      (testing "a fully literal map keeps :data and grows no entries"
        (is (nil? (:entries opts-map)))
        (is (= {:timeout-ms 10} (-> opts-map :inline-map :data))))))

  (testing "an odd map still pairs what it can rather than throwing"
    ;; `{:a}` alone is all-literal and so keeps the :data path; it takes a
    ;; non-literal value to reach the entries branch at all.
    (let [m (-> (setup/extract-models "(def x (f {:a b :c}))")
                :bindings first :args first)]
      (is (= 2 (count (:entries m))))
      (is (= "b" (-> m :entries first second :parameter-name)))
      (is (nil? (-> m :entries second second))))))

(deftest a-config-map-def-is-a-binding
  (testing "(def opts {:thresholds t}) classifies and exposes what it names"
    (let [flow (setup/extract-models
                "(def city-opts {:thresholds city-thresholds})")
          b (first (:bindings flow))]
      (is (= 0 (count (:diagnostics flow))))
      (is (= "city-opts" (:name b)))
      (is (= :function_call (:body-shape b)))
      (is (= "{}" (:head-sym b)))
      (is (true? (:is-collection b)))
      (is (= ["city-thresholds"]
             (mapv :parameter-name
                   (filter #(= :parameter_ref (:kind %)) (:args b))))
          "the reference is reachable, which is what draws the edge"))))

(deftest a-computed-vector-def-is-a-binding
  (testing "(def points [(f/point 0 0) ...]) is data built by calls"
    (let [b (-> (setup/extract-models
                 "(def points [(f/point 0 0) (f/point 10 10)])")
                :bindings first)]
      (is (= "[]" (:head-sym b)))
      (is (true? (:is-collection b)))
      (is (= [[:call "f/point"] [:call "f/point"]] (mapv (juxt :kind :head) (:args b))))))

  (testing "a set keeps its items too"
    (let [b (-> (setup/extract-models "(def s #{a b})") :bindings first)]
      (is (= "#{}" (:head-sym b)))
      (is (= #{"a" "b"} (into #{} (map :parameter-name) (:args b)))))))

(deftest a-fully-literal-collection-stays-a-parameter
  (testing "nothing changed for the literal case: it decodes to :data"
    (let [flow (setup/extract-models "(def thresholds {:far 2.0 :units [4 6]})")
          p (first (:parameters flow))]
      (is (= "thresholds" (:name p)))
      (is (= {:far 2.0 :units [4 6]} (-> p :value :data))))))

(deftest an-alias-def-is-a-binding
  (testing "(def output-dir data-dir) names what it aliases"
    (let [flow (setup/extract-models "(def output-dir data-dir)")
          b (first (:bindings flow))]
      (is (= 0 (count (:diagnostics flow))))
      (is (= "output-dir" (:name b)))
      (is (= "data-dir" (:head-sym b)) "the card reads output-dir <- data-dir")
      (is (true? (:is-alias b)))
      (is (= [[:parameter_ref "data-dir"]] (mapv (juxt :kind :parameter-name) (:args b)))
          "the reference is reachable, which is what draws the edge")))

  (testing "a namespaced alias is a symbol ref, so it draws no edge"
    (let [b (-> (setup/extract-models "(def x other/y)") :bindings first)]
      (is (= "other/y" (:head-sym b)))
      (is (= [:symbol_ref] (mapv :kind (:args b)))))))

(deftest a-binding-carries-the-whole-def-span
  (testing ":form-location is the def form; :location stays the RHS"
    ;; A caret on `(def study-analyzed` matched no node at all while the only
    ;; span recorded was the RHS's, which starts on the next line.
    (let [src "(def study-analyzed\n  (ds/rows parcels :as-maps))"
          b (-> (setup/extract-models src) :bindings first)]
      (is (= 1 (-> b :form-location :start-line)))
      (is (= 2 (-> b :form-location :end-line)))
      (is (= 2 (-> b :location :start-line))
          "the RHS span is where a diagnostic about the call belongs")))

  (testing "a threading binding carries it too"
    (let [src "(def m\n  (item-> :parcels (geo/buffer 10)))"
          b (-> (setup/extract-models src) :bindings first)]
      (is (= 1 (-> b :form-location :start-line))))))

(deftest a-parameter-carries-its-editable-fields
  (testing "a scalar is one field, with the span of the literal"
    (let [p (-> (setup/extract-models "(def crs \"EPSG:2249\")") :parameters first)]
      (is (= [{:kind :string :raw "\"EPSG:2249\""
               :location {:start-line 1 :start-column 10 :end-line 1 :end-column 21}}]
             (:fields p)))))

  (testing "each value of a flat map is a field under the text of its key"
    (let [src "(def scope {:district \"Jamaica Plain Neighborhood\"\n            :limit 40 :dry? true :mode :fast :pad nil})"
          p (-> (setup/extract-models src) :parameters first)]
      (is (= [":district" ":limit" ":dry?" ":mode" ":pad"] (mapv :key (:fields p))))
      (is (= [:string :number :boolean :keyword :nil] (mapv :kind (:fields p))))
      (is (= ["\"Jamaica Plain Neighborhood\"" "40" "true" ":fast" "nil"]
             (mapv :raw (:fields p))))
      (is (= {:start-line 1 :start-column 23 :end-line 1 :end-column 51}
             (:location (first (:fields p)))))
      (is (= {:start-line 2 :start-column 20 :end-line 2 :end-column 22}
             (:location (second (:fields p)))))))

  (testing "the columns count characters: a span after a non-ASCII character is exact"
    (let [f (-> (setup/extract-models "(def m {:name \"Säo\" :n 2})") :parameters first :fields second)]
      (is (= {:start-line 1 :start-column 24 :end-line 1 :end-column 25} (:location f)))))

  (testing "the members of a flat vector are fields with no key"
    (let [p (-> (setup/extract-models "(def ks [:MAP_PAR_ID :id])") :parameters first)]
      (is (= [[":MAP_PAR_ID" nil] [":id" nil]] (mapv (juxt :raw :key) (:fields p))))))

  (testing "a nested, long, empty or set literal has no fields"
    (doseq [src ["(def a {:k [1 2]})"
                 "(def b [1 2 3 4 5 6 7 8 9])"
                 "(def c {})"
                 "(def d #{:a :b})"]]
      (let [p (-> (setup/extract-models src) :parameters first)]
        (is (some? p) src)
        (is (not (contains? p :fields)) src)))))

#?(:cljs
   (defn- record-calls!
     "Wrap the method `k` of `proto` so that each call adds its receiver to
      the atom `!seen`. Returns a fn that puts the method back."
     [proto k !seen]
     (let [f (aget proto k)]
       (aset proto k (fn [& args] (this-as self (swap! !seen conj self) (.apply f self args))))
       (fn [] (aset proto k f)))))

#?(:cljs
   (deftest extract-models-frees-each-tree-and-keeps-one-parser
     (let [!parsers (atom [])
           !trees (atom [])
           restore-parse (record-calls! (.-prototype Parser) "parse" !parsers)
           restore-delete (record-calls! (.-prototype Tree) "delete" !trees)]
       (-> (parser/extract-models "(def a 1)")
           (.then (fn [_] (parser/extract-models "(def b 2)")))
           (.finally (fn [] (restore-parse) (restore-delete)))
           (.then (fn [_]
                    (is (= 2 (count @!parsers)))
                    (is (= 1 (count (set @!parsers))))
                    (is (= 2 (count @!trees)))))))))

;; The footer runs an init with a missing grammar before the init of the
;; suite. Parser.init replaces the wasm module of web-tree-sitter, so this
;; cannot run inside a test.
#?(:cljs (def ^:private !inits (atom [])))

#?(:cljs
   (deftest a-failed-init-is-not-cached
     (is (= [:failed :ok] @!inits))))

#?(:cljs (-> (backend/init! {:grammar-wasm "/no/such.wasm"})
             (.then (fn [_] :ok) (fn [_] :failed))
             (.then (fn [r] (swap! !inits conj r) (setup/init!)))
             (.then (fn [_] :ok) (fn [_] :failed))
             (.then (fn [r]
                      (swap! !inits conj r)
                      (run_tests_and_exit_BANG_ "cg.parser.extract-test")))))
