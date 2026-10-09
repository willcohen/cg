;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.dataset-test
  "Tests for the dataset runtime ops, among them coalesce, write-geojson,
   write-csv, partition-by and grouped? (the reducers are in
   cg.dataset.agg-test), and cg.run/when-row with the cg.macros
   parse-operation-form hook."
  (:require [clojure.test :refer [deftest is testing]]
            [cg.dataset :as dataset]
            [cg.feature :as f]
            [cg.run :as run]
            [cg.macros :as macros]
            [charred.api :as charred]))

(set! *warn-on-reflection* true)

(deftest coalesce-direct
  (is (= 0 (dataset/coalesce nil 0)))
  (is (= "fallback" (dataset/coalesce nil "fallback")))
  (is (= 42 (dataset/coalesce 42 0)))
  (is (= false (dataset/coalesce false :default)))
  (is (= "" (dataset/coalesce "" "default"))
      "coalesce replaces only nil, and an empty string is not nil"))

(deftest coalesce-resolves-via-run
  (let [resolved (run/resolve-operation "coalesce")]
    (is (= 7 (resolved nil 7)))
    (is (= 1 (resolved 1 99)))))

(deftest write-geojson-roundtrip
  (let [pt (f/point 0 0)
        ds (dataset/->dataset [{:geometry pt :name "origin"}])
        tmp (java.io.File/createTempFile "cg-dataset-test" ".geojson")
        path (.getAbsolutePath tmp)]
    (try
      (is (= :ok (dataset/write-geojson ds path)))
      (let [parsed (charred/read-json (slurp path))]
        (is (= "FeatureCollection" (get parsed "type")))
        (is (= 1 (count (get parsed "features"))))
        (let [feat (-> parsed (get "features") first)]
          (is (= "Feature" (get feat "type")))
          ;; dataset->geojson-string emits property keys via (str k);
          ;; keyword keys keep their leading colon.
          (is (= "origin" (get-in feat ["properties" ":name"])))
          (is (= "Point" (get-in feat ["geometry" "type"])))))
      (finally (.delete tmp)))))

(deftest geojson-drops-non-scalar-properties
  (testing "dataset->geojson-string keeps scalar property columns and drops
            nested and geometry columns, because a nested column such as
            :edges must not corrupt the FeatureCollection of the map"
    (let [pt  (f/point 1 2)
          ds  (dataset/->dataset [{:geometry pt :name "a" :units 3 :grade :low
                                   :edges [1 2 3] :env (f/point 0 0)}])
          parsed (charred/read-json (dataset/dataset->geojson-string ds))
          props  (get-in parsed ["features" 0 "properties"])]
      (is (= "a" (get props ":name")))
      (is (= 3 (get props ":units")))
      (is (= "low" (get props ":grade")))
      (is (not (contains? props ":edges")))
      (is (not (contains? props ":env")))
      (is (= "Point" (get-in parsed ["features" 0 "geometry" "type"]))))))

(deftest write-csv-roundtrip
  (let [ds (dataset/->dataset [{:id 1 :name "alpha"}
                               {:id 2 :name "beta"}])
        tmp (java.io.File/createTempFile "cg-dataset-test" ".csv")
        path (.getAbsolutePath tmp)]
    (try
      (is (= :ok (dataset/write-csv ds path)))
      (let [text (slurp path)]
        (is (re-find #"id" text))
        (is (re-find #"alpha" text))
        (is (re-find #"beta" text)))
      (finally (.delete tmp)))))

(deftest spatial-join-prepared-predicates
  (let [sq   (fn [x0 y0 s id]
               {:geometry (f/to-feature
                           (f/polygon [[x0 y0] [(+ x0 s) y0] [(+ x0 s) (+ y0 s)]
                                       [x0 (+ y0 s)] [x0 y0]])
                           {:id id})})
        rows  [(sq 0 0 2 "a") (sq 10 10 2 "b") (sq 1 1 2 "c")]
        ds    (dataset/->dataset rows)
        probe (f/to-feature (f/polygon [[0.5 0.5] [1.5 0.5] [1.5 1.5] [0.5 1.5] [0.5 0.5]])
                            {:id "probe"})
        dsi   (dataset/add-spatial-index ds :geometry)]
    (is (= 2 (dataset/row-count (dataset/select-intersects dsi probe :geometry))))))

(deftest when-row-resolves-via-run
  (is (some? (run/resolve-operation "when-row")))
  (testing "the (when ...) head is sugar at the parser/macro layer only"
    ;; parse-when-row-form changes both heads to :fn 'cg.run/when-row. Thus
    ;; the resolver never gets the bare "when" form at run time, and
    ;; (run/resolve-operation "when") has no entry on purpose.
    (is (nil? (run/resolve-operation "when")))))

(deftest when-row-pass-through-when-predicate-false
  (let [ds (dataset/->dataset [{:id 1 :name "alpha"}
                               {:id 2 :name "beta"}])
        result (run/when-row ds
                             {:predicate {:kind "has-column?" :arg :missing}
                              :options {"on-miss" "pass-through"}
                              ;; Body shape is required by the hook but
                              ;; never invoked on the false path; a stub
                              ;; with a nil :fn would still pass through.
                              :body {:fn "cg.dataset/select-rows" :params [[]]}})]
    (is (identical? ds result))))

(deftest when-row-drop-when-predicate-false
  (let [ds (dataset/->dataset [{:id 1 :name "alpha"}
                               {:id 2 :name "beta"}])
        result (run/when-row ds
                             {:predicate {:kind "has-column?" :arg :missing}
                              :options {"on-miss" "drop"}
                              :body {:fn "cg.dataset/select-rows" :params [[]]}})]
    (is (zero? (dataset/row-count result)))
    (is (= (set (dataset/column-names ds))
           (set (dataset/column-names result))))))

(deftest when-row-applies-body-when-predicate-true
  (let [ds (dataset/->dataset [{:id 1 :name "alpha"}
                               {:id 2 :name "beta"}])
        result (run/when-row ds
                             {:predicate {:kind "has-column?" :arg :id}
                              :options {"on-miss" "pass-through"}
                              :body {:fn "cg.dataset/select-rows" :params [[0]]}})]
    (is (= 1 (dataset/row-count result)))))

(deftest when-row-throws-on-unimplemented-predicate-kind
  (let [ds (dataset/->dataset [{:id 1}])]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"is not yet implemented at runtime"
         (run/when-row ds
                       {:predicate {:kind "totally-made-up" :arg :geometry}
                        :options {"on-miss" "pass-through"}
                        :body {:fn "cg.dataset/select-rows" :params [[]]}})))))

(deftest parse-when-row-form-shape
  (let [parsed (macros/parse-operation-form
                '(when-row {:has-column? :geometry} (cg.geo/buffer 100)))]
    (is (= 'cg.run/when-row (:fn parsed)))
    (is (vector? (:params parsed)))
    (is (= 1 (count (:params parsed))))
    (let [args (first (:params parsed))]
      (is (= "has-column?" (-> args :predicate :kind)))
      (is (= :geometry (-> args :predicate :arg)))
      (is (= {"on-miss" "pass-through"} (:options args))
          "default :on-miss fills from sigs/option-defaults")
      (is (= "cg.geo/buffer" (-> args :body :fn))
          "body :fn is stringified at hook time"))))

(deftest parse-when-row-form-with-explicit-on-miss-drop
  (let [parsed (macros/parse-operation-form
                '(when-row {:has-column? :geometry :on-miss :drop}
                           (cg.geo/buffer 100)))
        args (first (:params parsed))]
    (is (= {"on-miss" "drop"} (:options args)))
    (is (= "has-column?" (-> args :predicate :kind))
        "option-key partitioning leaves the predicate intact")))

(deftest parse-when-row-form-rejects-multi-predicate-key
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"multi-key composition is deferred"
       (macros/parse-operation-form
        '(when-row {:has-column? :geometry :row-count-gt 0}
                   (cg.geo/buffer 100))))))

(deftest parse-when-row-form-rejects-option-only-map
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"option-only maps are not a valid form"
       (macros/parse-operation-form
        '(when-row {:on-miss :drop} (cg.geo/buffer 100))))))

(deftest when-alias-parses-identically
  (is (= (macros/parse-operation-form
          '(when {:has-column? :geometry} (cg.geo/buffer 100)))
         (macros/parse-operation-form
          '(when-row {:has-column? :geometry} (cg.geo/buffer 100))))))

(deftest partition-by-groups-rows-by-column
  (let [d (dataset/->dataset {:zone ["A" "B" "A" "B" "A"]
                              :area [10 20 30 40 50]})
        grouped (dataset/partition-by d :zone)]
    (is (= #{"A" "B"} (set (keys grouped))))
    (is (every? dataset/dataset? (vals grouped)))
    (is (= 3 (dataset/row-count (get grouped "A"))))
    (is (= 2 (dataset/row-count (get grouped "B"))))))

(deftest partition-by-resolves-via-run
  (let [resolved (run/resolve-operation "partition-by")
        d (dataset/->dataset {:zone ["A" "B"] :area [1 2]})]
    (is (dataset/grouped? (resolved d :zone)))))

(deftest grouped-predicate-discriminates-partition-by-output
  (let [d (dataset/->dataset {:zone ["A" "B" "A"] :area [10 20 30]})]
    (is (true? (dataset/grouped? (dataset/partition-by d :zone)))))
  (is (false? (dataset/grouped? (dataset/->dataset {:area [1 2 3]}))))
  (is (false? (dataset/grouped? nil)))
  (is (false? (dataset/grouped? [])))
  (is (false? (dataset/grouped? {})))
  (is (false? (dataset/grouped? "not a map")))
  (is (false? (dataset/grouped? {:a 1 :b 2})))
  (is (false? (dataset/grouped? {"A" [1 2 3]}))))

(deftest filter-serializable-form
  (let [d (dataset/->dataset {:zone ["a" "b" "c" "a"] :n [1 2 3 4]})]
    (let [out (dataset/filter d :zone :in ["a" "b"])]
      (is (dataset/dataset? out))
      (is (= 3 (dataset/row-count out)))
      (is (= #{"a" "b"} (set (mapv :zone (dataset/rows out :as-maps))))))
    (is (= 2 (dataset/row-count (dataset/filter d :zone :equals "a"))))
    (is (= 2 (dataset/row-count (dataset/filter d :n :gt 2))))
    (is (= 1 (dataset/row-count (dataset/filter d #(= 3 (:n %))))))))

(deftest into-map-realizes-groups
  (let [d (dataset/->dataset {:zone ["a" "a" "b"] :v [1 2 3]})
        groups (dataset/group-by d :zone)
        result (dataset/into-map groups)]
    (testing "realizes a groups container to a plain keyed map"
      (is (= #{"a" "b"} (set (keys result))))
      (is (dataset/dataset? (get result "a")))
      (is (= 2 (dataset/row-count (get result "a"))))
      (is (= 1 (dataset/row-count (get result "b")))))
    (testing "identity in shape for an already-realized keyed map (post-map)"
      (let [realized {"a" (dataset/->dataset {:v [1 2]})
                      "b" (dataset/->dataset {:v [3]})}
            out (dataset/into-map realized)]
        (is (= #{"a" "b"} (set (keys out))))
        (is (dataset/dataset? (get out "a")))))
    (is (= {} (dataset/into-map nil)))))

(deftest aggregate-dispatches-to-registered-reducers
  (let [d (dataset/->dataset {:zone ["A" "B" "A" "B" "A"]
                              :area [10 20 30 40 50]})
        grouped (dataset/partition-by d :zone)
        result (run/aggregate grouped
                              {:total {:op :sum :over :area}
                               :rows  {:op :count}})]
    (is (dataset/dataset? result))
    (is (= 2 (dataset/row-count result)))
    (is (= #{:group :total :rows}
           (set (dataset/column-names result))))
    (let [rows (dataset/rows result)
          by-zone (into {} (map (juxt :group identity)) rows)]
      (is (= 90 (:total (get by-zone "A")))) ; 10+30+50
      (is (= 60 (:total (get by-zone "B")))) ; 20+40
      (is (= 3 (:rows (get by-zone "A"))))
      (is (= 2 (:rows (get by-zone "B")))))))

(deftest aggregate-rejects-non-grouped-input
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"partition-by-shaped"
       (run/aggregate (dataset/->dataset {:area [1 2 3]})
                      {:total {:op :sum :over :area}}))))

(deftest aggregate-rejects-non-reducer-op
  (let [grouped (dataset/partition-by
                 (dataset/->dataset {:zone ["A"] :area [1]})
                 :zone)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"does not resolve to a registered reducer"
         (run/aggregate grouped
                        {:total {:op :sumz :over :area}})))))

(deftest aggregate-handles-mean-min-max
  (let [d (dataset/->dataset {:zone ["A" "A" "A" "B"]
                              :area [10 20 30 100]})
        grouped (dataset/partition-by d :zone)
        result (run/aggregate grouped
                              {:m  {:op :mean :over :area}
                               :lo {:op :min  :over :area}
                               :hi {:op :max  :over :area}})
        by-zone (into {} (map (juxt :group identity))
                      (dataset/rows result))]
    (is (= 20.0 (:m (get by-zone "A"))))
    (is (= 10 (:lo (get by-zone "A"))))
    (is (= 30 (:hi (get by-zone "A"))))
    (is (= 100.0 (:m (get by-zone "B"))))
    (is (= 100 (:lo (get by-zone "B"))))
    (is (= 100 (:hi (get by-zone "B"))))))

(deftest max-by-and-min-by-on-coll-of-maps
  (let [items [{:k :a :v 10} {:k :b :v 30} {:k :c :v 20}]]
    (is (= {:k :b :v 30} (dataset/max-by items :v)))
    (is (= {:k :a :v 10} (dataset/min-by items :v))))
  (is (nil? (dataset/max-by [] :v)))
  (is (nil? (dataset/min-by [] :v)))
  (testing "last-encountered wins ties (matches clojure.core/max-key / min-key)"
    (let [items [{:k :a :v 10} {:k :b :v 10}]]
      (is (= :b (:k (dataset/max-by items :v))))
      (is (= :b (:k (dataset/min-by items :v)))))))

(deftest max-by-and-min-by-on-dataset
  (let [d (dataset/->dataset {:zone ["A" "B" "C"] :area [10 30 20]})]
    (is (= 30 (:area (dataset/max-by d :area))))
    (is (= 10 (:area (dataset/min-by d :area))))))

(deftest max-by-min-by-resolve-via-run
  (let [mb (run/resolve-operation "max-by")
        mn (run/resolve-operation "min-by")
        items [{:n 1} {:n 5} {:n 3}]]
    (is (= 5 (:n (mb items :n))))
    (is (= 1 (:n (mn items :n))))))

(deftest reduce-fold-kinds
  (let [nums [3 1 4 1 5 9 2 6]]
    (is (= 8 (dataset/reduce nums :count)))
    (is (= 7 (dataset/reduce nums :distinct-count)))
    (is (= 3 (dataset/reduce nums :first)))
    (is (= 6 (dataset/reduce nums :last)))
    (is (= 31 (dataset/reduce nums :sum)))
    (is (= 1 (dataset/reduce nums :min)))
    (is (= 9 (dataset/reduce nums :max)))
    (is (= (/ 31.0 8) (dataset/reduce nums :mean)))
    (is (= (/ (+ 3 4) 2.0) (dataset/reduce nums :median))))
  (is (= 0 (dataset/reduce [] :count)))
  (is (= 0 (dataset/reduce [] :sum)))
  (is (nil? (dataset/reduce [] :min)))
  (is (nil? (dataset/reduce [] :max)))
  (is (nil? (dataset/reduce [] :mean)))
  (is (nil? (dataset/reduce [] :median)))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"unknown reducer-kind"
       (dataset/reduce [1 2 3] :bogus))))

(deftest reduce-resolves-via-run
  (is (= 6 ((run/resolve-operation "reduce") [1 2 3] :sum))))

(deftest filter-and-group-by-resolve-via-run
  (let [f (run/resolve-operation "filter")
        g (run/resolve-operation "group-by")
        d (dataset/->dataset {:zone ["A" "B" "A"] :area [10 20 30]})]
    (is (= 2 (dataset/row-count (f d #(= "A" (:zone %))))))
    (is (= #{"A" "B"} (set (keys (g d :zone)))))))

(deftest filter-macro-dispatches-by-first-arg-shape
  (let [parsed (macros/parse-operation-form
                '(filter {:has-column? :geometry} (geo/buffer 100)))]
    (is (= 'cg.run/when-row (:fn parsed)))
    (is (= "has-column?" (-> parsed :params first :predicate :kind))))
  (is (not= 'cg.run/when-row (:fn (macros/parse-operation-form '(filter :keep?))))))

(deftest loader-op-registrations
  (doseq [op-name ["cg.dataset/load-geojson"
                   "cg.dataset/load-vector"
                   "cg.dataset/load-gpkg"
                   "cg.dataset/load-shapefile"
                   "cg.dataset/load-gdb"
                   "cg.dataset/load-gdal!"]]
    (let [info (f/get-operation-info op-name)]
      (is (= :source (:type info)) op-name)
      (is (vector? (:params info)) op-name))))

(deftest map-layer-sink-states-drawing-intent
  (testing "the map-layer sink returns pure layer data: the FeatureCollection
            plus the spec the host compiles into a style"
    (let [ds  (dataset/->dataset [{:geometry (f/point 0 0) :zoning "R1" :setback 12}
                                  {:geometry (f/point 1 1) :zoning "R2" :setback 8}])
          out (dataset/map-layer ds {:name "parcels" :kind :line
                                     :color-by :zoning :offset 2 :opacity 0.4})]
      (is (true? (:layer out)) "tagged so the host can tell it from a dataset")
      (is (= "parcels" (:var out)))
      (is (= "line" (get-in out [:spec :kind])))
      (is (= 2 (get-in out [:spec :offset])))
      (is (= 0.4 (get-in out [:spec :opacity])))
      ;; the host matches this against a geojson property name, and those
      ;; emit through (str k)
      (is (= ":zoning" (get-in out [:spec :color-by])))
      (let [parsed (charred/read-json (:geojson out))]
        (is (= "FeatureCollection" (get parsed "type")))
        (is (= 2 (count (get parsed "features"))))
        (is (= "R1" (get-in parsed ["features" 0 "properties" ":zoning"]))
            "the color-by column reaches the map under the name the spec gives")))))

(deftest map-layer-label-by-writes-the-property-the-map-reads
  (testing ":label-by is the one option that changes the data: the named
            column is copied to a plain `label` property, which is what the
            map's symbol layer annotates a line from"
    (let [ds  (dataset/->dataset [{:geometry (f/point 0 0) :setback 12}])
          out (dataset/map-layer ds {:label-by :setback})
          props (-> (charred/read-json (:geojson out))
                    (get-in ["features" 0 "properties"]))]
      (is (= "12" (get props "label"))
          "plain `label`, not \":label\". A keyword column gives the latter,
           and the symbol layer then finds nothing")
      (is (= 12 (get props ":setback"))))))

(deftest map-layer-passes-the-colors-in-their-order
  (let [ds    (dataset/->dataset [{:geometry (f/point 0 0) :grade "good"}])
        out   (dataset/map-layer ds {:color-by :grade
                                     :colors {:good "#111111" :bad "#222222"}})
        value (-> (charred/read-json (:geojson out))
                  (get-in ["features" 0 "properties" ":grade"]))]
    (is (= [["good" "#111111"] ["bad" "#222222"]] (get-in out [:spec :colors]))
        "pairs, because JSON keeps the order of an array")
    (is (= value (ffirst (get-in out [:spec :colors])))
        "the value of the feature is the name that the pair gives it")
    (is (nil? (get-in (dataset/map-layer ds) [:spec :colors])))))

(deftest map-layer-passes-the-popup-with-names-for-keywords
  (let [ds  (dataset/->dataset [{:geometry (f/point 0 0) :grade :good}])
        out (dataset/map-layer ds {:popup {:title [:name :num]
                                           :rules [{:actual :width :min :min-width}]
                                           :facts [{:value :share :format :percent}]}})]
    (is (= {"title" ["name" "num"]
            "rules" [{"actual" "width" "min" "min-width"}]
            "facts" [{"value" "share" "format" "percent"}]}
           (get-in out [:spec :popup])))
    (is (nil? (get-in (dataset/map-layer ds) [:spec :popup])))))

(deftest map-layer-defaults-are-quiet
  (testing "with no opts the sink still produces a drawable layer and a spec
            that asks for nothing, so the map styles it as it always has"
    (let [out (dataset/map-layer (dataset/->dataset [{:geometry (f/point 0 0)}]))]
      (is (= "result" (:var out)))
      (is (nil? (get-in out [:spec :kind])))
      (is (nil? (get-in out [:spec :color])))
      (is (nil? (get-in out [:spec :color-by])))
      (is (nil? (get-in out [:spec :offset]))))))

(deftest map-layer-keeps-its-dataset-in-the-metadata
  (let [ds (dataset/->dataset [{:geometry (f/point 0 0) :grade "good"}])]
    (is (identical? ds (:dataset (meta (dataset/map-layer ds {:name "g"})))))
    (is (identical? ds (:dataset (meta (dataset/map-layer ds {:label-by :grade}))))
        "the input, not the labeled copy")))

(deftest map-layer-is-a-registered-sink
  (testing "a threading pipeline may terminate in the bare map-layer, the
            way it may terminate in report-table. The ds/ spelling is an
            alias a .cg file's own require creates, not a resolver entry."
    (is (= "cg.dataset" (f/get-bare-op-namespace "map-layer")))
    (let [resolved (run/resolve-operation "map-layer")]
      (is (true? (:layer (resolved (dataset/->dataset [{:geometry (f/point 0 0)}])
                                   nil)))))))

;; This build has no GeoParquet support. A call must fail with one error that
;; names the cause, not with a missing namespace.
(deftest geoparquet-calls-fail-with-one-clear-error
  (doseq [f [#(dataset/write-geoparquet (dataset/->dataset [{:a 1}]) "x.parquet")
             #(dataset/read-geoparquet "x.parquet")]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"GeoParquet is not available"
                          (f)))))
