;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parcel-schema-test
  "cg.parcel and cg.zoning name no column and no threshold: the caller
   gives a parcel schema, a zoning schema and a full thresholds map.
   These tests give made-up names and values, and check that the readers
   use them, and that a missing key throws."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cg.run :as run]))

(set! *warn-on-reflection* true)

(use-fixtures :once
  (fn [t]
    (run/load-module "src/cg/parcel.cg")
    (run/load-module "src/cg/zoning.cg")
    (t)))

(defn- ev [code]
  (run/eval-cg (str "(require '[cg.parcel :as parcel] '[cg.zoning :as zoning]"
                    " '[cg.feature :as f] '[cg.dataset :as ds])\n" code)))

(defn- cg-var [qualified-sym]
  (deref (run/eval-cg (str "#'" qualified-sym))))

(def ^:private metre-units
  {:length "m" :area "m2" :large-area "ha" :area-per-large-area 10000.0})

;; Every threshold with the unit-free names, other values, in metres.
(def ^:private unit-free-thresholds
  {:units metre-units
   :min-front-street-width 6.0 :corner-max-angle-deg 135.0
   :rear-angle-tolerance-deg 45.0 :pointed-lot-max-angle-deg 80.0
   :pointed-lot-min-rear-length 3.0 :construct-imaginary-rear? true
   :primary-front-selector :widest-right-of-way :treat-curved-street-as-tangent? true
   :split-edge-at-abutting-changes? true :abutting-split-min-segment-length 0.6
   :min-frontage-length 3.0 :same-street-angle-deg 25.0 :same-street-offset 4.5
   :same-street-gap 7.5 :corner-gap 12.0 :through-parallel-tol-deg 25.0
   :vertex-tolerance 1.0e-3 :right-of-way-width-step 1.5 :right-of-way-width-max 150
   :right-of-way-width-resolution 0.03 :slice-vertex-snap 0.03 :slice-dedup 1.0e-3
   :split-min-coarse 2.0 :pointed-rear-step 0.15 :pointed-rear-max 150})

(deftest analyze-parcel-edges-reads-the-parcel-id-from-the-schema
  (let [[default-id pid-id only-id]
        (ev (str "(let [g   (f/polygon [[0 0] [50 0] [50 100] [0 100] [0 0]])
                   row {:geometry g :PID \"p-1\" :LOT_ID \"m-1\"}
                   pds (ds/->dataset [row])
                   id  (fn [r opts]
                         (:parcel-id (parcel/analyze-parcel-edges
                                      r pds (assoc opts :thresholds " (pr-str unit-free-thresholds) "))))]
               [(id row {:schema {:parcel-id-keys [:LOT_ID :id]}})
                (id row {:schema {:parcel-id-keys [:PID]}})
                (id {:geometry g :id \"i-1\"} {:schema {:parcel-id-keys [:LOT_ID :id]}})])"))]
    (is (= "m-1" default-id) "the first id key with a value")
    (is (= "p-1" pid-id) "a :schema in opts names another id column")
    (is (= "i-1" only-id) "the next id key when the first has no value")))

(deftest a-thresholds-map-without-a-key-throws
  (let [e (try (ev "(parcel/bend-continuation?
                     {:edge (f/linestring [[0.0 0.0] [10.0 0.0]])}
                     {:edge (f/linestring [[10.3 0.0] [20.0 1.0]])}
                     {})")
               nil
               (catch Exception e e))]
    (is (some? e) "no fallback: the library holds no threshold")
    (is (re-find #"the thresholds has no :" (str (ex-message e) (some-> e ex-cause ex-message)))
        "the message names the key")))

(deftest the-thresholds-name-no-unit-and-state-their-units
  (testing "the length readers use the unit-free names"
    (is (= 1 (ev (str "(count (parcel/qualifying-frontages
                                [{:edge (f/linestring [[0.0 0.0] [10.0 0.0]]) :right-of-way-width 8.0}] "
                      (pr-str unit-free-thresholds) "))"))))
    (is (true? (ev (str "(parcel/same-street-frontage?
                          {:edge (f/linestring [[0.0 0.0] [10.0 0.0]])}
                          {:edge (f/linestring [[12.0 1.0] [22.0 1.0]])} "
                        (pr-str unit-free-thresholds) ")")))))
  (testing "a thresholds map without :units throws and names it"
    (let [e (try (ev (str "(let [g (f/polygon [[0 0] [50 0] [50 100] [0 100] [0 0]])]
                             (parcel/analyze-parcel-edges
                              {:geometry g :LOT_ID \"m-1\"}
                              (ds/->dataset [{:geometry g :LOT_ID \"m-1\"}])
                              {:schema {:parcel-id-keys [:LOT_ID]}
                               :thresholds " (pr-str (dissoc unit-free-thresholds :units)) "}))"))
                 nil
                 (catch Exception e e))]
      (is (re-find #":units" (str (some-> e ex-message) (some-> e ex-cause ex-message)))))))

(deftest a-vertex-tolerance-override-reaches-the-junction-tests
  ;; The two street edges miss each other by 0.3, and meet at a shallow
  ;; bend. They count as one frontage only when the tolerance covers the
  ;; gap.
  (let [[strict loose]
        (ev (str "(let [th " (pr-str unit-free-thresholds) "
                   e1 {:edge (f/linestring [[0.0 0.0] [10.0 0.0]])}
                   e2 {:edge (f/linestring [[10.3 0.0] [20.0 1.0]])}]
               [(parcel/bend-continuation? e1 e2 th)
                (parcel/bend-continuation? e1 e2 (assoc th :vertex-tolerance 0.5))])"))]
    (is (false? strict))
    (is (true? loose))))

(deftest zone-attrs-read-the-zoning-schema
  (let [attrs  (cg-var "cg.zoning/zone-to-primary-attrs")
        schema {:district-col :DIST :subdistrict-col :SUB
                :subdistrict-type-col :KIND :max-far-col :FAR
                :rear-setback-col :REAR :max-height-col :HT
                :side-setback-col :SIDE :front-setback-col :FRONT
                :article-col :ART :unique-code-sep "/"
                :subdistrict-code->attrs
                (fn [code] (when (= code "R-A")
                             {:max-unit-count 4 :min-lot-size 2500}))}
        out    (attrs {:DIST "Elsewhere" :SUB "R-A" :KIND "Residential"
                       :FAR "0.8" :REAR 20 :HT "40" :SIDE 5 :FRONT "15"}
                      schema)]
    (is (= "Elsewhere/R-A" (:primary-unique-code out)))
    (is (= "Elsewhere" (:primary-zoning-district out)))
    (is (= "Residential" (:primary-subdistrict-type out)))
    (is (= [0.8 20.0 40.0 5.0 15.0]
           ((juxt :primary-max-far :primary-rear-setback :primary-max-height
                  :primary-side-setback :primary-front-setback) out)))
    (is (= 4 (:primary-max-unit-count out)))
    (is (= 2500 (:primary-min-lot-size out)))))

;; The ops with no map input take their schemas, and the library names no
;; column of its own. These use made-up names.
(def ^:private other-parcel-schema
  "{:parcel-id-keys [:PID] :required-id-keys [:PID]
    :gross-area-col :GFA :units-col :UNITS}")

(def ^:private other-zoning-schema
  "{:district-col :DIST :subdistrict-col :SUB :subdistrict-type-col :KIND
    :max-far-col :FAR :rear-setback-col :REAR :max-height-col :HT
    :side-setback-col :SIDE :front-setback-col :FRONT :article-col :ART
    :unique-code-sep \"/\"
    :subdistrict-code->attrs (fn [code] (when (= code \"R-A\")
                                          {:max-unit-count 4 :min-lot-size 2500}))}")

(def ^:private other-rules
  "(zoning/rule-set
    {:measures zoning/canonical-measures
     :columns {:gross-floor-area :actual-gross-area :dwelling-units :actual-units}
     :rules [{:id :lot-size :measure :lot-area :dir :floor
              :required-column :required-lot-area
              :limit {:form :lookup :by [:primary-article :primary-subdistrict]
                      :table {\"9\" {\"R-A\" {:form :per-unit :by :dwelling-units
                                              :base 2000 :base-n 1 :per 500}}}}}]})")

(deftest preliminary-checks-reads-its-parcel-schema
  (let [[ok flagged]
        (ev (str "(let [s " other-parcel-schema "
                        g (f/polygon [[0 0] [10 0] [10 10] [0 10] [0 0]])
                        chk (fn [row] (:prep-status (persistent!
                                        (parcel/preliminary-checks (transient row) s))))]
                    [(chk {:geometry g :PID \"p-1\"})
                     (chk {:geometry g :LOT_ID \"m-1\"})])"))]
    (is (nil? ok) "a row with the schema's id passes")
    (is (= :skipped-no-parcel-id flagged) "a row without it is flagged")))

(deftest get-primary-zoning-info-reads-its-zoning-schema
  (let [out (ev (str "(let [s " other-zoning-schema "
                            zds (ds/->dataset
                                 [{:DIST \"Elsewhere\" :SUB \"R-A\" :KIND \"Residential\"
                                   :FAR \"0.8\" :REAR 20 :HT \"40\" :SIDE 5 :FRONT \"15\"
                                   :ART \"9\"
                                   :geometry (f/polygon [[-5 -5] [20 -5] [20 20] [-5 20] [-5 -5]])}])]
                        (persistent!
                         (zoning/get-primary-zoning-info
                          (transient {:geometry (f/polygon [[0 0] [10 0] [10 10] [0 10] [0 0]])})
                          zds s)))"))]
    (is (= "Elsewhere/R-A" (:primary-unique-code out)))
    (is (= "9" (:primary-article out)))
    (is (= 4 (:primary-max-unit-count out)))
    (is (= 2500 (:primary-min-lot-size out)))))

(deftest analyze-parcel-conformity-reads-its-rules-and-the-parcel-schema
  (let [out (ev (str "(persistent!
                       (zoning/analyze-parcel-conformity
                        (transient {:edges [] :metrics {:lot-area 3000.0}
                                    :GFA 1200.0 :UNITS 3
                                    :primary-article \"9\" :primary-subdistrict \"R-A\"
                                    :primary-min-lot-size 2500 :primary-max-unit-count 4})
                        {:parcel-schema " other-parcel-schema "
                         :rules " other-rules "}))"))]
    (is (= 1200.0 (:actual-gross-area out)) "the gross area comes from :GFA")
    (is (= 3 (:actual-units out)) "the units come from :UNITS")
    (is (= 3000 (:required-lot-area out)) "the rule: 2,000 + 2 x 500")))

(deftest a-missing-schema-key-throws-and-names-it
  (let [e (try (ev "(parcel/preliminary-checks
                     (transient {:geometry (f/polygon [[0 0] [1 0] [1 1] [0 0]])}) {})")
               nil
               (catch Exception e e))]
    (is (some? e) "no default: the library names no column")
    (is (re-find #":required-id-keys" (str (ex-message e) (ex-message (ex-cause e))))
        "the message names the key")))

(defn- circle-coords
  "A closed ring of `n` points on a circle, as text for .cg code."
  [cx cy r n]
  (let [pt (fn [i]
             (let [a (/ (* 2.0 Math/PI i) n)]
               [(+ cx (* r (Math/cos a))) (+ cy (* r (Math/sin a)))]))]
    (pr-str (conj (mapv pt (range n)) (pt 0)))))

(deftest the-impervious-join-reads-a-large-polygon-by-grid-cells
  (let [[direct joined corner-direct corner-joined grids]
        (ev (str "(require '[cg.geo :as geo])
                  (let [big  (f/polygon " (circle-coords 500.0 500.0 400.0 12000) ")
                        imp  (ds/add-spatial-index (ds/->dataset [{:geometry big}]))
                        area (fn [lot]
                               (:impervious-area
                                (persistent!
                                 (parcel/analyze-parcel-impervious (transient {:geometry lot}) imp))))
                        ;; Across the edge of the circle, in one cell.
                        edge   (f/polygon [[480 80] [520 80] [520 120] [480 120] [480 80]])
                        ;; Across a corner of four cells (the grid has 3 by 3
                        ;; cells over 100 to 900), inside the circle.
                        corner (f/polygon [[340 340] [400 340] [400 400] [340 400] [340 340]])]
                    [(geo/area (geo/intersection big edge)) (area edge)
                     (geo/area (geo/intersection big corner)) (area corner)
                     (count @parcel/overlay-grids)])"))
        close? (fn [a b] (<= (Math/abs (- (double a) (double b))) (* 1.0e-9 (double a))))]
    (is (pos? grids) "a polygon over the limit gets a grid")
    (is (close? direct joined) "one cell gives the area of the direct overlay")
    (is (close? corner-direct corner-joined) "four cells give the area of the direct overlay")
    (is (close? 3600.0 corner-joined) "a lot inside the polygon is covered")))

(deftest the-impervious-join-names-no-unit
  (let [out (ev "(let [imp (ds/add-spatial-index
                             (ds/->dataset
                              [{:geometry (f/polygon [[0 0] [5 0] [5 10] [0 10] [0 0]])}]))]
                   (persistent!
                    (parcel/analyze-parcel-impervious
                     (transient {:geometry (f/polygon [[0 0] [10 0] [10 10] [0 10] [0 0]])})
                     imp)))")]
    (is (= 50.0 (:impervious-area out)))
    (is (not (contains? out :impervious-sf)))))
