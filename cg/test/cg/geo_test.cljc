;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; One suite for clojure.test (bb test) and cljs.test (bb test:cljs). A
;; promoted op (geo/area) is compared with its raw twin
;; (geo/geometry-get-area), and a static op with JTS or wasmts called
;; directly. A port with no independent implementation (offset-line,
;; merge-lines, one-sided buffers) only gets a type check. geom-eq? accepts
;; equalsExact or equalsTopo, because overlay ops can start at another vertex.
(ns cg.geo-test
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [cg.geo :as geo]
                    [cg.feature :as feature])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../src/cg/feature.mjs" :as feature]
                     ["../../src/cg/geo.mjs" :as geo]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]]))
  #?(:clj (:import (org.locationtech.jts.geom Coordinate Geometry GeometryFactory)
                   (org.locationtech.jts.geom.util GeometryFixer)
                   (org.locationtech.jts.operation.distance DistanceOp)
                   (org.locationtech.jts.simplify DouglasPeuckerSimplifier TopologyPreservingSimplifier)
                   (org.locationtech.jts.algorithm MinimumAreaRectangle MinimumDiameter))))

#?(:clj (set! *warn-on-reflection* true))

;; Built by a thunk, not top-level defs: on JS the feature/* geometry
;; constructors need globalThis.wasmts, which init-wasmts! installs only after
;; module load. Each deftest destructures (fx) so construction happens once the
;; runner has initialised wasmts. On JVM the thunk is a cheap rebuild.

(defn- mk-line [pts props] (feature/to-feature (feature/linestring pts) props))
(defn- mk-poly [pts props] (feature/to-feature (feature/polygon pts) props))

(defn- fx []
  {:square-a  (mk-poly [[-5 -5] [5 -5] [5 5] [-5 5] [-5 -5]] {:id "A"})
   :square-b  (mk-poly [[-2 -2] [8 -2] [8 8] [-2 8] [-2 -2]] {:id "B"})
   :point-in  (feature/to-feature (feature/point 1 1) {:id "P"})
   :line-diag (mk-line [[0 0] [10 10]] {:id "L"})
   :bend-line (mk-line [[0 0] [5 0.1] [10 0]] {:id "Z"})
   :line-1    (mk-line [[0 0] [10 0]] {:id "1"})
   :line-2    (mk-line [[0 5] [10 5]] {:id "2"})
   :seg-a     (mk-line [[0 0] [5 0]] {:id "SA"})
   :seg-b     (mk-line [[5 0] [10 0]] {:id "SB"})})

;; GeometryCollection has no public constructor through feature/* on JS, so the
;; num-geometries(collection) / geometry-n cases stay JVM-only.
#?(:clj
   (defn- mp-coll []
     (let [^GeometryFactory gf (GeometryFactory.)
           sq (fn [cx cy h]
                (.createPolygon
                 gf (.createLinearRing
                     gf ^"[Lorg.locationtech.jts.geom.Coordinate;" (into-array Coordinate
                                    [(Coordinate. (- cx h) (- cy h))
                                     (Coordinate. (+ cx h) (- cy h))
                                     (Coordinate. (+ cx h) (+ cy h))
                                     (Coordinate. (- cx h) (+ cy h))
                                     (Coordinate. (- cx h) (- cy h))]))))]
       (feature/to-feature
        (.createGeometryCollection gf (into-array Geometry [(sq 0.0 0.0 1.0)
                                                            (sq 5.0 5.0 1.0)]))
        {:id "MP"}))))

(defn- within? [tol a b]
  (< #?(:clj (Math/abs (double (- a b))) :cljs (js/Math.abs (- a b))) tol))
(defn- close? [a b] (within? 1.0e-9 a b))

(defn- geom-eq? [a b]
  (boolean
   (and a b
        #?(:clj  (or (.equalsExact ^Geometry a ^Geometry b 1.0e-9)
                     (.equalsTopo ^Geometry a ^Geometry b))
           :cljs (or (.equalsExact a b 1.0e-9) (.equalsTopo a b))))))

(defn- as-geom [x] (when (some? x) (if (feature/is-geometry? x) x (feature/geometry x))))
(defn- as-props [x] (when-not (feature/is-geometry? x) (feature/properties x)))

(defn- props-eq? [a b]
  #?(:clj  (= a b)
     :cljs (= (js/JSON.stringify (or a nil)) (js/JSON.stringify (or b nil)))))

(defn- eqv
  "Equivalence across the shapes ts / geo ops return: numbers (tolerant),
   strings/booleans, [[x y]...] coordinate vectors (structural), and
   geometry-or-feature results (geometry equality, plus property equality when
   both sides carry properties)."
  [a b]
  (cond
    (and (number? a) (number? b))   (close? a b)
    (and (string? a) (string? b))   (= a b)
    (and (boolean? a) (boolean? b)) (= a b)
    (and (vector? a) (vector? b))   (= a b)
    :else
    (let [ga (as-geom a) gb (as-geom b)]
      (if (and ga gb)
        (and (geom-eq? ga gb)
             (let [pa (as-props a) pb (as-props b)]
               (if (and (some? pa) (some? pb)) (props-eq? pa pb) true)))
        (= a b)))))

#?(:cljs
   (defn- coords->pairs [arr]
     (mapv (fn [c] [(.-x c) (.-y c)]) arr)))

;; simplify defaults to topology-preserving (the documented :preserve-topology
;; default); douglas-peucker-simplify and (simplify … {:preserve-topology false})
;; use plain DouglasPeucker.
(defn- o-simplify [f tol]
  #?(:clj  (TopologyPreservingSimplifier/simplify (feature/geometry f) tol)
     :cljs (js/globalThis.wasmts.simplify.TopologyPreservingSimplifier.simplify (feature/geometry f) tol)))

(defn- o-dp-simplify [f tol]
  #?(:clj  (DouglasPeuckerSimplifier/simplify (feature/geometry f) tol)
     :cljs (js/globalThis.wasmts.simplify.DouglasPeuckerSimplifier.simplify (feature/geometry f) tol)))

(defn- o-min-area [f]
  #?(:clj  (MinimumAreaRectangle/getMinimumRectangle (feature/geometry f))
     :cljs (js/globalThis.wasmts.algorithm.MinimumAreaRectangle.getMinimumRectangle (feature/geometry f))))

(defn- o-min-width [f]
  #?(:clj  (MinimumDiameter/getMinimumRectangle (feature/geometry f))
     :cljs (js/globalThis.wasmts.algorithm.MinimumDiameter.getMinimumRectangleStatic (feature/geometry f))))

(defn- o-make-valid [f]
  #?(:clj  (GeometryFixer/fix (feature/geometry f))
     :cljs (js/globalThis.wasmts.geom.util.GeometryFixer.fix (feature/geometry f))))

(defn- o-coordinates [f]
  #?(:clj  (mapv (fn [^Coordinate c] [(.getX c) (.getY c)])
                 (.getCoordinates ^Geometry (feature/geometry f)))
     :cljs (coords->pairs (.getCoordinates (feature/geometry f)))))

(defn- o-nearest [f1 f2]
  #?(:clj  (mapv (fn [^Coordinate c] [(.getX c) (.getY c)])
                 (DistanceOp/nearestPoints (feature/geometry f1) (feature/geometry f2)))
     :cljs (coords->pairs (js/globalThis.wasmts.operation.distance.DistanceOp.nearestPoints
                           (feature/geometry f1) (feature/geometry f2)))))

(defn- o-closest [f1 f2]
  ;; JTS DistanceOp.closestPoints == nearestPoints (same two points).
  #?(:clj  (mapv (fn [^Coordinate c] [(.getX c) (.getY c)])
                 (DistanceOp/nearestPoints (feature/geometry f1) (feature/geometry f2)))
     :cljs (coords->pairs (js/globalThis.wasmts.operation.distance.DistanceOp.closestPoints
                           (feature/geometry f1) (feature/geometry f2)))))

;; The bespoke ports (offset-line / offset-line-string / merge-lines / one-sided
;; buffers) have no independent second implementation to check against, so their
;; deftests assert a non-empty geometry of the expected type instead.
(defn- non-empty-geom-of-type?
  [x tname]
  (let [g (as-geom x)]
    (boolean
     (and g
          (= tname #?(:clj (.getGeometryType ^Geometry g) :cljs (.getGeometryType g)))
          (not #?(:clj (.isEmpty ^Geometry g) :cljs (.isEmpty g)))
          (pos? #?(:clj (.getNumPoints ^Geometry g) :cljs (.getNumPoints g)))))))

(deftest accessor-equivalence
  (let [{:keys [square-a line-diag]} (fx)]
    (testing "getArea"     (is (eqv (geo/area square-a)          (geo/geometry-get-area square-a))))
    (testing "getLength"   (is (eqv (geo/length line-diag)       (geo/geometry-get-length line-diag))))
    (testing "geometry-type" (is (eqv (geo/geometry-type square-a) (geo/geometry-get-geometry-type square-a))))
    (testing "num-geometries (single)"
      (is (eqv (geo/num-geometries square-a) (geo/geometry-get-num-geometries square-a))))
    (testing "isValid"     (is (eqv (geo/valid? square-a)        (geo/geometry-is-valid square-a))))
    (testing "isEmpty"     (is (eqv (geo/empty? square-a)   (geo/geometry-is-empty square-a))))))

(deftest geometry-returning-equivalence
  (let [{:keys [square-a point-in]} (fx)]
    (testing "centroid"    (is (eqv (geo/centroid square-a)      (geo/geometry-get-centroid square-a))))
    (testing "convex-hull" (is (eqv (geo/convex-hull square-a)   (geo/geometry-convex-hull square-a))))
    (testing "envelope"    (is (eqv (geo/envelope square-a)      (geo/geometry-get-envelope square-a))))
    (testing "boundary"    (is (eqv (geo/boundary square-a)      (geo/geometry-get-boundary square-a))))
    (testing "buffer (1-arg)" (is (eqv (geo/buffer point-in 1.0) (geo/geometry-buffer point-in 1.0))))))

(deftest buffer-opts
  (let [line (mk-line [[0 0] [10 0]] {:id "L"})
        ar   (fn [x] (.getArea ^Geometry (feature/geometry x)))]
    (testing "default (no opts) matches plain Geometry.buffer"
      (is (close? (ar (geo/buffer line 2.0)) (ar (geo/geometry-buffer line 2.0)))))
    (testing "integer distance works (static dispatch coerces Long -> double)"
      (is (close? (ar (geo/buffer line 3)) (ar (geo/buffer line 3.0)))))
    (testing ":cap-style :flat gives a bare rectangle (2*len*dist = 40), no round caps"
      (is (close? 40.0 (ar (geo/buffer line 2.0 {:cap-style :flat})))))
    (testing ":cap-style :flat area is smaller than the default round-capped area"
      (is (< (ar (geo/buffer line 2.0 {:cap-style :flat})) (ar (geo/buffer line 2.0)))))
    (testing "properties preserved through an opts buffer"
      (is (= "L" #?(:clj  (:id (feature/properties (geo/buffer line 2.0 {:cap-style :flat})))
                    :cljs (.-id (feature/properties (geo/buffer line 2.0 {:cap-style :flat})))))))))

(deftest binary-op-equivalence
  (let [{:keys [square-a square-b]} (fx)]
    (testing "distance"    (is (eqv (geo/distance square-a square-b)      (geo/geometry-distance square-a square-b))))
    (testing "equals-topo? (self)"     (is (eqv (geo/equals-topo? square-a square-a) (geo/geometry-equals-topo square-a square-a))))
    (testing "equals-topo? (distinct)" (is (eqv (geo/equals-topo? square-a square-b) (geo/geometry-equals-topo square-a square-b))))
    (testing "union"        (is (eqv (geo/union square-a square-b)        (geo/geometry-union square-a {:other square-b}))))
    (testing "intersection" (is (eqv (geo/intersection square-a square-b) (geo/geometry-intersection square-a square-b))))
    (testing "difference"   (is (eqv (geo/difference square-a square-b)   (geo/geometry-difference square-a square-b))))
    (testing "sym-difference" (is (eqv (geo/sym-difference square-a square-b) (geo/geometry-sym-difference square-a square-b))))))

(deftest union-collection-fold
  (let [{:keys [square-a square-b]} (fx)
        far (mk-poly [[20 20] [22 20] [22 22] [20 22] [20 20]] {:id "far"})]
    (testing "fold of overlapping squares merges to one geometry (100 + 100 - 49)"
      (is (close? 151.0 (.getArea ^Geometry (feature/geometry (geo/union [square-a square-b]))))))
    (testing "fold of disjoint squares sums area (100 + 4)"
      (is (close? 104.0 (.getArea ^Geometry (feature/geometry (geo/union [square-a far]))))))
    (testing "fold keeps the first feature's properties"
      (is (= "A" #?(:clj  (:id (feature/properties (geo/union [square-a far])))
                    :cljs (.-id (feature/properties (geo/union [square-a far])))))))))

(deftest overlay-property-strategy
  (let [a   (mk-poly [[-5 -5] [5 -5] [5 5] [-5 5] [-5 -5]] {:name "A" :zone 1})
        b   (mk-poly [[-2 -2] [8 -2] [8 8] [-2 8] [-2 -2]] {:name "B" :owner "x"})
        pget (fn [feat k] (get (feature/properties feat) k))]
    (testing "default keeps the first feature's properties"
      (is (= "A" (pget (geo/union a b) :name))))
    (testing ":keep-last keeps the second feature's properties"
      (is (= "B" (pget (geo/union a b {:property-strategy :keep-last}) :name))))
    (testing ":merge combines both (second wins on key conflict)"
      (let [u (geo/union a b {:property-strategy :merge})]
        (is (= "B" (pget u :name)))   ; b overrides a
        (is (= 1   (pget u :zone)))   ; a-only key kept
        (is (= "x" (pget u :owner))))) ; b-only key added
    (testing "intersection honors :merge too"
      (is (= "x" (pget (geo/intersection a b {:property-strategy :merge}) :owner))))))

(deftest hero-static-equivalence
  (let [{:keys [square-a bend-line]} (fx)]
    (testing "simplify (default: topology-preserving)"
      (is (eqv (o-simplify bend-line 1.0) (geo/simplify bend-line 1.0))))
    (testing "simplify {:preserve-topology false} routes to Douglas-Peucker"
      (is (eqv (o-dp-simplify bend-line 1.0) (geo/simplify bend-line 1.0 {:preserve-topology false}))))
    (testing "douglas-peucker-simplify" (is (eqv (o-dp-simplify bend-line 1.0) (geo/douglas-peucker-simplify bend-line 1.0))))
    (testing "get-minimum-area-rectangle"  (is (eqv (o-min-area square-a)  (geo/get-minimum-area-rectangle square-a))))
    (testing "get-minimum-width-rectangle" (is (eqv (o-min-width square-a) (geo/get-minimum-width-rectangle square-a))))
    (testing "make-valid"               (is (eqv (o-make-valid square-a)   (geo/make-valid square-a))))))

(deftest coord-pairs-equivalence
  (let [{:keys [square-a line-diag line-1 line-2]} (fx)]
    (testing "coordinates (line)"    (is (eqv (o-coordinates line-diag) (geo/coordinates line-diag))))
    (testing "coordinates (polygon)" (is (eqv (o-coordinates square-a)  (geo/coordinates square-a))))
    (testing "nearest-points"        (is (eqv (o-nearest line-1 line-2) (geo/nearest-points line-1 line-2))))
    (testing "closest-points"        (is (eqv (o-closest line-1 line-2) (geo/closest-points line-1 line-2))))))

(deftest bespoke-cross-platform-sanity
  (let [{:keys [bend-line seg-a seg-b]} (fx)]
    (testing "offset-line -> non-empty LineString"
      (is (non-empty-geom-of-type? (geo/offset-line bend-line 1.0) "LineString")))
    (testing "offset-line-string :line -> non-empty LineString"
      (is (non-empty-geom-of-type? (geo/offset-line-string bend-line 1.0 :line) "LineString")))
    (testing "merge-lines -> non-empty LineString"
      (is (non-empty-geom-of-type? (geo/merge-lines [seg-a seg-b]) "LineString")))))

;; The JS runtime builds a Point through wasmts.geom.fromFlat, which takes a
;; dimension. The Coordinate route it replaced stopped at XYZ, so the 4-arity
;; dropped the measure while the JVM kept it through CoordinateXYZM.

(defn- point-ordinates
  "The x, y, z and m of a point. An ordinate that the geometry does not carry
   reads as NaN on both platforms."
  [pt]
  #?(:clj (let [^Coordinate c (.getCoordinate ^Geometry pt)]
            [(.getX c) (.getY c) (.getZ c) (.getM c)])
     :cljs (let [c (.getCoordinate pt)]
             [(.-x c) (.-y c) (.-z c) (.-m c)])))

(defn- unset-ordinate?
  [v]
  #?(:clj (Double/isNaN (double v))
     :cljs (js/Number.isNaN v)))

(deftest point-dimensions
  (testing "2 args carry x and y only"
    (let [[x y z m] (point-ordinates (feature/point 1.5 2.5))]
      (is (= 1.5 x))
      (is (= 2.5 y))
      (is (unset-ordinate? z))
      (is (unset-ordinate? m))))
  (testing "3 args carry z"
    (let [[x y z m] (point-ordinates (feature/point 1.5 2.5 3.5))]
      (is (= 1.5 x))
      (is (= 2.5 y))
      (is (= 3.5 z))
      (is (unset-ordinate? m))))
  (testing "4 args carry z and m"
    (let [[x y z m] (point-ordinates (feature/point 1.5 2.5 3.5 4.5))]
      (is (= 1.5 x))
      (is (= 2.5 y))
      (is (= 3.5 z))
      (is (= 4.5 m)))))

(deftest collection-input-lift
  (let [{:keys [square-a square-b line-diag]} (fx)]
    (testing "coordinates over a vector of features"
      (is (= (mapv o-coordinates [line-diag square-a])
             (geo/coordinates [line-diag square-a]))))
    (testing "get-minimum-area-rectangle over a vector of features"
      (is (every? true?
                  (map eqv
                       (mapv o-min-area [square-a square-b])
                       (geo/get-minimum-area-rectangle [square-a square-b])))))))

#?(:clj
   (deftest jvm-only-sanity+equivalence
     (let [{:keys [bend-line]} (fx)
           coll (mp-coll)]
       ;; bespoke buffers/offset: sanity (no second impl to compare against)
       (testing "one-sided-buffer -> non-empty Polygon"
         (is (non-empty-geom-of-type? (geo/one-sided-buffer bend-line 1.0) "Polygon")))
       (testing "left-buffer -> non-empty Polygon"
         (is (non-empty-geom-of-type? (geo/left-buffer bend-line 1.0) "Polygon")))
       (testing "right-buffer -> non-empty Polygon"
         (is (non-empty-geom-of-type? (geo/right-buffer bend-line 1.0) "Polygon")))
       (testing "offset-line-string :polygon -> non-empty Polygon"
         (is (non-empty-geom-of-type? (geo/offset-line-string bend-line 1.0 :polygon) "Polygon")))
       ;; collection accessors: promoted name vs raw mint twin (real equivalence)
       (testing "num-geometries (collection)"
         (is (eqv (geo/num-geometries coll) (geo/geometry-get-num-geometries coll))))
       (testing "geometry-n (first child)"
         (is (eqv (geo/geometry-n coll 0) (geo/geometry-get-geometry-n coll 0)))))))

;; transform-geom (proj, not JTS). On JVM it is synchronous (JNA FFI) and compared
;; directly against geo. On JS proj-wasm is worker-backed, so transform-geom returns
;; a Promise; the test is ^:async (a no-op on the JVM's clojure.test) and awaits it,
;; after awaiting init-proj!. The JS assertion is cross-runtime: the transformed
;; coordinates match the JVM-computed EPSG:2249 values to within a foot.
(deftest ^:async transform-geom-equivalence
  (let [pt (feature/to-feature (feature/point -72.0 42.0) {:id "pt-1"})
        _  #?(:clj nil :cljs (await (geo/init-proj!)))
        r  #?(:clj (geo/transform-geom pt "EPSG:4326" "EPSG:2249")
              :cljs (await (geo/transform-geom pt "EPSG:4326" "EPSG:2249")))
        [[x y]] (geo/coordinates r)]
    (testing "transform-geom EPSG:4326 -> EPSG:2249 (cross-runtime coordinate parity)"
      (is (within? 1.0 x 520261.8155634654))
      (is (within? 1.0 y 2825423.2398844697))
      (is (= "pt-1" #?(:clj (:id (feature/properties r))
                       :cljs (.-id (feature/properties r))))))))

#?(:cljs (defn ^:async teardown! [] (await (geo/shutdown-proj!))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (run_tests_and_exit_BANG_ teardown! "cg.geo-test")))))
