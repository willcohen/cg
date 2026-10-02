;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.run-test
  "Tests for Flow Graph execution engine (cg.run). JVM only: each test here
   needs a host op, SCI, or tech.ml.dataset.

   Covers:
   - Components (a ^:table op whose body is a model)
   - Table mode (dataset batching)
   - Dataset batching and the trace events of a grouped run

   The basic run-model tests -- a source operation, a source with a transform,
   and a data binding -- are in cg.run-exec-test, which runs on both runtimes."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [cg.run :as run]
            [cg.feature :as f]
            [cg.dataset :as dataset]
            [cg.trace :as trace]
            [cg.macros :refer [table->]]
            [sci.core :as sci]
            [tech.v3.dataset :as ds])
  (:import [org.locationtech.jts.geom Geometry TopologyException]))

(set! *warn-on-reflection* true)

;; A host defn with ^:op does not register itself. cg.run/eval-cg registers
;; the ops of .cg code. A test that defines an op in Clojure registers it,
;; before a table-> form names it as a rung.
(defn- register-op! [v]
  (run/register-op! (str (:ns (meta v))) (run/var-op-decl v) @v))

;; Composition model fixtures: a row op that the table op runs over each
;; row, and the table op, whose body is the model.
(defn ^:op rc-incr
  {:row-reads #{:n} :row-writes #{:doubled}}
  [^:row parcel-row]
  (assoc! parcel-row :doubled (* 2 (:n parcel-row))))
(register-op! #'rc-incr)

(defn ^:op rc-analyze [^:table rows]
  (table-> :rows (rc-incr)))
(register-op! #'rc-analyze)

;; model->flow fuses a per-row chain of 2 ops or more; only the fused
;; step tags each row's trace events with an :item-id.
(defn ^:op rc-incr-more
  {:row-reads #{:doubled} :row-writes #{:doubled}}
  [^:row parcel-row]
  (assoc! parcel-row :doubled (inc (:doubled parcel-row))))
(register-op! #'rc-incr-more)

(defn ^:op rc-analyze-fused [^:table rows]
  (table-> :rows (rc-incr) (rc-incr-more)))
(register-op! #'rc-analyze-fused)

;; A per-row op slow enough that the idle compute workers each take a
;; chunk before one worker can drain the queue.
(defn ^:op rc-nap
  {:row-reads #{:doubled} :row-writes #{:napped}}
  [^:row parcel-row]
  (Thread/sleep 5)
  (assoc! parcel-row :napped (* 10 (:doubled parcel-row))))
(register-op! #'rc-nap)

(defn ^:op rc-analyze-napped [^:table rows]
  (table-> :rows (rc-incr) (rc-incr-more) (rc-nap)))
(register-op! #'rc-analyze-napped)

(deftest run-component-runs-a-table-op-once-per-table
  ;; A table op body is a per-row table-> chain. run-component on one
  ;; table must run the body ONCE over the whole table (batched + fused
  ;; internally), not treat the table as a single row. The component is named
  ;; by its bare spelling (what the map node carries) and resolves through the
  ;; same bare-name fallback resolve-operation uses.
  (testing "run-component runs the body once over the supplied table"
    (let [sub (ds/->>dataset [{:n 1} {:n 2} {:n 3}])
          out (run/run-component "rc-analyze" {:rows sub} {:timeout-ms 30000})]
      (is (dataset/dataset? out) "returns a dataset")
      (is (= 3 (dataset/row-count out)) "row count preserved")
      (is (= [2 4 6] (mapv :doubled (dataset/rows out :as-maps)))
          "the body ran per row over the whole table"))))

(defn- worker-busy-item-ids
  "The :item-id of each worker-busy-end event whose id is in `ids`. Polls,
   because recent-events drains the trace channel on another thread."
  [ids]
  (loop [tries 0]
    (let [seen (->> (trace/recent-events)
                    (filter #(= "worker-busy-end" (:kind %)))
                    (keep :item-id)
                    (filter ids)
                    set)]
      (if (or (= ids seen) (>= tries 40))
        seen
        (do (Thread/sleep 50) (recur (inc tries)))))))

(deftest run-component-labels-trace-rows-by-row-id-keys
  ;; The fused per-row chain tags each row's worker-busy events with an
  ;; :item-id. run-model's :row-id-keys opt names the columns to read, in
  ;; order; the default is [:id].
  (testing ":row-id-keys picks the column for the trace :item-id"
    (let [sub (ds/->>dataset [{:n 1 :PID "rk-p1" :id "rk-i1"}
                              {:n 2 :PID "rk-p2" :id "rk-i2"}])]
      (trace/clear-recent-events!)
      (run/run-component "rc-analyze-fused" {:rows sub}
                         {:timeout-ms 30000 :row-id-keys [:PID :id]})
      (is (= #{"rk-p1" "rk-p2"} (worker-busy-item-ids #{"rk-p1" "rk-p2" "rk-i1" "rk-i2"})))))
  (testing "with no :row-id-keys, the trace :item-id is :id"
    (let [sub (ds/->>dataset [{:n 1 :LOT_ID "rk-m3" :id "rk-i3"}])]
      (trace/clear-recent-events!)
      (run/run-component "rc-analyze-fused" {:rows sub} {:timeout-ms 30000})
      (is (= #{"rk-i3"} (worker-busy-item-ids #{"rk-m3" "rk-i3"}))))))

(defn- trace-events-of
  "The trace events of `kind` whose :item-id is in `ids`. Polls until one
   event for each id arrives, because recent-events drains the trace
   channel on another thread."
  [kind ids]
  (loop [tries 0]
    (let [evs (filterv #(and (= kind (:kind %)) (contains? ids (:item-id %)))
                       (trace/recent-events))]
      (if (or (>= (count evs) (count ids)) (>= tries 60))
        evs
        (do (Thread/sleep 50) (recur (inc tries)))))))

(deftest fused-chain-runs-each-row-once-on-the-compute-pool
  ;; The fused step splits the rows into one chunk per compute worker and
  ;; runs the whole chain on each row. The nh spines depend on this shape:
  ;; the study compute ran 7 rungs as one proc, 280 rows, 35 on each of 8
  ;; workers.
  (let [n     64
        ids   (mapv #(str "fp-" %) (range n))
        sub   (ds/->>dataset (mapv (fn [i id] {:n i :id id}) (range n) ids))
        _     (trace/clear-recent-events!)
        out   (run/run-component "rc-analyze-napped" {:rows sub} {:timeout-ms 30000})
        rows  (dataset/rows out :as-maps)
        busy  (trace-events-of "worker-busy-start" (set ids))]
    (testing "the chain ran on every row, in the input order"
      (is (= ids (mapv :id rows)))
      (is (= (mapv #(* 10 (inc (* 2 %))) (range n)) (mapv :napped rows))))
    (testing "each row ran one time"
      (is (= n (count busy)))
      (is (= (set ids) (set (map :item-id busy)))))
    (testing "more than one compute worker took rows"
      (is (<= 2 (count (set (map :worker-id busy))))
          (pr-str (frequencies (map :worker-id busy)))))
    (testing "the fused chain gives the rows that one proc per op gives"
      (let [step (fn [model d] (run/run-model model {:input d} {:timeout-ms 30000}))
            ;; table-> reads the op names at macro time, so each step is
            ;; written out.
            one-by-one (->> sub
                            (step (table-> :input (rc-incr)))
                            (step (table-> :input (rc-incr-more)))
                            (step (table-> :input (rc-nap))))]
        (is (= (mapv #(select-keys % [:id :n :doubled :napped]) rows)
               (mapv #(select-keys % [:id :n :doubled :napped])
                     (dataset/rows one-by-one :as-maps))))))))

(defn ^:op rc-needs-valid-other
  {:row-reads #{:other} :row-writes #{:other-ok}}
  [^:row parcel-row]
  (if (.isValid ^Geometry (:other parcel-row))
    (assoc! parcel-row :other-ok true)
    (throw (TopologyException. "invalid :other"))))
(register-op! #'rc-needs-valid-other)

(defn ^:op rc-repair-fused [^:table rows]
  (table-> :rows (rc-incr) (rc-needs-valid-other)))
(register-op! #'rc-repair-fused)

(defn ^:op rc-repair-alone [^:table rows]
  (table-> :rows (rc-needs-valid-other)))
(register-op! #'rc-repair-alone)

(deftest topology-retry-repairs-the-declared-reads
  ;; On a TopologyException, apply-row-chain repairs the geometry keys that
  ;; the op declares in :row-reads, then retries the op one time. Here the
  ;; bad geometry is :other, not :geometry.
  (let [bowtie (f/polygon [[0 0] [10 10] [10 0] [0 10] [0 0]])
        sub    (ds/->>dataset [{:n 1 :id "tr-1" :other bowtie}])]
    (doseq [component ["rc-repair-fused" "rc-repair-alone"]]
      (testing component
        (let [row (first (dataset/rows (run/run-component component {:rows sub}
                                                          {:timeout-ms 30000})
                                       :as-maps))]
          (is (nil? (:error row)) (:error row))
          (is (true? (:other-ok row))))))))

(defn ^:op rc-boom
  {:row-reads #{} :row-writes #{}}
  [^:row _parcel-row]
  (throw (ex-info "boom" {})))
(register-op! #'rc-boom)

(defn- root-out-str
  "Run `f` and return what it printed through the root binding of *out*.
   A flow thread does not get the test thread's *out* binding, so
   with-out-str does not see its prints."
  [f]
  (let [sw  (java.io.StringWriter.)
        old (.getRawRoot #'*out*)]
    (alter-var-root #'*out* (constantly sw))
    (try (f) (finally (alter-var-root #'*out* (constantly old))))
    (str sw)))

(deftest single-per-row-op-diag-reads-row-id-keys
  ;; A per-row op that runs alone takes the one-op path of operation->step,
  ;; not the fused step. Its DIAG line must use :row-id-keys too.
  (let [d   (ds/->>dataset [{:PID "rk-p9" :id "rk-i9"}])
        out (root-out-str
             #(run/run-model (table-> :input (rc-boom)) {:input d}
                             {:timeout-ms 30000 :row-id-keys [:PID]}))]
    (is (str/includes? out "row-id= rk-p9") out)))

(deftest filter-serializable-runs-in-table-flow
  ;; (filter :col :pred value) is the serializable form, distinct from the
  ;; fn-taking cg.dataset/filter.
  (testing "(filter :col :in vals) executes in a table-> flow"
    (let [d (ds/->>dataset [{:zone "a" :n 1} {:zone "b" :n 2} {:zone "c" :n 3}])
          model (table-> :input (filter :zone :in ["a" "b"]))
          out (run/run-model model {:input d} {:timeout-ms 15000})]
      (is (dataset/dataset? out) "returns a dataset")
      (is (= 2 (dataset/row-count out)) "narrowed to the two matching rows")
      (is (= #{"a" "b"} (set (mapv :zone (dataset/rows out :as-maps))))
          "kept exactly the :in rows"))))

(deftest map-over-groups-runs-each-chunk-and-preserves-keys
  (testing "map-over-groups runs the component per group, into keys the results"
    (let [d   (ds/->>dataset [{:zone "a" :n 1} {:zone "a" :n 2} {:zone "b" :n 3}])
          model (table-> :input (group-by :zone) (map rc-analyze) (into {}))
          out (run/run-model model {:input d} {:timeout-ms 60000})]
      (is (map? out) "into demotes the groups container to a keyed map")
      (is (= #{"a" "b"} (set (keys out))) "one result per group, keyed by group")
      (is (dataset/dataset? (get out "a")) "each group's result is a dataset")
      (is (= 2 (dataset/row-count (get out "a"))) "group a kept its 2 rows")
      (is (= 1 (dataset/row-count (get out "b"))) "group b kept its 1 row")
      (is (= [2 4] (sort (mapv :doubled (dataset/rows (get out "a") :as-maps))))
          "group a's body doubled n per row")
      (is (= [6] (mapv :doubled (dataset/rows (get out "b") :as-maps)))
          "group b's body doubled n per row"))))

(deftest map-over-groups-emits-per-group-trace
  ;; The over-groups executor emits per-group lifecycle trace events
  ;; (run-component-groups) so the canvas can light the map node per group.
  ;; Assert one map-group-complete per group, bracketed by a single
  ;; map-groups-start/complete pair, all carrying the map node's pid
  ;; (<model-name>__<node-id>) so the canvas correlates them to the node's
  ;; proc-* events.
  (testing "a map-over-groups run emits one map-group-complete per group"
    (let [d      (ds/->>dataset [{:zone "a" :n 1} {:zone "a" :n 2} {:zone "b" :n 3}])
          model  (table-> :input (group-by :zone) (map rc-analyze) (into {}))
          _      (trace/clear-recent-events!)
          _      (run/run-model model {:input d}
                                {:timeout-ms 60000 :name "trace-test-flow"})
          mine?  (fn [e] (boolean (some-> (:pid e)
                                          (str/starts-with? "trace-test-flow__"))))
          ;; recent-events drains the trace channel asynchronously; poll until
          ;; the terminal map-groups-complete lands (it is put after every
          ;; per-group event, so its arrival means the per-group events are in).
          events (loop [tries 0]
                   (let [evs (filterv mine? (trace/recent-events))]
                     (if (or (some #(= "map-groups-complete" (:kind %)) evs)
                             (>= tries 60))
                       evs
                       (do (Thread/sleep 50) (recur (inc tries))))))
          of-kind (fn [k] (filterv #(= k (:kind %)) events))]
      (is (= 2 (count (of-kind "map-group-complete")))
          "one map-group-complete per group")
      (is (= #{"a" "b"} (set (map :group-key (of-kind "map-group-complete"))))
          "per-group events key by the group-by column value")
      (is (= 1 (count (of-kind "map-groups-start")))
          "one map-groups-start brackets the run")
      (is (= 1 (count (of-kind "map-groups-complete")))
          "one map-groups-complete brackets the run")
      (is (= 2 (:group-count (first (of-kind "map-groups-start"))))
          "map-groups-start reports the group count"))))

(deftest copied-dynamic-vars-keep-the-host-root
  ;; cg.dsl copies cg.run into SCI one time, when cg.dsl first loads. If
  ;; that load happens inside a host binding, the copy takes the bound
  ;; value; root-dynamic-vars! gives it the root again.
  (binding [run/*session-file* "bound.cg"
            run/*script-file* "script.cg"]
    (let [sci-ns (sci/create-ns 'cg.run)
          copied {'*session-file* (sci/copy-var run/*session-file* sci-ns)
                  '*script-file*  (sci/copy-var run/*script-file* sci-ns)}]
      (is (= "bound.cg" (deref (get copied '*session-file*)))
          "copy-var takes the bound value")
      ((requiring-resolve 'cg.dsl/root-dynamic-vars!) 'cg.run copied)
      (is (nil? (deref (get copied '*session-file*))))
      (is (nil? (deref (get copied '*script-file*)))))))

;; In :table mode a unary geometry op maps over the :geometry column and
;; does not get the whole dataset.
(def ^:private table-op-polys
  [(f/polygon [[0 0] [4 0] [4 4] [0 4] [0 0]])
   (f/polygon [[10 0] [14 0] [14 4] [10 4] [10 0]])
   (f/polygon [[20 0] [24 0] [24 4] [20 4] [20 0]])])

(defn- run-table-op
  "Run one geometry op in :table mode over a 3-polygon dataset; return the result."
  [op-fn params]
  (let [test-ds (ds/->dataset {:id [1 2 3]
                               :name ["A" "B" "C"]
                               :geometry table-op-polys})
        model {:nodes [{:type :source :id :data-src :data-ref :data}
                       {:type :operation :id :op-1 :fn op-fn :params params
                        :mode :table :execution :table-level}
                       {:type :sink :id :result :output-binding :result}]
               :edges [{:from :data-src :to :op-1 :type :data-flow}
                       {:from :op-1 :to :result :type :data-flow}]
               :config {}}]
    (run/run-model model {:data test-ds} {:timeout-ms 10000})))

(defn- table-op-invariants
  "Routing assertions shared by every per-geometry table-mode op."
  [result]
  (is (some? result) "Should return a result")
  (is (ds/dataset? result) "Result should be a dataset")
  (is (= 3 (ds/row-count result)) "Should preserve row count")
  (is (ds/has-column? result :geometry) "Should have geometry column")
  (is (ds/has-column? result :id) "Should preserve id column")
  (is (ds/has-column? result :name) "Should preserve name column")
  (is (every? some? (vec (:geometry result))) "All geometries present"))

(deftest test-table-mode-simple-buffer
  (testing "Table mode: per-geometry buffer over the geometry column"
    (let [result (run-table-op 'cg.geo/buffer {:distance 5})]
      (table-op-invariants result)
      (is (every? #(instance? org.locationtech.jts.geom.Polygon %) (vec (:geometry result)))
          "buffer of a polygon is a polygon")
      (is (every? #(> (.getArea ^org.locationtech.jts.geom.Geometry %) 16.0)
                  (vec (:geometry result)))
          "each 4 x 4 square grew"))))

(deftest test-table-mode-simplify
  (testing "Table mode: per-geometry simplify over the geometry column"
    (let [result (run-table-op 'cg.geo/simplify {:tolerance 0.5})]
      (table-op-invariants result)
      (is (every? #(instance? org.locationtech.jts.geom.Polygon %) (vec (:geometry result)))
          "simplify of a polygon stays a polygon"))))

(deftest test-table-mode-centroid
  (testing "Table mode: per-geometry centroid over the geometry column"
    (let [result (run-table-op 'cg.geo/centroid {})]
      (table-op-invariants result)
      (is (every? #(instance? org.locationtech.jts.geom.Point %) (vec (:geometry result)))
          "centroid of a polygon is a point"))))

(deftest test-table-mode-convex-hull
  (testing "Table mode: per-geometry convex-hull over the geometry column"
    (let [result (run-table-op 'cg.geo/convex-hull {})]
      (table-op-invariants result)
      (is (every? #(instance? org.locationtech.jts.geom.Polygon %) (vec (:geometry result)))
          "convex hull of a square is a polygon"))))

(deftest test-table-mode-envelope
  (testing "Table mode: per-geometry envelope over the geometry column"
    (let [result (run-table-op 'cg.geo/envelope {})]
      (table-op-invariants result)
      (is (every? #(instance? org.locationtech.jts.geom.Polygon %) (vec (:geometry result)))
          "envelope of a polygon is a rectangular polygon"))))

(deftest test-table-mode-boundary
  (testing "Table mode: per-geometry boundary over the geometry column"
    (let [result (run-table-op 'cg.geo/boundary {})]
      (table-op-invariants result)
      (is (every? #(instance? org.locationtech.jts.geom.LineString %) (vec (:geometry result)))
          "boundary of a polygon is its line ring"))))

(deftest test-table-mode-context-store
  (testing "Dataset context store registration and cleanup"
    (dataset/reset-dataset-context-store!)

    (let [points [(f/point 0 0)]
          test-ds (ds/->dataset {:id [1] :geometry points})

          ctx-id (dataset/register-dataset-context! test-ds {:geom-column :geometry})]

      (is (uuid? ctx-id) "Should return a UUID")

      (let [ctx (dataset/get-dataset-context ctx-id)]
        (is (some? ctx) "Should retrieve context")
        (is (= [:id :geometry] (:schema ctx)) "Schema should match columns")
        (is (= :geometry (:geom-column ctx)) "Geometry column should be stored")
        (is (= 1 (:row-count ctx)) "Row count should be stored"))

      (dataset/unregister-dataset-context! ctx-id)
      (is (nil? (dataset/get-dataset-context ctx-id)) "Context should be removed"))))

(deftest test-dataset-batching
  (testing "Dataset batching and reassembly"
    ;; Test with a small dataset - should return as single batch
    (let [n-rows 100
          points (repeatedly n-rows #(f/point (rand 100) (rand 100)))
          test-ds (ds/->dataset {:id (range n-rows)
                                 :geometry (vec points)})

          batches (dataset/dataset->batches test-ds {})]

      (is (>= (count batches) 1) "Should create at least one batch")
      (is (= n-rows (reduce + (map ds/row-count batches)))
          "Total rows across batches should equal original")

      (let [reassembled (dataset/batches->dataset batches)]
        (is (= n-rows (ds/row-count reassembled))
            "Reassembled dataset should have all rows")
        (is (ds/has-column? reassembled :id) "Should preserve id column")
        (is (ds/has-column? reassembled :geometry) "Should preserve geometry column")))))
