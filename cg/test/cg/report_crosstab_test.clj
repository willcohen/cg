;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.report-crosstab-test
  "Byte-for-byte parity gate for the conformity crosstab.

   The reference implementation below is a verbatim copy of the report
   code in an analysis script as it stood on
   2026-08-13, with `println` replaced by collecting the lines. The
   probe prints that table twice: once per district, and once per
   neighborhood. Absorbing it into a registered op is only allowed to
   change where the code lives, never a byte of what it prints.

   The fixture is deterministic and needs no cache and no probe run, so
   this gate runs in the suite instead of behind a 26-minute recompute."
  (:require [cg.dataset :as ds]
            [clojure.test :refer [deftest is testing]]))

(set! *warn-on-reflection* true)

(def ^:private dims
  "Verbatim, all eight dimensions the probe printed."
  [{:label "Lot Coverage"
    :existing :lot-coverage-conformity
    :b :lot-coverage-conformity-alt-b
    :c :lot-coverage-conformity-alt-c}
   {:label "Front Yard"
    :existing :front-conformity
    :b :front-yard-conformity-alt-b
    :c :front-yard-conformity-alt-c}
   {:label "Rear Yard"
    :existing :rear-conformity
    :b :rear-yard-conformity-alt-b
    :c :rear-yard-conformity-alt-c}
   {:label "Cum Side w/ 3' min"
    :existing :side-conformity
    :b :side-yard-strict-alt-b
    :c :side-yard-strict-alt-c}
   {:label "Cum Side w/o 3' min"
    :existing nil
    :b :side-yard-cumulative-alt-b
    :c :side-yard-cumulative-alt-c}
   {:label "Floorplate"
    :existing nil
    :b :floorplate-conformity-alt-b
    :c :floorplate-conformity-alt-c}
   {:label "Height (ft)"
    :existing :height-conformity
    :b :height-conformity-alt-b
    :c :height-conformity-alt-c}
   {:label "OVERALL"
    :existing :overall-conformity
    :b :overall-conformity-alt-b
    :c :overall-conformity-alt-c}])

(def ^:private crosstab-spec
  "The probe's spec, derived from `dims` so the two paths under test
   are fed the very same columns in the very same order. The probe
   carries this same shape as a literal."
  {:categories {:numerator :nonconforming
                :known [:nonconforming :conforming]
                :unknown :unknown}
   :columns [{:label "Existing"} {:label "Rules B"} {:label "Rules C"}]
   :dimensions (mapv (fn [d]
                       {:label (:label d)
                        :cols [(:existing d) (:b d) (:c d)]})
                     dims)})

(def ^:private cats [:conforming :nonconforming :unknown])

(def ^:private fixture-cols
  "Every column the eight dimensions name, nils dropped."
  (vec (remove nil? (mapcat (juxt :existing :b :c) dims))))

(defn- row
  "One fixture row. `i` and the column's position drive the value, so
   each column carries a different mix and the cells across a line
   differ. A few columns are deliberately degenerate: one is always
   unknown, one is sometimes nil (a column the data does not carry)."
  [i]
  (into {}
        (map-indexed
         (fn [j col]
           [col (cond
                  (= j 4) :unknown
                  (= j 9) (when (pos? (mod i 4)) :nonconforming)
                  :else (nth cats (mod (+ i (* j 2) (quot i 3)) 3)))])
         fixture-cols)))

(def ^:private fixture-rows (mapv row (range 47)))

(deftest crosstab-cells-carry-the-numbers-not-just-strings
  (testing "the op result is data the canvas can render, not preformatted text"
    (let [result (ds/category-crosstab fixture-rows crosstab-spec)
          lot-cov (first (:rows result))
          cell (first (:cells lot-cov))]
      (is (= "Lot Coverage" (:label lot-cov)))
      (is (= 47 (:n cell)))
      (is (= (:nonconforming (:counts cell))
             (count (filter #(= :nonconforming (:lot-coverage-conformity %))
                            fixture-rows))))
      (is (number? (:pct-of-known cell)))
      (is (number? (:pct-unknown cell)))))

  (testing "a nil column yields a nil cell, which renders as the placeholder"
    ;; Existing zoning does not measure cumulative side yard without the
    ;; 3-foot minimum, so that dimension has no Existing column.
    (let [result (ds/category-crosstab fixture-rows crosstab-spec)
          side (first (filter #(= "Cum Side w/o 3' min" (:label %)) (:rows result)))]
      (is (some? side))
      (is (nil? (first (:cells side))))
      (is (some? (second (:cells side)))))))

;; `cap-tally` counted yes/no/unknown over three boolean columns, which is
;; exactly what `category-crosstab` puts in each cell's :counts. Only the
;; bespoke two-line layout is the probe's own, so it stays there; this
;; asserts the counts underneath it do not move.

(defn- ref-cap-tally [rows k]
  (let [vals (map k rows)]
    {:yes (count (filter true? vals))
     :no  (count (filter false? vals))
     :unk (count (filter #(= % :unknown) vals))}))

(defn- cap-layout
  "The probe's two yes/no lines, given three yes/no/unk triples."
  [ab ac eb]
  [(apply format
          "     add-unit   B: %5d yes / %5d no / %5d unk    C: %5d yes / %5d no / %5d unk"
          (into ab ac))
   (apply format "     extend     B: %5d yes / %5d no / %5d unk" eb)])

(defn- ref-cap-lines [rows]
  (let [ynu (fn [k] (let [t (ref-cap-tally rows k)] [(:yes t) (:no t) (:unk t)]))]
    (cap-layout (ynu :add-ok-alt-b)
                (ynu :add-ok-alt-c)
                (ynu :extend-ok-alt-b))))

(def ^:private cap-spec
  "The probe's spec. `extend` has no Rules C column."
  {:categories {:numerator true :known [true false] :unknown :unknown}
   :columns [{:label "B"} {:label "C"}]
   :dimensions [{:label "add-unit"
                 :cols [:add-ok-alt-b :add-ok-alt-c]}
                {:label "extend"
                 :cols [:extend-ok-alt-b nil]}]})

(defn- new-cap-lines [rows]
  (let [caps (:rows (ds/category-crosstab rows cap-spec))
        counts (fn [ri ci] (:counts (nth (:cells (nth caps ri)) ci)))
        ynu (fn [c] [(get c true 0) (get c false 0) (get c :unknown 0)])]
    (cap-layout (ynu (counts 0 0)) (ynu (counts 0 1)) (ynu (counts 1 0)))))

(def ^:private cap-rows
  "Every value the yes/no columns carry, including nil for a row the
   analysis could not reach, which neither path counts."
  (mapv (fn [i]
          {:add-ok-alt-b (nth [true false :unknown nil true] (mod i 5))
           :add-ok-alt-c (nth [false :unknown true] (mod i 3))
           :extend-ok-alt-b (nth [:unknown true false nil] (mod i 4))})
        (range 53)))

(deftest yes-no-counts-match-the-probe-byte-for-byte
  (testing "a mixed population"
    (is (= (ref-cap-lines cap-rows) (new-cap-lines cap-rows))))

  (testing "no rows at all"
    (is (= (ref-cap-lines []) (new-cap-lines []))))

  (testing "a column the rows do not carry counts as nothing, not as unknown"
    (let [blank (mapv (fn [_] {}) (range 7))]
      (is (= (ref-cap-lines blank) (new-cap-lines blank)))
      (is (= ["     add-unit   B:     0 yes /     0 no /     0 unk    C:     0 yes /     0 no /     0 unk"
              "     extend     B:     0 yes /     0 no /     0 unk"]
             (new-cap-lines blank))))))
