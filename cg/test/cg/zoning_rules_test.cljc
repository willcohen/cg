;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.zoning-rules-test
  "The rule-set checker of zoning.cg, on both runtimes. A rule set is data,
   and each test here is an example of one form of that data. The JS runtime
   holds a keyword as a string and a map as an object, therefore each form
   runs on both runtimes."
  #?(:clj (:require [clojure.test :refer [deftest is testing use-fixtures]]
                    [cg.run :as run])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../src/cg/run.mjs" :as run]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private zoning "src/cg/zoning.cg")

#?(:clj (use-fixtures :once (fn [t] (run/load-module zoning) (t))))

(defn- ev [src]
  (run/eval-cg (str "(require '[cg.zoning :as zoning]) " src)))

;; limit-value tells a number, a limit form and an expression apart with
;; number?, map? and vector?. The values come from a rule set, not from a
;; literal that the compiler sees.
(deftest a-value-from-data-keeps-its-kind
  (is (= [:number :map :vector :other :other]
         (vec (ev "(let [kind (fn [v] (cond (number? v) :number
                                             (map? v)    :map
                                             (vector? v) :vector
                                             :else       :other))
                          rule {:n 3
                                :limit {:form :row :key :k}
                                :expr [:larger 4.0 [:* 0.1 :lot-frontage]]}]
                      (mapv kind [(get rule :n) (get rule :limit) (get rule :expr)
                                  (get-in rule [:limit :form])
                                  (rest (get rule :expr))]))")))))

(defn- limit-value [limit values facts]
  (ev (str "(zoning/limit-value " limit " " values " " facts ")")))

(deftest a-limit-is-a-number
  (is (= 12.5 (limit-value "12.5" "{}" "{}"))))

(deftest a-row-limit-reads-a-column
  (let [limit "{:form :row :key :cap}"]
    (is (= 20.0 (limit-value limit "{}" "{:cap 20.0}")))
    (is (nil? (limit-value limit "{}" "{}")))
    (is (nil? (limit-value limit "{}" "{:cap \"none\"}"))
        "a value that is not a number is no limit")))

(deftest a-lookup-limit-reads-a-nested-table
  (let [table "{\"A\" {\"x\" 7.0 \"y\" {:form :row :key :cap}}}"
        limit (str "{:form :lookup :table " table " :by [:zone :sub]}")]
    (testing ":by gives the row keys whose values are the path"
      (is (= 7.0 (limit-value limit "{}" "{:zone \"A\" :sub \"x\"}"))))
    (testing "a leaf can be a limit form"
      (is (= 9.0 (limit-value limit "{}" "{:zone \"A\" :sub \"y\" :cap 9.0}"))))
    (testing "no entry is no limit"
      (is (nil? (limit-value limit "{}" "{:zone \"B\" :sub \"x\"}")))
      (is (nil? (limit-value limit "{}" "{}"))))
    (testing ":at gives literal last steps"
      (is (= 11.0 (limit-value "{:form :lookup :table {\"A\" {:height 11.0}}
                                 :by [:zone] :at [:height]}"
                               "{}" "{:zone \"A\"}"))))))

(deftest a-brackets-limit-changes-with-a-measure
  (let [limit "{:form :brackets :by :lot-area
                :steps [{:up-to 500.0 :limit 0.9}
                        {:up-to 900.0 :limit 0.7}
                        {:limit [:larger 0.3 [:/ 2400.0 :lot-area]]}]}"
        at    (fn [area] (limit-value limit (str "{:lot-area " area "}") "{}"))]
    (is (= 0.9 (at 400.0)))
    (is (= 0.9 (at 500.0)) "a value equal to :up-to is in the step")
    (is (= 0.7 (at 600.0)))
    (is (= 0.4 (at 6000.0)) "the last step has no :up-to, and its limit is an expression")
    (is (= 0.3 (at 12000.0)))
    (is (nil? (limit-value limit "{}" "{}")) "an unknown measure is no limit")))

(deftest a-per-unit-limit-adds-for-each-unit
  (let [limit "{:form :per-unit :by :dwelling-units :base 3000 :base-n 1 :per 1000}"
        at    (fn [units] (limit-value limit (str "{:dwelling-units " units "}") "{}"))]
    (is (= 5000 (at 3)) "3,000 plus 1,000 for each unit more than 1")
    (is (= 3000 (at 1)))
    (is (= 3000 (limit-value limit "{}" "{}")) "an unknown count gives :base")
    (is (= 6000 (limit-value "{:form :per-unit :by :dwelling-units :base 6000 :base-n 2 :per nil}"
                             "{:dwelling-units 5}" "{}"))
        "a nil :per adds nothing")
    (is (= 6000 (limit-value "{:form :per-unit :by :dwelling-units :base 6000 :base-n 2 :per 500}"
                             "{:dwelling-units 1}" "{}"))
        "never less than :base")))

(deftest a-first-limit-is-a-fallback
  (let [limit "{:form :first :of [{:form :lookup :table {\"A\" 7.0} :by [:zone]}
                                  {:form :row :key :cap}]}"]
    (is (= 7.0 (limit-value limit "{}" "{:zone \"A\" :cap 20.0}")))
    (is (= 20.0 (limit-value limit "{}" "{:zone \"B\" :cap 20.0}")))
    (is (nil? (limit-value limit "{}" "{:zone \"B\"}")))))

(deftest an-expression-limit-calculates
  (testing "each operator"
    (is (= 3 (limit-value "[:+ 1 2]" "{}" "{}")))
    (is (= 6 (limit-value "[:- 10 4]" "{}" "{}")))
    (is (= 12 (limit-value "[:* 3 4]" "{}" "{}")))
    (is (= 2.0 (limit-value "[:/ 6 3]" "{}" "{}")))
    (is (= 9.0 (limit-value "[:larger 4.0 9.0]" "{}" "{}")))
    (is (= 4.0 (limit-value "[:smaller 4.0 9.0]" "{}" "{}"))))
  (testing "an operand is a number, a measure id, an expression or a limit form"
    (is (= 20.0 (limit-value "[:* 0.5 :lot-frontage]" "{:lot-frontage 40.0}" "{}")))
    (is (= 23.0 (limit-value "[:+ 18.0 [:larger 0 [:- :building-height 30.0]]]"
                             "{:building-height 35.0}" "{}")))
    (is (= 18.0 (limit-value "[:+ 18.0 [:larger 0 [:- :building-height 30.0]]]"
                             "{:building-height 20.0}" "{}")))
    (is (= 6.0 (limit-value "[:larger {:form :row :key :cap} [:* 0.1 :lot-frontage]]"
                            "{:lot-frontage 40.0}" "{:cap 6.0}"))))
  (testing "an operand with no value, and a division by 0, give no limit"
    (is (nil? (limit-value "[:* 0.5 :lot-frontage]" "{}" "{}")))
    (is (nil? (limit-value "[:/ 2400.0 :lot-area]" "{:lot-area 0}" "{}")))))

#?(:clj
   (deftest a-limit-keeps-a-long-and-divides-doubles
     (is (integer? (limit-value "[:+ 3000 [:* 2 1000]]" "{}" "{}")))
     (is (integer? (limit-value "[:- 3000 1000]" "{}" "{}")))
     (is (integer? (limit-value "{:form :per-unit :by :dwelling-units :base 3000 :base-n 1 :per 1000}"
                                "{:dwelling-units 3}" "{}")))
     (is (double? (limit-value "[:/ 6 3]" "{}" "{}")) "two longs must not give a ratio")))

(defn- condition-value [condition values]
  (ev (str "(zoning/condition-value " condition " " values " {})")))

(deftest a-comparison-condition
  (testing "each operator"
    (is (= true (condition-value "[:= :dwelling-units 2]" "{:dwelling-units 2}")))
    (is (= false (condition-value "[:= :dwelling-units 2]" "{:dwelling-units 3}")))
    (is (= true (condition-value "[:not= :dwelling-units 2]" "{:dwelling-units 3}")))
    (is (= true (condition-value "[:< :lot-area 5000.0]" "{:lot-area 4000.0}")))
    (is (= false (condition-value "[:< :lot-area 5000.0]" "{:lot-area 5000.0}")))
    (is (= true (condition-value "[:<= :lot-area 5000.0]" "{:lot-area 5000.0}")))
    (is (= true (condition-value "[:> :dwelling-units 2]" "{:dwelling-units 3}")))
    (is (= false (condition-value "[:> :dwelling-units 2]" "{:dwelling-units 2}")))
    (is (= true (condition-value "[:>= :dwelling-units 2]" "{:dwelling-units 2}"))))
  (testing "an operand is a number, true, false, a measure id or an expression"
    (is (= true (condition-value "[:= :corner? true]" "{:corner? true}")))
    (is (= false (condition-value "[:= :corner? true]" "{:corner? false}")))
    (is (= true (condition-value "[:> [:* 2 :dwelling-units] 5]" "{:dwelling-units 3}")))
    (is (= true (condition-value "[:= :dwelling-units 2]" "{:dwelling-units 2.0}"))
        "2 and 2.0 are equal"))
  (testing "an operand with no value makes the condition unknown"
    (is (nil? (condition-value "[:> :dwelling-units 2]" "{}")))
    (is (nil? (condition-value "[:= :corner? true]" "{}")))))

(deftest a-membership-condition-takes-literal-names
  (let [c "[:in :lot-type [:corner :end-cap]]"]
    (is (= true (condition-value c "{:lot-type :corner}")))
    (is (= false (condition-value c "{:lot-type :interior}")))
    (is (= true (condition-value c "{:lot-type \"end-cap\"}"))
        "a cached row holds a keyword as its name")
    (is (nil? (condition-value c "{}")))))

(deftest a-logic-condition-has-three-values
  (let [both   "[:and [:> :a 1] [:> :b 1]]"
        either "[:or [:> :a 1] [:> :b 1]]"
        not-a  "[:not [:> :a 1]]"]
    (testing ":and is false when one part is false, else unknown when one part is unknown"
      (is (= true (condition-value both "{:a 2 :b 2}")))
      (is (= false (condition-value both "{:a 2 :b 0}")))
      (is (= false (condition-value both "{:a 0}")))
      (is (nil? (condition-value both "{:a 2}"))))
    (testing ":or is the mirror"
      (is (= true (condition-value either "{:a 2}")))
      (is (= false (condition-value either "{:a 0 :b 0}")))
      (is (nil? (condition-value either "{:a 0}"))))
    (testing ":not"
      (is (= false (condition-value not-a "{:a 2}")))
      (is (= true (condition-value not-a "{:a 0}")))
      (is (nil? (condition-value not-a "{}"))))))

(defn- measure-values [measures row schema]
  (ev (str "(zoning/measure-values {:measures " measures "} " row " " schema ")")))

(defn- measure
  "The value of the one measure `spec`, which has the id :m."
  [spec row schema]
  (get (measure-values (str "[" spec "]") row schema) :m))

(def ^:private edges-row
  "{:edges [{:edge-type :front :setback 10.0 :length 40.0}
            {:edge-type :primary-front :setback 12.0 :length 30.0}
            {:edge-type :side :setback 4.0 :length 80.0}
            {:edge-type :side :setback 6.0 :length 80.0}
            {:edge-type :rear :setback nil :length 40.0}]}")

(deftest a-row-measure-and-a-metric-measure
  (is (= 30.5 (measure "{:id :m :form :row :key :h}" "{:h 30.5}" "{}")))
  (is (nil? (measure "{:id :m :form :row :key :h}" "{}" "{}")))
  (is (= 5000.0 (measure "{:id :m :form :metric :key :lot-area}"
                         "{:metrics {:lot-area 5000.0}}" "{}"))))

(deftest a-schema-col-measure-reads-the-column-that-the-schema-names
  (let [units "{:id :m :form :schema-col :key :units-col :parse :long}"
        known "{:id :m :form :schema-col :key :units-col :parse :long :pos-only? true}"
        area  "{:id :m :form :schema-col :key :gross-area-col :parse :double}"]
    (is (= 3 (measure units "{:UNITS \"3\"}" "{:units-col :UNITS}")))
    (is (= 1200.5 (measure area "{:GFA \"1200.5\"}" "{:gross-area-col :GFA}")))
    (is (nil? (measure units "{:UNITS 3}" "{}")) "a schema with no such key gives no value")
    (testing ":pos-only? makes a value that is not positive unknown"
      (is (= 2 (measure known "{:UNITS 2}" "{:units-col :UNITS}")))
      (is (nil? (measure known "{:UNITS 0}" "{:units-col :UNITS}")))
      (is (= 0 (measure units "{:UNITS 0}" "{:units-col :UNITS}"))))))

(deftest an-edges-measure-aggregates-one-key-of-the-edges
  (let [spec (fn [types k agg]
               (str "{:id :m :form :edges :types " types " :key " k " :agg " agg "}"))]
    (is (= 10.0 (measure (spec "[:front :primary-front]" ":setback" ":min") edges-row "{}")))
    (is (= 12.0 (measure (spec "[:front :primary-front]" ":setback" ":max") edges-row "{}")))
    (is (= 10.0 (measure (spec "[:side]" ":setback" ":sum") edges-row "{}")))
    (is (= 70.0 (measure (spec "[:front :primary-front]" ":length" ":sum") edges-row "{}")))
    (is (nil? (measure (spec "[:rear]" ":setback" ":min") edges-row "{}"))
        "no numeric value gives no value")
    (is (nil? (measure (spec "[:side]" ":setback" ":min") "{}" "{}")))))

(deftest a-ratio-measure-and-an-alias-measure
  (let [ratio "[{:id :a :form :row :key :a} {:id :b :form :row :key :b}
                {:id :m :form :ratio :num :a :den :b}]"
        alias "[{:id :a :form :row :key :a} {:id :m :form :alias :of :a}]"]
    (is (= 0.3 (get (measure-values ratio "{:a 1200.0 :b 4000.0}" "{}") :m)))
    (is (nil? (get (measure-values ratio "{:a 1200.0 :b 0}" "{}") :m))
        "a denominator that is not positive gives no value")
    (is (nil? (get (measure-values ratio "{:b 4000.0}" "{}") :m)))
    (is (= 7.5 (get (measure-values alias "{:a 7.5}" "{}") :m)))))

(deftest the-canonical-measures-read-an-analyzed-row
  (let [row    (str "(assoc " edges-row
                    " :metrics {:lot-area 4000.0 :building-coverage-ratio 0.25}
                      :main-building-footprint-area 900.0 :main-building-height 28.0
                      :GFA 2000.0 :UNITS 2)")
        values (ev (str "(zoning/measure-values {:measures zoning/canonical-measures} " row
                        " {:gross-area-col :GFA :units-col :UNITS})"))]
    (is (= 10.0 (get values :front-setback)))
    (is (nil? (get values :rear-setback)))
    (is (= 4.0 (get values :side-setback)))
    (is (= 10.0 (get values :cumulative-side-setback)))
    (is (= 4000.0 (get values :lot-area)))
    (is (= 0.25 (get values :building-lot-coverage)))
    (is (= 900.0 (get values :building-footprint)))
    (is (= 28.0 (get values :building-height)))
    (is (= 2000.0 (get values :gross-floor-area)))
    (is (= 2 (get values :dwelling-units)))
    (is (= 0.5 (get values :floor-area-ratio)))
    (is (= 2000.0 (get values :lot-area-per-dwelling-unit)))
    (testing "the two measures with no data"
      (is (nil? (get values :roof-slope)))
      (is (nil? (get values :off-street-parking-spaces))))))

#?(:clj
   (deftest a-ratio-of-two-longs-is-a-double
     (is (double? (get (measure-values "[{:id :a :form :row :key :a} {:id :b :form :row :key :b}
                                         {:id :m :form :ratio :num :a :den :b}]"
                                       "{:a 6 :b 3}" "{}")
                       :m)))))

(defn- rule-set-message
  "The message of the exception that rule-set throws for `data`, or nil
   when the data is legal."
  [data]
  (ev (str "(require '[cg.util :as util])
            (util/try-catch (fn [] (zoning/rule-set " data ") nil)
                            (fn [e] (ex-message e)))")))

(def ^:private two-measures
  "[{:id :height :quantity :length :form :row :key :h}
    {:id :area :quantity :area :form :row :key :a}]")

(defn- with-rule
  "A rule set of two-measures and the one rule `rule`."
  [rule]
  (str "{:measures " two-measures " :rules [" rule "]}"))

(defn- with-measure
  "A rule set of two-measures plus the measure `spec`, and no rule."
  [spec]
  (str "{:measures (conj " two-measures " " spec ") :rules []}"))

(defn- names-both? [message bad legal]
  (and (string? message) (re-find bad message) (re-find legal message)))

(deftest a-legal-rule-set-loads
  (is (nil? (rule-set-message
             (with-rule "{:id :tall :measure :height :dir :ceiling :core? true :limit 35.0}")))))

(deftest rule-set-rejects-a-form-that-is-not-legal-and-lists-the-legal-values
  (testing "a measure form"
    (is (names-both? (rule-set-message (with-measure "{:id :x :form :rows :key :h}"))
                     #"rows" #"schema-col")))
  (testing "an aggregate and a parse of a measure"
    (is (names-both? (rule-set-message
                      (with-measure "{:id :x :form :edges :types [:side] :key :setback :agg :mean}"))
                     #"mean" #"sum"))
    (is (names-both? (rule-set-message
                      (with-measure "{:id :x :form :schema-col :key :c :parse :int}"))
                     #"int" #"double")))
  (testing "a limit form, also as the leaf of a lookup table"
    (is (names-both? (rule-set-message
                      (with-rule "{:id :r :measure :height :dir :floor :limit {:form :table}}"))
                     #"table" #"brackets"))
    (is (names-both? (rule-set-message
                      (with-rule "{:id :r :measure :height :dir :floor
                                   :limit {:form :lookup :by [:zone]
                                           :table {\"A\" {:form :table}}}}"))
                     #"table" #"brackets")))
  (testing "an operator of an expression and of a condition"
    (is (names-both? (rule-set-message
                      (with-rule "{:id :r :measure :height :dir :floor :limit [:max 1 2]}"))
                     #"max" #"larger"))
    (is (names-both? (rule-set-message
                      (with-rule "{:id :r :measure :height :dir :floor :limit 1
                                   :when [:gt :height 1]}"))
                     #"gt" #"not=")))
  (testing "a direction"
    (is (names-both? (rule-set-message
                      (with-rule "{:id :r :measure :height :dir :at-least :limit 1}"))
                     #"at-least" #"ceiling"))))

(deftest rule-set-rejects-a-name-that-is-no-measure-and-lists-the-measures
  (doseq [rule ["{:id :r :measure :depth :dir :floor :limit 1}"
                "{:id :r :measure :height :dir :floor :limit [:* 0.5 :depth]}"
                "{:id :r :measure :height :dir :floor
                  :limit {:form :brackets :by :depth :steps [{:limit 1}]}}"
                "{:id :r :measure :height :dir :floor :limit 1 :when [:> :depth 1]}"
                "{:id :r :measure :height :dir :floor :limit 1 :when [:in :depth [:a]]}"
                "{:id :r :measure :height :dir :floor :limit 1 :lower-bound-without :depth}"
                "{:id :r :measure :height :dir :floor :limit 1 :margin-times :depth}"]]
    (is (names-both? (rule-set-message (with-rule rule)) #"depth" #"height, area") rule))
  (testing "the parts of a :ratio and the target of an :alias"
    (is (names-both? (rule-set-message
                      (with-measure "{:id :x :form :ratio :num :height :den :depth}"))
                     #"depth" #"height, area"))
    (is (names-both? (rule-set-message (with-measure "{:id :x :form :alias :of :depth}"))
                     #"depth" #"height, area")))
  (testing "a key of :columns"
    (is (names-both? (rule-set-message
                      (str "{:measures " two-measures " :rules [] :columns {:depth :actual-depth}}"))
                     #"depth" #"height, area"))))

(deftest rule-set-rejects-a-name-that-is-no-rule
  (let [tall "{:id :tall :measure :height :dir :ceiling :limit 35.0}"]
    (testing ":all-of names rules that come before it"
      (is (names-both? (rule-set-message
                        (str "{:measures " two-measures
                             " :rules [" tall " {:id :both :all-of [:tall :wide]}]}"))
                       #"wide" #"tall")))
    (testing ":per-edge names rules on an :edges measure"
      (is (names-both? (rule-set-message
                        (str "{:measures " two-measures " :rules [" tall "]
                               :per-edge {:column :each-edge :rules [:tall]}}"))
                       #"tall" #"edges")))))

(deftest rule-set-rejects-two-results-with-one-column
  (let [rule (fn [id more] (str "{:id " id " :measure :height :dir :ceiling :limit 35.0 " more "}"))
        set  (fn [rules more] (str "{:measures " two-measures " :rules [" rules "] " more "}"))]
    (is (re-find #"same-column"
                 (rule-set-message (set (str (rule ":a" ":column :same-column")
                                             (rule ":b" ":column :same-column"))
                                        ""))))
    (is (re-find #"a-conformity"
                 (rule-set-message (set (rule ":a" "") ":columns {:area :a-conformity}")))
        "a measure column and a verdict column")
    (is (re-find #"\ba\b" (rule-set-message (set (str (rule ":a" "") (rule ":a" "")) "")))
        "two rules with one id")))

(deftest rule-set-rejects-a-measure-that-reads-itself
  (is (re-find #"reads itself"
               (rule-set-message "{:measures [{:id :a :form :alias :of :b}
                                              {:id :b :form :ratio :num :a :den :a}]
                                   :rules []}"))))

(deftest rule-set-puts-the-measures-in-a-safe-order
  (is (= 7.5 (get (ev "(zoning/measure-values
                         (zoning/rule-set {:measures [{:id :m :form :alias :of :a}
                                                      {:id :a :form :row :key :a}]
                                           :rules []})
                         {:a 7.5} {})")
                  :m))))

(def ^:private three-measures
  "[{:id :height :form :row :key :h}
    {:id :area :form :row :key :a}
    {:id :units :form :schema-col :key :units-col :parse :long :pos-only? true}]")

(defn- checked
  "Evaluate `expr` with `rs` (the rule set of three-measures, `rules` and
   `more`), `values` and `results` for `row` in scope."
  [rules more row expr]
  (ev (str "(let [rs (zoning/rule-set {:measures " three-measures " :rules " rules " " more "})
                  values (zoning/measure-values rs " row " {:units-col :UNITS})
                  results (zoning/rule-results rs values " row ")]
              " expr ")")))

;; A keyword is a string on the JS runtime, therefore str drops its colon.
(defn- kw-text [k] (str ":" (name k)))

(defn- verdict-of [rules row id]
  (checked rules "" row (str "(get (get results " (kw-text id) ") :verdict)")))

(def ^:private tall "{:id :tall :measure :height :dir :ceiling :core? true :limit 35.0}")
(def ^:private big "{:id :big :measure :area :dir :floor :limit 5000}")

(deftest a-floor-and-a-ceiling
  (let [rules (str "[" tall " " big "]")]
    (is (= :conforming (verdict-of rules "{:h 35.0 :a 5000.0}" :tall))
        "a value equal to the limit conforms")
    (is (= :nonconforming (verdict-of rules "{:h 35.1 :a 5000.0}" :tall)))
    (is (= :conforming (verdict-of rules "{:h 35.0 :a 5000.0}" :big)))
    (is (= :nonconforming (verdict-of rules "{:h 35.0 :a 4999.0}" :big)))))

(deftest a-verdict-is-unknown-with-no-value-or-no-limit
  (is (= :unknown (verdict-of (str "[" tall "]") "{}" :tall)))
  (is (= :unknown (verdict-of "[{:id :tall :measure :height :dir :ceiling
                                 :limit {:form :row :key :cap}}]"
                              "{:h 30.0}" :tall)))
  (testing "a rule on a canonical measure that has no data"
    (is (= :unknown
           (ev "(let [rs (zoning/rule-set {:measures zoning/canonical-measures
                                           :rules [{:id :roof :measure :roof-slope
                                                    :dir :ceiling :limit 0.5}]})
                      values (zoning/measure-values rs {} {})]
                  (get (get (zoning/rule-results rs values {}) :roof) :verdict))")))))

(deftest a-lower-bound-rule-decides-only-a-nonconforming-lot
  (let [rules "[{:id :big :measure :area :dir :floor :lower-bound-without :units
                 :limit {:form :per-unit :by :units :base 3000 :base-n 1 :per 1000}}]"]
    (testing "an unknown unit count: the limit is :base"
      (is (= :nonconforming (verdict-of rules "{:a 2000.0 :UNITS 0}" :big)))
      (is (= :unknown (verdict-of rules "{:a 9000.0 :UNITS 0}" :big)))
      (is (= 3000 (checked rules "" "{:a 9000.0 :UNITS 0}" "(get (get results :big) :required)"))))
    (testing "a known unit count"
      (is (= :conforming (verdict-of rules "{:a 9000.0 :UNITS 3}" :big)))
      (is (= :nonconforming (verdict-of rules "{:a 4000.0 :UNITS 3}" :big))))))

(deftest a-rule-with-a-condition
  (let [rules "[{:id :tall :measure :height :dir :ceiling :limit 35.0 :when [:> :units 2]}]"]
    (is (= :nonconforming (verdict-of rules "{:h 40.0 :UNITS 3}" :tall)))
    (is (= :not-applicable (verdict-of rules "{:h 40.0 :UNITS 1}" :tall)))
    (is (= :unknown (verdict-of rules "{:h 40.0 :UNITS 0}" :tall))
        "an unknown condition gives :unknown")))

(deftest a-compound-rule-is-the-and-of-its-rules
  (let [rules (str "[" tall " " big " {:id :both :all-of [:tall :big]}]")
        gated (str "[{:id :tall :measure :height :dir :ceiling :limit 35.0 :when [:> :units 2]} "
                   big " {:id :both :all-of [:tall :big]}]")]
    (is (= :conforming (verdict-of rules "{:h 30.0 :a 6000.0}" :both)))
    (is (= :nonconforming (verdict-of rules "{:h 40.0 :a 6000.0}" :both)))
    (is (= :unknown (verdict-of rules "{:h 30.0}" :both)))
    (is (= :nonconforming (verdict-of rules "{:h 40.0}" :both))
        ":nonconforming wins over :unknown")
    (testing ":not-applicable is ignored"
      (is (= :conforming (verdict-of gated "{:h 40.0 :a 6000.0 :UNITS 1}" :both)))
      (is (= :not-applicable
             (verdict-of "[{:id :tall :measure :height :dir :ceiling :limit 35.0
                            :when [:> :units 2]}
                           {:id :both :all-of [:tall]}]"
                         "{:h 40.0 :UNITS 1}" :both))))))

(deftest a-result-holds-the-value-the-limit-and-the-margin
  (let [rules (str "[" tall " {:id :big :measure :area :dir :floor :limit 5000.0
                               :margin-times :height}]")
        at    (fn [id k] (checked rules "" "{:h 30.0 :a 5600.0}"
                                  (str "(get (get results " (kw-text id) ") "
                                       (kw-text k) ")")))]
    (is (= 30.0 (at :tall :actual)))
    (is (= 35.0 (at :tall :required)))
    (is (= 5.0 (at :tall :margin)) "a ceiling: the limit minus the value")
    (is (= 600.0 (at :big :margin)) "a floor: the value minus the limit")
    (is (= 18000.0 (at :big :scaled-margin)) "the margin times the measure")
    (is (nil? (checked rules "" "{:a 5600.0}" "(get (get results :tall) :margin)")))))

(deftest the-overall-verdict-reads-only-the-core-rules
  (let [rules   (str "[" tall " " big "]")
        overall (fn [row] (checked rules "" row "(zoning/overall-verdict rs results)"))]
    (is (= :conforming (overall "{:h 30.0 :a 100.0}")) "a rule that is not core does not count")
    (is (= :nonconforming (overall "{:h 40.0 :a 6000.0}")))
    (is (= :unknown (overall "{:a 6000.0}")))))

(defn- column-names [rules more]
  (vec (checked rules more "{:h 30.0 :a 5600.0}"
                "(vec (sort (map name (keys (zoning/result-columns rs values results)))))")))

(deftest the-names-of-the-result-columns
  (testing "the defaults: a verdict, the overall verdict, and each measure that a rule reads"
    (is (= ["actual-height" "overall-conformity" "tall-conformity"]
           (column-names (str "[" tall "]") ""))))
  (testing ":columns names the measures to write"
    (is (= ["a-out" "overall-conformity" "tall-conformity"]
           (column-names (str "[" tall "]") ":columns {:area :a-out}"))))
  (testing ":column and :required-column on a rule"
    (is (= ["actual-height" "height-ok" "max-height" "overall-conformity"]
           (column-names "[{:id :tall :measure :height :dir :ceiling :limit 35.0
                            :column :height-ok :required-column :max-height}]"
                         ""))))
  (testing ":required?, :margins? and :suffix on the rule set"
    (is (= ["actual-height-x" "overall-conformity-x" "required-tall-x"
            "tall-conformity-x" "tall-margin-x"]
           (column-names (str "[" tall "]") ":required? true :margins? true :suffix \"x\"")))))

(deftest the-values-of-the-result-columns
  (let [cols (checked "[{:id :tall :measure :height :dir :ceiling :core? true :limit 35.0
                         :required-column :max-height}]"
                      ":margins? true" "{:h 30.0}"
                      "(zoning/result-columns rs values results)")]
    (is (= 30.0 (get cols :actual-height)))
    (is (= :conforming (get cols :tall-conformity)))
    (is (= :conforming (get cols :overall-conformity)))
    (is (= 35.0 (get cols :max-height)))
    (is (= 5.0 (get cols :tall-margin)))))

(deftest a-rule-popup-names-the-columns-of-each-rule
  (let [p (ev (str "(zoning/rule-popup (zoning/rule-set " (with-rule "
                     {:id :tall :measure :height :dir :ceiling :core? true
                      :limit {:form :row :key :max-h}}
                     {:id :big :measure :area :dir :floor :limit 9.0
                      :required-column :min-area}
                     {:id :both :all-of [:tall :big]}") ")
                     {:title :name :labels {:tall \"Height\"} :units {:length \"m\"}})"))
        [tall big] (get p :rules)]
    (is (= :name (get p :title)))
    (is (= :overall-conformity (get p :status)))
    (is (= :nonconforming (get p :highlight)))
    (is (= 2 (count (get p :rules))) "a rule with no measure has no row")
    (is (= ["Height" :actual-height :max-h :tall-conformity "m"]
           (mapv #(get tall %) [:label :actual :max :status :unit])))
    (is (= ["big" :actual-area :min-area :big-conformity]
           (mapv #(get big %) [:label :actual :min :status]))
        "a floor rule has a :min, and its label is its id")
    (is (nil? (get big :unit)) "a quantity with no unit")))

(deftest the-op-writes-the-columns-of-its-rule-set
  (let [out (ev (str "(let [edge  {:id :front-setback :form :edges :types [:front :primary-front]
                                   :key :setback :agg :min}
                            units {:id :units :form :schema-col :key :units-col
                                   :parse :long :pos-only? true}
                            rs    (zoning/rule-set
                                   {:measures [edge units]
                                    :columns {:front-setback :actual-front :units :actual-units}
                                    :rules [{:id :front :measure :front-setback :dir :floor
                                             :core? true :limit {:form :row :key :min-front}}
                                            {:id :unit-count :measure :units :dir :ceiling
                                             :limit 2}]
                                    :per-edge {:column :each-edge :rules [:front]}})]
                        (persistent!
                         (zoning/analyze-parcel-conformity
                          (transient (assoc " edges-row " :min-front 11.0 :UNITS 0))
                          {:rules rs :parcel-schema {:units-col :UNITS}})))"))
        edge (fn [i] (get (get out :each-edge) i))]
    (is (= 10.0 (get out :actual-front)))
    (is (= :nonconforming (get out :front-conformity)))
    (is (= :nonconforming (get out :overall-conformity)))
    (testing "the column of a :pos-only? measure keeps the value that the data gives"
      (is (= 0 (get out :actual-units)))
      (is (= :unknown (get out :unit-count-conformity))))
    (testing ":per-edge gives one verdict for each edge"
      (is (= 5 (count (get out :each-edge))))
      (is (= :nonconforming (get (edge 0) :edge-setback-conformity)) "10 ft against 11 ft")
      (is (= :conforming (get (edge 1) :edge-setback-conformity)) "12 ft against 11 ft")
      (is (= :unknown (get (edge 2) :edge-setback-conformity)) "a side edge has no rule")
      (is (= 40.0 (get (edge 0) :edge-length))))
    (is (= 28.0 (get out :main-building-height 28.0)) "the op keeps the other keys of the row")))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (run/load-module zoning)))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.zoning-rules-test")))))
