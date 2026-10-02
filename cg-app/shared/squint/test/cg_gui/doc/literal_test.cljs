;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.doc.literal-test
  "The text of a literal box, the source text of what the user types, and
   the splice of that source text into a .cg text."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_gui/doc/literal.mjs" :as lit]))

(deftest a-box-shows-a-string-without-its-quotes
  (is (= "Jamaica Plain Neighborhood" (lit/field-text "string" "\"Jamaica Plain Neighborhood\"")))
  (is (= "" (lit/field-text "string" "\"\"")))
  (is (= ":city" (lit/field-text "keyword" ":city")))
  (is (= "2000" (lit/field-text "number" "2000"))))

(deftest typed-text-becomes-source-text-of-the-same-kind
  (is (= "\"Jamaica Plain Neighborhood\"" (lit/field-source "string" "Jamaica Plain Neighborhood")))
  (is (= "\" padded \"" (lit/field-source "string" " padded ")))
  (is (= "\"say \\\"hi\\\"\"" (lit/field-source "string" "say \"hi\"")))
  (is (= "\"a\\nb\"" (lit/field-source "string" "a\\nb")))
  (is (= "500" (lit/field-source "number" " 500 ")))
  (is (= "-1.5e3" (lit/field-source "number" "-1.5e3")))
  (is (= ":fill" (lit/field-source "keyword" "fill")))
  (is (= ":overall-conformity" (lit/field-source "keyword" ":overall-conformity")))
  (is (= "false" (lit/field-source "boolean" "false")))
  (is (= ":city" (lit/field-source "nil" ":city")))
  (is (= "\"x\"" (lit/field-source "nil" "\"x\"")))
  (is (= "nil" (lit/field-source "nil" "nil"))))

(deftest text-of-another-kind-is-refused
  (is (nil? (lit/field-source "number" "12 ft")))
  (is (nil? (lit/field-source "number" "")))
  (is (nil? (lit/field-source "string" "ends with \\")))
  (is (nil? (lit/field-source "string" "bad \\q escape")))
  (is (nil? (lit/field-source "keyword" "two words")))
  (is (nil? (lit/field-source "keyword" "(f)")))
  (is (nil? (lit/field-source "boolean" "yes")))
  (is (nil? (lit/field-source "nil" "bare")))
  (is (nil? (lit/field-source "nil" "\"open"))))

(def ^:private text
  "(ns demo)\n;; Säo\n(def scope {:district \"Jamaica Plain Neighborhood\"})\n(def n 40)\n")

(def ^:private district-loc
  #js {:start-line 3 :start-column 23 :end-line 3 :end-column 51})

(deftest splice-replaces-the-span-only
  (is (= "(ns demo)\n;; Säo\n(def scope {:district \"Example District\"})\n(def n 40)\n"
         (lit/splice text district-loc "\"Jamaica Plain Neighborhood\"" "\"Example District\"")))
  (is (= "(ns demo)\n;; Säo\n(def scope {:district \"Jamaica Plain Neighborhood\"})\n(def n 7)\n"
         (lit/splice text #js {:start-line 4 :start-column 8 :end-line 4 :end-column 10} "40" "7"))))

(deftest splice-refuses-a-span-that-no-longer-holds-the-literal
  (is (nil? (lit/splice text district-loc "\"Elsewhere\"" "\"x\"")))
  (is (nil? (lit/splice text #js {:start-line 9 :start-column 1 :end-line 9 :end-column 2} "" "x")))
  (is (nil? (lit/splice "(def n 40)" #js {:start-line 1 :start-column 8 :end-line 1 :end-column 40} "40" "7"))))

(deftest unclosed-names-a-text-in-the-middle-of-an-edit
  (is (nil? (lit/unclosed text)))
  (is (nil? (lit/unclosed "(def s \"a ) \\\" b\") ; an open ( in a comment\n[\\( #\"\\d+\"]")))
  (is (= "an open string" (lit/unclosed "(def scope {:district \"Jamaica})\n(def n 4)")))
  (is (= "an open (" (lit/unclosed "(def n 40")))
  (is (= "an open {" (lit/unclosed "(def m {:a 1)")))
  (is (= "a ) with no open bracket" (lit/unclosed "(def n 40))"))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
