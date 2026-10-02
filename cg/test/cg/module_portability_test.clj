;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.module-portability-test
  "The .cg modules name no JVM class, so squint can compile them as SCI
   runs them. A JVM static call (Double/parseDouble) or constructor
   (java.io.File.) does not exist under squint. A catch form has no
   portable spelling: SCI on the JVM needs a class, and SCI rejects the
   ClojureScript :default. The modules call cg.util/try-catch instead.
   Method calls such as (.toUpperCase s) are not checked, because a JS
   string has the same methods."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [edamame.core :as e]))

(set! *warn-on-reflection* true)

(defn- class-segment?
  "True for a name segment that starts with a capital letter, as a Java
   class name does: Double, File, Math."
  [s]
  (boolean (re-matches #"[A-Z].*" s)))

(defn- class-ref?
  "True for a symbol that names a JVM class or a static member of one:
   Double/parseDouble, java.io.File., java.lang.Math/round."
  [sym]
  (let [ns-part (namespace sym)
        nm (str/replace (name sym) #"\.$" "")]
    (or (and ns-part (some class-segment? (str/split ns-part #"\.")))
        (and (str/includes? nm ".")
             (some class-segment? (str/split nm #"\."))))))

(defn- host-forms
  "Every class reference and catch form in the forms, as strings with
   their line."
  [forms]
  (let [found (atom [])]
    (letfn [(walk [x]
              (cond
                (symbol? x) (when (class-ref? x)
                              (swap! found conj (str x)))
                (seq? x) (do (when (= 'catch (first x))
                               (swap! found conj
                                      (str "(catch " (second x) " ...) line "
                                           (:row (meta x)))))
                             (run! walk x))
                (coll? x) (run! walk x)))]
      (run! walk forms))
    @found))

(defn- cg-files []
  (->> ["src/cg" "examples"]
       (mapcat #(.listFiles (io/file %)))
       (filter #(str/ends-with? (.getName ^java.io.File %) ".cg"))
       (map str)
       sort))

(deftest cg-modules-name-no-jvm-class
  (let [offenders (into (sorted-map)
                        (keep (fn [path]
                                (let [forms (e/parse-string-all (slurp path) {:all true})
                                      hits (host-forms forms)]
                                  (when (seq hits) [path hits]))))
                        (cg-files))]
    (is (seq (cg-files)))
    (is (= {} offenders))))
