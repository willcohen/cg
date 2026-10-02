;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.dependency-pins-test
  "cg pins org.graalvm.* and dtype-next, and its native dependencies declare
   them too. tools.deps gives a consumer of cg the newest version that a
   dependency declares, so a cg pin below them makes cg's own tests run other
   versions than a consumer gets. Truffle also runs the wasm and JS guests
   interpreted, about 6x slower, when the org.graalvm.* artifacts and the
   JDK's libgraal differ."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]])
  (:import [org.graalvm.polyglot Engine]))

(set! *warn-on-reflection* true)

(defn- polyglot-version []
  (with-open [e (Engine/create (into-array String []))]
    (.getVersion e)))

(defn- pom-version
  "The version of `artifact` that the pom at the classpath resource
   `pom-resource` declares."
  [pom-resource artifact]
  (some->> (io/resource pom-resource)
           slurp
           (re-find (re-pattern (str "<artifactId>" artifact
                                     "</artifactId>\\s*<version>([^<]+)</version>")))
           second))

(defn- resolved-version
  "The version of the jar on the classpath, from its pom.properties."
  [group artifact]
  (some->> (io/resource (str "META-INF/maven/" group "/" artifact "/pom.properties"))
           slurp
           (re-find #"(?m)^version=(.+)$")
           second))

(defn- jdk-graal-version
  "The GraalVM release of the JDK, so 'GraalVM CE 25.3.4.1+1.1' gives
   '25.3.4.1'. nil on another JDK."
  []
  (some->> (System/getProperty "java.vendor.version")
           (re-find #"GraalVM CE (\d+(?:\.\d+){2,})")
           second))

(deftest polyglot-is-the-version-clj-proj-declares
  (is (= (pom-version "META-INF/maven/net.willcohen/proj/pom.xml" "polyglot")
         (polyglot-version))))

(deftest polyglot-is-the-jdk-libgraal
  (when-let [jdk (jdk-graal-version)]
    (is (= jdk (polyglot-version)))))

(deftest dtype-next-is-the-version-clj-native-declares
  (is (= (pom-version "META-INF/maven/net.willcohen/native/pom.xml" "dtype-next")
         (resolved-version "cnuernber" "dtype-next"))))
