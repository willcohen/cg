;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns build
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.build.api :as b]))

(defn- json-string
  "The first string value of `k` in the JSON text `s`."
  [s k]
  (second (re-find (re-pattern (str "\"" (java.util.regex.Pattern/quote k) "\": \"([^\"]+)\""))
                   s)))

(def lib 'net.willcohen/cg)
;; The jar reads the version from package.json, because npm reads it only
;; from there.
(def ^:private package-json (slurp "package.json"))
(def version (json-string package-json "version"))
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(def ^:private repo-url "https://github.com/willcohen/cg")

;; cg.parser.jvm reads these from the classpath dir first. A Clojars user
;; has no node_modules.
(def ^:private parser-dir "cg/parser/tree-sitter")

(def ^:private parser-files
  [["web-tree-sitter" "web-tree-sitter.cjs"]
   ["web-tree-sitter" "web-tree-sitter.wasm"]
   ["@yogthos/tree-sitter-clojure" "tree-sitter-clojure.wasm"]])

;; The jar copies the code of these packages, and their licenses must go with
;; each copy.
(def ^:private parser-licenses
  [["web-tree-sitter" "LICENSE"]
   ["@yogthos/tree-sitter-clojure" "COPYING.txt"]])

(def ^:private basis (delay (b/create-basis {:project "deps.edn"})))

(defn- native-classifier? [lib]
  (str/starts-with? (name lib) "gdal-native$"))

;; deps.edn names each gdal-native classifier jar, because tools.deps does not
;; read the pom profile of gdal. A pom user gets the jar of the host from that
;; profile, and three classifier deps would put all three libs on its path.
(defn- pom-basis []
  (update @basis :libs #(into {} (remove (comp native-classifier? key)) %)))

(defn clean [_]
  (b/delete {:path "target"}))

(defn pom [_]
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis (pom-basis)
                :src-dirs ["src"]
                :resource-dirs ["resources"]
                :pom-data [[:description "A small DSL for GIS analysis in urban planning, for the JVM and JavaScript."]
                           [:url repo-url]
                           [:licenses
                            [:license
                             [:name "Apache-2.0 WITH LLVM-exception"]
                             [:url "https://llvm.org/LICENSE.txt"]
                             [:distribution "repo"]]]
                           [:developers
                            [:developer
                             [:name "Will Cohen"]]]
                           [:scm
                            [:url repo-url]
                            [:tag version]]]}))

(defn- installed-version [pkg]
  (let [f (io/file "node_modules" pkg "package.json")]
    (when (.exists f)
      (json-string (slurp f) "version"))))

(defn- check-parser-packages! []
  (doseq [pkg (distinct (map first parser-files))
          :let [want (json-string package-json pkg)
                have (installed-version pkg)]]
    (when-not (= want have)
      (throw (ex-info (str "node_modules/" pkg " is " (or have "missing")
                           ", and package.json names " want ". Run bb npm:install first.")
                      {:package pkg :want want :have have})))))

(defn- copy-parser-files! []
  (doseq [[pkg f] parser-files]
    (b/copy-file {:src (str "node_modules/" pkg "/" f)
                  :target (str class-dir "/" parser-dir "/" f)}))
  (doseq [[pkg f] parser-licenses]
    (b/copy-file {:src (str "node_modules/" pkg "/" f)
                  :target (str class-dir "/META-INF/third-party/" (last (str/split pkg #"/")) "/" f)})))

;; The tools.build default ignores, and each .mjs. The squint output is not
;; in git, and no JVM consumer loads a .mjs. npm ships the JS.
(def ^:private jar-ignores [".*~$" "^#.*#$" "^\\.#.*" "^.DS_Store$" ".*\\.mjs$"])

(defn jar
  "Build target/cg-<version>.jar and its pom, with the web-tree-sitter files
   of node_modules for the parser."
  [_]
  (check-parser-packages!)
  (clean nil)
  (pom nil)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir
               :ignores jar-ignores})
  (copy-parser-files!)
  (b/copy-file {:src "LICENSE" :target (str class-dir "/META-INF/LICENSE")})
  (b/jar {:class-dir class-dir
          :jar-file jar-file})
  (println "Jar built:" jar-file))
