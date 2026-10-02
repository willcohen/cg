;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; The SCI evaluation context for .cg files (JVM only). cg.run evaluates each
;; file in the context that create-cg-context builds. The JS runtime has no SCI:
;; cg.run/eval-cg-cljs compiles each form with squint.
;;
;; SCI blocks host interop unless the context permits the class. This context
;; permits the JTS geometry classes and java.io.File. .cg code cannot call
;; System/getenv, java.nio.file.Files or Math/*: it uses cg.math for math and
;; cg.util/current-time-ms for the clock.

;; Because sci/copy-ns names each library namespace by symbol, clj-kondo sees
;; no use of its alias. The require loads the namespace.
(ns cg.dsl
  {:clj-kondo/config '{:linters {:unused-namespace
                                 {:exclude [cg.model cg.io cg.feature cg.geo cg.run
                                            cg.dataset clojure.core.async.flow
                                            cg.math cg.util]}}}}
  ;; Every require is :clj-only. Under :cljs squint detects a cycle via
  ;; cg.macros -> cg.introspect -> cg.dsl -> cg.macros, and all uses in this
  ;; ns are inside :clj reader conditionals.
  #?(:clj (:require [cg.model :as model]
                    [cg.macros :as macros]
                    [sci.core :as sci]
                    [cg.io :as io]
                    [cg.feature :as feature]
                    [cg.geo :as geo]
                    [cg.run :as run]
                    [cg.dataset :as dataset]
                    [clojure.core.async.flow :as flow]
                    [cg.math :as cg-math]
                    [cg.util :as cg-util])))

#?(:clj (set! *warn-on-reflection* true))

;; cg.run breaks the require cycle: its ns form does not name cg.dsl. cg.run
;; calls (require 'cg.dsl) at first use, then resolves create-cg-context. Do
;; not put cg.dsl in the cg.run ns form.

#?(:clj
   (def cg-namespaces
     "The namespaces that a .cg file can require. SCI copies each one into
  the context. From cg.macros the context copies only item->, coll-> and
  table->."
     {'cg.io (sci/copy-ns cg.io (sci/create-ns 'cg.io))
      'cg.feature (sci/copy-ns cg.feature (sci/create-ns 'cg.feature))
      'cg.geo (sci/copy-ns cg.geo (sci/create-ns 'cg.geo))
      'cg.model (sci/copy-ns cg.model (sci/create-ns 'cg.model))
      'cg.run (sci/copy-ns cg.run (sci/create-ns 'cg.run))
      'cg.macros {'item-> (sci/copy-var macros/item-> (sci/create-ns 'cg.macros))
                  'coll-> (sci/copy-var macros/coll-> (sci/create-ns 'cg.macros))
                  'table-> (sci/copy-var macros/table-> (sci/create-ns 'cg.macros))}
      'clojure.core.async.flow (sci/copy-ns clojure.core.async.flow (sci/create-ns 'clojure.core.async.flow))
      'cg.dataset (sci/copy-ns cg.dataset (sci/create-ns 'cg.dataset))
      'cg.math (sci/copy-ns cg.math (sci/create-ns 'cg.math))
      ;; cg.util gives .cg code the clock wrappers (current-time-ms,
      ;; elapsed-ms) that replace the host interop that the sandbox blocks.
      'cg.util (sci/copy-ns cg.util (sci/create-ns 'cg.util))}))

#?(:clj
   (defn root-dynamic-vars!
     "Give each dynamic var in `sci-vars` (a copy-ns map of `host-ns`) the
      root value of its host var, and return `sci-vars`. copy-ns copies the
      thread-bound value, and cg.dsl can first load inside a host binding."
     [host-ns sci-vars]
     (doseq [[sym sci-var] sci-vars
             :let [host-var (ns-resolve host-ns sym)]
             :when (and (var? host-var) (:dynamic (meta host-var)))]
       (sci/alter-var-root sci-var
                           (constantly (.getRawRoot ^clojure.lang.Var host-var))))
     sci-vars))

;; A separate top-level form, not a fn: the copy-ns expansions of
;; cg-namespaces are too large for one method.
#?(:clj
   (doseq [[ns-sym sci-vars] (dissoc cg-namespaces 'cg.macros)]
     (root-dynamic-vars! ns-sym sci-vars)))

#?(:clj
   (defn- extract-alias-from-libspec
     "{:ns <ns> :alias <alias>} for a vector libspec with :as, else nil."
     [libspec]
     (cond
       (symbol? libspec)
       nil

       (vector? libspec)
       (let [ns-sym (first libspec)
             opts (apply hash-map (rest libspec))]
         (when-let [alias-sym (:as opts)]
           {:ns ns-sym :alias alias-sym}))

       :else nil)))

#?(:clj
   (defn create-cg-context
     "Make a new SCI context for .cg evaluation. `opts` merges into the
  sci/init map, to add namespaces or classes."
     ([]
      (create-cg-context {}))
     ([opts]
      (let [ctx (sci/init (merge {:namespaces (merge {'clojure.core {'println println
                                                                     'print print
                                                                     'prn prn
                                                                     'pr pr
                                                                     'slurp slurp
                                                                     ;; A script writes its text reports
                                                                     ;; with spit: ds/write-* writes only
                                                                     ;; datasets.
                                                                     'spit spit
                                                                     ;; A flow, a fetch and a module load
                                                                     ;; give a Promise on the JS runtime and
                                                                     ;; a value here.
                                                                     'await identity
                                                                     'cg-register-require-alias!
                                                                     (fn [libspec]
                                                                       (when-let [{:keys [ns alias]} (extract-alias-from-libspec libspec)]
                                                                         (macros/register-alias! alias ns)))}}
                                                     cg-namespaces)
                                  :classes {'org.locationtech.jts.geom.Geometry {:allow :all}
                                            'org.locationtech.jts.geom.Point {:allow :all}
                                            'org.locationtech.jts.geom.LineString {:allow :all}
                                            'org.locationtech.jts.geom.LinearRing {:allow :all}
                                            'org.locationtech.jts.geom.Polygon {:allow :all}
                                            'org.locationtech.jts.geom.MultiPoint {:allow :all}
                                            'org.locationtech.jts.geom.MultiLineString {:allow :all}
                                            'org.locationtech.jts.geom.MultiPolygon {:allow :all}
                                            'org.locationtech.jts.geom.GeometryCollection {:allow :all}
                                            'org.locationtech.jts.geom.CoordinateSequence {:allow :all}
                                            'org.locationtech.jts.geom.Coordinate {:allow :all}
                                            'org.locationtech.jts.geom.Envelope {:allow :all}
                                            ;; .cg code checks files on disk with (java.io.File. path)
                                            ;; and .exists. A constructor call by name needs the
                                            ;; symbol->Class form: an {:allow :all} entry permits only
                                            ;; method calls on instances.
                                            'java.io.File java.io.File}}
                                 opts))]
        ;; SCI's built-in require is read-only. This `require` in the user
        ;; namespace shadows it: it registers each alias for macro expansion,
        ;; then calls clojure.core/require.
        (sci/eval-string* ctx
                          "(defn require [& libspecs]
                             (doseq [ls libspecs]
                               (cg-register-require-alias! ls))
                             (apply clojure.core/require libspecs))")
        ctx))))
