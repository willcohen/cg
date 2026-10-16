;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.core
  "Entry point of the static .cg parser. `parse` and `extract-models` are sync
   on the JVM and return a Promise on cljs."
  (:require [cg.parser.extract :as extract]
            [cg.parser.types :as types]
            [cg.parser.validate :as validate]
            #?(:clj  [cg.parser.jvm :as backend]
               :cljs [cg.parser.squint :as backend]))
  #?(:clj (:import [org.graalvm.polyglot Value])))

#?(:clj (set! *warn-on-reflection* true))

;; One GraalJS Context serves each call, and JS admits one thread at a time.
#?(:clj (defonce ^:private js-lock (Object.)))

(defn parse
  "Return the tree-sitter Tree for Clojure source text. On cljs it returns a
   Promise, and `opts` :grammar-wasm is a grammar path or URL for the first call."
  ([text] (parse text nil))
  ([text opts] (backend/parse text opts)))

(defn- extract-tree
  "extract, then free the Tree: web-tree-sitter has no finalizer, and a Tree
   holds wasm memory until its delete."
  [tree opts]
  (try (extract/extract tree opts)
       (finally #?(:clj  (.invokeMember ^Value tree "delete" (object-array 0))
                   :cljs (.delete tree)))))

(defn ^:async extract-models
  "Parse, extract, validate and type-check text. Returns the CgFlow, as a
   Promise on cljs. `opts` keys: :grammar-wasm, :opaque-blocks."
  ([text] (extract-models text nil))
  ([text opts]
   (-> #?(:clj  (locking js-lock (extract-tree (parse text opts) opts))
          :cljs (extract-tree (await (parse text opts)) opts))
       validate/validate types/check)))

#?(:cljs
   (defn init!
     "Loads the parser on cljs. After the Promise resolves, `operations`
      parses with no Promise. `opts` as for parse."
     ([] (init! nil))
     ([opts] (backend/init! opts))))

(defn operations
  "Return the OperationDecls of `text` with no validation or type check. On
   cljs, nil until init! resolves."
  [text]
  #?(:clj  (:operations (locking js-lock (extract-tree (parse text nil) nil)))
     :cljs (some-> (backend/parse-sync text) (extract-tree nil) :operations)))

(defn recheck
  "Run the static checker again on a CgFlow with extended :operations, with
   no new parse."
  [cg-flow]
  (types/recheck cg-flow))
