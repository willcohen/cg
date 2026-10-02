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
               :cljs [cg.parser.squint :as backend])))

#?(:clj (set! *warn-on-reflection* true))

(defn parse
  "Return the tree-sitter Tree for Clojure source text. On cljs it returns a
   Promise, and `opts` :grammar-wasm is a grammar path or URL for the first call."
  ([text] (parse text nil))
  ([text opts] (backend/parse text opts)))

(defn ^:async extract-models
  "Parse, extract, validate and type-check text. Returns the CgFlow, as a
   Promise on cljs. `opts` keys: :grammar-wasm, :opaque-blocks."
  ([text] (extract-models text nil))
  ([text opts]
   #?(:clj  (-> (parse text opts) (extract/extract opts) validate/validate types/check)
      :cljs (let [tree (await (parse text opts))]
              (-> tree (extract/extract opts) validate/validate types/check)))))

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
  #?(:clj  (:operations (extract/extract (parse text nil) nil))
     :cljs (when-let [tree (backend/parse-sync text)]
             (:operations (extract/extract tree nil)))))

(defn recheck
  "Run the static checker again on a CgFlow with extended :operations, with
   no new parse."
  [cg-flow]
  (types/recheck cg-flow))
