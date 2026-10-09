;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.squint
  "Cljs backend: web-tree-sitter under squint. The grammar path comes from the
   init! or parse opts, else from @yogthos/tree-sitter-clojure. Must stay free
   of eval: do not require cg.dsl, cg.macros, cg.run or cg.codegen."
  (:require ["web-tree-sitter" :refer [Parser Language]]))

(defonce ^:private language (atom nil))
(defonce ^:private init-promise (atom nil))

(defn- default-grammar
  "The grammar wasm of @yogthos/tree-sitter-clojure, resolved as a module
   import resolves: through node_modules on Node, through the importmap in
   a browser. web-tree-sitter reads a Node path with fs, not a file: URL.
   On win32 it removes the slash of /C:/."
  []
  (let [url (.resolve js/import.meta "@yogthos/tree-sitter-clojure/tree-sitter-clojure.wasm")]
    (if (.startsWith url "file:")
      (.replace (js/decodeURIComponent (.-pathname (js/URL. url))) #"^/([A-Za-z]:)" "$1")
      url)))

(defn- ^:async do-init!
  "Call init!, which caches the Promise: concurrent loads of the same wasm
   leave a half-initialized Language whose version reads as 0."
  [opts]
  (let [wasm-path (or (:grammar-wasm opts) (default-grammar))]
    (await (.init Parser))
    (reset! language (await (.load Language wasm-path)))))

(defn ^:async init!
  "The first call caches the init Promise, and concurrent callers all await it."
  ([] (init! nil))
  ([opts]
   (when (nil? @init-promise)
     (reset! init-promise (do-init! opts)))
   (await @init-promise)))

(defn parse-sync
  "Parses source text with no Promise. Returns the tree-sitter Tree, or
   nil before init! resolves."
  [text]
  (when-let [lang @language]
    (let [parser (Parser.)]
      (.setLanguage parser lang)
      (.parse parser text))))

(defn ^:async parse
  "Parses source text and returns the tree-sitter Tree. `opts` goes to init!
   on the first call."
  ([text] (parse text nil))
  ([text opts]
   (await (init! opts))
   (parse-sync text)))
