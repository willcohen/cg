;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.extract
  "The parser of cg core for the extension. The overlay read is in
   cg-gui.doc.overlay."
  (:require ["../../../cg/src/cg/parser/core.mjs" :as parser]
            ["node:module" :refer [createRequire]]))

(defn- grammar-wasm-path
  "createRequire needs import.meta.url, which esbuild shims to a file URL in
   the CJS bundle."
  []
  (.resolve (createRequire (js* "import.meta.url"))
            "@yogthos/tree-sitter-clojure/tree-sitter-clojure.wasm"))

(defn ^:async extract-models
  "Promise of the CgFlow of `text`. A form that the parser rejects becomes a
   block, which the canvas shows as a read-only card."
  [text]
  (parser/extract-models text {:grammar-wasm (grammar-wasm-path)
                               :opaque-blocks true}))

(defn recheck
  "Runs the parser checks again after the merge of the operations of a followed
   module. The first parse marks each call to those operations as unresolved."
  [flow]
  (parser/recheck flow))
