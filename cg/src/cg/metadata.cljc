;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.metadata
  "Parse the visual metadata of a .cg file (EDN after a ^:cgproj tag, read as
  data and never evaluated)."
  (:require [clojure.string :as str]
            #?(:clj [clojure.edn :as edn]
               :cljs ["squint-cljs" :as squint-compiler])))

#?(:clj (set! *warn-on-reflection* true))

(defn split-code-and-metadata
  "Split the content of a .cg file at the ^:cgproj tag into {:code :metadata}.
  :metadata is {} when there is no tag."
  [file-content]
  (if-let [idx (str/index-of file-content "^:cgproj")]
    (let [code (subs file-content 0 idx)
          metadata-start (+ idx (count "^:cgproj"))
          metadata-str (str/trim (subs file-content metadata-start))
          ;; The JVM reads the metadata with the EDN reader, with no eval.
          ;; squint has no EDN reader: compile the text as an expression and
          ;; eval it. :elide-imports and :elide-exports give JS that eval can
          ;; run with no module errors.
          metadata (if (empty? metadata-str)
                    {}
                    #?(:clj (edn/read-string metadata-str)
                       :cljs (let [result-state ((.-compileStringEx squint-compiler)
                                                 metadata-str
                                                 #js {:context "expr"
                                                      :elide-imports true
                                                      :elide-exports true})
                                   compiled (.-javascript result-state)]
                               ;; An indirect eval runs in the global scope,
                               ;; where the squint_core import of this module
                               ;; is not visible. EDN compiles to plain JS
                               ;; literals and does not need it.
                               ((js* "(0, eval)") compiled))))]
      ;; A squint map is a plain JS object. No clj->js is necessary.
      {:code (str/trim code)
       :metadata metadata})
    {:code (str/trim file-content)
     :metadata {}}))
