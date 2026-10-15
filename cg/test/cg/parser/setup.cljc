;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.setup
  "Helpers for the parser tests, which run on the JVM and on JS. On JS,
   cg.parser.core/extract-models gives a Promise. The extract-models here
   gives the CgFlow with no Promise, after init! resolves."
  #?(:clj (:require [cg.parser.core :as parser])
     :cljs (:require ["../../../src/cg/parser/core.mjs" :as parser]
                     ["../../../src/cg/parser/squint.mjs" :as backend]
                     ["../../../src/cg/parser/extract.mjs" :as extract]
                     ["../../../src/cg/parser/types.mjs" :as types]
                     ["../../../src/cg/parser/validate.mjs" :as validate])))

#?(:clj (set! *warn-on-reflection* true))

#?(:cljs
   (defn init!
     "Load the parser. Returns a Promise."
     []
     (parser/init!)))

(defn extract-models
  "Return the CgFlow of `text`, as cg.parser.core/extract-models does."
  ([text] (extract-models text nil))
  ([text opts]
   #?(:clj (parser/extract-models text opts)
      :cljs (-> (backend/parse-sync text) (extract/extract opts) validate/validate types/check))))
