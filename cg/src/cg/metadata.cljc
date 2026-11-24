;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.metadata
  "Parse the visual metadata of a .cg file (EDN after a ^:cgproj tag).
  cg.util/read-edn-string reads the EDN. On the JVM it reads the EDN as data.
  On JS it compiles the EDN with squint and evaluates it."
  (:require [cg.util :as util]
            [clojure.string :as str]))

#?(:clj (set! *warn-on-reflection* true))

(defn split-code-and-metadata
  "Split the content of a .cg file at the ^:cgproj tag into {:code :metadata}.
  :metadata is {} when there is no tag."
  [file-content]
  (if-let [idx (str/index-of file-content "^:cgproj")]
    (let [code (subs file-content 0 idx)
          metadata-start (+ idx (count "^:cgproj"))
          metadata-str (str/trim (subs file-content metadata-start))
          metadata (if (empty? metadata-str) {} (util/read-edn-string metadata-str))]
      ;; A squint map is a plain JS object. No clj->js is necessary.
      {:code (str/trim code)
       :metadata metadata})
    {:code (str/trim file-content)
     :metadata {}}))
