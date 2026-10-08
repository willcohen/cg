;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.module-file
  "The file of a module path. Pure, so that a node test loads it without
   vscode."
  (:require ["path" :as path]))

(defn find-under
  "The first file `path-str` under one of `dirs`, in order, for which
   (exists? file) is true, or nil. A nil dir is skipped."
  [dirs path-str exists?]
  (some (fn [dir]
          (when dir
            (let [f (path/resolve dir path-str)]
              (when (exists? f) f))))
        dirs))
