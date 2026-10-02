;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.dev-shell
  "Runs a command in the dev shell, because VS Code can start without it and
   then gives `bb run` a Java that is not GraalVM. No vscode dependency.")

(defn- sh-quote [s]
  (str "'" (.replaceAll (str s) "'" "'\\''") "'"))

(defn spawn-args
  "#js [program args] that run `argv` through a login shell and `direnv exec`
   in `dir`. In the dev shell already, `argv` runs as it is."
  [env dir argv]
  (if (or (aget env "IN_NIX_SHELL") (aget env "DIRENV_DIR"))
    #js [(first argv) (into-array (rest argv))]
    #js [(or (aget env "SHELL") "/bin/zsh")
         #js ["-lc" (str "exec direnv exec " (sh-quote dir) " "
                         (.join (.map (into-array argv) sh-quote) " "))]]))
