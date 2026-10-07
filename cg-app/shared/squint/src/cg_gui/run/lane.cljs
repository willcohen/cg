;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.run.lane
  "Pure backend decisions: the runtime choice and the route of a Run, in both
   hosts. It has no host code, because node tests load it.")

(defn resolve-runtime
  "An explicit choice passes through. auto follows backend reachability."
  [choice jvm-reachable?]
  (if (= choice "auto")
    (if jvm-reachable? "jvm" "js")
    choice))

(defn chip-text
  "The status text of a runtime choice and its resolution, in both hosts."
  [choice resolved]
  (cond
    (= choice "jvm") "CG: jvm"
    (= choice "js") "CG: js"
    (= resolved "jvm") "CG: auto->jvm"
    (= resolved "js") "CG: auto->js"
    :else "CG: auto"))

(defn execute-route
  "The route at submission time. auto resolves to js when the backend is
   unreachable; an explicit jvm choice is refused, with no fallback."
  [choice jvm-reachable?]
  (let [resolved (resolve-runtime choice jvm-reachable?)]
    (if (and (= resolved "jvm") (not jvm-reachable?))
      {:refused true
       :runtime "jvm"
       :reason "JVM runtime selected but the backend is unreachable"}
      {:refused false :runtime resolved})))
