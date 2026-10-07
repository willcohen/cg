;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.run.lane
  "Pure backend decisions: the runtime choice and the route of a Run, in both
   hosts. It has no host code, because node tests load it.")

(defn resolve-runtime [choice jvm-reachable?]
  (if (= choice "auto")
    (if jvm-reachable? "jvm" "js")
    choice))

(defn chip-text [choice resolved]
  (cond
    (= choice "jvm") "CG: jvm"
    (= choice "js") "CG: js"
    (= resolved "jvm") "CG: auto->jvm"
    (= resolved "js") "CG: auto->js"
    :else "CG: auto"))

(def ^:private js-texts
  {"ready" "The JS runtime is ready in the CG Map tab."
   "starting" "The JS runtime is starting in the CG Map tab."
   "closed" "The JS runtime starts with the CG Map tab, and a JS run opens it."})

(defn runtime-report
  "`js-state` is ready, starting or closed."
  [choice resolved js-state reachable? port]
  (str "CG: runtime " choice
       (when (= "auto" choice) (str ", which resolves to " resolved))
       ". " (get js-texts js-state)
       " The JVM backend is " (if reachable? (str "up on port " port) "not running")
       (when (and (not reachable?) (= "jvm" choice)) "; a JVM run starts it")
       "."))

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
