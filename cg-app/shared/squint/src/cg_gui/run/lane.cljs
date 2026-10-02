;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.run.lane
  "Pure backend decisions: the runtime choice, a refused web Run and the
   route of a map run. It has no host code, because node tests load it.")

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

(defn flow-needs-jvm?
  "True when the parsed flow has :loaded-modules. The caller then routes the
   run to the JVM backend. File I/O is not detected."
  [flow]
  (boolean (seq (:loaded-modules flow))))

(defn route-map-run
  "The route of a map run: :jvm, :js, :unsupported (the file needs the JVM and the
   backend is down) or :refused (an explicit jvm choice and the backend is down)."
  [{:keys [needs-jvm? backend-reachable? resolved-runtime]}]
  (cond
    needs-jvm?                (if backend-reachable? :jvm :unsupported)
    (= :jvm resolved-runtime) (if backend-reachable? :jvm :refused)
    :else                     :js))
