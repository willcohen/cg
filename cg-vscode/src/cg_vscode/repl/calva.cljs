;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.repl.calva
  "The Calva connect sequences of the two CG REPLs. Pure, so that a node test
   loads it without vscode.")

(defn file-ns
  "The name of the ns form at the start of `text`, or nil."
  [text]
  (second (re-find #"^\s*(?:;[^\n]*\n\s*)*\(ns\s+(?:\^\S+\s+)*([^\s\)]+)" text)))

;; Each sequence keeps the file patterns of its Calva project type, because
;; ours replace them. A .cg goes to the JVM REPL when both are connected.
(def ^:private runtimes
  {"jvm" {:name "CG JVM" :projectType "generic" :cljsType "none" :session "cg-jvm"
          :files {"always-claim" ["*.cg" "*.clj" "*.edn"]}}
   "js"  {:name "CG JS" :projectType "squint" :cljsType "ClojureScript nREPL" :session "cg-js"
          :files {"always-claim" ["*.cljs"] "is-fallback-for" ["*.cg"]}}})

(defn connect-sequence
  "The Calva connect sequence of the REPL of `runtime` (jvm or js) for a
   document under `root` whose ns form names `ns-name`. Only the JVM REPL
   starts in that ns: the JS REPL evaluates at global scope. find-ns guards a
   session that has not run the file yet."
  [runtime root ns-name]
  (let [{:keys [name projectType cljsType session files]} (get runtimes runtime)]
    (cond-> {:name name
             :projectType projectType
             :cljsType cljsType
             :replSessionNames {:primary session}
             :replSessionFilePatterns {:primary files}}
      root (assoc :projectRootPath [root])
      (and ns-name (= "jvm" runtime))
      (assoc :afterPrimaryReplConnectedCode
             (str "(when (find-ns '" ns-name ") (in-ns '" ns-name "))")))))
