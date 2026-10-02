;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.dev-shell-test
  "The spawn of a command in the dev shell.
   Runs standalone: node test/cg_vscode/dev_shell_test.mjs."
  (:require [cljs.test :as t :refer [deftest is]]
            ["child_process" :as cp]
            ["../../src/cg_vscode/dev_shell.mjs" :as dev-shell]))

(deftest outside-the-dev-shell-a-login-shell-runs-direnv-exec
  (let [[prog args] (dev-shell/spawn-args #js {:SHELL "/bin/zsh"} "/w/cg"
                                          ["bb" "run" "/w/it's.cg" "--model" "m"])]
    (is (= "/bin/zsh" prog))
    (is (= ["-lc" "exec direnv exec '/w/cg' 'bb' 'run' '/w/it'\\''s.cg' '--model' 'm'"]
           (vec args)))))

(deftest in-the-dev-shell-the-command-runs-as-it-is
  (let [[prog args] (dev-shell/spawn-args #js {:IN_NIX_SHELL "impure"} "/w/cg" ["bb" "run" "a.cg"])]
    (is (= "bb" prog))
    (is (= ["run" "a.cg"] (vec args)))))

(deftest the-quoting-keeps-each-argument-whole
  ;; A shell function stands in for direnv, because a login shell resets PATH
  ;; and the test must not depend on the direnv of the machine.
  (let [[_ args] (dev-shell/spawn-args #js {:SHELL "/bin/sh"} "/"
                                       ["printf" "%s|" "a b" "it's" "$HOME"])
        line (.replace (aget args 1) #"^exec " "")
        r (cp/spawnSync "/bin/sh" #js ["-c" (str "direnv() { shift 2; \"$@\"; }; " line)]
                        #js {:encoding "utf8"})]
    (is (= "a b|it's|$HOME|" (.-stdout r)) (.-stderr r))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err (or (get results "error") 0)]
           (js/process.exit (if (pos? (+ fail err)) 1 0)))))
