;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.runtime
  (:require ["vscode" :as vscode]
            [cg-vscode.dev-shell :as dev-shell]
            [cg-vscode.extension.lifecycle :as lc]
            [cg-vscode.runtime.jvm-client :as jvm-client]
            ["../../../cg-app/shared/squint/src/cg_gui/run/jvm.mjs" :as jvm]
            ["../../../cg-app/shared/squint/src/cg_gui/run/lane.mjs" :as lane]))

(defn config-runtime []
  (.. vscode/workspace (getConfiguration "cg") (get "runtime" "jvm")))

(defn- set-config-runtime!
  "Writes the runtime setting, which each run reads: to the workspace when a
   folder is open, else to the user settings, because a window with no folder
   has no workspace settings. Gives a Promise."
  [value]
  (.. vscode/workspace (getConfiguration "cg")
      (update "runtime" value (if (seq (.-workspaceFolders vscode/workspace))
                                vscode/ConfigurationTarget.Workspace
                                vscode/ConfigurationTarget.Global))))

(defn- repo-root [!state]
  (let [^js ctx (:extension/context @!state)]
    (.resolve (js/require "path") (.. ctx -extensionUri -fsPath) "..")))

(defn- backend-port-file
  "`bb backend:dev` writes the port that the OS assigned to this file."
  [!state]
  (.resolve (js/require "path") (repo-root !state) "cg-app" ".cg-backend-port"))

(defn- read-backend-port [!state]
  (let [fs (js/require "fs")
        p (backend-port-file !state)]
    (try
      (when (.existsSync fs p)
        (let [n (js/parseInt (.trim (.toString (.readFileSync fs p))) 10)]
          (when (and (not (js/isNaN n)) (pos? n)) n)))
      (catch :default _ nil))))

(defn resolve-jvm-port [!state]
  (or (read-backend-port !state) jvm-client/default-port))

(defn- check-jvm-reachable [port]
  (jvm/health? (jvm-client/send-to port 2000)))

(defonce !backend-proc (atom nil))

;; A cold GraalVM boot can take about 3 minutes. A warm boot takes 10 to 30 s.
(def backend-spawn-timeout-ms 180000)

(defn- sleep [ms] (js/Promise. (fn [res _] (js/setTimeout res ms))))

(def ^:private win32? (= "win32" js/process.platform))

(defn- spawn-backend! [!state ^js channel]
  (let [cp (js/require "child_process")
        path (js/require "path")
        repo (repo-root !state)
        [prog args] (dev-shell/spawn-args js/process.platform js/process.env repo
                                          ["clj" "-M:backend" "--port-file" (backend-port-file !state)])
        ;; detached gives a process group to kill. win32 has none.
        ^js proc (.spawn cp prog args #js {:cwd (.join path repo "cg-app") :detached (not win32?)
                                           :windowsHide true :stdio "ignore"})]
    (reset! !backend-proc proc)
    (.on proc "error" (fn [^js e]
                        (.appendLine channel (str "CG: backend spawn failed — " (.-message e)))
                        (reset! !backend-proc nil)))
    (.on proc "exit" (fn [code _sig]
                       (.appendLine channel (str "CG: backend process exited (code " code ")"))
                       (reset! !backend-proc nil)))
    (.unref proc)
    proc))

(defn stop-backend! []
  (when-let [^js proc @!backend-proc]
    (try (if win32?
           (.spawnSync (js/require "child_process") "taskkill"
                       #js ["/pid" (str (.-pid proc)) "/t" "/f"] #js {:windowsHide true})
           (js/process.kill (* -1 (.-pid proc)) "SIGTERM"))
         (catch :default _
           (try (.kill proc "SIGTERM") (catch :default _ nil))))
    (reset! !backend-proc nil)))

(defn ^:async wait-for-reachable
  "Polls until the backend is reachable or `deadline` (epoch ms) passes.
   Returns a boolean."
  [!state deadline]
  (if (await (check-jvm-reachable (resolve-jvm-port !state)))
    true
    (if (> (js/Date.now) deadline)
      false
      (do (await (sleep 1500))
          (await (wait-for-reachable !state deadline))))))

(defn ^:async ensure-backend!
  "Gives {:reachable :port}, with no error on a failure: `auto` then uses the
   JS backend. `on-stage`, when not nil, gets \"start\" and \"done\" around
   the wait."
  [!state on-stage]
  (let [port0 (resolve-jvm-port !state)
        ^js channel (:runtime/output-channel @!state)]
    (if (await (check-jvm-reachable port0))
      {:reachable true :port port0}
      (do
        (when on-stage (on-stage "start"))
        (when-not @!backend-proc
          (.appendLine channel "CG: JVM backend not reachable — starting it (first boot ~1-3 min)...")
          (spawn-backend! !state channel))
        (let [ok (await (wait-for-reachable !state (+ (js/Date.now) backend-spawn-timeout-ms)))
              port (resolve-jvm-port !state)]
          (when on-stage (on-stage "done"))
          (.appendLine channel (if ok
                                 (str "CG: JVM backend ready on " port)
                                 "CG: JVM backend did not become ready (is `direnv`/`clj` on PATH?)"))
          {:reachable ok :port port})))))

(defn jvm-choice? []
  (= "jvm" (config-runtime)))

(defn resolve-map-target
  "Probes the backend, also for an explicit choice, and applies the choice.
   Returns a Promise of {:resolved 'jvm'|'js' :reachable bool :port n}."
  [!state]
  (let [cfg (config-runtime)
        port (resolve-jvm-port !state)]
    (swap! !state assoc :runtime/choice cfg)
    (-> (check-jvm-reachable port)
        (.then (fn [reachable]
                 (let [resolved (lane/resolve-runtime cfg reachable)]
                   (swap! !state assoc :runtime/resolved resolved)
                   {:resolved resolved :reachable reachable :port port}))))))

(defn- select-runtime [!state]
  (let [cfg (config-runtime)]
    (swap! !state assoc :runtime/choice cfg)
    (if (= cfg "auto")
      (.then (resolve-map-target !state) (fn [r] (:resolved r)))
      (js/Promise.resolve cfg))))

(defn- update-status-bar! [!state]
  (let [^js bar (:runtime/status-bar @!state)
        choice (:runtime/choice @!state)
        resolved (:runtime/resolved @!state)]
    (set! (.-text bar) (lane/chip-text choice resolved))))

(defn- create-status-bar-item [!state]
  (let [^js bar (vscode/window.createStatusBarItem vscode/StatusBarAlignment.Right 100)]
    (set! (.-text bar) "CG: jvm")
    (set! (.-tooltip bar) "Click to set CG runtime")
    (set! (.-command bar) "cg.setRuntime")
    (.show bar)
    (lc/push-disposable! !state bar)
    (swap! !state assoc :runtime/status-bar bar)))

(defn- pick-runtime []
  (vscode/window.showQuickPick
   (to-array ["auto" "jvm" "js"])
   #js {:placeHolder "Select CG runtime"}))

(defn- show-runtime! [!state resolved]
  (update-status-bar! !state)
  (.appendLine ^js (:runtime/output-channel @!state) (str "CG: runtime resolved to " resolved)))

(defn init-runtime! [!state]
  (create-status-bar-item !state)
  (.then (select-runtime !state) (fn [resolved] (show-runtime! !state resolved))))

(defn ^:async apply-runtime-choice!
  "The status bar changes only after the save, because a run reads the
   setting."
  [!state choice]
  (try
    (await (set-config-runtime! choice))
    (swap! !state assoc :runtime/choice choice)
    (if (= choice "auto")
      (show-runtime! !state (await (select-runtime !state)))
      (do (swap! !state assoc :runtime/resolved choice)
          (update-status-bar! !state)))
    (catch :default e
      (vscode/window.showErrorMessage (str "CG: the runtime setting was not saved: " (.-message e))))))

(defn ^:async probe-runtime!
  "Gives {:choice :resolved :reachable :port}."
  [!state]
  (let [r (await (resolve-map-target !state))]
    (update-status-bar! !state)
    (assoc r :choice (:runtime/choice @!state))))

(defn register-runtime-commands! [!state]
  (lc/register-command! !state "cg.setRuntime"
                        (fn [state]
                          (.then (pick-runtime)
                                 (fn [choice] (when choice (apply-runtime-choice! state choice)))))))
