;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.runtime
  (:require ["vscode" :as vscode]
            [cg-vscode.extension.lifecycle :as lc]
            [cg-vscode.runtime.jvm-client :as jvm]
            ["../../../cg-app/shared/squint/src/cg_gui/run/lane.mjs" :as lane]))

(def default-port jvm/default-port)

(defn- config-runtime []
  (.. vscode/workspace (getConfiguration "cg") (get "runtime" "auto")))

(defn- set-config-runtime! [value]
  (.. vscode/workspace (getConfiguration "cg") (update "runtime" value)))

(defn- backend-port-file
  "The path of the backend port file, cg-app/.cg-backend-port. `bb backend:dev`
   writes the port that the OS assigned there."
  [!state]
  (let [^js ctx (:extension/context @!state)
        ext-path (.. ctx -extensionUri -fsPath)
        path (js/require "path")]
    (.resolve path ext-path ".." "cg-app" ".cg-backend-port")))

(defn- read-backend-port [!state]
  (let [fs (js/require "fs")
        p (backend-port-file !state)]
    (try
      (when (.existsSync fs p)
        (let [n (js/parseInt (.trim (.toString (.readFileSync fs p))) 10)]
          (when (and (not (js/isNaN n)) (pos? n)) n)))
      (catch :default _ nil))))

(defn resolve-jvm-port
  "The JVM backend port: the port in the port file, else default-port. Stores
   it under :runtime/jvm-port and returns it."
  [!state]
  (let [port (or (read-backend-port !state) default-port)]
    (swap! !state assoc :runtime/jvm-port port)
    port))

(defn- check-jvm-reachable [port]
  (js/Promise.
   (fn [resolve _reject]
     (let [http (js/require "http")]
       (-> (.request http
            #js {:hostname "localhost" :port port :method "GET" :path "/api/health" :timeout 2000}
            (fn [res]
              (.resume res)
              (resolve (= 200 (.-statusCode res)))))
           (.on "error" (fn [_err] (resolve false)))
           (.on "timeout" (fn [] (resolve false)))
           (.end))))))

;; The backend spawns through a login shell, because a host started from the
;; Finder or Dock has no nix, direnv or clj on PATH.

(defonce !backend-proc (atom nil))

;; A cold GraalVM boot can take about 3 minutes. A warm boot takes 10 to 30 s.
(def backend-spawn-timeout-ms 180000)

(defn- sleep [ms] (js/Promise. (fn [res _] (js/setTimeout res ms))))

(defn- repo-root [!state]
  (let [^js ctx (:extension/context @!state)
        path (js/require "path")]
    (.resolve path (.. ctx -extensionUri -fsPath) "..")))

(defn- spawn-backend!
  "Spawns `clj -M:backend --port-file <cg-app/.cg-backend-port>` detached,
   through a login shell (for PATH), and stores the process in !backend-proc."
  [!state ^js channel]
  (let [cp (js/require "child_process")
        path (js/require "path")
        repo (repo-root !state)
        cg-app (.join path repo "cg-app")
        pf (backend-port-file !state)
        shell (or (aget (.-env js/process) "SHELL") "/bin/zsh")
        inner (str "cd '" cg-app "' && exec direnv exec '" repo "' clj -M:backend --port-file '" pf "'")
        ^js proc (.spawn cp shell #js ["-lc" inner] #js {:detached true :stdio "ignore"})]
    (reset! !backend-proc proc)
    (.on proc "error" (fn [^js e]
                        (when channel (.appendLine channel (str "CG: backend spawn failed — " (.-message e))))
                        (reset! !backend-proc nil)))
    (.on proc "exit" (fn [code _sig]
                       (when channel (.appendLine channel (str "CG: backend process exited (code " code ")")))
                       (reset! !backend-proc nil)))
    (.unref proc)
    proc))

(defn stop-backend!
  "Kills the spawned backend and its process group (shell, direnv, clj, JVM).
   Does nothing when the extension did not spawn one."
  []
  (when-let [^js proc @!backend-proc]
    (try (js/process.kill (* -1 (.-pid proc)) "SIGTERM")
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
  "Spawns the JVM backend when it is down and waits for it. Returns {:reachable
   :port :spawned}, with no error on a failure: `auto` then uses the JS backend."
  [!state ^js channel]
  (let [port0 (resolve-jvm-port !state)]
    (if (await (check-jvm-reachable port0))
      {:reachable true :port port0 :spawned false}
      (do
        (when-not @!backend-proc
          (when channel (.appendLine channel "CG: JVM backend not reachable — starting it (first boot ~1-3 min)..."))
          (spawn-backend! !state channel))
        (let [ok (await (wait-for-reachable !state (+ (js/Date.now) backend-spawn-timeout-ms)))
              port (resolve-jvm-port !state)]
          (when channel
            (.appendLine channel (if ok
                                   (str "CG: JVM backend ready on " port)
                                   "CG: JVM backend did not become ready (is `direnv`/`clj` on PATH?)")))
          {:reachable ok :port port :spawned true})))))

(defn wants-jvm?
  "True when the runtime choice allows the JVM backend (jvm or auto, not js)."
  []
  (not= "js" (config-runtime)))

(defn eval-jvm
  "Evaluates `text` (.cg source) on the JVM REST backend. Returns a Promise of
   {:result <body>} or {:error <msg>}."
  [!state text filename]
  (let [port (resolve-jvm-port !state)]
    (jvm/eval-cg port text filename)))

(defn resolve-map-target
  "Probes the backend, also for an explicit choice, and applies the choice.
   Returns a Promise of {:resolved 'jvm'|'js' :reachable bool :port n}."
  [!state]
  (let [cfg (config-runtime)
        port (resolve-jvm-port !state)]
    (swap! !state assoc :runtime/choice cfg)
    (-> (check-jvm-reachable port)
        (.then (fn [reachable]
                 (swap! !state assoc :runtime/jvm-reachable reachable)
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
    (when bar
      (set! (.-text bar) (lane/chip-text choice resolved)))))

(defn- create-status-bar-item [!state]
  (let [^js bar (vscode/window.createStatusBarItem vscode/StatusBarAlignment.Right 100)]
    (set! (.-text bar) "CG: auto")
    (set! (.-tooltip bar) "Click to set CG runtime")
    (set! (.-command bar) "cg.setRuntime")
    (.show bar)
    (lc/push-disposable! !state bar)
    (swap! !state assoc :runtime/status-bar bar)
    bar))

(defn- pick-runtime []
  (vscode/window.showQuickPick
   (to-array ["auto" "jvm" "js"])
   #js {:placeHolder "Select CG runtime"}))

(defn init-runtime! [!state]
  (let [^js channel (:runtime/output-channel @!state)]
    (create-status-bar-item !state)
    (-> (select-runtime !state)
        (.then (fn [resolved]
                 (update-status-bar! !state)
                 (.appendLine channel (str "CG: runtime resolved to " resolved)))))))

(defn- active-cg-document
  "Return [doc text filename] for the current .cg editor, or nil if none."
  []
  (let [^js active-editor (.. vscode -window -activeTextEditor)]
    (when (and active-editor
               (-> active-editor .-document .-uri .-path (.endsWith ".cg")))
      (let [^js doc (.-document active-editor)
            text (.getText doc)
            path-str (.-path (.-uri doc))
            filename (last (.split path-str "/"))]
        [doc text filename]))))

(defn- post-jvm-vars-to-panel!
  "Posts the JVM eval var list as cg/jvm-vars to the flow-editor panel of the
   active document, if one is open."
  [!state ^js doc filename vars]
  (let [uri-str (str (.-uri doc))]
    (when-let [^js panel (get-in @!state [:editor/panels uri-str])]
      (.. panel -webview
          (postMessage #js {:type "cg/jvm-vars"
                            :file filename
                            :vars vars})))))

(defn run-eval-jvm!
  "Runs the active .cg document on the JVM REST backend and prints the results
   to the CG output channel."
  [!state]
  (let [^js channel (:runtime/output-channel @!state)]
    (if-let [[doc text filename] (active-cg-document)]
      (let [port (or (:runtime/jvm-port @!state) default-port)]
        (.appendLine channel (str "CG: Eval (JVM) " filename " against localhost:" port))
        (-> (eval-jvm !state text filename)
            (.then (fn [{:keys [result error]}]
                     (if error
                       (.appendLine channel (str "CG: Eval (JVM) error: " error))
                       (let [vars (:vars result)
                             files (:files result)]
                         (.appendLine channel
                                      (str "CG: Eval (JVM) ok — "
                                           (count files) " file(s), "
                                           (count vars) " var(s)"))
                         (doseq [v vars]
                           (.appendLine channel
                                        (str "  " (:qualified v)
                                             " :: " (:type v)
                                             (when-let [rc (:row-count v)]
                                               (str " (" rc " rows)")))))
                         (post-jvm-vars-to-panel! !state doc filename vars)))))))
      (.appendLine channel "CG: Eval (JVM) — no active .cg document"))))

(defn register-runtime-commands! [!state]
  (lc/register-command! !state "cg.setRuntime"
                        (fn [state]
                          (-> (pick-runtime)
                              (.then (fn [choice]
                                       (when choice
                                         (set-config-runtime! choice)
                                         (swap! state assoc :runtime/choice choice)
                                         (if (= choice "auto")
                                           (-> (select-runtime state)
                                               (.then (fn [resolved]
                                                        (update-status-bar! state)
                                                        (let [^js ch (:runtime/output-channel @state)]
                                                          (.appendLine ch (str "CG: runtime resolved to " resolved))))))
                                           (do
                                             (swap! state assoc :runtime/resolved choice)
                                             (update-status-bar! state)))))))))

  (lc/register-command! !state "cg.refreshRuntime"
                        (fn [state]
                          (let [^js channel (:runtime/output-channel @state)]
                            (.appendLine channel "CG: Refreshing runtime...")
                            (-> (select-runtime state)
                                (.then (fn [resolved]
                                         (update-status-bar! state)
                                         (.appendLine channel (str "CG: runtime resolved to " resolved))))))))

  (lc/register-command! !state "cg.evalJvm"
                        (fn [state]
                          (run-eval-jvm! state))))
