;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.backend.server
  "JVM HTTP server wrapping the cg library."
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [reitit.ring :as ring]
            [cg-app.backend.api :as api]
            [cg-app.backend.middleware :as middleware]
            [cg.run :as run]
            [clojure.tools.logging :as log])
  (:gen-class))

(set! *warn-on-reflection* true)

(def routes
  [["/api"
    ["/health" {:get api/health}]
    ["/execute-code" {:post api/execute-code}]
    ["/execute" {:post api/execute-model}]
    ["/execution"
     ["/start" {:post api/execution-start}]
     ["/:id/trace" {:get api/execution-trace}]
     ["/:id/result" {:get api/execution-result}]]
    ["/transform" {:post api/transform-geometry}]
    ["/project" {:post api/project-geometry}]
    ["/session"
     ["/create" {:post api/session-create}]
     ["/delete" {:delete api/session-delete}]
     ["/load" {:post api/session-load}]
     ["/vars" {:get api/session-vars}]
     ["/source" {:post api/session-source}]]
    ["/data/:var" {:post api/data-var}]]])

(def handler
  (ring/ring-handler
   (ring/router routes)
   (ring/create-default-handler
    {:not-found (constantly {:status 404
                             :body {:error "Not found"}})})))

(defonce server (atom nil))

(defn create-app []
  (-> handler
      (wrap-keyword-params)
      (wrap-params)
      (wrap-json-body {:keywords? true :bigdecimals? true})
      (wrap-json-response)
      (middleware/wrap-cors)
      (middleware/wrap-logging)))

(defn start-server!
  "Starts the Jetty server. The defaults are loopback only (127.0.0.1) and a
  port that the OS assigns (0). Pass :host or :port to override."
  [& {:keys [port host] :or {port 0 host "127.0.0.1"}}]
  (when-not @server
    (let [s (jetty/run-jetty (create-app)
                             {:port port
                              :host host
                              :join? false})]
      (reset! server s)
      (log/info "Backend server started on" host "port" port)
      s)))

(defn bound-port
  "The port that the OS assigned, read after the bind."
  [^org.eclipse.jetty.server.Server s]
  (.getLocalPort ^org.eclipse.jetty.server.NetworkConnector (first (.getConnectors s))))

(defn write-port-file!
  "Writes the bound port to `path` as the only content, for the native
  supervisor to read."
  [path port]
  (spit path (str port)))

(defn stop-server! []
  (when-let [^org.eclipse.jetty.server.Server s @server]
    (.stop s)
    (reset! server nil)
    (log/info "Backend server stopped")))

(defn- parse-port-file
  "Extract the value following --port-file in args, or nil."
  [args]
  (->> args (partition 2 1) (some (fn [[a b]] (when (= a "--port-file") b)))))

(defn init-workers!
  "Starts the JVM workload pool that all sessions share, because a flow throws
  on a nil :compute-exec. A failure is logged and does not stop the server."
  []
  (try
    (run/ensure-cg-workers!)
    (log/info "CG workload pool initialized")
    (catch Throwable t
      (log/warn t "CG workload pool init failed; analysis flows will error until it is available"))))

(defn -main [& args]
  (log/info "Starting CG backend...")
  (let [s (start-server!)
        port (bound-port s)]
    (log/info "Backend bound port" port)
    (init-workers!)
    (when-let [path (parse-port-file args)]
      (write-port-file! path port)
      (log/info "Wrote port" port "to" path))
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. ^Runnable stop-server!))))
