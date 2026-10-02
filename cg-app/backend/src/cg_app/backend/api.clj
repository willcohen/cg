;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.backend.api
  "API handlers for the CG backend. A run returns a summary, not geometries,
   and a client fetches one dataset through /api/data/:var."
  (:require [cg.run :as run]
            [cg.geo :as geo]
            [cg.dsl :as dsl]
            [cg.dataset :as ds]
            [cg.metadata :as metadata]
            [cg.feature :as feature]
            [cg.trace :as trace]
            [sci.lang]
            [charred.api :as charred]
            [clojure.core.async :as async]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.tools.logging :as log]
            [ring.core.protocols :as protocols]))

(set! *warn-on-reflection* true)

(defonce sessions (atom {}))

(def cg-project-root
  "Root of the CG core project. load-module paths resolve relative to this."
  (let [cwd (System/getProperty "user.dir")
        parent (.getParent (java.io.File. ^String cwd))]
    (cond
      (.endsWith cwd "cg-app") (str parent "/cg")
      (.endsWith cwd "cg") cwd
      (.exists (java.io.File. ^String (str cwd "/cg"))) (str cwd "/cg")
      :else cwd)))

(def ^:private builtin-ns-prefixes
  "Prefixes of the namespaces whose vars are not user vars: the library
   namespaces in the SCI context, and the Clojure, SCI and nREPL namespaces."
  (into ["clojure." "sci." "nrepl."]
        (map str (keys dsl/cg-namespaces))))

(def ^:private session-ttl-ms
  "Sessions expire after 30 minutes of inactivity."
  (* 30 60 1000))

(defn- now-ms [] (System/currentTimeMillis))

(defn- resolve-cg-path
  "Resolves a relative path against the CG project root when the file exists
   there. Else returns `path`."
  [path]
  (let [f (java.io.File. ^String path)]
    (if (.isAbsolute f)
      path
      (let [cg-file (java.io.File. ^String cg-project-root ^String path)]
        (if (.exists cg-file)
          (.getAbsolutePath cg-file)
          path)))))

(defonce ^:private default-context-rooted
  ;; run/run-file evaluates in the default cg.run context, where load-module
  ;; reads paths against the process cwd. This roots load-module and slurp at
  ;; the CG project. It calls the host fn, because that keeps the content hash
  ;; and the load-cycle guard of load-module.
  (delay
    ;; ensure-cg-context! is private.
    (let [env (:env (#'run/ensure-cg-context!))]
      (swap! env assoc-in [:namespaces 'cg.run 'load-module]
             (fn [path] (run/load-module (resolve-cg-path path))))
      (swap! env assoc-in [:namespaces 'clojure.core 'slurp]
             (fn [path & opts] (apply slurp (resolve-cg-path path) opts)))
      true)))

(defn- new-session
  "Creates a session with its own SCI context. Overrides load-module and slurp
   to resolve paths against the CG project root."
  []
  (let [ctx (dsl/create-cg-context)
        env (:env ctx)
        ;; path -> content hash. Record the path before the eval, or two
        ;; modules that load each other recurse to a StackOverflowError.
        ;; Remove a module that throws, because the next load must try again.
        loaded (atom {})]
    (swap! env assoc-in [:namespaces 'cg.run 'load-module]
           (fn [path]
             (let [resolved (resolve-cg-path path)
                   content (slurp resolved)
                   h (hash content)]
               (when-not (= h (get @loaded resolved))
                 (swap! loaded assoc resolved h)
                 (try
                   (run/eval-cg* ctx content)
                   (catch Throwable e
                     (swap! loaded dissoc resolved)
                     (throw e))))
               {:path resolved})))
    (swap! env assoc-in [:namespaces 'clojure.core 'slurp]
           (fn [path & opts]
             (apply slurp (resolve-cg-path path) opts)))
    {:sci-ctx ctx
     :files []
     :var-meta {}
     :last-access (now-ms)}))

(defn- touch-session!
  [sid]
  (swap! sessions update sid assoc :last-access (now-ms)))

(defn- expire-sessions!
  []
  (let [cutoff (- (now-ms) session-ttl-ms)]
    (swap! sessions
           (fn [m]
             (into {} (filter (fn [[_ s]] (> (:last-access s) cutoff)) m))))))

(defn- get-session-id [request]
  (or (get-in request [:headers "x-session-id"])
      (get-in request [:params :session-id])))

(defn- get-session! [request]
  (expire-sessions!)
  (let [sid (get-session-id request)]
    (when-not sid
      (throw (ex-info "Missing session ID" {:status 400})))
    (let [sess (get @sessions sid)]
      (when-not sess
        (throw (ex-info "Session not found or expired" {:status 404})))
      (touch-session! sid)
      sess)))

(defn- get-dataset-crs [dataset]
  (or (:crs (meta dataset))
      (when-let [first-row (first (ds/rows dataset :as-maps))]
        (when-let [geom (:geometry first-row)]
          (feature/get-crs geom)))))

(defn- crs-4326?
  "True if `crs-str` names EPSG:4326 or OGC:CRS84. For PROJJSON, only the
   top-level id counts, because a projected CRS embeds its 4326 base_crs."
  [crs-str]
  (boolean
   (when (string? crs-str)
     (let [s (str/trim crs-str)]
       (if (str/starts-with? s "{")
         (let [id   (get (charred/read-json s) "id")
               code (get id "code")]
           (or (and (= "EPSG" (get id "authority")) (contains? #{4326 "4326"} code))
               (and (= "OGC" (get id "authority")) (= "CRS84" code))))
         (or (re-find #"(?i)4326" s) (re-find #"(?i)CRS84" s)))))))

(defn- introspect-var [var-name val]
  (if (ds/dataset? val)
    {:name var-name
     :type "dataset"
     :row-count (ds/row-count val)
     :crs (get-dataset-crs val)}
    {:name var-name
     :type (cond
             (fn? val) "function"
             (number? val) "number"
             (string? val) "string"
             (map? val) "map"
             (sequential? val) "collection"
             :else (.getName (class val)))}))

(defn- builtin-ns? [ns-str]
  (some #(.startsWith ^String ns-str %) builtin-ns-prefixes))

(defn- user-vars
  "The user-defined vars of an SCI context, each as an introspect-var map with
   :ns and :qualified."
  [sci-ctx]
  (let [env (:env sci-ctx)
        namespaces (get @env :namespaces)]
    (->> namespaces
         (mapcat (fn [[ns-sym ns-map]]
                   (when (and (symbol? ns-sym)
                              (not (builtin-ns? (str ns-sym))))
                     (->> ns-map
                          (filter (fn [[k _]]
                                    (and (symbol? k)
                                         (not (#{:obj :aliases :refers :imports :refer-clojure} k))
                                         (not= k 'require))))
                          (keep (fn [[k v]]
                                  (let [val (if (instance? sci.lang.Var v) @v v)]
                                    (when-not (or (= val :sci.impl.vars/unrealized)
                                                  (nil? val))
                                      (assoc (introspect-var (str k) val)
                                             :ns (str ns-sym)
                                             :qualified (str ns-sym "/" k))))))))))
         (into []))))

(defn- lookup-var-value [sci-ctx var-name]
  (let [env @(:env sci-ctx)
        deref-var (fn [v] (if (instance? sci.lang.Var v) @v v))]
    (or (when-let [v (get-in env [:namespaces 'user (symbol var-name)])]
          (deref-var v))
        (when-let [[_ ns-str name-str] (re-matches #"(.+)/(.+)" var-name)]
          (when-let [v (get-in env [:namespaces (symbol ns-str) (symbol name-str)])]
            (deref-var v)))
        ;; A module file opened as the session entry defines its vars in its
        ;; own namespace, but the client fetches by the unqualified name from
        ;; /api/session/vars.
        (some (fn [[ns-sym ns-map]]
                (when (and (symbol? ns-sym) (not (builtin-ns? (str ns-sym))))
                  (when-let [v (get ns-map (symbol var-name))]
                    (deref-var v))))
              (get env :namespaces)))))

(defn health [_request]
  {:status 200
   :body {:status "ok"
          :service "cg-app-backend"
          :backend "jvm"}})

(defn- deref-if-var
  "The value of `v` when it is a SCI Var, else `v`. eval-cg of a .cg file that
   ends with a def returns the Var."
  [v]
  (if (instance? sci.lang.Var v) @v v))

(defn- resolve-source-bindings
  "Adds each missing source-node :data-ref to `bindings`. A named ref resolves
   in the shared eval context. Any other ref is the data and binds to itself."
  [flow-graph bindings]
  (let [refs (->> (:nodes flow-graph)
                  (filter #(= :source (:type %)))
                  (keep :data-ref)
                  (remove #(contains? bindings %)))]
    (reduce (fn [acc r]
              (if (or (instance? clojure.lang.Named r) (string? r))
                (assoc acc r (deref-if-var (run/eval-cg (name r))))
                (assoc acc r r)))
            bindings refs)))

(defn- result-count [results]
  (cond
    (ds/dataset? results) (ds/row-count results)
    (coll? results) (count results)
    (some? results) 1
    :else 0))

(defn- run-request
  "Runs the model of a request body and returns the results. `code` runs
   through cg.run/run-file with no overlay. A `flow-graph` runs as it is."
  [body]
  (let [opts (walk/keywordize-keys (or (:opts body) {}))]
    (if-let [code-str (:code body)]
      (do @default-context-rooted
          (:result (run/run-file (:code (metadata/split-code-and-metadata code-str)) opts)))
      (let [fg (walk/keywordize-keys (:flow-graph body))]
        (when-not (and (map? fg) (seq (:nodes fg)))
          (throw (ex-info "provide 'code' or a 'flow-graph' with nodes" {})))
        (run/run-model fg
                       (resolve-source-bindings fg (walk/keywordize-keys (or (:bindings body) {})))
                       opts)))))

(defn execute-code
  "Runs CG code on the JVM and responds with a summary ({:status :count
  :message}), because JTS geometries are not JSON-serializable."
  [request]
  (try
    (let [results (run-request (:body request))
          n (result-count results)]
      (log/info "Execution successful, produced" n "results")
      {:status 200
       :body {:status "success"
              :count n
              :message (str "Executed successfully, produced " n " result(s)")}})
    (catch Exception e
      (log/error e "Failed to execute CG code")
      {:status 500
       :body {:error (.getMessage e)
              :type (-> e class .getName)}})))

(defonce executions
  ;; id -> {:id :chan :status (:running, :complete or :failed) :result
  ;;        :session-id :started-at :ended-at}. Public, because tests seed a
  ;; live entry for the 409 path.
  (atom {}))

(def ^:private execution-ttl-ms
  "Finished executions and their results expire after 30 minutes."
  (* 30 60 1000))

(defn- prune-executions! []
  (let [cutoff (- (now-ms) execution-ttl-ms)]
    (swap! executions
           (fn [m]
             (into {} (remove (fn [[_ e]]
                                (and (not= :running (:status e))
                                     (< (:ended-at e 0) cutoff)))
                              m))))))

(defn- live-execution [m]
  (some #(when (= :running (:status %)) %) (vals m)))

(defn- finish-execution!
  "Records the outcome, untaps the trace stream, puts the end sentinel and
   closes the channel. The sleep before the untap lets in-flight events arrive."
  [id updates sentinel]
  (let [{:keys [chan]} (get @executions id)]
    (swap! executions update id merge updates {:ended-at (now-ms)})
    (Thread/sleep 100)
    (trace/untap-trace chan)
    (async/put! chan (merge {:kind "execution-ended" :execution-id id} sentinel))
    (async/close! chan)))

(defn- run-execution!
  "Body of the execution future: run the request's model, then finish."
  [id body]
  (try
    (let [_ (log/info "Async execution" id "starting")
          results (run-request body)
          n (result-count results)]
      (log/info "Async execution" id "complete," n "results")
      (finish-execution! id
                         {:status :complete
                          :result {:count n
                                   :message (str "Executed successfully, produced " n " result(s)")}}
                         {:outcome "success" :count n}))
    (catch Throwable e
      (log/error e "Async execution failed" id)
      (finish-execution! id
                         {:status :failed
                          :result {:error (.getMessage e)
                                   :type (-> e class .getName)}}
                         {:outcome "error" :message (.getMessage e)}))))

(defn execution-start
  "POST /api/execution/start: runs the request in a future and returns
   {:execution-id id}. Only one execution streams at a time; a second gets 409."
  [request]
  (try
    (prune-executions!)
    (let [body (:body request)
          id (str (java.util.UUID/randomUUID))
          ;; The mult waits for each tap. A full fixed buffer with no reader
          ;; blocks the global trace stream for the whole process. A dropped
          ;; event shows as a gap in :seq.
          ch (async/chan (async/sliding-buffer 4096))
          entry {:id id
                 :chan ch
                 :status :running
                 :session-id (get-session-id request)
                 :started-at (now-ms)}
          [_ new] (swap-vals! executions
                              (fn [m] (if (live-execution m) m (assoc m id entry))))]
      (if-not (contains? new id)
        (do (async/close! ch)
            {:status 409
             :body {:error "Another execution is already streaming; trace events are global, so executions serialize"
                    :live-execution-id (:id (live-execution new))}})
        (do (trace/tap-trace ch)
            (future (run-execution! id body))
            {:status 200
             :body {:execution-id id}})))
    (catch Exception e
      (log/error e "Failed to start async execution")
      {:status 500
       :body {:error (.getMessage e)
              :type (-> e class .getName)}})))

(defn- write-sse-event!
  "Writes one `data: <json>` frame and flushes. An event that JSON cannot
   serialize becomes a stub, because one bad event must not stop the stream."
  [^java.io.Writer w evt]
  (let [json (try (charred/write-json-str evt)
                  (catch Throwable e
                    (charred/write-json-str {:kind (get evt :kind "unserializable")
                                             :seq (:seq evt)
                                             :ts (:ts evt)
                                             :pid (:pid evt)
                                             :serialization-error (.getMessage e)})))]
    (.write w (str "data: " json "\n\n"))
    (.flush w)))

(defn execution-trace
  "GET /api/execution/:id/trace: an SSE stream of the trace events, to the
   end sentinel. Only one reader for each execution: two readers split it."
  [request]
  (let [id (get-in request [:path-params :id])
        {:keys [chan] :as e} (get @executions id)]
    (if-not e
      {:status 404
       :body {:error (str "No such execution: " id)}}
      {:status 200
       :headers {"Content-Type" "text/event-stream"
                 "Cache-Control" "no-cache"}
       :body (reify protocols/StreamableResponseBody
               (write-body-to-stream [_ _ output-stream]
                 (with-open [w (java.io.OutputStreamWriter.
                                ^java.io.OutputStream output-stream
                                java.nio.charset.StandardCharsets/UTF_8)]
                   (loop []
                     (when-let [evt (async/<!! chan)]
                       (write-sse-event! w evt)
                       (when-not (= "execution-ended" (:kind evt))
                         (recur)))))))})))

(defn execution-result
  "GET /api/execution/:id/result: the status, plus :result when finished
   ({:count :message} on success, {:error :type} on failure)."
  [request]
  (let [id (get-in request [:path-params :id])
        e (get @executions id)]
    (if-not e
      {:status 404
       :body {:error (str "No such execution: " id)}}
      {:status 200
       :body (cond-> {:execution-id id
                      :status (name (:status e))}
               (:result e) (assoc :result (:result e)))})))

(defn execute-model
  "DEPRECATED: Use execute-code instead."
  [request]
  (try
    (let [body (:body request)
          flow-graph (walk/keywordize-keys (:flow-graph body))
          bindings (walk/keywordize-keys (or (:bindings body) {}))
          opts (walk/keywordize-keys (or (:opts body) {}))
          _ (log/info "Executing Flow Graph with" (count (:nodes flow-graph)) "nodes")
          results (run/run-model flow-graph bindings opts)
          result-count (if (coll? results) (count results) 1)]
      (log/info "Execution successful, produced" result-count "results")
      {:status 200
       :body {:status "success"
              :count result-count
              :results results
              :message (str "Executed successfully, produced " result-count " result(s)")}})
    (catch Exception e
      (log/error e "Failed to execute Flow Graph")
      {:status 500
       :body {:error (.getMessage e)
              :type (-> e class .getName)}})))

(defn transform-geometry
  "Transform geometry using cg.geo functions."
  [request]
  (try
    (let [{:keys [geometry operation params]} (:body request)
          _ (log/info "Transforming geometry with operation:" operation)
          result (case operation
                   :buffer (geo/buffer geometry (:distance params))
                   :simplify (geo/simplify geometry (:tolerance params))
                   :union (geo/union geometry (:other params))
                   {:error "Unknown operation"})]
      {:status 200
       :body {:result result}})
    (catch Exception e
      (log/error e "Failed to transform geometry")
      {:status 500
       :body {:error (.getMessage e)}})))

(defn project-geometry
  "Returns 501: projection is not implemented."
  [_request]
  {:status 501
   :body {:error "Projection not yet implemented"
          :message "CRS transformation will be available in future version"}})

(defn session-create
  "POST /api/session/create - Create a new isolated session."
  [_request]
  (expire-sessions!)
  (let [sid (str (java.util.UUID/randomUUID))]
    (swap! sessions assoc sid (new-session))
    (log/info "Created session" sid)
    {:status 200
     :body {:session-id sid}}))

(defn session-delete
  "DELETE /api/session/delete - Destroy a session and free resources.
   Session ID from X-Session-Id header."
  [request]
  (let [sid (get-session-id request)]
    (if (and sid (get @sessions sid))
      (do (swap! sessions dissoc sid)
          (log/info "Deleted session" sid)
          {:status 200
           :body {:status "ok"}})
      {:status 404
       :body {:error "Session not found"}})))

(defn session-load
  "POST /api/session/load: evaluates .cg code in the session. The body has
   `code` (with an optional `filename`), or `path` for a file on the server."
  [request]
  (try
    (let [sess (get-session! request)
          sid (get-session-id request)
          {:keys [code path filename]} (:body request)
          [content label] (cond
                            code [code (or filename "<eval>")]
                            path [(slurp (resolve-cg-path path)) path]
                            :else (throw (ex-info "Provide 'code' or 'path'" {:status 400})))
          _ (log/info "Loading" label "in session" sid)
          ctx (:sci-ctx sess)
          ;; Bind the entry filename (basename) for the eval. A module file can
          ;; then tell, through cg.run/session-file, whether it is the opened
          ;; file or a load-module dependency.
          _ (binding [run/*session-file* (last (str/split label #"/"))]
              (run/eval-cg* ctx (str "(do " content ")")))
          vars (user-vars ctx)
          var-meta-map (into {} (map (fn [v] [(:qualified v) v]) vars))]
      (swap! sessions update sid
             (fn [s]
               (-> s
                   (update :files #(vec (distinct (conj % label))))
                   (update :var-meta merge var-meta-map))))
      (log/info "Loaded" label "- found" (count vars) "user vars")
      {:status 200
       :body {:status "ok"
              :files (get-in @sessions [sid :files])
              :vars vars}})
    (catch Exception e
      (log/error e "Failed to load .cg code")
      {:status (or (:status (ex-data e)) 500)
       :body {:error (.getMessage e)
              :type (-> e class .getName)}})))

(defn session-vars
  "GET /api/session/vars - List all user vars in the session."
  [request]
  (try
    (let [sess (get-session! request)
          vars (user-vars (:sci-ctx sess))]
      {:status 200
       :body {:vars vars}})
    (catch Exception e
      {:status (or (:status (ex-data e)) 500)
       :body {:error (.getMessage e)}})))

(defn data-var
  "POST /api/data/:var - Get dataset as GeoJSON, filtered by bbox."
  [request]
  (try
    (let [sess (get-session! request)
          var-name (get-in request [:path-params :var])
          {:keys [bbox limit]} (:body request)
          limit (or limit 5000)
          ctx (:sci-ctx sess)
          val (lookup-var-value ctx var-name)]
      (when-not val
        (throw (ex-info (str "Var not found: " var-name) {:var var-name})))
      (when-not (ds/dataset? val)
        (throw (ex-info (str "Var is not a dataset: " var-name) {:var var-name})))
      (let [crs (get-dataset-crs val)
            crs-str (cond
                      (string? crs) crs
                      (map? crs) (get-in crs [:properties :name])
                      :else "EPSG:4326")
            is-4326? (crs-4326? crs-str)
            needs-reproject? (not is-4326?)
            filtered (if (and bbox (= 4 (count bbox)))
                       (let [[x1 y1 x2 y2] bbox
                             bbox-poly-4326 (feature/polygon
                                             [[x1 y1] [x2 y1] [x2 y2] [x1 y2] [x1 y1]])
                             ;; The bbox is in EPSG:4326, but select-intersects
                             ;; filters the dataset in its native CRS. Reproject
                             ;; the bbox, or a 4326 rectangle intersects nothing
                             ;; in a projected dataset.
                             bbox-poly (if needs-reproject?
                                         (geo/transform-geom bbox-poly-4326 "EPSG:4326" crs-str)
                                         bbox-poly-4326)]
                         (ds/select-intersects val bbox-poly))
                       val)
            limited (if (> (ds/row-count filtered) limit)
                      (ds/head filtered limit)
                      filtered)
            served (if (and needs-reproject? crs-str)
                     (do
                       (log/info "Reprojecting" var-name "from" crs-str "to EPSG:4326"
                                 "(" (ds/row-count limited) "rows)")
                       (let [rows (ds/rows limited :as-maps)
                             reprojected (mapv (fn [row]
                                                 (if-let [geom (:geometry row)]
                                                   (assoc row :geometry
                                                          (geo/transform-geom geom crs-str "EPSG:4326"))
                                                   row))
                                               rows)]
                         (ds/->dataset reprojected)))
                     limited)
            geojson-str (ds/dataset->geojson-string served)
            fc (charred/read-json geojson-str :key-fn keyword)]
        ;; :total-rows is the row count after the bbox filter and before the
        ;; limit. :returned-rows is the count in this response.
        {:status 200
         :body (assoc fc :crs "EPSG:4326"
                      :total-rows (ds/row-count filtered)
                      :returned-rows (ds/row-count served))}))
    (catch Exception e
      (log/error e "Failed to get data for var")
      {:status (or (:status (ex-data e)) 500)
       :body {:error (.getMessage e)}})))

(defn session-source
  "POST /api/session/source - Return source file + line for a var."
  [request]
  (try
    (let [sess (get-session! request)
          {:keys [var]} (:body request)
          ctx (:sci-ctx sess)
          env @(:env ctx)
          [ns-str name-str] (if-let [[_ n s] (re-matches #"(.+)/(.+)" var)]
                              [n s]
                              ["user" var])
          sci-var (get-in env [:namespaces (symbol ns-str) (symbol name-str)])
          m (when (instance? sci.lang.Var sci-var)
              (meta sci-var))]
      {:status 200
       :body {:file (:file m)
              :line (:line m)
              :column (:column m)}})
    (catch Exception e
      {:status (or (:status (ex-data e)) 500)
       :body {:error (.getMessage e)}})))
