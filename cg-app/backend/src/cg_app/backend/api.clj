;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.backend.api
  "API handlers for the CG backend. A run gives a summary and the GeoJSON of
   its map-layer models (see wire-result). A client gets the rows of one
   dataset through /api/data/:var."
  (:require [babashka.nrepl.server :as nrepl-server]
            [cg.run :as run]
            [cg.geo :as geo]
            [cg.dsl :as dsl]
            [cg.dataset :as ds]
            [cg.metadata :as metadata]
            [cg.feature :as feature]
            [cg.trace :as trace]
            [sci.core :as sci]
            [sci.lang]
            [charred.api :as charred]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [ring.core.protocols :as protocols]))

(set! *warn-on-reflection* true)

(defonce sessions (atom {}))

(def cg-project-root
  "Root of the CG core project. session-input reads a relative path here when
   the root of the session does not have it."
  (let [cwd (System/getProperty "user.dir")
        parent (.getParent (java.io.File. ^String cwd))]
    (cond
      (.endsWith cwd "cg-app") (str parent "/cg")
      (.endsWith cwd "cg") cwd
      (.exists (java.io.File. ^String (str cwd "/cg"))) (str cwd "/cg")
      :else cwd)))

(def ^:private builtin-ns-prefixes
  "Prefixes of the namespaces whose vars are not user vars. cg.session holds
   the results of the last run."
  (into ["clojure." "sci." "nrepl." "cg.session"]
        (map str (keys dsl/cg-namespaces))))

(def ^:private session-ttl-ms
  "Sessions expire after 30 minutes of inactivity, except a session with a
   REPL (see session-repl)."
  (* 30 60 1000))

(defn- now-ms [] (System/currentTimeMillis))

(defn- file-under
  "The absolute path of `path` under the directory `dir` when that file
   exists, else nil."
  [dir path]
  (when dir
    (let [f (java.io.File. ^String dir ^String path)]
      (when (.exists f)
        (.getAbsolutePath f)))))

(defn- session-input
  "What the session `sid` reads for `path`: an absolute path as it is, else the
   file under the :root of the session, else the file under the CG project
   root, else the classpath resource of the library as cg.run/module-text
   finds it, else `path`."
  [sid path]
  (let [f (java.io.File. ^String path)]
    (if (.isAbsolute f)
      path
      ;; The root is read at each call, because a later request can change it.
      (or (file-under (get-in @sessions [sid :root]) path)
          (file-under cg-project-root path)
          (when-not (.exists f)
            (io/resource (str/replace-first path #"^src/" "")))
          path))))

(defn- set-session-root!
  "The last root wins, so a later REPL eval resolves paths like the last run."
  [sid root]
  (when root
    (swap! sessions #(cond-> % (contains? % sid) (assoc-in [sid :root] root)))))

(defn- new-session
  "Overrides load-module and slurp to read paths as session-input does."
  [sid]
  (let [ctx (dsl/create-cg-context)
        env (:env ctx)
        ;; path -> content hash. Record the path before the eval, or two
        ;; modules that load each other recurse to a StackOverflowError.
        ;; Remove a module that throws, because the next load must try again.
        loaded (atom {})]
    (swap! env assoc-in [:namespaces 'cg.run 'load-module]
           (fn [path]
             (let [input (session-input sid path)
                   resolved (str input)
                   content (slurp input)
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
             (apply slurp (session-input sid path) opts)))
    {:sci-ctx ctx
     :last-access (now-ms)}))

(defn- touch-session!
  [sid]
  (swap! sessions update sid assoc :last-access (now-ms)))

(defn- expire-sessions!
  []
  (let [cutoff (- (now-ms) session-ttl-ms)]
    (swap! sessions
           (fn [m]
             (into {} (filter (fn [[_ s]] (or (:repl s) (> (:last-access s) cutoff))) m))))))

(defn- get-session-id [request]
  (get-in request [:headers "x-session-id"]))

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

(defn- dataset-crs-str
  "The CRS of `dataset` as a string, EPSG:4326 when it has none."
  [dataset]
  (let [crs (get-dataset-crs dataset)]
    (cond
      (string? crs) crs
      (map? crs) (get-in crs [:properties :name])
      :else "EPSG:4326")))

(defn- dataset-in-4326
  "`dataset` with each geometry in EPSG:4326, from the CRS `crs-str`."
  [dataset crs-str]
  (if (crs-4326? crs-str)
    dataset
    (ds/->dataset (mapv (fn [row]
                          (if-let [geom (:geometry row)]
                            (assoc row :geometry (geo/transform-geom geom crs-str "EPSG:4326"))
                            row))
                        (ds/rows dataset :as-maps)))))

(defn- builtin-ns? [ns-str]
  (some #(.startsWith ^String ns-str %) builtin-ns-prefixes))

(defn- deref-if-var
  "A namespace map of the SCI env holds each def as a Var."
  [v]
  (if (instance? sci.lang.Var v) @v v))

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
                          (filter (fn [[k _]] (and (symbol? k) (not= k 'require))))
                          (keep (fn [[k v]]
                                  (let [val (deref-if-var v)]
                                    (when-not (or (= val :sci.impl.vars/unrealized)
                                                  (nil? val))
                                      (assoc (introspect-var (str k) val)
                                             :ns (str ns-sym)
                                             :qualified (str ns-sym "/" k))))))))))
         (into []))))

(defn- lookup-var-value [sci-ctx var-name]
  (let [env @(:env sci-ctx)]
    (or (when-let [v (get-in env [:namespaces 'user (symbol var-name)])]
          (deref-if-var v))
        (when-let [[_ ns-str name-str] (re-matches #"(.+)/(.+)" var-name)]
          (when-let [v (get-in env [:namespaces (symbol ns-str) (symbol name-str)])]
            (deref-if-var v)))
        ;; A module file opened as the session entry defines its vars in its
        ;; own namespace, but the client fetches by the unqualified name from
        ;; /api/session/vars.
        (some (fn [[ns-sym ns-map]]
                (when (and (symbol? ns-sym) (not (builtin-ns? (str ns-sym))))
                  (when-let [v (get ns-map (symbol var-name))]
                    (deref-if-var v))))
              (get env :namespaces)))))

(defn health [_request]
  {:status 200
   :body {:status "ok"}})

(defn- result-count [results]
  (cond
    (ds/dataset? results) (ds/row-count results)
    (sequential? results) (count results)
    (some? results) 1
    :else 0))

(defn- layer-sink
  "The map-layer result in `r`, which the JVM collects in a vector, or nil."
  [r]
  (let [sink? (fn [x] (and (map? x) (:layer x)))]
    (cond
      (sink? r) r
      (and (vector? r) (= 1 (count r)) (sink? (first r))) (first r))))

(defn- wire-result
  "A run result as JSON data for the map: a map-layer result as it is, a
   dataset as a plain map-layer in EPSG:4326, else nil, because JSON cannot
   hold a JTS geometry."
  [model r]
  (cond
    (layer-sink r) r
    (ds/dataset? r) (ds/map-layer (dataset-in-4326 r (dataset-crs-str r)) {:name model})
    :else nil))

(defn- kept-value
  "The value that a session keeps of a run result: the dataset that went into
   map-layer, else the result."
  [r]
  (or (some-> (layer-sink r) meta :dataset) r))

(defn- keep-results!
  "Interns cg.session/results in `ctx`: model name -> kept-value, for the
   model of the run and each map-layer model that did not fail."
  [ctx {:keys [model result layers]}]
  (sci/intern ctx (sci/create-ns 'cg.session) 'results
              (into {model (kept-value result)}
                    (keep (fn [l] (when-not (contains? l :error)
                                    [(:model l) (kept-value (:result l))])))
                    layers)))

(defn- run-in-session
  "Gives {:count :name :results :layers}, the shape that the map hosts read."
  [sess body]
  (let [ctx (:sci-ctx sess)
        filename (:filename body)
        opts (cond-> (assoc (or (:opts body) {}) :map-layers true)
               filename (assoc :file filename))
        out (binding [run/*cg-context* ctx
                      run/*session-file* (some-> filename (str/split #"/") last)]
              (run/run-file (:code (metadata/split-code-and-metadata (:code body))) opts))]
    (keep-results! ctx out)
    {:count (result-count (:result out))
     :name (:model out)
     :results (wire-result (:model out) (:result out))
     :layers (mapv (fn [l] (if (contains? l :error)
                             l
                             {:model (:model l) :result (wire-result (:model l) (:result l))}))
                   (:layers out))}))

(defonce executions
  ;; id -> {:id :chan :status (:running, :complete or :failed) :result
  ;;        :ended-at}. Public, because tests seed a live entry for the 409 path.
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
  "The sleep before the untap lets in-flight events arrive."
  [id updates sentinel]
  (let [{:keys [chan]} (get @executions id)]
    (swap! executions update id merge updates {:ended-at (now-ms)})
    (Thread/sleep 100)
    (trace/untap-trace chan)
    (async/put! chan (merge {:kind "execution-ended" :execution-id id} sentinel))
    (async/close! chan)))

(defn- trace-writer
  "A Writer that sends each line written to it as an \"output\" trace event
   of `stream`. The trace is the stream of an execution to its client, and
   the output of the extension's backend process goes nowhere."
  ^java.io.Writer [stream]
  (let [buf (StringBuilder.)
        emit! (fn [^String text] (trace/emit! "output" {:stream stream :text text}))
        lines! (fn []
                 (loop []
                   (let [i (.indexOf buf "\n")]
                     (when (>= i 0)
                       (emit! (.substring buf 0 (inc i)))
                       (.delete buf 0 (inc i))
                       (recur)))))]
    (proxy [java.io.Writer] []
      (write
        ([x]
         (cond
           (string? x)  (.append buf ^String x)
           (integer? x) (.append buf (char (int x)))
           :else        (.append buf ^chars x))
         (lines!))
        ([x off len]
         (if (string? x)
           (.append buf ^String x (int off) (+ (int off) (int len)))
           (.append buf ^chars x (int off) (int len)))
         (lines!)))
      (flush [] (lines!))
      (close []
        (when (pos? (.length buf))
          (emit! (str buf))
          (.setLength buf 0))))))

(defn- with-run-output
  "(f) with its printed text sent as output trace events. The REPL printers
   of a session write to sci/out, therefore both bindings."
  [f]
  (let [out (trace-writer "out")
        err (trace-writer "err")]
    (try
      (binding [*out* out *err* err]
        (sci/binding [sci/out out sci/err err]
          (f)))
      (finally
        (.close out)
        (.close err)))))

(defn- run-execution! [id body sess]
  (try
    (log/info "Async execution" id "starting")
    (let [summary (with-run-output #(run-in-session sess body))]
      (log/info "Async execution" id "complete," (:count summary) "results")
      (finish-execution! id
                         {:status :complete :result summary}
                         {:outcome "success" :count (:count summary)}))
    (catch Throwable e
      (log/error e "Async execution failed" id)
      (finish-execution! id
                         {:status :failed
                          :result {:error (.getMessage e)}}
                         {:outcome "error" :message (.getMessage e)}))))

(defn execution-start
  "POST /api/execution/start: runs the `code` of the body in the session of
   X-Session-Id (see run-in-session), in a future, and returns
   {:execution-id id}. A request with no session ID or no `code` gets 400, and
   an unknown session gets 404. A `root` in the body becomes the :root of the
   session (see session-input). Only one execution streams at a time, and a
   second gets 409."
  [request]
  (try
    (prune-executions!)
    (let [body (:body request)
          sid (get-session-id request)
          sess (get-session! request)
          _ (when-not (:code body)
              (throw (ex-info "Provide 'code'" {:status 400})))
          id (str (java.util.UUID/randomUUID))
          ;; The mult waits for each tap. A full fixed buffer with no reader
          ;; blocks the global trace stream for the whole process. A dropped
          ;; event shows as a gap in :seq.
          ch (async/chan (async/sliding-buffer 4096))
          entry {:id id :chan ch :status :running}
          [_ new] (swap-vals! executions
                              (fn [m] (if (live-execution m) m (assoc m id entry))))]
      (if-not (contains? new id)
        (do (async/close! ch)
            {:status 409
             :body {:error "Another execution is already streaming; trace events are global, so executions serialize"
                    :live-execution-id (:id (live-execution new))}})
        (do (set-session-root! sid (:root body))
            (trace/tap-trace ch)
            (future (run-execution! id body sess))
            {:status 200
             :body {:execution-id id}})))
    (catch Exception e
      (log/error e "Failed to start async execution")
      {:status (or (:status (ex-data e)) 500)
       :body {:error (.getMessage e)}})))

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
   (the map of run-in-session on success, {:error} on failure)."
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

(defn session-create [_request]
  (expire-sessions!)
  (let [sid (str (java.util.UUID/randomUUID))]
    (swap! sessions assoc sid (new-session sid))
    (log/info "Created session" sid)
    {:status 200
     :body {:session-id sid}}))

(defn- sci-out-printer
  "`f` with *out* bound to the current sci/out, which an nREPL eval binds to
   its client. The cg context prints with the JVM fns, which write to *out*."
  [f]
  (fn [& xs] (binding [*out* @sci/out] (apply f xs))))

(defn- start-repl-server!
  "Starts an nREPL server on the SCI context `ctx`, on 127.0.0.1 and a port
   that the OS assigns. Gives {:server :port}."
  [ctx]
  (swap! (:env ctx) update-in [:namespaces 'clojure.core] merge
         (update-vals {'println println 'print print 'prn prn 'pr pr} sci-out-printer))
  (let [server (nrepl-server/start-server! ctx {:host "127.0.0.1" :port 0 :quiet true})]
    {:server server
     :port (.getLocalPort ^java.net.ServerSocket (:socket server))}))

(defonce ^:private repl-lock (Object.))

(defn session-repl
  "POST /api/session/repl: starts an nREPL server on the context of the
   session, one time for each session, and gives {:port}. The REPL and the
   runs of the session share their vars."
  [request]
  (try
    (let [sess (get-session! request)
          sid (get-session-id request)
          repl (locking repl-lock
                 (or (get-in @sessions [sid :repl])
                     (let [r (start-repl-server! (:sci-ctx sess))]
                       (swap! sessions assoc-in [sid :repl] r)
                       r)))]
      (log/info "REPL of session" sid "on port" (:port repl))
      {:status 200
       :body {:port (:port repl)}})
    (catch Exception e
      (log/error e "Failed to start the session REPL")
      {:status (or (:status (ex-data e)) 500)
       :body {:error (.getMessage e)}})))

(defn session-delete
  "DELETE /api/session/delete: stops the nREPL server of the session and
   removes the session."
  [request]
  (let [sid (get-session-id request)]
    (if (and sid (get @sessions sid))
      (do (some-> (get-in @sessions [sid :repl :server]) nrepl-server/stop-server!)
          (swap! sessions dissoc sid)
          (log/info "Deleted session" sid)
          {:status 200
           :body {:status "ok"}})
      {:status 404
       :body {:error "Session not found"}})))

(defn session-load
  "POST /api/session/load: evaluates .cg code in the session. The body has
   `code`, an optional `filename` and an optional `root` for the session (see
   session-input)."
  [request]
  (try
    (let [sess (get-session! request)
          sid (get-session-id request)
          {:keys [code filename root]} (:body request)
          _ (set-session-root! sid root)
          _ (when-not code
              (throw (ex-info "Provide 'code'" {:status 400})))
          label (or filename "<eval>")
          _ (log/info "Loading" label "in session" sid)
          ctx (:sci-ctx sess)
          ;; A module file uses cg.run/session-file to tell whether it is the
          ;; opened file or a load-module dependency.
          _ (binding [run/*session-file* (last (str/split label #"/"))]
              (run/eval-cg* ctx (str "(do " code ")")))
          vars (user-vars ctx)]
      (log/info "Loaded" label "- found" (count vars) "user vars")
      {:status 200
       :body {:status "ok"
              :vars vars}})
    (catch Exception e
      (log/error e "Failed to load .cg code")
      {:status (or (:status (ex-data e)) 500)
       :body {:error (.getMessage e)}})))

(defn session-vars [request]
  (try
    (let [sess (get-session! request)
          vars (user-vars (:sci-ctx sess))]
      {:status 200
       :body {:vars vars}})
    (catch Exception e
      {:status (or (:status (ex-data e)) 500)
       :body {:error (.getMessage e)}})))

(defn data-var
  "POST /api/data/:var: the dataset of the var as GeoJSON in EPSG:4326, at
   most `limit` rows (5000 when the body has no limit). An optional `bbox` in
   EPSG:4326 keeps the rows that intersect it."
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
      (let [crs-str (dataset-crs-str val)
            needs-reproject? (not (crs-4326? crs-str))
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
            served (dataset-in-4326 limited crs-str)
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
