;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.run
  "Flow-based execution runtime for CG models. `run-model` runs a model, and
  `eval-cg` and `load-module` evaluate .cg code with SCI (JVM) or squint (JS)."
  (:require [cg.model :as model]
            [cg.feature :as feature]
            [cg.util :as util]
            [cg.dataset :as dataset]
            ;; Registers the reducers that resolve-reducer looks up.
            #?(:clj [cg.dataset.agg])
            [clojure.string :as str]
            #?(:clj [clojure.core.async :as async])
            #?(:clj [clojure.core.async.flow :as flow])
            #?(:clj [sci.core :as sci])
            #?(:clj [net.willcohen.native.workload-pool :as wp])
            #?(:clj [net.willcohen.proj.handler :as proj-handler])
            #?(:clj [cg.pool :as cg-pool-reg])
            [cg.run.fusion :as fusion]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs ["cljc-flow/flow" :as flow])
            #?(:cljs ["cljc-flow/core" :as ca])
            #?(:cljs ["ffi-wasm" :as wp])
            #?(:cljs ["proj-wasm" :as proj])
            #?(:cljs ["gdal-wasm" :as gdal])
            #?(:cljs [cg.pool :as cg-pool-reg])
            #?(:cljs [cg.geo :as geo])
            ;; Reached through js* (js-math), which clj-kondo does not see.
            #?(:cljs #_{:clj-kondo/ignore [:unused-namespace]} [cg.math :as math])
            #?(:cljs ["squint-cljs" :as squint-compiler])
            #?(:cljs [cg.macros :as macros])
            #?(:cljs [cg.io :as io])
            #?(:cljs [cg.parser.core :as parser])
            #?(:clj [cg.trace :as trace]
               :cljs [cg.trace-js :as trace])))

#?(:clj (set! *warn-on-reflection* true))

;; One worker-router pool (CLJS only) hosts the proj, wasmts and gdal handlers.
;; Each worker answers all three RPC types. Because of this, a pipeline that
;; uses more than one library runs on one worker, and no payload crosses a
;; worker boundary. The proj handler tags each PJ object with `worker_idx`,
;; and later calls on that object go to the same worker.

#?(:cljs
   (defn cg-pool
     "Return the live joint pool, or nil if init-cg-workers! did not run."
     []
     (cg-pool-reg/current-pool)))

#?(:cljs
   (defn- resolve-wasmts-handler-url
     "The URL of cg-wasmts-handler.mjs next to the bundle dist/cg.mjs."
     []
     (str (js/URL. "./cg-wasmts-handler.mjs" js/import.meta.url))))

#?(:cljs
   (defn- resolve-gdal-handler-url
     "The URL of the `gdal-wasm/gdal-handler` module, or the bare package
      subpath when the bundler has no import.meta.resolve."
     []
     (let [im   js/import.meta
           rslv (.-resolve im)]
       (if rslv
         (.call rslv im "gdal-wasm/gdal-handler")
         "gdal-wasm/gdal-handler"))))

#?(:cljs
   (defn ^:async init-cg-workers!
     "Start the joint proj/wasmts/gdal worker pool and give it, or the existing pool.
      `opts`: :wasmts-js-url (required), :size, :wasmts-wasm-binary, :proj-init,
      :gdal-init, :wasmts-handler-url (required from unbundled src/cg/run.mjs)."
     ([] (init-cg-workers! {}))
     ([opts]
      ;; A shared process (a test suite with --test-isolation=none) calls this
      ;; more than one time. terminate-cg-workers! permits a new pool.
      (or (cg-pool-reg/current-pool)
          (let [size            (or (:size opts)
                                    (when (exists? js/navigator)
                                      (.-hardwareConcurrency js/navigator))
                                    4)
                wasmts-js-url   (:wasmts-js-url opts)
                wasmts-wasm-bin (:wasmts-wasm-binary opts)]
            (when-not wasmts-js-url
              (throw (js/Error. "init-cg-workers!: :wasmts-js-url is required")))
            (let [registry           (wp/init-workload-pool! {:size size})
                  wasmts-handler-url (or (:wasmts-handler-url opts)
                                         (resolve-wasmts-handler-url))
                  proj-init-args     (await (proj/handler-default-init-args {}))
                  gdal-init          (:gdal-init opts)]
              (wp/register-handler! registry :compute "net.willcohen.proj"
                                    (proj/handler-spec proj-init-args))
              (wp/register-handler! registry :compute "net.willcohen.wasmts"
                                    {:module wasmts-handler-url
                                     :args #js {:wasmtsJsUrl wasmts-js-url
                                                :wasmtsWasmBinary wasmts-wasm-bin}})
              ;; Register the gdal handler only with :gdal-init. Its init throws
              ;; when dbBytes is absent, and the workers then hang.
              (when gdal-init
                (wp/register-handler! registry :compute "net.willcohen.gdal"
                                      {:module (resolve-gdal-handler-url)
                                       :args (clj->js gdal-init)}))
              (let [the-pool (await (wp/ensure-pool! registry))]
                ;; With :pool, proj/init! adopts this pool and does not start
                ;; its own.
                (await (proj/init! (clj->js (merge {:pool the-pool}
                                                   (:proj-init opts)))))
                (when gdal-init
                  (await (gdal/init! (clj->js (merge {:pool the-pool}
                                                     gdal-init)))))
                (cg-pool-reg/set-registry! registry)
                the-pool)))))))

#?(:cljs
   (defn ^:async terminate-cg-workers!
     "Run the :pre-terminate hook of each handler, then terminate the joint pool.
      Gives :terminated or :no-pool."
     []
     (if-let [registry (cg-pool-reg/current-registry)]
       (do (await (wp/shutdown-pool! registry))
           (cg-pool-reg/clear-registry!)
           :terminated)
       :no-pool)))

;; Each thread of the JVM compute pool runs net.willcohen.proj.handler/init one
;; time, which makes a PROJ Context for that thread. create-flow gets the pool
;; as :compute-exec, because transform-coords-step (:workload :compute) must
;; run on a thread that has a Context.

#?(:clj
   (defn init-cg-workers!
     "Start the JVM workload pool with the :proj handler and give its handle
      {:registry :compute-exec}. Throws when a pool exists. `opts`: :size."
     ([] (init-cg-workers! {}))
     ([opts]
      (when (cg-pool-reg/current-jvm-pool)
        (throw (ex-info "init-cg-workers!: pool already initialized; call terminate-cg-workers! first" {})))
      (let [registry (wp/init-workload-pool! opts)
            _ (wp/register-handler! registry :compute :proj (proj-handler/spec))
            compute-exec (wp/as-executor-service registry :compute)
            handle {:registry registry :compute-exec compute-exec}]
        (cg-pool-reg/set-jvm-pool! handle)
        handle))))

#?(:clj
   (defn ensure-cg-workers!
     "As init-cg-workers!, but gives the handle that exists when the pool is
      up."
     ([] (ensure-cg-workers! {}))
     ([opts]
      (or (cg-pool-reg/current-jvm-pool)
          (init-cg-workers! opts)))))

#?(:clj
   (defn terminate-cg-workers!
     "Shut the JVM workload pool down. Gives :terminated or :no-pool."
     []
     (if-let [handle (cg-pool-reg/current-jvm-pool)]
       (do (wp/shutdown-pool! (:registry handle))
           (cg-pool-reg/clear-jvm-pool!)
           :terminated)
       :no-pool)))

(def ^:private namespace-registry
  "Known namespaces for operation resolution, keyed by namespace name string
  on JS. Empty on the JVM, which uses requiring-resolve."
  ;; On cljs a squint :as alias of a module is the module object itself.
  (atom #?(:clj {}
           :cljs #_{:clj-kondo/ignore [:unresolved-var]}
           {"cg.geo" geo
            "cg.feature" feature
            "cg.dataset" dataset})))

(defn register-namespace!
  "Register the namespace object `ns-obj` for operation resolution under
  `ns-sym` (a string key on JS)."
  [ns-sym ns-obj]
  (swap! namespace-registry assoc
         #?(:clj ns-sym :cljs (str ns-sym))
         ns-obj))

;; The bare-op namespace map and the fn cache live in cg.feature (a leaf ns).
;; cg.macros and cg.geo must reach them, and a require of cg.run from there
;; makes a cycle (geo -> run -> dataset -> geo).

(defn register-bare-op!
  "Record that the bare op name `op-name` lives in `ns-name`. With `fn-value`,
   also store the fn, which an op that a .cg file defines needs."
  ([op-name ns-name]
   (feature/register-bare-op-namespace! op-name ns-name))
  ([op-name ns-name fn-value]
   (feature/register-bare-op-namespace! op-name ns-name)
   (feature/register-bare-op-fn! ns-name op-name fn-value)))

;; An operation is a defn whose name has the metadata ^:op. Its decl has
;; the shape of the parser's OperationDecl. Because of this, a test can
;; compare the decl that the JVM reads from var metadata with the decl that
;; the parser reads from the text.

(defn- check-role-params!
  [{op-name :name inputs :inputs}]
  (let [roled (keep-indexed (fn [i in] (when (:role in) i)) inputs)]
    (when (next roled)
      (throw (ex-info (str "op '" op-name "' has more than one role param")
                      {:op op-name})))
    (when (and (seq roled) (not= 0 (first roled)))
      (throw (ex-info (str "op '" op-name "': a role param must be the first param")
                      {:op op-name})))))

(defn op-decl->registration
  "The register-operation! map of an op decl. The schema has each param except
   the :row param, and an option map param has :option-map true."
  [{:keys [inputs row-reads row-writes]}]
  {:type :transformer
   :row-shape (if (some #(= :row (:role %)) inputs) :per-row :whole-dataset)
   :reads (into #{} (map keyword) row-reads)
   :writes (into #{} (map keyword) row-writes)
   :params (into []
                 (comp (remove #(= :row (:role %)))
                       (map (fn [{:keys [name type role options]}]
                              (cond-> {:name (keyword name)}
                                (and type (not= "any" type)) (assoc :type (keyword type))
                                role (assoc :role role)
                                options (assoc :option-map true)))))
                 inputs)})

(defn- checked-row-op
  "The fn `f` of the row op `op-name`, with a check that each result is a
   value and not a Model."
  [op-name f]
  (fn [& args]
    (let [r (apply f args)]
      ;; model? reads only :nodes and :edges, and a row can have :edges.
      ;; create-model gives each Model a :config too.
      (when (and (model/model? r) (some? (get r :config)))
        (throw (ex-info (str "op '" op-name "' takes a ^:row param and returns a model. "
                             "A row op must return a value. Use a ^:table op to return a model.")
                        {:op op-name})))
      r)))

(defn register-op!
  "Register `f`, the fn of the op `decl`, as a flow op under its bare name and
   under `ns-name`/name, with the param schema of the decl."
  [ns-name decl f]
  (check-role-params! decl)
  (register-bare-op! (:name decl) ns-name
                     (if (= :row (:role (first (:inputs decl))))
                       (checked-row-op (:name decl) f)
                       f))
  (feature/register-operation! (str ns-name "/" (:name decl))
                               (op-decl->registration decl)))

(defn- qualify-op-name
  "The fully qualified name of a namespaced op name. A name with a registered fn
   stays as written, and any other name outside cg. gets the cg. prefix."
  [s]
  (if (or (str/starts-with? s "cg.") (feature/get-bare-op-fn s))
    s
    (str "cg." s)))

(defn resolve-operation
  "Resolve an op reference (a fn, symbol, keyword or string) to its fn. A short
  name gets the `cg.` prefix; a bare string uses its registered namespace."
  [op-ref]
  (if (fn? op-ref)
    op-ref
    (let [qualified-name (cond
                           #?@(:clj
                               [(symbol? op-ref) (str op-ref)

                                (keyword? op-ref)
                                (let [s (str op-ref)]
                                  (qualify-op-name (if (= \: (first s)) (subs s 1) s)))])

                           (string? op-ref)
                           (if (str/includes? op-ref "/")
                             (qualify-op-name op-ref)
                             (str (or (feature/get-bare-op-namespace op-ref)
                                      "cg.geo")
                                  "/" op-ref))

                           :else (throw (ex-info "Operation must be symbol, keyword, string, or function"
                                                 {:op-ref op-ref :type (type op-ref)})))]
      #?(:clj
         (or (feature/get-bare-op-fn qualified-name)
             (requiring-resolve (symbol qualified-name)))

         :cljs
         (or
          (feature/get-bare-op-fn qualified-name)
          (let [[ns-name fn-name] (str/split qualified-name #"/")
                ns-obj (get @namespace-registry ns-name)]
            (when-not ns-obj
              (throw (ex-info (str "Namespace not registered: " ns-name
                                   ". Call (register-namespace! \"" ns-name "\" <ns-obj>) first.")
                              {:namespace ns-name
                               :registered (keys @namespace-registry)})))
            (let [fn-obj (aget ns-obj (util/munge-name fn-name))]
              (when-not fn-obj
                (throw (ex-info (str "Function not found: " qualified-name)
                                {:qualified-name qualified-name
                                 :namespace ns-name
                                 :function fn-name})))
              fn-obj)))))))

(defn- component-qualified-name
  "The registered \"<ns>/<name>\" key of a component reference, or nil when a
   bare name has no registered namespace."
  [component]
  (let [s (cond
            (string? component)  component
            (keyword? component) (name component)
            :else                (str component))]
    (if (str/includes? s "/")
      s
      (when-let [ns-name (feature/get-bare-op-namespace s)]
        (str ns-name "/" s)))))

(declare run-model)

(defn run-component
  "Run the :role :table op `component` one time over a whole table. `bindings`
   maps input keywords to values, or is a bare table for the first input.
   Gives the result, or runs a body Model result with `opts` (run-model opts)."
  [component bindings opts]
  (let [qn   (component-qualified-name component)
        info (when qn (feature/get-operation-info qn))
        f    (resolve-operation component)]
    (when-not f
      (throw (ex-info (str "run-component: cannot resolve component " (pr-str component))
                      {:component component :qualified-name qn})))
    (let [param-names (mapv :name (:params info))
          ;; Test dataset? before map?, because a dataset is map-like on the JVM.
          bmap        (cond
                        (dataset/dataset? bindings) {(first param-names) bindings}
                        (map? bindings)             bindings
                        :else                       {(first param-names) bindings})
          args        (mapv (fn [pn] (get bmap pn)) param-names)
          r           (apply f args)]
      (if (model/model? r)
        (run-model r (zipmap param-names args)
                   (merge {:name (str (util/unq-name (or qn component)) "-body")} opts))
        r))))

;; cg.run registers the fn for (into {}) here, because cg.dataset cannot
;; require cg.run (a cycle).
#?(:clj (feature/register-bare-op-fn! "cg.dataset" "into" dataset/into-map))

#?(:clj
   (defn- run-component-groups
     "Run `component` over each [group-key sub-dataset] of `groups` and give
      {group-key -> result}. Emits map-groups-* and map-group-* trace events
      with `pid`."
     [component groups pid run-opts]
     (let [groups (vec groups)]
       (trace/emit! "map-groups-start"
                    {:pid pid :component (str component)
                     :group-count (count groups)
                     :group-keys (mapv (comp str first) groups)})
       ;; A future, not the compute ExecutorService: a group task blocks on
       ;; the row tasks of its inner flow, and group tasks on the fixed pool
       ;; can take every worker (a deadlock). trace/emit! stamps :worker-id
       ;; from the thread, and the group events come from the future.
       (let [futs (mapv (fn [[k sub-ds]]
                          (future
                            (let [t0 (System/currentTimeMillis)
                                  rc (try (dataset/row-count sub-ds)
                                          (catch Throwable _ nil))]
                              (trace/emit! "map-group-start"
                                           {:pid pid :group-key (str k) :row-count rc})
                              (try
                                (let [result (run-component component sub-ds run-opts)]
                                  (trace/emit! "map-group-complete"
                                               {:pid pid :group-key (str k)
                                                :row-count rc
                                                :ms (- (System/currentTimeMillis) t0)})
                                  [k result])
                                (catch Throwable e
                                  (trace/emit! "map-group-error"
                                               {:pid pid :group-key (str k)
                                                :error (.getMessage e)
                                                :ms (- (System/currentTimeMillis) t0)})
                                  (throw e))))))
                        groups)
             result (into {} (mapv deref futs))]
         (trace/emit! "map-groups-complete" {:pid pid :group-count (count groups)})
         result))))

;; when-row lives in cg.run and not in cg.dataset, because the resolution of
;; its body fn from cg.dataset makes a cycle (cg.dataset -> cg.run). The
;; predicate applies to the whole input dataset, not to each row.

(defn- evaluate-when-row-predicate
  "Evaluate the when-row `predicate` against `dset`. Only \"has-column?\" is
   implemented; any other kind throws."
  ;; The param is `dset`, not `dataset`. Under squint a param named `dataset`
  ;; shadows the cg.dataset alias: (dataset/has-column? ...) then reads a
  ;; property of the dataset object, which is undefined, and the flow step
  ;; fails silently ("Flow execution timeout"). The JVM is not affected.
  [predicate dset]
  (let [kind (:kind predicate)
        arg (:arg predicate)]
    (case kind
      "has-column?"
      (dataset/has-column? dset arg)

      (throw (ex-info (str "when-row predicate '" kind
                           "' is not yet implemented at runtime")
                      {:predicate predicate
                       :predicate-kinds-implemented #{"has-column?"}})))))

(defn- op-schema
  "The registered :params schema of the op that `node` names."
  [node]
  (:params (feature/get-operation-info (fusion/node->op-key node))))

(defn- param-values
  "The values of the params map `params` as positional args: the names in
   `schema` in schema order, then other keys in map order."
  [params schema]
  (let [names (filterv #(contains? params %) (map :name schema))
        named (set names)]
    (into (mapv #(get params %) names)
          (for [[k v] params :when (not (contains? named k))] v))))

(defn- when-row-apply-body
  "Apply the parsed body op `body`, {:fn <qualified-name> :params ...}, to
   `dataset`. Throws for a body with no :fn (a nested pipeline or when-row)."
  [body dataset]
  (let [fn-ref (:fn body)]
    (when (nil? fn-ref)
      (throw (ex-info "when-row body lacks :fn; nested pipeline/when-row bodies are not yet runnable"
                      {:body body})))
    (let [op-fn (resolve-operation fn-ref)
          params (:params body)]
      (cond
        (map? params) (apply op-fn dataset (param-values params (op-schema body)))
        (vector? params) (apply op-fn dataset params)
        (some? params) (op-fn dataset params)
        :else (op-fn dataset)))))

(defn when-row
  "Apply `body` to the dataset when the predicate holds. Otherwise give the
   dataset (on-miss \"pass-through\", the default) or an empty dataset (\"drop\").
   `args` is the map that the when-row hook of cg.macros emits."
  ;; `dset`, not `dataset`: see evaluate-when-row-predicate.
  [dset args]
  (let [{:keys [predicate options body]} args
        on-miss (get options "on-miss" "pass-through")
        passes? (evaluate-when-row-predicate predicate dset)]
    (cond
      passes?
      (when-row-apply-body body dset)

      (= on-miss "drop")
      (dataset/select-rows dset [])

      :else
      dset)))

;; when-row is a whole-dataset op. Without :row-shape, operation->step sends a
;; when-row node down the per-geometry path, which fails ("Geometry column not
;; found").
(feature/register-operation! "cg.run/when-row"
                             {:type :transformer
                              :row-shape :whole-dataset})

;; aggregate lives in cg.run for the same reason as when-row: its :op needs
;; resolve-operation.

(defn- resolve-reducer
  "The reducer fn of the config :op keyword `op-kw`. Throws when the key
   names no registered :role :reducer op."
  [op-kw]
  (let [fqn (str "cg.dataset.agg/" (name op-kw))]
    (when-not (feature/reducer? fqn)
      (throw (ex-info (str "aggregate :op " op-kw " does not resolve to a "
                           "registered reducer (looked up " fqn ")")
                      {:op op-kw :fqn fqn})))
    (resolve-operation fqn)))

(defn aggregate
  "Aggregate `grouped`, a {group-value -> sub-dataset} map. `config` maps each
  output column to {:op <reducer-kw> :over <col>?}. Gives one row per group,
  with the column :group and one column per config key."
  [grouped config]
  (when-not (dataset/grouped? grouped)
    (throw (ex-info "aggregate input must be partition-by-shaped (grouped? false)"
                    {:input grouped})))
  (let [group-keys (keys grouped)
        result-rows
        (mapv (fn [gk]
                (let [sub-ds (get grouped gk)]
                  (into {:group gk}
                        (map (fn [[output-col {:keys [op over]}]]
                               (let [reducer (resolve-reducer op)
                                     v (if (some? over)
                                         (reducer sub-ds over)
                                         (reducer sub-ds))]
                                 [output-col v]))
                             config))))
              group-keys)]
    (dataset/->dataset
     (reduce (fn [acc row]
               (reduce-kv (fn [m k v]
                            (update m k (fnil conj []) v))
                          acc row))
             {} result-rows))))

(defn- apply-params
  "Call `op-fn` with `item` (omitted when nil) and the params: the values of a
  map in `schema` order, the elements of a vector, or any other value as one arg."
  [op-fn item params schema]
  (if (nil? item)
    (cond
      (map? params)
      (apply op-fn (param-values params schema))

      (vector? params)
      (apply op-fn params)

      (some? params)
      (op-fn params)

      :else
      (op-fn))
    (cond
      (map? params)
      (apply op-fn item (param-values params schema))

      (vector? params)
      (apply op-fn item params)

      (some? params)
      (op-fn item params)

      :else
      (op-fn item))))

(defn source-operation->step
  "A step fn for a source operation: on its dummy :trigger input it emits the
  result one time. `schema` sets the order of the values in a params map."
  [op-fn params schema]
  (fn
    ([] {:params params
         :ins {:trigger "Trigger"}
         :outs {:out "Geometry"}})

    ([_args] {:emitted false})

    ([state _transition] state)

    ([state _in-name _msg]
     (if (:emitted state)
       [state {}]
       (let [result (apply-params op-fn nil params schema)]
         [(assoc state :emitted true) {:out [result]}])))))

(declare ^:private wire-pid)

(defn collection-operation->step
  "A step fn that collects :expected-count items, then applies the collection
  operation one time and emits the result. `schema` orders params map values."
  [op-fn params schema]
  (fn
    ([] {:params (assoc params :expected-count 0) ; Will be set during flow construction
         :ins {:in "Items"}
         :outs {:out "Result"}})

    ([args]
     (let [expected (:expected-count args 0)]
       {:collected []
        :expected expected}))

    ([state _transition]
     state)

    ([state _in-name msg]
     (let [new-collected (conj (:collected state) msg)
           expected (:expected state)]
       (if (= (count new-collected) expected)
         (let [result (apply-params op-fn new-collected params schema)]
           [(assoc state :collected new-collected) {:out [result]}])
         [(assoc state :collected new-collected) {}])))))

#?(:cljs
   (defn- thenable? [x]
     (and (some? x) (fn? (.-then x)))))

#?(:cljs
   (defn- lift1-async->step
     "As flow/lift1->step, for an op that can give a Promise; the step then
      gives a Promise of [state outputs]."
     [f]
     (fn
       ([] {:ins {:in (str "the argument to " f)}
            :outs {:out (str "the return of " f)}})
       ([_arg-map] nil)
       ([_state _transition] nil)
       ([_state _input msg]
        (let [emit (fn [m] [nil (when (some? m) {:out [m]})])
              r (f msg)]
          (if (thenable? r)
            (.then r emit)
            (emit r)))))))

#?(:cljs
   (defn- ^:async run-rows
     "Apply `row-fn` to each row, with rows-progress at about each 5 percent
      and a turn of the event loop each 100 ms, for timers and a repaint."
     [rows row-fn pid]
     (let [total (count rows)
           step  (max 1 (min 500 (max 50 (quot total 20))))
           out   (js/Array. total)]
       (loop [i 0 turn-start (js/Date.now)]
         (if (< i total)
           (let [row (nth rows i)
                 r   (fusion/apply-row-chain row [row-fn])
                 r   (if (thenable? r)
                       (await (.catch r (fn [t] (assoc row :error (ex-message t)))))
                       r)
                 n   (inc i)]
             (aset out i r)
             (when (or (zero? (rem n step)) (= n total))
               (trace/emit! "rows-progress" {:pid pid :done n :total total}))
             (if (> (- (js/Date.now) turn-start) 100)
               (do (await (trace/next-turn))
                   (recur n (js/Date.now)))
               (recur n turn-start)))
           out)))))

#?(:cljs
   (defn- per-row-step
     "The step of a per-row op in :table mode on the JS runtime: the rows of a
      batch go through `row-fn` on this thread, as one worker-busy pair."
     [row-fn op-name]
     (lift1-async->step
      (fn [wrapped-batch]
        (let [rows (dataset/rows (dataset/unwrap-dataset wrapped-batch) :as-maps)
              pid  (trace/current-step-pid)
              tag  (trace/busy-start! "main" "main"
                                      (str op-name ": " (count rows) " rows") pid)]
          (.then (run-rows rows row-fn pid)
                 (fn [rows']
                   (trace/busy-end! tag)
                   (dataset/wrap-dataset (dataset/maps->dataset rows')))
                 (fn [e]
                   (trace/busy-end! tag (ex-message e))
                   (throw e))))))))

(defn operation->step
  "Convert the operation node `op-node` to a step fn for flow/create-flow.
  `opts` are the run-model opts."
  ([op-node] (operation->step op-node {}))
  (#_{:clj-kondo/ignore [:unused-binding]}
   [op-node opts]
   (let [op-fn (resolve-operation (:fn op-node))
         params (:params op-node)
        ;; :execution and the *-level values are aliases of :mode.
         mode (:mode op-node (:execution op-node :item))
         exec-type (case mode
                     :item :item-level
                     :coll :collection-level
                     :table :table-level
                     :item-level :item-level
                     :collection-level :collection-level
                     :table-level :table-level
                     :item-level)

        ;; The registry key is a string on both runtimes (squint has no symbol
        ;; constructor). A bare name gets its namespace from bare-op-namespaces,
        ;; because a per-row op registers as "<ns>/<op-name>".
         raw-fn-sym (let [fn-ref (:fn op-node)]
                      #?(:clj (cond
                                (keyword? fn-ref) (subs (str fn-ref) 1)
                                :else (str fn-ref))
                         :cljs (str fn-ref)))
         fn-sym (if (str/includes? raw-fn-sym "/")
                  raw-fn-sym
                  (if-let [ns-part (feature/get-bare-op-namespace raw-fn-sym)]
                    (str ns-part "/" raw-fn-sym)
                    raw-fn-sym))

         op-info (feature/get-operation-info fn-sym)
         schema (:params op-info)
         op-type (:type op-info :transformer)]

     (cond
       (= op-type :source)
       (source-operation->step op-fn params schema)

       (= op-type :step-factory)
       (let [param-values (if (and (map? params) (seq params))
                            (let [param-schema (:params op-info [])
                                  param-names (map :name param-schema)]
                              (map #(get params %) param-names))
                            [])]
         (apply op-fn param-values))

       :else
       (case exec-type
         :item-level
         (#?(:clj flow/lift1->step :cljs lift1-async->step)
          (fn [item]
            (apply-params op-fn item params schema)))

         :collection-level
         (collection-operation->step op-fn params schema)

         :table-level
         (cond
          ;; A sink op derives :row-shape :terminal and must get the whole
          ;; dataset, not one geometry ("raw-ds.objects is not a function").
           (contains? #{:whole-dataset :terminal} (:row-shape op-info))
           (flow/lift1->step
            (fn [wrapped-batch]
              (dataset/wrap-dataset
               (apply-params op-fn (dataset/unwrap-dataset wrapped-batch) params schema))))

          ;; JVM only: the group scatter uses futures and the compute-exec
          ;; (see run-component-groups).
           (= :over-groups (:row-shape op-info))
           (flow/lift1->step
            (fn [wrapped-batch]
              (let [#_{:clj-kondo/ignore [:unused-binding]}
                    groups (dataset/unwrap-dataset wrapped-batch)]
                (dataset/wrap-dataset
                 #?(:clj (run-component-groups
                          (:component op-node)
                          groups
                          ;; The pid that instrument-flow-step gives the proc-*
                          ;; events of this node: wire-pid reads only :name, and
                          ;; the model carries the run-model :name.
                          (wire-pid {:name (:name opts)} (:id op-node))
                          (select-keys opts [:timeout-ms :batch-size]))
                    :cljs (throw (ex-info "map-over-groups executor is JVM-only (no cljs Worker-pool port yet)"
                                          {:component (:component op-node)})))))))

          ;; A per-row op in :table mode. The single op goes through
          ;; fusion/apply-row-chain as a chain of one, because a row op must
          ;; always get a transient row and give one, as in a fused chain.
           (= :per-row (:row-shape op-info))
           #?(:clj
              (flow/lift1->step
               (fn [wrapped-batch]
                 (let [batch (dataset/unwrap-dataset wrapped-batch)
                       rows (dataset/rows batch :as-maps)
                       row-fn (with-meta
                                (fn [row] (apply-params op-fn row params schema))
                                (assoc (select-keys opts [:row-id-keys])
                                       :row-reads (:reads op-info)))
                       processed (mapv (fn [row]
                                         (fusion/apply-row-chain row [row-fn]))
                                       rows)]
                   (dataset/wrap-dataset (dataset/maps->dataset processed)))))
              :cljs
              (per-row-step (fn [row] (apply-params op-fn row params schema))
                            (last (str/split fn-sym #"/"))))

           :else
           (flow/lift1->step
            (fn [wrapped-batch]
              (let [batch (dataset/unwrap-dataset wrapped-batch)
                    transformed (dataset/map-geometry batch
                                                      (fn [geom]
                                                        (apply-params op-fn geom params schema))
                                                      {:preserve-index true})]
                (dataset/wrap-dataset transformed)))))

         (throw (ex-info (str "Unknown execution type: " exec-type)
                         {:node op-node
                          :execution-type exec-type
                          :operation-type op-type})))))))

(defn- expand-groups
  "Replace each :type :group node of `model` with its :expansion, rewiring
  group edges to the :entry and :exit nodes."
  [model]
  (let [nodes (:nodes model)
        edges (:edges model)
        group-nodes (filter #(= :group (:type %)) nodes)
        non-group-nodes (filter #(not= :group (:type %)) nodes)]

    (if (empty? group-nodes)
      model
      (let [group-info (into {}
                             (map (fn [g]
                                    [(:id g)
                                     {:entry (get-in g [:expansion :entry])
                                      :exit (get-in g [:expansion :exit])
                                      :nodes (get-in g [:expansion :nodes])
                                      :edges (get-in g [:expansion :edges])}])
                                  group-nodes))

            internal-nodes (mapcat (fn [g]
                                     (let [mode (:mode g)]
                                       (map #(assoc % :mode mode :type :operation)
                                            (get-in g [:expansion :nodes]))))
                                   group-nodes)

            internal-edges (mapcat #(get-in % [:expansion :edges]) group-nodes)

            rewired-edges (map (fn [edge]
                                 (let [from-id (:from edge)
                                       to-id (:to edge)
                                       to-group (get group-info to-id)
                                       from-group (get group-info from-id)]
                                   (cond-> edge
                                     to-group (assoc :to (:entry to-group))
                                     from-group (assoc :from (:exit from-group)))))
                               edges)

            new-nodes (vec (concat non-group-nodes internal-nodes))
            new-edges (vec (concat rewired-edges internal-edges))]

        (assoc model :nodes new-nodes :edges new-edges)))))

(defn- source-step
  "A step fn that emits the items of `data` one time, on a dummy :trigger
  input."
  [data]
  (letfn [(step-fn
            ([] {:params {}
                 :ins {:trigger "Start signal"}
                 :outs {:out "Data"}})

            ([_args]
             {:items (vec data)
              :emitting false})

            ([state _lifecycle] state)

            ([state _in-name _msg]
             (if (:emitting state)
               [state {}]
               [(assoc state :emitting true)
                {:out (:items state)}])))]
    step-fn))

(defn- sink-step
  "A step fn that collects results in `result-atom` and calls `completion-fn`
  when `expected-count` results are in. Only completion-fn marks completion."
  ([result-atom] (sink-step result-atom nil nil))
  ([result-atom expected-count completion-fn]
   (letfn [(step-fn
             ([] {:params {}
                  :ins {:in "Data"}
                  :outs {}})
             ([_args] {:results []})
             ([state lifecycle]
              ;; Use the literal, not ::flow/stop. squint builds an auto-resolved
              ;; keyword from the module specifier: ::flow/stop compiles to
              ;; "cljc-flow/flow/stop" on CLJS, which does not match the keyword
              ;; that the flow sends.
              (when (= lifecycle :clojure.core.async.flow/stop)
                (reset! result-atom (:results state)))
              state)
             ([state _in-name msg]
              (let [new-state (update state :results conj msg)]
                (when (and expected-count completion-fn
                           (= (count (:results new-state)) expected-count))
                  (reset! result-atom (:results new-state))
                  (completion-fn))
                [new-state {}])))]
     step-fn)))

(defn- table-source-step
  "A source step for table-> mode: it registers the dataset context and emits
  `ds` in batches, each in a FlowDataset wrapper."
  [ds opts]
  ;; No index in the cljs context: no step queries it, and its build walks
  ;; each geometry of the table for each flow.
  (let [ctx-id (dataset/register-dataset-context!
                ds #?(:clj opts :cljs (assoc opts :build-index? false)))
        batches (dataset/dataset->batches ds opts)
        batch-count (count batches)
        table-meta {:cg.dataset/table-rows (dataset/row-count ds)}
        wrapped-batches (mapv #(dataset/wrap-dataset % table-meta) batches)]
    (fn step-fn
      ([] {:params {:context-id ctx-id
                    :batch-count batch-count}
           :ins {:trigger "Start signal"}
           :outs {:out "Dataset batches"}})

      ([_args]
       {:batch-count batch-count
        :batches wrapped-batches
        :context-id ctx-id
        :emitting false})

      ([state lifecycle]
       (when (= lifecycle :clojure.core.async.flow/stop)
         (dataset/unregister-dataset-context! (:context-id state)))
       state)

      ([state _in-name _msg]
       (if (:emitting state)
         [state {}]
         [(assoc state :emitting true)
          {:out (:batches state)}])))))

#?(:clj
   (defn- assemble-batches
     "Unwrap the batches into one dataset, and rebuild the spatial index when a
      batch is marked `:cg.dataset/needs-reindex`."
     [wrapped-batches]
     (let [unwrapped (mapv dataset/unwrap-dataset wrapped-batches)
           needs-reindex? (some #(:cg.dataset/needs-reindex (meta %)) unwrapped)
           assembled (dataset/batches->dataset unwrapped)]
       (if needs-reindex?
         (dataset/add-spatial-index (vary-meta assembled dissoc :spatial))
         assembled))))

#?(:cljs
   (defn- assemble-batches
     "Unwrap the batches into one dataset, and rebuild the spatial index when a
      batch wrapper is marked `:cg.dataset/needs-reindex`."
     [wrapped-batches]
     (let [needs-reindex? (some #(:cg.dataset/needs-reindex (dataset/fd-meta %))
                                wrapped-batches)
           unwrapped (mapv dataset/unwrap-dataset wrapped-batches)
           assembled (dataset/batches->dataset unwrapped)]
       (if needs-reindex?
         (dataset/add-spatial-index assembled)
         assembled))))

(defn- table-sink-step
  "A sink step for table-> mode: when `expected-batch-count` batches are in,
  it stores the assembled dataset in `result-atom` and calls `completion-fn`."
  [result-atom expected-batch-count completion-fn]
  (fn step-fn
    ([] {:params {:expected-batch-count expected-batch-count}
         :ins {:in "Dataset batches"}
         :outs {}})

    ;; The batches stay wrapped in the state, because postwalk must not walk
    ;; into them.
    ([_args]
     {:received-count 0
      :expected expected-batch-count
      :batches []})

    ([state lifecycle]
     (when (= lifecycle :clojure.core.async.flow/stop)
       (when (and (nil? @result-atom)
                  (seq (:batches state)))
         (reset! result-atom (assemble-batches (:batches state)))))
     state)

    ([state _in-name msg]
     (let [new-batches (conj (:batches state) msg)
           new-count (inc (:received-count state))
           new-state (assoc state :batches new-batches :received-count new-count)]
       (when (and (:expected new-state)
                  (= new-count (:expected new-state))
                  completion-fn)
         (reset! result-atom (assemble-batches new-batches))
         (completion-fn))
       [new-state {}]))))

(defn- bind-sources
  "Map each source node id of `model` to its data in `bindings`, which has
  the :data-ref of the node as the key. Throws when a binding is absent."
  [model bindings]
  (reduce
   (fn [acc node]
     (if (= :source (:type node))
       (let [source-id (:id node)
             data-ref (:data-ref node)]
         (when-not (contains? bindings data-ref)
           (throw (ex-info (str "Source node missing data binding: " data-ref)
                           {:source source-id
                            :data-ref data-ref
                            :available-bindings (keys bindings)})))
         (assoc acc source-id (get bindings data-ref)))
       acc))
   {}
   (:nodes model)))

(defn ^:private wire-pid
  "The pid `<model>__<id>` of a node, which is the id convention of the
  canvas. The model name defaults to \"anon\"."
  [model node-id]
  (str (or (:name model) "anon")
       "__"
       (if (keyword? node-id) (name node-id) (str node-id))))

#?(:clj
   (defn- preview-value
     "`v` as a short JSON-safe string for a trace event: WKT for a JTS
     Geometry, a `[rows × cols]` summary for a dataset, pr-str for any other
     value. Truncates to `max-len` characters."
     [v max-len]
     (let [s (cond
               (nil? v) nil
               (instance? org.locationtech.jts.geom.Geometry v)
               (try (.write (org.locationtech.jts.io.WKTWriter.) v)
                    (catch Throwable _ (str v)))
               (dataset/dataset? v)
               (try (str "#tech.v3.dataset ["
                         (dataset/row-count v) " rows × "
                         (count (dataset/column-names v)) " cols]")
                    (catch Throwable _ "#tech.v3.dataset"))
               :else
               (try (pr-str v)
                    (catch Throwable _ "<unprintable>")))]
       (cond
         (nil? s) nil
         (> (count s) max-len) (str (subs s 0 (- max-len 3)) "...")
         :else s))))

#?(:clj
   (defn- results-preview
     "A string -> string preview map for the flow-complete trace event, with one
     entry for a dataset."
     [results]
     (cond
       (nil? results) nil
       (dataset/dataset? results)
       {"_result" (preview-value results 500)}
       (map? results)
       (reduce-kv (fn [acc k v]
                    (assoc acc
                           (if (keyword? k) (name k) (str k))
                           (preview-value v 500)))
                  {} results)
       :else
       {"_result" (preview-value results 500)})))

#?(:clj
   (defn instrument-flow-step
     "Wrap the step fn `step-fn` to emit `proc-start`, `proc-complete` and
     `proc-error` trace events with `pid` (`<model>__<node-id>`)."
     [pid step-fn]
     (fn instrumented
       ([] (step-fn))
       ([state] (step-fn state))
       ([state arg] (step-fn state arg))
       ([state in-id msg]
        (let [t0 (System/currentTimeMillis)]
          (trace/emit! "proc-start" {:pid pid :in in-id})
          (try
            (let [result (step-fn state in-id msg)
                  t1 (System/currentTimeMillis)
                  out (second result)
                  out-count (when (map? out)
                              (reduce + 0 (map count (vals out))))]
              (trace/emit! "proc-complete"
                           (cond-> {:pid pid :ms (- t1 t0)}
                             out-count (assoc :out-count out-count)))
              result)
            (catch Throwable e
              (let [t1 (System/currentTimeMillis)]
                (trace/emit! "proc-error"
                             {:pid pid :ms (- t1 t0)
                              :message (.getMessage e)}))
              (throw e))))))))

#?(:cljs
   (defn instrument-flow-step
     "CLJS twin of the JVM instrument-flow-step, through cg.trace-js listeners.
     For a Promise result (an async wasmts op), the event goes out when it settles."
     [pid step-fn]
     (fn instrumented
       ([] (step-fn))
       ([state] (step-fn state))
       ([state arg] (step-fn state arg))
       ([state in-id msg]
        (let [t0 (js/Date.now)
              complete! (fn [result]
                          (let [out (second result)
                                out-count (when (map? out)
                                            (reduce + 0 (map count (vals out))))]
                            (trace/emit! "proc-complete"
                                         (cond-> {:pid pid :ms (- (js/Date.now) t0)}
                                           out-count (assoc :out-count out-count)))))
              error! (fn [e]
                       (trace/emit! "proc-error"
                                    {:pid pid :ms (- (js/Date.now) t0)
                                     :message (or (.-message e) (str e))}))]
          (trace/emit! "proc-start" {:pid pid :in in-id})
          (trace/set-step-pid! pid)
          (try
            (let [result (step-fn state in-id msg)]
              (if (and result (.-then result))
                (.then result
                       (fn [resolved] (complete! resolved) resolved)
                       (fn [e] (error! e) (js/Promise.reject e)))
                (do (complete! result) result)))
            (catch :default e
              (error! e)
              (throw e))
            (finally
              (trace/set-step-pid! nil))))))))

(defn- make-process
  "flow/process for `step-fn`, with the :workload and :compute-timeout-ms of
   its describe map as opts."
  [step-fn]
  (let [desc (step-fn)
        opts (cond-> {}
               (:workload desc) (assoc :workload (:workload desc))
               (:compute-timeout-ms desc) (assoc :compute-timeout-ms (:compute-timeout-ms desc)))]
    (if (seq opts)
      (flow/process step-fn opts)
      (flow/process step-fn))))

(defn model->flow
  "A flow config for a model: {:config {:procs :conns} :result-atom :completed
  :sources :is-table-mode?}. `opts`: :batch-size (max rows of a table-> batch,
  default 64000) and :row-id-keys (the row keys that name a row, default [:id])."
  ([model bindings] (model->flow model bindings {}))
  ([model bindings opts]
   (let [model (expand-groups model)
         ;; A sink such as map-layer makes one result from the whole table.
         sink? (some (fn [n]
                       (when-let [qn (some-> (:fn n) component-qualified-name)]
                         (= :sink (:kind (feature/get-operation-info qn)))))
                     (:nodes model))
         batch-size (if sink?
                      #?(:clj Long/MAX_VALUE :cljs js/Number.MAX_SAFE_INTEGER)
                      (:batch-size opts))
         ;; The JVM dataset->batches reads :max-batch-size, and the cljs one
         ;; reads :batch-size.
         batch-opts (when batch-size
                      {:max-batch-size batch-size :batch-size batch-size})

        ;; Per-row fusion: consecutive :operation nodes in :mode :table whose op
        ;; has :row-shape :per-row become one proc, under the id of the first
        ;; node. cg.run.fusion holds the rules.
         #_{:clj-kondo/ignore [:unused-binding]}
         node-by-id #?(:clj (into {} (map (juxt :id identity)) (:nodes model))
                       :cljs {})
         fusable-groups #?(:clj
                           (->> (:groups (fusion/plan-fusion model))
                                (filterv (fn [g]
                                           (and (>= (count g) 2)
                                                (every? (fn [id]
                                                          (let [n (node-by-id id)]
                                                            (and (= :operation (:type n))
                                                                 (= :table (:mode n)))))
                                                        g)))))
                           :cljs [])
         skipped-ids (into #{}
                           (mapcat (fn [g] (rest g)))
                           fusable-groups)
         last-to-rep (into {}
                           (map (fn [g] [(peek (vec g)) (first g)]))
                           fusable-groups)

         sources (bind-sources model bindings)

         is-table-mode? (some #(= :table (:mode %))
                              (filter #(= :operation (:type %))
                                      (:nodes model)))

        ;; Completion needs the item count of all sources: the items of each
        ;; :source node, and 1 item for each source operation node.
         source-op-count (count
                          (filter (fn [node]
                                    (when (= :operation (:type node))
                                      (let [fn-ref (:fn node)
                                            fn-sym (when fn-ref
                                                     #?(:clj (cond
                                                               (keyword? fn-ref) (subs (str fn-ref) 1)
                                                               :else (str fn-ref))
                                                        :cljs (str fn-ref)))
                                            op-info (feature/get-operation-info fn-sym)]
                                        (= :source (:type op-info)))))
                                  (:nodes model)))

        ;; In table mode the count is in batches, and each source sets it.
         total-items (if is-table-mode?
                       nil
                       (+ (reduce + 0 (map count (vals sources)))
                          source-op-count))

        ;; With a collection operation the sink expects 1 item, because that
        ;; operation makes 1 result from N items.
         has-collection-op? (some #(= :coll (:mode % :item))
                                  (filter #(= :operation (:type %))
                                          (:nodes model)))
         sink-expected-count (cond
                               is-table-mode? nil
                               has-collection-op? 1
                               :else total-items)

         result-atom (atom nil)
         completed (atom false)
         completion-fn #(reset! completed true)
         batch-counts (atom {})

        ;; trace/with-worker-busy attributes the work on each row to the pool
        ;; thread that ran it.
         fused-steps
         #?(:clj
            (into {}
                  (for [g fusable-groups]
                    (let [rep (first g)
                          row-id-keys (or (:row-id-keys opts)
                                          fusion/default-row-id-keys)
                          row-fns (mapv (fn [id]
                                          (let [n (node-by-id id)
                                                op-fn (resolve-operation (:fn n))
                                                params (:params n)
                                                op-info (feature/get-operation-info
                                                         (fusion/node->op-key n))
                                                schema (:params op-info)
                                                ;; register-op! registers :row-reads as :reads.
                                                ;; The row-fn carries them to apply-row-chain's
                                                ;; retry-after-topology-exception, which fixes
                                                ;; only those keys.
                                                reads (:reads op-info)]
                                            (with-meta
                                              (fn [row]
                                                (apply-params op-fn row params schema))
                                              {:row-reads reads
                                               :row-id-keys row-id-keys})))
                                        g)
                          exec (cg-pool-reg/current-compute-exec)
                          apply-chain (fn [row]
                                        (trace/with-worker-busy
                                          {:item-id (fusion/row-id row row-id-keys)
                                           :pid (wire-pid model rep)}
                                          (fusion/apply-row-chain row row-fns)))]
                      [rep
                       (flow/lift1->step
                        (fn [wrapped-batch]
                          (let [batch (dataset/unwrap-dataset wrapped-batch)
                                rows (dataset/rows batch :as-maps)
                                ;; Progress: a silent batch of some minutes looks
                                ;; like a hang. Log and emit rows-progress at
                                ;; about each 5% (a step of 50 to 500 rows).
                                total (count rows)
                                done (java.util.concurrent.atomic.AtomicLong.)
                                step (max 1 (min 500 (max 50 (quot total 20))))
                                pid-str (wire-pid model rep)
                                counted (fn [row]
                                          (let [r (apply-chain row)
                                                n (.incrementAndGet done)]
                                            (when (or (zero? (rem n step)) (= n total))
                                              (log/info (str "row-chain " pid-str ": " n "/" total " rows"))
                                              (trace/emit! "rows-progress"
                                                           {:pid pid-str :done n :total total}))
                                            r))
                                processed (if exec
                                            (fusion/parallel-map exec counted rows)
                                            ;; No workload pool (for example a
                                            ;; unit test that did not start the
                                            ;; workers): run the rows serially.
                                            (mapv counted rows))]
                            (dataset/wrap-dataset
                             (dataset/maps->dataset processed)))))])))
            :cljs {})

         procs (reduce
                (fn [acc node]
                  (let [pid (:id node)]
                    (cond
                      ;; A fused node that is not the representative has no
                      ;; proc: the proc of the representative does its work.
                      (contains? skipped-ids pid)
                      acc

                      (contains? fused-steps pid)
                      (assoc acc pid
                             {:proc #?(:clj  (make-process
                                              (instrument-flow-step
                                               (wire-pid model pid)
                                               (get fused-steps pid)))
                                       :cljs (make-process (get fused-steps pid)))
                              :args {}})

                      :else
                      (assoc acc pid
                             (case (:type node)
                               :source
                               (if is-table-mode?
                                 (let [ds (get sources pid)
                                       _ (when-not (dataset/dataset? ds)
                                           (throw (ex-info "table-> mode requires dataset bindings"
                                                           {:source pid
                                                            :data-type (type ds)})))
                                       step (table-source-step ds (or batch-opts {}))
                                       batch-count (:batch-count (:params (step)))]
                                   (swap! batch-counts assoc pid batch-count)
                                   {:proc (make-process step)
                                    :args {}})
                                 {:proc (make-process (source-step (get sources pid)))
                                  :args {}})

                               :operation
                               (let [has-incoming-edge? (some #(= pid (:to %)) (:edges model))
                                     mode (:mode node :item)
                                     args (if (= mode :coll)
                                            {:expected-count total-items}
                                            {})]
                                 (if has-incoming-edge?
                                   {:proc (make-process
                                           (instrument-flow-step
                                            (wire-pid model pid)
                                            (operation->step node opts)))
                                    :args args}
                                ;; No incoming edge: a source operation.
                                   {:proc (make-process
                                           (instrument-flow-step
                                            (wire-pid model pid)
                                            (source-operation->step
                                             (resolve-operation (:fn node))
                                             (:params node)
                                             (op-schema node))))
                                    :args {}}))

                               :sink
                               (if is-table-mode?
                                 (let [expected-batches (reduce + 0 (vals @batch-counts))]
                                   {:proc (make-process (table-sink-step result-atom
                                                                         expected-batches
                                                                         completion-fn))
                                    :args {}})
                                 {:proc (make-process (sink-step result-atom
                                                                 sink-expected-count
                                                                 completion-fn))
                                  :args {}})

                               (throw (ex-info (str "Unknown node type: " (:type node))
                                               {:node node})))))))
                {}
                (:nodes model))

        ;; Each edge is a connection [[from :out] [to :in]]. No trigger conn is
        ;; necessary, because both flow systems make the :trigger channels
        ;; from the step descriptions. Fusion: an edge into a folded node is
        ;; internal and is dropped, and an edge from the last node of a fused
        ;; group leaves from the representative.
         conns (into []
                     (keep (fn [edge]
                             (let [{:keys [from to]} edge]
                               (cond
                                 (contains? skipped-ids to) nil
                                 (contains? last-to-rep from)
                                 [[(get last-to-rep from) :out] [to :in]]
                                 (contains? skipped-ids from) nil
                                 :else [[from :out] [to :in]]))))
                     (:edges model))]

     {:config {:procs procs
               :conns conns}
      :result-atom result-atom
      :completed completed
      :sources sources
      :is-table-mode? is-table-mode?})))

(defn expand-model
  "`model` with each group node expanded (see expand-groups). Use it to
   examine a model before flow construction."
  [model]
  (expand-groups model))

(defn model->flow-config
  "The flow config of `model`, without execution. See model->flow for the
  shape."
  [model bindings]
  (model->flow model bindings))

(defn create-flow
  "Create a flow from `model` and `bindings`. The flow is not started."
  [model bindings]
  ;; The flow needs the compute executor before model->flow reads it, because
  ;; a :workload :compute step such as transform-coords-step calls PROJ
  ;; through the per-thread Context of a pool worker.
  #?(:clj (ensure-cg-workers!))
  (let [{:keys [config]} (model->flow model bindings)]
    #?(:clj (flow/create-flow
             (assoc config
                    :compute-exec (cg-pool-reg/current-compute-exec)))
       :cljs (flow/create-flow
              (cond-> config
                (cg-pool-reg/current-pool)
                (assoc :compute-exec (cg-pool-reg/joint-compute-pool)))))))

(defn validate-flow-config
  "Check the flow config of `model`: each connection names a defined proc,
  and each source node has data. Gives nil, or throws ex-info."
  [model bindings]
  (let [{:keys [config sources]} (model->flow model bindings)
        {:keys [procs conns]} config]
    (doseq [[[from-pid _] [to-pid _]] conns]
      (when-not (get procs from-pid)
        (throw (ex-info (str "Connection references undefined process: " from-pid)
                        {:from from-pid :procs (keys procs)})))
      (when-not (get procs to-pid)
        (throw (ex-info (str "Connection references undefined process: " to-pid)
                        {:to to-pid :procs (keys procs)}))))

    (doseq [node (:nodes model)]
      (when (= :source (:type node))
        (when-not (get sources (:id node))
          (throw (ex-info (str "Source node missing data binding: " (:id node))
                          {:source (:id node)
                           :available-bindings (keys bindings)})))))

    nil))

#?(:cljs
   (defn- chan-queued
     "The messages in the buffer of a cljc-flow channel. The blocked puts do
      not count: their queue also holds the dead handlers of each alts."
     [ch]
     (let [buf  (when ch (.-buf ch))
           ring (when buf (.-buf buf))]
       (if (and ring (number? (.-length ring))) (.-length ring) 0))))

#?(:cljs
   (defn- queued [chans]
     (reduce + 0 (map chan-queued (vals (or chans {}))))))

#?(:cljs
   (defn- snapshot-watch
     "The flow-snapshot events of a flow: {:ping-all! :start! :stop!}. A proc
      answers a ping only between two steps; with no fresh answer it is \"busy\"."
     [flow-obj flow-model flow-name ids]
     (let [seen    #js {}
           at      #js {}
           waiting #js {}
           timers  #js {:tick nil :emit nil}
           ping!   (fn [id]
                     (when-not (aget waiting id)
                       (aset waiting id true)
                       (.then (flow/ping-proc flow-obj id :timeout-ms 250)
                              (fn [reply]
                                (aset waiting id false)
                                (when reply
                                  (aset seen id reply)
                                  (aset at id (js/Date.now)))
                                nil)
                              (fn [_] (aset waiting id false) nil))))
           proc    (fn [id]
                     (let [reply (aget seen id)
                           fresh? (and reply (< (- (js/Date.now) (aget at id)) 600))]
                       {:pid (wire-pid flow-model id)
                        :status (if fresh?
                                  (get reply :clojure.core.async.flow/status)
                                  "busy")
                        :count (if reply (get reply :clojure.core.async.flow/count) 0)
                        :in-queued (queued (when reply (get reply :clojure.core.async.flow/ins)))
                        :out-queued (queued (when reply (get reply :clojure.core.async.flow/outs)))}))
           emit!   (fn []
                     (trace/emit! "flow-snapshot" {:flow flow-name :procs (mapv proc ids)}))
           tick!   (fn []
                     (doseq [id ids] (ping! id))
                     (aset timers "emit" (js/setTimeout emit! 300)))]
       {:ping-all! (fn [] (js/Promise.all (to-array (map ping! ids))))
        :start!    (fn [] (aset timers "tick" (js/setInterval tick! 1000)))
        :stop!     (fn []
                     (js/clearInterval (aget timers "tick"))
                     (js/clearTimeout (aget timers "emit")))})))

(defn- flow-error-ex
  "The ex-info for a report from the flow error channel, with the report as
   ex-data and, on the JVM, the underlying exception as cause."
  [err]
  (let [pid (get err :clojure.core.async.flow/pid)
        op  (get err :clojure.core.async.flow/op)
        n   (get err :clojure.core.async.flow/count)
        ex  (get err :clojure.core.async.flow/ex)
        msg (str "Flow execution error"
                 (when pid (str " in " pid))
                 (when op (str " during " op))
                 (when n (str " (message " n ")"))
                 (when ex
                   (str ": "
                        #?(:clj (.getName (class ex))
                           :cljs (or (.-name ex) "Error"))
                        (when-let [m (ex-message ex)]
                          (str " - " m)))))]
    #?(:clj (ex-info msg {:type :flow-error :error err} ex)
       :cljs (ex-info msg {:type :flow-error :error err}))))

(defn run-model
  "Run `flow-model` with `bindings` (source :data-ref -> data) and give the sink
  results, a Promise on JS. `opts`: :timeout-ms (60000), :batch-size, :name,
  :return-flow? ({:results :flow}). Throws ex-info :type :timeout or :flow-error."
  [flow-model bindings opts]
  (let [flow-model #?(:cljs (if (object? flow-model)
                              ;; A squint keyword is a string. Because of this,
                              ;; js->clj gives the right shape for the :type,
                              ;; :mode and :data-ref values.
                              (util/js->clj flow-model :keywordize-keys true)
                              flow-model)
                      :clj flow-model)
        bindings #?(:cljs (if (object? bindings)
                            ;; A shallow copy: do not recurse into geometry objects.
                            (into {} (map (fn [[k v]] [k v])
                                          (js/Object.entries bindings)))
                            bindings)
                    :clj bindings)
        opts #?(:cljs (if (object? opts)
                        (util/js->clj opts :keywordize-keys true)
                        opts)
                :clj opts)
        ;; wire-pid reads :name from the model. With :name in opts,
        ;; run-component names the sub-flow of a table op `<op>-body`, and a
        ;; caller names its top-level flow.
        flow-model (cond-> flow-model
                     (:name opts) (assoc :name (:name opts)))]

    (when-not (model/model? flow-model)
      (throw (ex-info "Invalid model" {:model flow-model})))
    (validate-flow-config flow-model bindings)

    ;; See create-flow: the compute executor must exist before model->flow.
    #?(:clj (ensure-cg-workers!))
    (let [{:keys [config result-atom completed]} (model->flow flow-model bindings opts)
          flow-obj #?(:clj (flow/create-flow
                            (assoc config
                                   :compute-exec (cg-pool-reg/current-compute-exec)))
                      :cljs (flow/create-flow
                             (cond-> config
                               (cg-pool-reg/current-pool)
                               (assoc :compute-exec (cg-pool-reg/joint-compute-pool)))))]

      #?(:clj
         (let [flow-name (or (:name flow-model) "anon")
               trace-session (trace/open-file-writer! flow-name)
               flow-t0 (System/currentTimeMillis)
               _ (trace/emit! "flow-start"
                              {:flow flow-name
                               :pids (mapv #(wire-pid flow-model (:id %))
                                           (filter #(= :operation (:type %))
                                                   (:nodes flow-model)))})]
           (try
             (let [timeout-ms (get opts :timeout-ms 60000)
                   return-flow? (get opts :return-flow? false)

                 ;; flow/start leaves the processes paused.
                   {:keys [error-chan]} (flow/start flow-obj)
                   _ (flow/resume flow-obj)

                 ;; A trigger starts each source: a :source node, and an
                 ;; :operation node whose op has :type :source.
                   _ (doseq [node (:nodes flow-model)]
                       (cond
                         (= :source (:type node))
                         (flow/inject flow-obj [(:id node) :trigger] [:start])

                         (= :operation (:type node))
                         (let [op-info (feature/get-operation-info (:fn node))]
                           (when (= :source (:type op-info))
                             (flow/inject flow-obj [(:id node) :trigger] [:start])))))

                   start-time (System/currentTimeMillis)
                   results (loop []
                             (let [elapsed (- (System/currentTimeMillis) start-time)]
                               (cond
                                 @completed
                                 @result-atom

                               ;; The error check: this clause throws or falls through.
                                 (let [err (async/poll! error-chan)]
                                   (when err
                                     (throw (flow-error-ex err))))
                                 false

                                 (> elapsed timeout-ms)
                                 (throw (ex-info "Flow execution timeout"
                                                 {:type :timeout
                                                  :timeout-ms timeout-ms
                                                  :elapsed-ms elapsed}))

                                 :else
                                 (do
                                   (Thread/sleep 100)
                                   (recur)))))]

               (flow/stop flow-obj)

               (trace/emit! "flow-complete"
                            (cond-> {:flow flow-name
                                     :ms (- (System/currentTimeMillis) flow-t0)}
                              (some? results)
                              (assoc :results-preview (results-preview results))))

               (if return-flow?
                 {:results results :flow flow-obj}
                 results))
             (catch Throwable e
               (trace/emit! "flow-error"
                            {:flow flow-name
                             :ms (- (System/currentTimeMillis) flow-t0)
                             :message (.getMessage e)})
               (throw e))
             (finally
               ;; Let the trace mult drain the events in flight before the file
               ;; writer closes.
               (Thread/sleep 50)
               (trace/close-file-writer! trace-session))))

         :cljs
       ;; flow-start, flow-complete and flow-error mirror the JVM branch.
       ;; resolve and reject wrap the raw settlers, because each exit path
       ;; must emit its lifecycle event one time.
         (js/Promise.
          (fn [resolve* reject*]
            (let [flow-name (or (:name flow-model) "anon")
                  flow-t0 (js/Date.now)
                  _ (trace/emit! "flow-start"
                                 {:flow flow-name
                                  :pids (mapv #(wire-pid flow-model (:id %))
                                              (filter #(= :operation (:type %))
                                                      (:nodes flow-model)))})
                  ;; The procs of the config, because a group node of the
                  ;; model has no proc of its own.
                  watch (snapshot-watch flow-obj flow-model flow-name
                                        (vec (keys (:procs config))))
                  resolve (fn [v]
                            ((:stop! watch))
                            (trace/emit! "flow-complete"
                                         {:flow flow-name
                                          :ms (- (js/Date.now) flow-t0)})
                            (resolve* v))
                  reject (fn [e]
                           ((:stop! watch))
                           (trace/emit! "flow-error"
                                        {:flow flow-name
                                         :ms (- (js/Date.now) flow-t0)
                                         :message (or (and e (.-message e)) (str e))})
                           (reject* e))
                  timeout-ms (get opts :timeout-ms 60000)
                  return-flow? (get opts :return-flow? false)
                  {:keys [error-chan]} (flow/start flow-obj)]

            ;; The first ping finds the channels of each proc while the procs
            ;; are paused, because a proc in a step does not answer.
            ;; flow/resume gives a Promise. Inject the triggers only after it
            ;; resolves.
              (-> ((:ping-all! watch))
                  (.then (fn []
                           ((:start! watch))
                           (flow/resume flow-obj)))
                  (.then (fn []
                           (doseq [node (:nodes flow-model)]
                             (cond
                               (= :source (:type node))
                               (flow/inject flow-obj [(:id node) :trigger] [:start])

                             ;; An operation node with no incoming edge is a source operation.
                               (= :operation (:type node))
                               (let [has-incoming? (some #(= (:id node) (:to %)) (:edges flow-model))]
                                 (when-not has-incoming?
                                   (flow/inject flow-obj [(:id node) :trigger] [:start])))))

                           (let [start-time (js/Date.now)
                                 check-done (fn check-done []
                                              (let [elapsed (- (js/Date.now) start-time)
                                                    err (ca/poll! error-chan)]
                                                (cond
                                                  @completed
                                                  (do
                                                    (flow/stop flow-obj)
                                                    ;; No clj->js here: a squint map or vector is a plain
                                                    ;; JS object or array, and clj->js recurses into
                                                    ;; WasmTS geometry proxies and throws.
                                                    (if return-flow?
                                                      (resolve {:results @result-atom :flow flow-obj})
                                                      (resolve @result-atom)))

                                                ;; A step error goes to error-chan and the process
                                                ;; loop continues. Without this poll, an op that
                                                ;; throws gives a silent hang until the timeout.
                                                  (some? err)
                                                  (do
                                                    (flow/stop flow-obj)
                                                    (reject (flow-error-ex err)))

                                                  (> elapsed timeout-ms)
                                                  (reject (ex-info "Flow execution timeout"
                                                                   {:type :timeout
                                                                    :timeout-ms timeout-ms
                                                                    :elapsed-ms elapsed}))

                                                  :else
                                                  (js/setTimeout check-done 100))))]
                             (check-done))))
                  (.catch reject)))))))))

(defn run-operation
  "Run one operation through a flow. Not implemented: this fn always throws."
  [op-fn data params _opts]
  (throw (ex-info "Single operation execution not yet implemented"
                  {:operation op-fn
                   :data-count (count data)
                   :params params})))

;; cg.dsl loads on first use, because a require of it here makes a cycle.
#?(:clj
   (def ^:private cg-context-atom
     "The CG context, made on first use (see ensure-cg-context!)."
     (atom nil)))

#?(:clj
   (def ^:private loaded-module-hashes
     "Module path -> content hash of the last load-module evaluation in the
      current CG context. reset-context! clears it."
     (atom {})))

#?(:clj
   (defn- ensure-cg-context!
     []
     (when-not @cg-context-atom
       (require 'cg.dsl)
       (reset! cg-context-atom ((resolve 'cg.dsl/create-cg-context))))
     @cg-context-atom))

#?(:clj (declare ^:dynamic *cg-context*))

#?(:cljs (def ^:private js-model (js* "model")))
#?(:cljs (def ^:private js-macros (js* "macros")))
#?(:cljs (def ^:private js-util (js* "util")))
#?(:cljs (def ^:private js-geo (js* "geo")))
#?(:cljs (def ^:private js-feature (js* "feature")))
#?(:cljs (def ^:private js-io (js* "io")))
#?(:cljs (def ^:private js-dataset (js* "dataset")))
#?(:cljs (def ^:private js-math (js* "math")))
#?(:cljs (def ^:private js-string (js* "str")))
#?(:cljs
   (defn- compile-with-macros
     "Compile CG source to JS with squint and the cg macros in `cg-macros`, with
      imports and exports elided for eval."
     [src cg-macros]
     (.-javascript
      ((.-compileStringEx squint-compiler)
       src
       #js {:macros cg-macros
            :context "statement"
            :elide-imports true
            :elide-exports true}
       ((.-compileStringEx squint-compiler) "(ns user)" #js {:macros cg-macros})))))

#?(:cljs
   (defn- compile-expr-with-macros
     "Compile CG source to a JS expression, the cg macros in `cg-macros`."
     [src cg-macros]
     (.-javascript
      ((.-compileStringEx squint-compiler)
       src
       #js {:macros cg-macros
            :context "expr"
            :elide-imports true
            :elide-exports true}
       ((.-compileStringEx squint-compiler) "(ns user)" #js {:macros cg-macros})))))

#?(:cljs
   (defn- cg-macros-option
     "squint's :macros option for CG code, with string keys. A threading macro
      compiles a lifted rung argument with the same :macros."
     []
     (let [cg-macros #js {"cg.macros" #js {}}
           entries (aget cg-macros "cg.macros")
           compile-expr (fn [src] (compile-expr-with-macros src cg-macros))]
       (aset entries "item->" (macros/threading* :item compile-expr))
       (aset entries "coll->" (macros/threading* :coll compile-expr))
       (aset entries "table->" (macros/threading* :table compile-expr))
       cg-macros)))

#?(:cljs
   (defn- compile-cg-js
     "CG source to JS, with the cg.macros require dropped and the bare macro heads
      qualified ((item-> ...) becomes (cg.macros/item-> ...))."
     [code-str]
     (compile-with-macros
      (-> code-str
          (str/replace #"\(require\s+'\[cg\.macros[^\]]*\]\)" ";; macros pre-loaded")
          (str/replace #"\((item->|coll->|table->)(?=\s)" "(cg.macros/$1"))
      (cg-macros-option))))

#?(:cljs (defonce ^:private module-namespaces (atom {})))
#?(:cljs (defonce ^:private module-loads (atom {})))
#?(:cljs (defonce ^:private module-reader (atom nil)))

#?(:cljs
   (defn- module-namespace
     "The namespace object of a .cg module on the JS runtime, with its defs under
      their munged names. It exists before the module runs; load-module fills it."
     [ns-name]
     (or (get @module-namespaces ns-name)
         (let [o #js {}]
           (swap! module-namespaces assoc ns-name o)
           o))))

#?(:cljs (declare load-module load-module-text! load-deps! session-file script-file))

#?(:cljs
   (defn- run-js
     "The cg.run fns that compiled CG code calls through an alias of
      cg.run, under their munged names."
     []
     #js {:run_model run-model
          :run_component run-component
          :expand_model expand-model
          :init_cg_workers_BANG_ init-cg-workers!
          :terminate_cg_workers_BANG_ terminate-cg-workers!
          :load_module load-module
          :session_file session-file
          :script_file script-file}))

#?(:cljs
   (defn- ns-module
     "The JS value of an alias of `ns-name` in compiled CG code: a cg namespace,
      clojure.string, or the namespace object of a .cg module."
     [ns-name]
     (case ns-name
       "cg.feature" js-feature
       "cg.geo" js-geo
       "cg.io" js-io
       "cg.dataset" js-dataset
       "cg.model" js-model
       "cg.macros" js-macros
       "cg.util" js-util
       "cg.math" js-math
       "clojure.string" js-string
       "cg.run" (run-js)
       (module-namespace ns-name))))

#?(:cljs
   (defn- code-aliases
     "The [ns alias] pairs that CG code declares in the :require clause of its ns
      form and in (require '[x :as y]) forms."
     [code]
     (mapv (fn [[_ ns-name alias]] [ns-name alias])
           (re-seq #"\[([A-Za-z][\w.\-]*)\s+:as\s+([^\s\]\)]+)" code))))

#?(:cljs
   (defn- alias-values!
     "{munged-alias value} for the aliases of `code`. Also registers each
      alias for the threading macros (geo -> cg.geo)."
     [code]
     (reduce (fn [acc [ns-name alias]]
               (when-not (= ns-name "cg.macros")
                 (macros/register-alias! alias ns-name))
               (assoc acc (util/munge-alias alias) (ns-module ns-name)))
             {}
             (code-aliases code))))

#?(:cljs
   (defn- eval-globals
     "The globals that compiled CG code reads: squint_core, the cg namespaces
      under their usual aliases, and the threading-macro fns."
     []
     (let [cg-println (fn [& args]
                        (apply js/console.log
                               (map (fn [arg]
                                      (if (and arg (feature/is-geometry? arg))
                                        (io/write-wkt arg)
                                        arg))
                                    args)))
           ;; js* reaches the module binding, because squint compiles
           ;; `import * as squint_core from ...` into this namespace.
           cg-squint-core (js/Object.assign #js {} (js* "squint_core") #js {"println" cg-println})]
       {"model" js-model
        "cg$model" js-model
        "macros" js-macros
        "run" (run-js)
        "geo" js-geo
        "feature" js-feature
        "ds" js-dataset
        "io" js-io
        "squint_core" cg-squint-core
        "item__GT_" macros/item->*
        "coll__GT_" macros/coll->*
        "table__GT_" macros/table->*
        "register_alias_BANG_" macros/register-alias!})))

#?(:cljs
   (defn- set-globals! [m]
     (doseq [[k v] m]
       (unchecked-set js/globalThis k v))))

#?(:cljs
   (def ^:private op-marker
     "An ^:op on a defn name. Text with no marker has no op to register and
      needs no parse."
     #"\^:op\b"))

#?(:cljs
   (defn- register-ops-of!
     "Register in `ns-name` each op that the .cg source `text` declares, before
      squint compiles `text`. Each registered fn finds its defn at call time."
     [text ns-name lookup]
     (when (re-find op-marker text)
       (let [decls (parser/operations text)]
         (when (nil? decls)
           (throw (ex-info (str "A defn with ^:op needs the parser. Await "
                                "cg.parser.core/init! before the eval.")
                           {:ns ns-name})))
         (doseq [decl decls
                 :when (some #(= "op" %) (:flags decl))]
           (let [munged (util/munge-name (:name decl))]
             (register-op! ns-name decl
                           (fn [& args] (apply (lookup munged) args)))))))))

#?(:cljs
   (defn- eval-cg-cljs
     "Evaluate CG code on the JS runtime at global scope, where each def and alias
      stays a global for the next eval. `bindings` become globals under their
      munged names."
     ([code-str] (eval-cg-cljs code-str {}))
     ([code-str bindings]
      ;; The aliases first: the threading macros resolve them (geo ->
      ;; cg.geo) while the code compiles.
      (let [aliases (alias-values! code-str)
            _ (register-ops-of! code-str "user"
                                (fn [munged] (unchecked-get js/globalThis munged)))
            compiled-js (compile-cg-js code-str)]
        (set-globals! (merge (eval-globals)
                             aliases
                             (into {} (map (fn [[sym val]]
                                             [(str/replace (str sym) "-" "_") val]))
                                   bindings)))
        ((js* "(0, eval)") compiled-js)))))

#?(:clj
   (defn get-cg-context
     "The SCI context that eval-cg uses, made on first use."
     []
     (ensure-cg-context!)))

(def ^:dynamic *session-file*
  "Basename of the .cg file that a map session loads, or nil. Read it through
  session-file, because a copied SCI var does not see the binding."
  nil)

(defn session-file
  "The session-entry filename bound by the backend during a session/load
  eval, or nil outside one. See *session-file*."
  []
  *session-file*)

(def ^:dynamic *script-file*
  "Basename of the .cg file that cg.cli/run-model-file runs, or nil. Read it
  through script-file."
  nil)

(defn script-file
  "The filename that cg.cli/run-model-file binds while it runs a .cg file,
  or nil outside one. See *script-file*."
  []
  *script-file*)

#?(:clj
   (defn- arglist-params
     "The fixed params of an arglist, and its rest param (after &) or nil."
     [arglist]
     (let [[fixed [_ rest-param]] (split-with #(not= '& %) arglist)]
       {:fixed (vec fixed) :rest rest-param})))

#?(:clj
   (def ^:private role-flags
     "The param flags that give a param a role. A param with a role must be the
      first param."
     [:row :table :reducer]))

#?(:clj
   (defn- key-name
     "The name of a key in an option map: a symbol as written, a keyword
      without its colon."
     [k]
     (if (keyword? k) (subs (str k) 1) (str k))))

#?(:clj
   (defn- option-map
     "The required keys, optional keys (in arglist order) and default texts of
      the option map `& {:keys! [...] :keys [...] :or {...}}`, as the parser
      gives them. nil when the rest param is not a map."
     [p]
     (when (map? p)
       {:required (mapv key-name (:keys! p))
        :optional (mapv key-name (:keys p))
        :defaults (into {} (map (fn [[k v]] [(key-name k) (pr-str v)])) (:or p))})))

#?(:clj
   (defn- param-input
     "The decl input of the param `p` at index `i`. A destructured param gets the
      name of its position; the keys of an option map go under :options."
     [i p optional? rest?]
     (let [m (meta p)
           options (when rest? (option-map p))]
       (cond-> {:name (if (symbol? p) (name p) (str "arg" i))
                :type (if-let [t (:type m)] (name t) "any")
                :role (some #(when (get m %) %) role-flags)
                :optional optional?}
         options (assoc :options options)))))

#?(:clj
   (defn var-op-decl
     "The parser-shaped OperationDecl (no :body or :location) of the op of the
      SCI var `v`. Params come from the widest arity. Params past the shortest
      arity are optional, except an option map with a required key in each arity."
     [v]
     (let [m (meta v)
           arities (mapv arglist-params (:arglists m))
           width (fn [a] (+ (count (:fixed a)) (if (:rest a) 1 0)))
           widest (reduce (fn [a b] (if (> (width b) (width a)) b a)) arities)
           min-fixed (reduce min (map (comp count :fixed) arities))]
       {:name (name (:name m))
        :inputs (vec (concat
                      (map-indexed (fn [i p] (param-input i p (>= i min-fixed) false))
                                   (:fixed widest))
                      (when-let [r (:rest widest)]
                        (let [in (param-input (count (:fixed widest)) r true true)]
                          [(cond-> in
                             (and (seq (get-in in [:options :required])) (every? :rest arities))
                             (assoc :optional false))]))))
        :outputs (if-let [outs (:outputs m)]
                   (mapv (fn [[k {t :type}]] {:name (name k) :type (if t (name t) "any")})
                         outs)
                   [{:name "result" :type "any"}])
        :row-reads (some->> (:row-reads m) (map name) sort vec)
        :row-writes (some->> (:row-writes m) (map name) sort vec)
        :flags (vec (keep (fn [[k x]] (when (true? x) (name k))) m))})))

#?(:clj
   (defn- register-op-var!
     "Register the op of `v` when `v` is the var of a defn with ^:op."
     [v]
     (when (and (instance? sci.lang.Var v) (:op (meta v)))
       (when-not (seq (:arglists (meta v)))
         (throw (ex-info (str "op '" (:name (meta v)) "' must be a defn") {:var v})))
       (register-op! (str (:ns (meta v))) (var-op-decl v) @v))))

#?(:clj
   (defn eval-cg*
     "Evaluate the CG code string `code-str` in the SCI context `ctx` form by
      form and give the last value. A ^:op defn registers before the next form
      expands. The forms of a top-level (do ...) are top-level forms."
     [ctx code-str]
     (let [rdr (sci/reader code-str)
           eval-top (fn eval-top [form]
                      (if (and (seq? form) (= 'do (first form)))
                        (reduce (fn [_ f] (eval-top f)) nil (rest form))
                        (doto (sci/eval-form ctx form) register-op-var!)))]
       (sci/binding [sci/ns @sci/ns]
         (loop [v nil]
           (let [form (sci/parse-next ctx rdr)]
             (if (= :sci.core/eof form)
               v
               (recur (eval-top form)))))))))

(defn eval-cg
  "Evaluate a CG code string in the default context (SCI on the JVM, squint
  on JS) and give the value of the last form. A def stays visible in the next
  call. On JS the result gets no clj->js conversion."
  [code-str]
  #?(:clj (eval-cg* (ensure-cg-context!) code-str)
     :cljs (eval-cg-cljs code-str)))

(defn eval-cg-file
  "Evaluate the CG file at `path` in the default context and give the value of
  its last form (the value of the Var for a def)."
  [path]
  (let [content (util/read-file path)
        result (eval-cg (str "(do " content ")"))]
    #?(:clj (if (or (var? result) (instance? sci.lang.Var result))
              @result
              result)
       :cljs result)))

(defn- model-names
  "The names of the threading-macro bindings of `text` in source order, the
  models that Run can run. A Promise on cljs."
  [text opts]
  #?(:clj (let [extract (requiring-resolve 'cg.parser.core/extract-models)]
            (->> (:bindings (extract text opts))
                 (filter #(= :threading_macro (:body-shape %)))
                 (mapv :name)))
     :cljs (.then (parser/extract-models text opts)
                  (fn [flow]
                    (->> (:bindings flow)
                         (filter #(= :threading_macro (:body-shape %)))
                         (mapv :name))))))

#?(:clj (declare parse-ns-form))
#?(:cljs (declare load-entry-module!))

#?(:clj
   (defn- file-ns-prefix
     "\"<ns>/\" for the ns form of `text`, or nil."
     [text]
     (some-> (parse-ns-form text) :ns-name (str "/"))))

(defn- binding-value
  "The value of the def `nm`, or nil. `file-ns` is the file-ns-prefix on the
  JVM and the namespace object on cljs. Throws when `nm` is not a symbol name."
  [nm file-ns]
  (when (re-find #"[\s()\[\]{}\"';@^`~\\#,]" nm)
    (throw (ex-info (str "'" nm "' is not a binding name") {:type :not-a-model :model nm})))
  #?(:clj (try (let [v (eval-cg (str file-ns nm))]
                 (if (var? v) @v v))
               (catch Exception _ nil))
     ;; squint's eval env holds a model as compiler-side persistent data,
     ;; and its clj->js gives the plain object that run-model reads. Data
     ;; stays as it is: clj->js would descend into the geometries.
     :cljs (if file-ns
             (unchecked-get file-ns (util/munge-name nm))
             (try (eval-cg (str "(clj->js " nm ")"))
                  (catch :default _ nil)))))

(defn- source-bindings
  "{data-ref value} for each source node of `model`, from the def of each
  binding name. A def value spliced into :data-ref is its own data."
  [model file-ns]
  (into {}
        (for [node (:nodes model)
              :when (= :source (:type node))
              :let [r (:data-ref node)]]
          (if #?(:clj (or (instance? clojure.lang.Named r) (string? r))
                 :cljs (string? r))
            [r #?(:clj (eval-cg (str file-ns (name r)))
                  :cljs (if file-ns
                          (unchecked-get file-ns (util/munge-name (name r)))
                          (eval-cg (name r))))]
            [r r]))))

(defn- model-names-or-nil
  "model-names for an error message: nil when the parser cannot load."
  [text opts]
  #?(:clj (try (model-names text opts) (catch Exception _ nil))
     :cljs (.catch (model-names text opts) (fn [_] nil))))

(defn- not-a-model [nm names]
  (ex-info (str "'" nm "' is not a model."
                (when names
                  (str " The models of this file: "
                       (if (seq names) (str/join ", " names) "none"))))
           {:type :not-a-model :model nm :models names}))

(defn- map-layer-model?
  "True when a node of `model` is the map-layer sink, by any alias."
  [model]
  (boolean (some #(= "map-layer" (some-> (:fn %) str (str/split #"/") last))
                 (:nodes model))))

(defn ^:async run-map-layer-model
  "{:model :result} of the model `m` named `nm`, or {:model :error}, because a
  layer that fails must not fail the run that it is drawn with."
  [nm m file-ns run-opts]
  (try
    {:model nm
     :result #?(:clj (run-model m (source-bindings m file-ns) (assoc run-opts :name nm))
                :cljs (await (run-model m (source-bindings m file-ns) (assoc run-opts :name nm))))}
    (catch #?(:clj Exception :cljs :default) e
      {:model nm :error (ex-message e)})))

(defn ^:async run-map-layer-models
  "run-map-layer-model of each model of `names` but `skip` that ends in map-layer,
  in source order. A map host draws them with the model of the run."
  [names skip file-ns run-opts]
  (loop [todo (remove #(= skip %) names) out []]
    (if-let [nm (first todo)]
      (let [m (binding-value nm file-ns)]
        (if (and (some? m) (map-layer-model? m))
          (recur (rest todo)
                 (conj out #?(:clj (run-map-layer-model nm m file-ns run-opts)
                              :cljs (await (run-map-layer-model nm m file-ns run-opts)))))
          (recur (rest todo) out)))
      out)))

(defn ^:async run-file
  "Evaluate the .cg source `text` and run one model. Gives {:model :result :layers}, a Promise on cljs.
  `opts`: :model, :file, :grammar-wasm, :map-layers (see run-map-layer-models); others go to run-model."
  ([text] (run-file text nil))
  ([text opts]
   #?(:cljs (when (re-find op-marker text)
              (await (parser/init! opts))))
   #?(:cljs (await (load-deps! text)))
   (let [file-ns #?(:clj (do (binding [*script-file* (or (:file opts) *script-file*)]
                               (eval-cg text))
                             (file-ns-prefix text))
                    :cljs (if (re-find #"^\s*(?:;[^\n]*\n\s*)*\(ns\s" text)
                            (await (load-entry-module! text (:file opts)))
                            (do (eval-cg text) nil)))
         nm (or (:model opts)
                (last #?(:clj (model-names text opts)
                         :cljs (await (model-names text opts)))))]
     (when-not nm
       (throw (ex-info "no threading-macro model binding to execute" {:type :not-a-model})))
     (let [m (binding-value nm file-ns)]
       (when-not (and (some? m) (some? (:nodes m)))
         (throw (not-a-model nm #?(:clj (model-names-or-nil text opts)
                                   :cljs (await (model-names-or-nil text opts))))))
       (let [run-opts (assoc (dissoc opts :model :grammar-wasm :file :map-layers) :name nm)
             result #?(:clj (run-model m (source-bindings m file-ns) run-opts)
                       :cljs (await (run-model m (source-bindings m file-ns) run-opts)))
             layers? (and (:map-layers opts) (map-layer-model? m))
             names (when layers?
                     #?(:clj (model-names text opts)
                        :cljs (await (model-names text opts))))
             layers (when layers?
                      #?(:clj (run-map-layer-models names nm file-ns run-opts)
                         :cljs (await (run-map-layer-models names nm file-ns run-opts))))]
         (cond-> {:model nm :result result}
           layers? (assoc :layers layers)))))))

#?(:clj
   (defn- parse-ns-form
     "Parse the (ns ...) form at the start of CG module source. Gives
      {:ns-name :docstring :requires}, or nil when there is no valid ns form."
     [source]
     (try
       (let [reader (java.io.PushbackReader. (java.io.StringReader. source))
             first-form (read reader)]
         (when (and (list? first-form) (= 'ns (first first-form)))
           (let [ns-name (second first-form)
                 rest-forms (drop 2 first-form)
                 [docstring clauses] (if (string? (first rest-forms))
                                       [(first rest-forms) (rest rest-forms)]
                                       [nil rest-forms])
                 requires (some (fn [clause]
                                  (when (and (list? clause)
                                             (= :require (first clause)))
                                    (vec (rest clause))))
                                clauses)]
             {:ns-name ns-name
              :docstring docstring
              :requires (or requires [])})))
       (catch Exception _
         nil))))

#?(:clj
   (defn- module-text
     "The text of the module at `path`: the file, else the classpath
      resource of the library, whose name has no src/."
     [path]
     (if-let [r (when-not (util/file-exists? path)
                  (clojure.java.io/resource (str/replace-first path #"^src/" "")))]
       (slurp r)
       (util/read-file path))))

#?(:clj
   (defn load-module
     "Load the CG module at `path`, which must start with an (ns ...) form.
      Gives {:ns-name :path :docstring :requires}. Skips an unchanged module."
     [path]
     (let [content (module-text path)
           ns-info (parse-ns-form content)]
       (when-not ns-info
         (throw (ex-info (str "Module must start with (ns ...) form: " path)
                         {:path path
                          :hint "Add (ns my.namespace (:require ...)) at top of file"})))
       ;; One evaluation for each content in a context. The hash is recorded
       ;; BEFORE the eval, because a load cycle must end: module A loads B,
       ;; whose top loads A, which must read as loaded. A module that throws
       ;; removes its record and is evaluated again on the next load.
       (let [h (hash content)]
         (when-not (= h (get @loaded-module-hashes path))
           (swap! loaded-module-hashes assoc path h)
           (try
             (eval-cg content)
             (catch Throwable e
               (swap! loaded-module-hashes dissoc path)
               (throw e)))))
       {:ns-name (:ns-name ns-info)
        :path path
        :docstring (:docstring ns-info)
        :requires (:requires ns-info)})))

#?(:cljs
   (defn set-module-reader!
     "Set how load-module reads a module on the JS runtime: (f path) gives its
      text, or a Promise of it. With no reader, load-module reads the file (Node)."
     [f]
     (reset! module-reader f)
     nil))

#?(:cljs
   (defn- module-def-names
     "The names of a module's top-level defs (a form at the start of a
      line)."
     [text]
     (mapv (fn [m] (aget m 1))
           (js/Array.from
            (.matchAll text (js/RegExp. "^\\((?:def|defn|defn-|defonce|defmulti)\\s+(?:\\^\\S+\\s+)*([^\\s\\[\\]()\"]+)"
                                        "gm"))))))

#?(:cljs
   (defn- module-js
     "The JS of a module: an async function of its namespace object and aliases.
      The namespace object has a getter for each def, which reads it once it runs."
     [text alias-params]
     (let [getters (str/join ", "
                             (map (fn [n]
                                    (let [m (util/munge-name n)]
                                      (str "\"" m "\": {get: () => typeof " m " === \"undefined\" ? undefined : " m
                                           ", enumerable: true, configurable: true}")))
                                  (module-def-names text)))
           body (str/replace text #"\(run/load-module\s+(\"[^\"]+\")\)"
                             "(await (run/load-module $1))")]
       (str "(async function (__cg_ns" (str/join (map (fn [a] (str ", " a)) alias-params)) ") {\n"
            "Object.defineProperties(__cg_ns, {" getters "});\n"
            (compile-cg-js body) "\n})"))))

#?(:cljs
   (defn- module-file
     "The file of the module `path` on Node: in the working folder, else in
      the cg package, one level above dist/cg.mjs or two above src/cg/."
     [path]
     (or (some (fn [p] (when (util/file-exists? p) p))
               (cons path
                     (map (fn [up]
                            (js/decodeURIComponent
                             (.-pathname (js/URL. (str up path) js/import.meta.url))))
                          ["../" "../../"])))
         path)))

#?(:cljs
   (defn ^:async load-module-text!
     "Evaluate `text` as the CG module of `path`. See load-module."
     [path text]
     ;; Skips metadata on the name, such as ^:no-doc.
     (let [ns-name (second (re-find #"\(ns\s+(?:\^\S+\s+)*([^\s\)]+)" text))]
       (when-not ns-name
         (throw (ex-info (str "Module must start with (ns ...) form: " path)
                         {:path path})))
       (let [info {:ns-name ns-name :path path}
             aliases (vals (into {} (map (fn [[n a]] [a [n a]])) (code-aliases text)))
             _ (swap! module-loads assoc path info)
             ;; The aliases first: the threading macros resolve them while
             ;; the module compiles.
             _ (doseq [[n a] aliases]
                 (when-not (= n "cg.macros")
                   (macros/register-alias! a n)))
             _ (when (re-find op-marker text)
                 (await (parser/init! nil)))
             _ (register-ops-of! text ns-name
                                 (fn [munged] (unchecked-get (module-namespace ns-name) munged)))
             module-fn ((js* "(0, eval)")
                        (module-js text (map (fn [[_ a]] (util/munge-alias a)) aliases)))]
         (set-globals! (eval-globals))
         (try
           (await (.apply module-fn nil
                          (vec (cons (module-namespace ns-name)
                                     (map (fn [[n _]] (ns-module n)) aliases)))))
           (catch :default e
             (swap! module-loads dissoc path)
             (throw e)))
         info))))

#?(:cljs
   (defn ^:async load-module
     "Load a .cg module (it must start with an (ns ...) form) on the JS runtime into
      its own namespace object. Resolves to {:ns-name :path}, at once when the path
      is loaded or loading. Reads the file with the reader of set-module-reader!."
     [path]
     (or (get @module-loads path)
         (await (load-module-text!
                 path
                 (await (js/Promise.resolve (if-let [f @module-reader]
                                              (f path)
                                              (util/read-file (module-file path))))))))))

#?(:cljs
   (defn ^:async load-entry-module!
     "Evaluate the opened file as a module, because only a module can wait
      at its top level. Resolves to its namespace object."
     [text file]
     (let [path (or file "<entry>")]
       (swap! module-loads dissoc path)
       (set! *script-file* file)
       (try
         (module-namespace (:ns-name (await (load-module-text! path text))))
         (finally (set! *script-file* nil))))))

#?(:cljs
   (defn ^:async load-deps!
     "Load each module that `code` names in a (run/load-module \"x\") form, in
      order. The host calls this before it evaluates `code`."
     [code]
     (doseq [[_ path] (re-seq #"\(run/load-module\s+\"([^\"]+)\"\)" code)]
       (await (load-module path)))))

#?(:clj
   (defn reset-context!
     "Make a new default CG context. This clears all bindings and the record
      of loaded modules. Gives the new context."
     []
     (require 'cg.dsl)
     (reset! loaded-module-hashes {})
     (reset! cg-context-atom ((resolve 'cg.dsl/create-cg-context)))
     @cg-context-atom))

#?(:clj
   (def ^:dynamic *cg-context*
     "Not read by this namespace: the default context is in cg-context-atom
      (see get-cg-context)."
     nil))
