;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; It shares a worker with proj-handler: extractCoords -> projTransArray -> applyCoords of a
;; batch runs on one worker, and the geom handles stay in worker memory. Node fetch does not
;; serve file:// URLs, and a patched globalThis.fetch returns the bytes. Every form is :cljs.
(ns cg.wasmts-handler
  "Worker-router handler for wasmts (JTS WASM), key `net.willcohen.wasmts`.
   In Node, pass the .wasm bytes as `initArgs.wasmtsWasmBinary`."
  #?(:cljs (:require ["ffi-wasm/handler" :refer [makeHandler]])))

#?(:clj (set! *warn-on-reflection* true))

#?(:cljs (do

;; Worker-local state. defonce keeps the geom handles in `*pending` when a
;; test harness loads the module again.
(defonce ^:private *wts (atom nil))
(defonce ^:private *worker-idx (atom -1))
(defonce ^:private *profiling? (atom false))

;; Map of batch-id (string) -> JS array of wasmts Geometry handles, kept from
;; extractCoords to applyCoords on the same worker. The handles are in the
;; WASM heap of the worker and cannot move to another worker.
(defonce ^:private *pending (atom {}))

(def ^:private node?
  (and (exists? js/process)
       (some? (.. js/process -versions))
       (some? (.. js/process -versions -node))))

;; The fetch of wasmts.js.wasm at import time fails under Node.
(defn- patch-node-fetch!
  "Patch globalThis.fetch to return the wasm bytes for a .wasm URL; other URLs pass through."
  [bin]
  (let [buf (if (instance? js/Uint8Array bin)
              (.slice (.-buffer bin)
                      (.-byteOffset bin)
                      (+ (.-byteOffset bin) (.-byteLength bin)))
              bin)
        orig-fetch (.-fetch js/globalThis)]
    (set! (.-fetch js/globalThis)
          (fn [url & rest-args]
            (let [s (str url)]
              (if (or (.endsWith s ".wasm")
                      (.includes s "wasmts.js.wasm"))
                (js/Promise.resolve
                 #js {:ok true
                      :arrayBuffer (fn [] (js/Promise.resolve buf))})
                (if orig-fetch
                  (.apply orig-fetch js/globalThis
                          (.concat #js [url] rest-args))
                  (js/Promise.reject (js/Error. "no fetch")))))))))

(defonce ^:private *load-args (atom nil))
(defonce ^:private *loading (atom nil))

(defn- ^:async load-wasmts!
  "Import wasmts from the init args and give it when wasmts.geom is ready."
  [args]
  (let [js-url   (.-wasmtsJsUrl args)
        wasm-bin (.-wasmtsWasmBinary args)]
    (when (and node? wasm-bin)
      (patch-node-fetch! wasm-bin))
    ;; On some paths the wasmts loader resolves its .wasm through __filename,
    ;; relative to the .js URL. Set it before the import.
    (set! (.-__filename js/globalThis) js-url)
    (await (js/import js-url))
    ;; The loader runs the WASM init on a later tick. Guard each access.
    (loop [i 0]
      (let [w (.-wasmts js/globalThis)
            g (when w (.-geom w))]
        (cond
          (some? g) w

          (>= i 200)
          (throw (js/Error.
                  "cg.wasmts-handler: wasmts.geom not available after 10s"))

          :else
          (do (await (js/Promise. (fn [r] (js/setTimeout r 50))))
              (recur (inc i))))))))

;; WasmTS loads at the first call that needs it, to keep its heap out of a
;; worker that never gets a geometry with z.
(defn- ensure-wts!
  "wasmts, or a Promise of it on the first call."
  []
  (or @*wts
      (do (when (nil? @*loading)
            (reset! *loading
                    (-> (load-wasmts! @*load-args)
                        (.then (fn [w] (reset! *wts w) w))
                        (.catch (fn [e] (reset! *loading nil) (throw e))))))
          @*loading)))

(defn init
  "Keep the load args of wasmts. initArgs: wasmtsJsUrl (required),
   wasmtsWasmBinary (ArrayBuffer or Uint8Array, required in Node) and
   workerIdx (the pool-slot index of this worker)."
  [args _ctx]
  (let [args    (or args #js {})
        wkr-idx (.-workerIdx args)]
    (when-not (.-wasmtsJsUrl args)
      (throw (js/Error. "cg.wasmts-handler/init: missing initArgs.wasmtsJsUrl")))
    (when (number? wkr-idx)
      (reset! *worker-idx wkr-idx))
    (reset! *load-args args)
    nil))

(defn- fingerprint
  "Identity of the load args (wasmtsJsUrl and binary length) for the re-init guard of makeHandler.
   A matching init() shares one factory; a mismatch throws."
  [args]
  (let [args (or args #js {})
        url  (or (.-wasmtsJsUrl args) "")
        bin  (.-wasmtsWasmBinary args)
        n    (if bin (.-byteLength bin) 0)]
    (str "js:" url "|wasm:" n)))

(defn set-config!
  "Set the config of this worker: profilingEnabled (bool) and workerIdx
   (number), which replaces the slot from init."
  [args]
  (when (some? (.-profilingEnabled args))
    (reset! *profiling? (boolean (.-profilingEnabled args))))
  (when (number? (.-workerIdx args))
    (reset! *worker-idx (.-workerIdx args)))
  nil)

(defn ^:async extract-coords
  "Decode `geomsWkb` and keep the geom handles under `batchId` for applyCoords.
   Returns coords (Float64Array, stride 4: x y z m), boundaries (Int32Array, vertex count of
   each geom) and hasZ (true when a geom has a z that is not NaN)."
  [args]
  (let [wts (await (ensure-wts!))]
    (let [batch-id  (.-batchId args)
          geoms-wkb (.-geomsWkb args)
          n         (.-length geoms-wkb)
          geoms     (js/Array. n)
          boundaries (js/Int32Array. n)
          ;; In wasmts alpha5, WKBReader.read is static (reader, bytes). One
          ;; reader instance serves the batch.
          wkb-reader-cls (.. wts -io -WKBReader)
          wkb-reader (.create0 wkb-reader-cls)
          total (loop [i 0 acc 0]
                  (if (>= i n)
                    acc
                    (let [wkb (aget geoms-wkb i)
                          g   (.read wkb-reader-cls wkb-reader wkb)
                          c   (.getNumPoints (.. wts -geom) g)]
                      (aset geoms i g)
                      (aset boundaries i c)
                      (recur (inc i) (+ acc c)))))
          coords (js/Float64Array. (* total 4))]
      (let [has-z?
            (loop [i 0 off 0 saw-z? false]
              (if (>= i n)
                saw-z?
                (let [g  (aget geoms i)
                      cs (.getCoordinates (.. wts -geom) g)
                      m  (alength cs)
                      z? (loop [k 0 saw? saw-z?]
                           (if (>= k m)
                             saw?
                             (let [c     (aget cs k)
                                   x     (.-x c)
                                   y     (.-y c)
                                   z     (.-z c)
                                   z-ok? (and (number? z)
                                              (not (js/Number.isNaN z)))
                                   slot  (* (+ off k) 4)]
                               (aset coords slot x)
                               (aset coords (+ slot 1) y)
                               ;; JTS Coordinate.z is NaN for a 2D geom. PROJ
                               ;; expects 0 for an unused dim, because NaN
                               ;; propagates through a transform.
                               (aset coords (+ slot 2) (if z-ok? z 0))
                               (aset coords (+ slot 3) 0)
                               (recur (inc k) (or saw? z-ok?)))))]
                  (recur (inc i) (+ off m) z?))))]
        (swap! *pending assoc batch-id geoms)
        #js {:coords     coords
             :boundaries boundaries
             :hasZ       has-z?}))))

(defn ^:async apply-coords
  "Apply `transformedCoords` (stride 4) and `boundaries` to the geoms kept under `batchId`,
   and drop that batch. Returns wkb, a JS array of Uint8Array."
  [args]
  (let [wts (await (ensure-wts!))]
    (let [batch-id   (.-batchId args)
          coords     (.-transformedCoords args)
          boundaries (.-boundaries args)
          geoms      (get @*pending batch-id)]
      (when (nil? geoms)
        (throw (js/Error.
                (str "cg.wasmts-handler: no pending state for batchId "
                     batch-id))))
      (let [n   (.-length geoms)
            out (js/Array. n)
            ;; In wasmts alpha5, WKBWriter.write is static (writer, geom). One
            ;; writer instance serves the batch. The default writer is 2D and
            ;; drops each z, and this path is the one for a geometry with z.
            wkb-writer-cls (.. wts -io -WKBWriter)
            wkb-writer (.create1 wkb-writer-cls 3)]
        (loop [i 0 off 0]
          (when (< i n)
            (let [orig (aget geoms i)
                  m    (aget boundaries i)
                  ;; subarray is a view with no copy.
                  slice (.subarray coords (* off 4) (* (+ off m) 4))
                  ;; applyCoordinates(geom, flatArr, valuesPerCoord) returns a
                  ;; new geom with the same topology. The stride is 4 (x y z
                  ;; m), and wasmts reads only the dimensions of the source
                  ;; geom.
                  g'  (.applyCoordinates (.. wts -geom) orig slice 4)
                  wkb (.write wkb-writer-cls wkb-writer g')]
              (aset out i wkb)
              (recur (inc i) (+ off m)))))
        (swap! *pending dissoc batch-id)
        #js {:wkb out}))))

(defn cleanup
  "Drop the pending state of a batch after an abort or an error. Idempotent."
  [args]
  (swap! *pending dissoc (.-batchId args))
  #js {})

(def ^:private method-map
  #js {:setConfig     set-config!
       :extractCoords extract-coords
       :applyCoords   apply-coords
       :cleanup       cleanup})

;; extractCoords and applyCoords use the WASM heap through wasmts, and the
;; handler runtime emits BUSY-INC/DEC events for them. setConfig and cleanup
;; change only JS maps.
(def ^:private busy-methods #js ["extractCoords" "applyCoords"])
(def ^:private destroy-methods #js [])

(def create
  (makeHandler
   #js {:init           init
        :methods        method-map
        :busyMethods    busy-methods
        :destroyMethods destroy-methods
        :fingerprint    fingerprint
        :label          "cg.wasmts"}))

;; worker-router/worker-bootstrap.ts picks mod.default ?? mod.handler ??
;; mod.create. squint does not emit `export default` for a namespace def.
;; The bootstrap finds `handler` on its second probe.
(def handler create)

))
