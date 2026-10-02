;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.trace
  "Lifecycle event stream for cg flows: `emit!` produces, `tap-trace` on `trace-mult` consumes.
   Keeps `recent-events`, and writes JSONL per run under `traces-dir` when it is set."
  (:require [clojure.core.async :as async]
            [clojure.string :as str]
            [charred.api :as json])
  (:import [java.io BufferedWriter FileWriter]
           [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]))

(set! *warn-on-reflection* true)

(def ^:private trace-buffer-size 16384)
(def ^:private recent-events-max 1024)
(defn traces-dir
  "The JSONL trace folder: the cg.trace.dir property, else CG_TRACE_DIR. nil when neither is set."
  []
  (some not-empty [(System/getProperty "cg.trace.dir")
                   (System/getenv "CG_TRACE_DIR")]))
(def trace-wire-version 1)

(defonce ^:private seq-counter (atom 0))

(defonce trace-chan
  ;; The one producer channel. With a sliding buffer, the oldest events drop
  ;; under back-pressure, and a consumer sees a drop as a gap in `:seq`.
  ;; `defonce`, because a REPL reload must not orphan the go-loops of the
  ;; other `defonce` forms.
  (async/chan (async/sliding-buffer trace-buffer-size)))

(defonce trace-mult
  (async/mult trace-chan))

(defn tap-trace
  "Attach `ch` as a consumer of the trace stream. The caller owns `ch`."
  [ch]
  (async/tap trace-mult ch)
  ch)

(defn untap-trace [ch]
  (async/untap trace-mult ch))

(def recent-events-atom
  (atom clojure.lang.PersistentQueue/EMPTY))

(defn- conj-bounded [^clojure.lang.PersistentQueue q evt max-len]
  (let [q' (conj q evt)]
    (if (> (count q') max-len)
      (pop q')
      q')))

(defn recent-events
  "A snapshot of the queue of recent events, as a vector."
  []
  (vec @recent-events-atom))

(defn clear-recent-events! []
  (reset! recent-events-atom clojure.lang.PersistentQueue/EMPTY))

(defn- now-ms ^long [] (System/currentTimeMillis))

(defn emit!
  "Stamp an event of string `kind` and fields `data` with `:v`, `:seq`, `:ts` and `:worker-id`
   (the calling thread name), put it on `trace-chan`, and return it."
  [kind data]
  (let [evt (merge {:v trace-wire-version
                    :seq (swap! seq-counter inc)
                    :ts (now-ms)
                    :kind kind
                    :worker-id (.getName (Thread/currentThread))}
                   data)]
    (async/put! trace-chan evt)
    evt))

(defmacro with-worker-busy
  "Emit `worker-busy-start` and `worker-busy-end` around the body, both with `tag-map`, and return
   the body value. Give a stable `:item-id` to pair them. On a throw, the end event has `:error`."
  [tag-map & body]
  `(let [tag# ~tag-map]
     (emit! "worker-busy-start" tag#)
     (try
       (let [r# (do ~@body)]
         (emit! "worker-busy-end" tag#)
         r#)
       (catch Throwable t#
         (emit! "worker-busy-end" (assoc tag# :error (.getMessage t#)))
         (throw t#)))))

#_{:clj-kondo/ignore [:unused-private-var]}
(defonce ^:private recent-events-tap
  (let [ch (async/chan (async/sliding-buffer recent-events-max))]
    (tap-trace ch)
    (async/go-loop []
      (when-let [evt (async/<! ch)]
        (swap! recent-events-atom conj-bounded evt recent-events-max)
        (recur)))
    ch))

(defn- ensure-traces-dir! [^String dir]
  (let [p (Path/of dir (make-array String 0))]
    (when-not (Files/exists p (make-array java.nio.file.LinkOption 0))
      (Files/createDirectories p (make-array FileAttribute 0)))
    p))

(defn- safe-fname-segment [s]
  (-> (str s)
      (str/replace #"[^A-Za-z0-9._-]" "_")
      (subs 0 (min 64 (count (str s))))))

(defn- iso-stamp []
  (-> (Instant/now) .toString (str/replace #"[:.]" "-")))

(defn- write-latest-symlink! [^Path traces ^Path target]
  (let [latest (.resolve traces "latest.jsonl")
        rel    (.relativize traces target)]
    (try
      (Files/deleteIfExists latest)
      (Files/createSymbolicLink latest rel (make-array FileAttribute 0))
      (catch java.nio.file.FileAlreadyExistsException _ nil)
      (catch UnsupportedOperationException _
        ;; A filesystem with no symlink support: copy the file.
        (Files/copy ^Path target ^Path latest
                    ^"[Ljava.nio.file.CopyOption;"
                    (into-array java.nio.file.CopyOption
                                [java.nio.file.StandardCopyOption/REPLACE_EXISTING]))))))

(defrecord FileWriterSession [^BufferedWriter writer tap-ch path])

(defonce ^:private active-writer-count
  ;; The count of open file writers. See open-file-writer! for a nested call.
  (atom 0))

;; A nested flow gets no second file: the outer tap already gets its events.
(defn open-file-writer!
  "Open a JSONL writer for one run under `traces-dir`. Returns a session for `close-file-writer!`,
   a no-op session when `traces-dir` is nil or a writer is already open."
  [flow-name]
  (if-let [dir (and (= 1 (swap! active-writer-count inc)) (traces-dir))]
    (let [^java.nio.file.Path traces (ensure-traces-dir! dir)
          fname  (str (safe-fname-segment flow-name) "-" (iso-stamp) ".jsonl")
          ^java.nio.file.Path target (.resolve traces fname)
          _      (Files/createFile target (make-array FileAttribute 0))
          _      (write-latest-symlink! traces target)
          writer (BufferedWriter. (FileWriter. (.toFile target) true))
          tap-ch (async/chan (async/sliding-buffer trace-buffer-size))
          abs    (.toString (.toAbsolutePath target))]
      (println (str "[cg.trace] writing events to " abs))
      (tap-trace tap-ch)
      (async/go-loop []
        (if-let [evt (async/<! tap-ch)]
          (do
            (try
              ;; go-loop drops a ^String hint on an argument form, and
              ;; .write then reflects; a hint on a let local survives.
              (let [^String line (json/write-json-str evt)]
                (.write writer line))
              (.newLine writer)
              (.flush writer)
              (catch Throwable _ nil))
            (recur))
          (try (.close writer) (catch Throwable _ nil))))
      (->FileWriterSession writer tap-ch target))
    ;; No folder, or a nested call.
    (->FileWriterSession nil nil nil)))

(defn close-file-writer! [{:keys [tap-ch]}]
  (swap! active-writer-count #(max 0 (dec %)))
  (when tap-ch
    (untap-trace tap-ch)
    (async/close! tap-ch)))
