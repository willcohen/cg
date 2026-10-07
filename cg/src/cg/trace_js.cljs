;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.trace-js
  "Lifecycle event stream for cg flows on JS, with the wire fields of cg.trace.
  `emit!` calls each listener of `add-trace-listener` synchronously.")

(def trace-wire-version 1)

(defonce ^:private listeners (js/Set.))

;; A mutable box, not an atom: squint's reset! returns undefined, and this
;; counter must give the stamped value.
(defonce ^:private counter #js {:n 0})

(defn add-trace-listener
  "Register `f` to get each trace event, and return `f`. An exception from `f`
  does not fail the flow step that emitted."
  [f]
  (.add listeners f)
  f)

(defn remove-trace-listener
  "Remove a listener of add-trace-listener. Gives true when it was
  registered."
  [f]
  (.delete listeners f))

(defn emit!
  "Stamp the event `data` of string `kind` with `:v`, `:seq`, `:ts` and
  `:worker-id`, call each listener with it, and return it."
  [kind data]
  (let [n (inc (aget counter "n"))
        _ (aset counter "n" n)
        evt (merge {:v trace-wire-version
                    :seq n
                    :ts (js/Date.now)
                    :kind kind
                    :worker-id "js"}
                   data)]
    (.forEach listeners
              (fn [f]
                (try (f evt) (catch :default _ nil))))
    evt))

;; A flow step runs its synchronous part inside instrument-flow-step, which
;; holds the pid here. A step that hands work to the pool reads it at entry
;; to tag its worker-busy events.
(defonce ^:private step-pid #js {:pid nil})

(defn set-step-pid! [pid]
  (aset step-pid "pid" pid)
  nil)

(defn current-step-pid []
  (aget step-pid "pid"))

(defn busy-start!
  "Emit worker-busy-start for work on `worker` (a pool index or \"main\")
  and give the tag for busy-end!."
  [worker handler label pid]
  (let [n (inc (or (aget counter "busy") 0))
        _ (aset counter "busy" n)
        tag (cond-> {:worker-id (str worker)
                     :handler handler
                     :label label
                     :item-id (str "js-" n)}
              pid (assoc :pid pid))]
    (emit! "worker-busy-start" tag)
    tag))

(defn busy-end!
  "Emit the worker-busy-end of a busy-start! tag, with `error` when the work
  failed."
  ([tag] (busy-end! tag nil))
  ([tag error]
   (emit! "worker-busy-end" (cond-> tag error (assoc :error error)))
   nil))

(defn next-turn
  "A Promise that resolves in a later turn of the event loop, after timers
  and a repaint can run."
  []
  (js/Promise. (fn [resolve] (js/setTimeout resolve 0))))

;; The def events light the loader card of a def: the canvas gives the card of
;; a top-level def the id binding-<name>, and the pid of each event is that id.
;; cg.run/run-file puts a call of def-start! or def-end! before each top-level
;; form. The forms of a module run in order, so one def is open at a time.

(defonce ^:private open-def #js {:pid nil :t0 0})

;; pid -> the rows that the fetches of the def read.
(defonce ^:private def-rows #js {})

(defn current-def-pid
  "The pid of the open def, or nil."
  []
  (aget open-def "pid"))

(defn def-end!
  "Complete the open def with def-complete, or with def-error for the error
   `e`. No effect when no def is open."
  ([] (def-end! nil))
  ([e]
   (when-let [pid (aget open-def "pid")]
     (aset open-def "pid" nil)
     (emit! (if e "def-error" "def-complete")
            (cond-> {:pid pid :ms (- (js/Date.now) (aget open-def "t0"))}
              e (assoc :message (or (.-message e) (str e))))))
   nil))

(defn def-start!
  "Complete the open def, then open the def `def-name` with def-start. The pid
   is binding-<name>."
  [def-name]
  (def-end!)
  (let [pid (str "binding-" def-name)]
    (aset open-def "pid" pid)
    (aset open-def "t0" (js/Date.now))
    (aset def-rows pid 0)
    (emit! "def-start" {:pid pid :name def-name})
    nil))

(defn def-rows!
  "Add `n` rows to the count of the def `pid`, and emit def-progress. No
   effect when `pid` is nil."
  [pid n]
  (when pid
    (let [done (+ (or (aget def-rows pid) 0) n)]
      (aset def-rows pid done)
      (emit! "def-progress" {:pid pid :done done}))))
