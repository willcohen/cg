;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; A flow does not stop by itself. A proc also cannot close the channels of its
;; connections. Thus each proc of a table flow sends `eos` after its last
;; batch. A sink completes when each of its inputs sent `eos`. This ns does not
;; require cg.run, because cg.run requires the namespaces of the fetch steps.

(ns cg.run.eos)

#?(:clj (set! *warn-on-reflection* true))

(def eos
  "The end-of-stream message. In squint a keyword is a string, and nothing in
   a channel can change a string."
  :cg.run/eos)

(defn eos?
  "True when `msg` is the end-of-stream message."
  [msg]
  ;; The JVM interns a keyword, and squint compares two strings by value.
  (identical? eos msg))

#?(:cljs
   (defn- thenable? [x]
     (and (some? x) (fn? (.-then x)))))

(defn- with-count
  "`state` with the count of end-of-stream messages. A step that keeps no
   state, such as one from lift1->step, gives nil."
  [state n]
  (assoc (or state {}) :cg.run.eos/count n))

(defn- eos-count [state]
  (or (:cg.run.eos/count state) 0))

(defn forward-eos
  "Wrap the step fn `step-fn` of a proc that has `n-inputs` connections into
   its inputs. The step does not get `eos`. The wrapper sends one `eos` on :out
   after each input sent `eos`."
  [step-fn n-inputs]
  (fn forwarded
    ([] (step-fn))
    ([args] (with-count (step-fn args) 0))
    ([state transition]
     (with-count (step-fn state transition) (eos-count state)))
    ([state in-id msg]
     (let [n (eos-count state)]
       (if (eos? msg)
         (let [n' (inc n)]
           [(with-count state n') (if (>= n' n-inputs) {:out [eos]} {})])
         (let [keep-count (fn [[state' outputs]] [(with-count state' n) outputs])
               result (step-fn state in-id msg)]
           #?(:clj (keep-count result)
              :cljs (if (thenable? result)
                      (.then result keep-count)
                      (keep-count result)))))))))

(defn add-eos
  "The [state outputs] of a source step, with `eos` after the messages on
   :out. A source that emits nothing does not send `eos`."
  [[state outputs]]
  (if (seq (:out outputs))
    [state (update outputs :out (fn [msgs] (conj (vec msgs) eos)))]
    [state outputs]))

(defn end-with-eos
  "Wrap the step fn `step-fn` of a source that emits one time. The wrapper
   sends `eos` after the messages of that emission."
  [step-fn]
  (fn ended
    ([] (step-fn))
    ([args] (step-fn args))
    ([state transition] (step-fn state transition))
    ([state in-id msg]
     (let [result (step-fn state in-id msg)]
       #?(:clj (add-eos result)
          :cljs (if (thenable? result)
                  (.then result add-eos)
                  (add-eos result)))))))
