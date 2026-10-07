;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.map.queue
  "The jobs of the map page, one at a time: a run or a REPL form. The trace
   listeners of cg see each run that is on, and the canvas must get the
   messages of one run.")

(defn make
  "A queue whose first job waits for `boot`, a Promise of the context of a
   job. `post` sends a message to the host."
  [boot post]
  {:chain (atom boot) :busy (atom 0) :post post})

(defn- wait-event [state]
  #js {:type "cg/trace-event"
       :event #js {:kind "stage" :label "wait in the queue" :state state
                   :ts (js/Date.now)}})

(defn enqueue!
  "Runs (f ctx m) after the jobs before it, and gives a Promise of its end. A
   failure does not stop the queue. A run (`run?`) that must wait posts a
   wait stage. A REPL form posts none, because no run end would close it."
  [q f m run?]
  (let [busy   (:busy q)
        post   (:post q)
        waits? (and run? (pos? @busy))
        p      (.then @(:chain q)
                      (fn [ctx]
                        (when waits? (post (wait-event "done")))
                        (.then (.catch (f ctx m) (fn [_] nil))
                               (fn [_] (swap! busy dec) ctx))))]
    (swap! busy inc)
    (when waits? (post (wait-event "start")))
    (reset! (:chain q) p)
    p))
