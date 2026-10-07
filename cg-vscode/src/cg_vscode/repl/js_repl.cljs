;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.repl.js-repl
  "An nREPL server for the JS runtime of the CG Map tab, with no vscode
   dependency. The squint nREPL compiles each form in Node for a page with an
   import map. This server sends the source, and the page evaluates it as a
   run does. (eval-fn code) gives a Promise of #js {:value :out} or #js {:err
   :out}."
  (:require ["net" :as net]
            ["crypto" :as crypto]
            [cg-vscode.repl.bencode :as b]))

(def ^:private ops
  #js {:clone #js {} :close #js {} :describe #js {} :eval #js {} :load-file #js {}})

(defn- reply [^js msg ^js m]
  (aset m "id" (aget msg "id"))
  (aset m "session" (aget msg "session"))
  m)

(defn- eval-replies [^js msg ^js r]
  (let [out (aget r "out")
        err (aget r "err")]
    (cond-> []
      (seq out) (conj (reply msg #js {:out out}))
      err (conj (reply msg #js {:err (str err "\n")})
                (reply msg #js {:ex "Error" :root-ex "Error" :status #js ["eval-error"]}))
      (not err) (conj (reply msg #js {:value (aget r "value") :ns "user"}))
      true (conj (reply msg #js {:status #js ["done"]})))))

(defn ^:async replies [^js msg eval-fn]
  (case (aget msg "op")
    "clone" [(reply msg #js {:new-session (crypto/randomUUID) :status #js ["done"]})]
    "describe" [(reply msg #js {:ops ops
                                :versions #js {:cg-js-repl #js {:version-string "0.1.0"}}
                                :status #js ["done"]})]
    "eval" (eval-replies msg (await (eval-fn (aget msg "code"))))
    "load-file" (eval-replies msg (await (eval-fn (aget msg "file"))))
    "close" [(reply msg #js {:status #js ["done"]})]
    [(reply msg #js {:status #js ["error" "unknown-op" "done"]})]))

(defn- write-replies! [^js sock rs]
  (doseq [r rs]
    (when-not (.-destroyed sock)
      (.write sock (b/encode r)))))

(defn- serve! [^js sock eval-fn]
  (let [!buf (atom (js/Buffer.alloc 0))
        !queue (atom (js/Promise.resolve nil))
        answer (fn [msg]
                 (-> (replies msg eval-fn)
                     (.then (fn [rs] (write-replies! sock rs)))
                     (.catch (fn [e] (js/console.error "cg JS REPL:" e)))))]
    (.on sock "error" (fn [_] nil))
    (.on sock "data"
         (fn [chunk]
           (let [[msgs tail] (b/decode-all (js/Buffer.concat #js [@!buf chunk]))]
             (reset! !buf tail)
             (doseq [m msgs]
               (swap! !queue (fn [p] (.then p (fn [_] (answer m)))))))))))

(defn start-server!
  "Serves nREPL on 127.0.0.1 at a port that the OS assigns. Gives a Promise of
   #js {:server :port :sockets}."
  [eval-fn]
  (js/Promise.
   (fn [resolve reject]
     (let [sockets (js/Set.)
           server (net/createServer
                   (fn [^js sock]
                     (.add sockets sock)
                     (.on sock "close" (fn [] (.delete sockets sock)))
                     (serve! sock eval-fn)))]
       (.on server "error" reject)
       (.listen server 0 "127.0.0.1"
                (fn [] (resolve #js {:server server
                                     :port (.-port (.address server))
                                     :sockets sockets})))))))

(defn stop-server!
  "Closes the server `srv` of start-server! and its connections. Gives a
   Promise."
  [^js srv]
  (js/Promise.
   (fn [resolve _]
     (.forEach (aget srv "sockets") (fn [^js s] (.destroy s)))
     (.close (aget srv "server") (fn [_] (resolve nil))))))
