;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.backend.server-test
  "The ephemeral loopback bind and the port-file handshake of the backend: bind
  to loopback on an OS-assigned port, and report the port after the bind."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [cg-app.backend.server :as server]))

(set! *warn-on-reflection* true)

(deftest ephemeral-loopback-port
  (testing "start-server! binds 127.0.0.1 on an OS-assigned port and reports it"
    (let [s (server/start-server! :port 0 :host "127.0.0.1")
          p (server/bound-port s)
          f (java.io.File/createTempFile "cg-port" ".txt")]
      (try
        (is (pos? p))
        (is (not= 3000 p) "must be ephemeral, not the old default")
        (server/write-port-file! (.getPath f) p)
        (is (= (str p) (str/trim (slurp f))))
        (finally (server/stop-server!) (.delete f))))))
