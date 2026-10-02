;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.backend.middleware-test
  (:require [clojure.test :refer [deftest is testing]]
            [cg-app.backend.middleware :as middleware]))

(defn- call
  "The response of an ok handler behind wrap-cors, for a request with the
   Origin header `origin` (nil for none)."
  [method origin]
  ((middleware/wrap-cors (fn [_] {:status 200 :headers {} :body "ok"}))
   {:request-method method
    :uri "/api/execute-code"
    :headers (if origin {"origin" origin} {})}))

(deftest a-page-of-a-different-site-is-refused
  (doseq [origin ["https://example.com" "http://example.com:3000"
                  "http://localhost.example.com" "null"]]
    (testing origin
      (is (= 403 (:status (call :post origin))))
      (is (= 403 (:status (call :options origin))) "the preflight also"))))

(deftest a-local-page-and-a-shell-pass
  (doseq [origin ["http://localhost:3100" "http://127.0.0.1:8080"
                  "vscode-webview://0abc" "app://localhost"]]
    (testing origin
      (let [resp (call :post origin)]
        (is (= 200 (:status resp)))
        (is (= origin (get-in resp [:headers "Access-Control-Allow-Origin"]))))
      (is (= 204 (:status (call :options origin)))))))

(deftest a-request-with-no-origin-passes
  (let [resp (call :post nil)]
    (is (= 200 (:status resp)))
    (is (nil? (get-in resp [:headers "Access-Control-Allow-Origin"])))))
