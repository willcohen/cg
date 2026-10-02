;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.backend.middleware
  (:require [clojure.tools.logging :as log]))

(set! *warn-on-reflection* true)

(def ^:private cors-headers
  {"Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS"
   "Access-Control-Allow-Headers" "Content-Type, Authorization, X-Session-Id"
   "Vary" "Origin"})

(defn- allowed-origin?
  "False for a page of a different site and for a sandboxed frame."
  [origin]
  (boolean
   (or (re-matches #"https?://(?:localhost|127\.0\.0\.1|\[::1\])(?::\d+)?" origin)
       (and (not= "null" origin)
            (not (re-find #"^https?:" origin))))))

(defn wrap-cors
  "Gives 403 to a page of a different site, because the backend evaluates
   the code that it receives."
  [handler]
  (fn [request]
    (let [origin  (get-in request [:headers "origin"])
          headers (when origin
                    (assoc cors-headers "Access-Control-Allow-Origin" origin))]
      (cond
        (and origin (not (allowed-origin? origin)))
        {:status 403
         :headers {"Content-Type" "application/json"}
         :body "{\"error\": \"origin not allowed\"}"}

        (= :options (:request-method request))
        {:status 204 :headers (or headers {})}

        ;; A throw that escapes the handler reaches the Jetty error page,
        ;; which has no CORS headers, and a cross-origin browser client then
        ;; reports a CORS violation in place of the 500. Catch here, where
        ;; the headers are added.
        :else
        (try
          (update (handler request) :headers merge headers)
          (catch Throwable t
            (log/error t (str "Unhandled error " (name (:request-method request)) " " (:uri request)))
            {:status 500
             :headers (merge {"Content-Type" "application/json"} headers)
             :body (str "{\"error\": " (pr-str (str (.getName (class t)) ": " (.getMessage t))) "}")}))))))

(defn wrap-logging [handler]
  (fn [request]
    (let [method (:request-method request)
          uri (:uri request)]
      (log/info (str (name method) " " uri))
      (handler request))))
