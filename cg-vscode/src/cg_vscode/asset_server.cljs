;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.asset-server
  "Serves the files of the map webview on 127.0.0.1, because a worker in a
   webview cannot import a vscode-resource URL. No vscode dependency."
  (:require ["node:http" :as http]
            ["node:fs" :as fs]
            ["node:path" :as path]))

(def ^:private mime
  {".mjs" "text/javascript" ".js" "text/javascript" ".wasm" "application/wasm" ".css" "text/css"
   ".map" "application/json"})

(defn- inside? [root file]
  (let [rel (path/relative root file)]
    (not (or (.startsWith rel "..") (path/isAbsolute rel)))))

(defn url-for
  "The URL path /<name>/<rel> of the absolute path `file` in `roots`."
  [roots file]
  (some (fn [[nm root]]
          (when (inside? root file)
            (str "/" nm "/" (.join (.split (path/relative root file) path/sep) "/"))))
        roots))

(defn- file-for
  "The file of the request URL `url`: an alias, or a file in `roots`."
  [roots aliases url]
  (let [p (try (js/decodeURIComponent (first (.split url "?"))) (catch :default _ ""))]
    (or (get aliases p)
        (when-let [[_ nm rel] (.match p #"^/([^/]+)/(.+)$")]
          (when-let [root (get roots nm)]
            (let [file (path/resolve root rel)]
              (when (inside? root file) file)))))))

(defn- not-found! [^js res] (doto res (.writeHead 404) (.end)))

(defn- serve! [roots aliases ^js req ^js res]
  (if-let [file (file-for roots aliases (.-url req))]
    (fs/readFile file
                 (fn [err data]
                   (if err
                     (not-found! res)
                     ;; Module scripts load with CORS, and the webview has
                     ;; another origin.
                     (doto res
                       (.writeHead 200 #js {"Content-Type" (get mime (path/extname file) "application/octet-stream")
                                            "Access-Control-Allow-Origin" "*"})
                       (.end data)))))
    (not-found! res)))

(defn start!
  "Resolves to the http.Server."
  [roots aliases]
  (let [srv (http/createServer (fn [req res] (serve! roots aliases req res)))]
    (js/Promise. (fn [resolve reject]
                   (.on srv "error" reject)
                   (.listen srv 0 "127.0.0.1" (fn [] (resolve srv)))))))
