;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.open-url-test
  "The URL checks, the file name, the url query parameter and the fetch
   messages of open-url."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_app/ui/open_url.mjs" :as ou]))

(def ^:private page "https://willcohen.github.io/cg/")

(def ^:private boston
  "https://raw.githubusercontent.com/willcohen/cg/main/cg/examples/boston.cg")

(deftest a-raw-url-stays-as-it-is
  (is (= boston (aget (ou/source-url (str "  " boston "\n") page) "url"))))

(deftest a-github-file-page-becomes-its-raw-url
  (is (= boston (aget (ou/source-url "https://github.com/willcohen/cg/blob/main/cg/examples/boston.cg"
                                     page)
                      "url"))))

(deftest a-relative-url-resolves-against-the-page
  (is (= "https://willcohen.github.io/cg/examples/simple.cg"
         (aget (ou/source-url "examples/simple.cg" page) "url"))))

(deftest bad-input-gives-a-reason
  (is (some? (aget (ou/source-url "  " page) "error")))
  (is (some? (aget (ou/source-url "not a url" nil) "error")))
  (is (some? (aget (ou/source-url "file:///etc/passwd" page) "error")))
  (is (some? (aget (ou/source-url "javascript:alert(1)" page) "error"))))

(deftest file-name-is-the-last-part-of-the-path
  (is (= "boston.cg" (ou/file-name boston)))
  (is (= "my file.cg" (ou/file-name "https://a.example/x/my%20file.cg?raw=1")))
  (is (= "untitled.cg" (ou/file-name "https://a.example/"))))

(deftest url-param-reads-the-query
  (is (= boston (ou/url-param (str "?url=" (js/encodeURIComponent boston)))))
  (is (= boston (ou/url-param (str "?url=" boston))))
  (is (nil? (ou/url-param "")))
  (is (nil? (ou/url-param "?url=")))
  (is (nil? (ou/url-param "?other=1"))))

(deftest page-href-sets-and-removes-the-param
  (let [with (ou/page-href "https://willcohen.github.io/cg/?x=1" boston)]
    (is (= boston (ou/url-param (.-search (js/URL. with)))))
    (is (= "https://willcohen.github.io/cg/?x=1" (ou/page-href with nil)))))

(deftest http-errors-name-the-file-and-the-status
  (is (.includes (ou/http-error "boston.cg" 404) "HTTP 404"))
  (is (.includes (ou/http-error "boston.cg" 500) "HTTP 500"))
  (is (.includes (ou/network-error "boston.cg") "CORS")))

(defn- ^:async fetch-message
  "The message of the Error that fetch-text throws when js/fetch is `f`."
  [f]
  (let [real (.-fetch js/globalThis)]
    (set! (.-fetch js/globalThis) f)
    (try
      (await (ou/fetch-text boston))
      "no error"
      (catch :default e (.-message e))
      (finally (set! (.-fetch js/globalThis) real)))))

(deftest ^:async fetch-text-turns-a-failure-into-a-message
  (is (.includes (await (fetch-message (fn [_] (js/Promise.reject (js/TypeError. "Failed to fetch")))))
                 "CORS"))
  (is (.includes (await (fetch-message (fn [_] (js/Promise.resolve #js {:ok false :status 404}))))
                 "boston.cg: HTTP 404")))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
