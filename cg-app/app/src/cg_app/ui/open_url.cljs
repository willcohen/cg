;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.open-url
  "Gets a .cg file from a URL, for the URL text box and the url query
   parameter of the page.")

(defn- raw-href
  "The raw.githubusercontent.com address of a github.com file page, because
   github.com sends no CORS header. Other addresses come back unchanged."
  [^js u]
  (let [parts (.split (.-pathname u) "/")]
    (if (and (= "github.com" (.-hostname u))
             (> (.-length parts) 5)
             (= "blob" (aget parts 3)))
      (str "https://raw.githubusercontent.com/"
           (.join (.concat (.slice parts 1 3) (.slice parts 4)) "/"))
      (.-href u))))

(defn source-url
  "#js {:url} for the address in `text`, or #js {:error} with the reason. A
   relative address resolves against `base`."
  [text base]
  (let [s (.trim (str (or text "")))
        u (when-not (= "" s)
            (try (if base (js/URL. s base) (js/URL. s))
                 (catch :default _ nil)))]
    (cond
      (= "" s) #js {:error "Type the URL of a .cg file."}
      (nil? u) #js {:error (str "The text is not a URL: " s)}
      (not (.includes #js ["http:" "https:"] (.-protocol u)))
      #js {:error "Use a URL that starts with https:// or http://."}
      :else #js {:url (raw-href u)})))

(defn file-name
  "The last part of the path of `url`, or untitled.cg when the path has none."
  [url]
  (let [parts (.filter (.split (.-pathname (js/URL. url)) "/") (fn [p] (not= "" p)))
        last-part (when (pos? (.-length parts)) (aget parts (dec (.-length parts))))]
    (if last-part
      (try (js/decodeURIComponent last-part) (catch :default _ last-part))
      "untitled.cg")))

(defn url-param
  "The url query parameter in `search`, or nil when it is absent or empty."
  [search]
  (let [v (.get (js/URLSearchParams. (or search "")) "url")]
    (when (and v (not= "" v)) v)))

(defn page-href
  "`href` with its url query parameter set to `url`, or with no url parameter
   when `url` is nil."
  [href url]
  (let [u (js/URL. href)
        params (.-searchParams u)]
    (if url (.set params "url" url) (.delete params "url"))
    (.-href u)))

(defn http-error
  "The message for an HTTP `status` that is not ok."
  [name status]
  (if (= 404 status)
    (str name ": HTTP 404. The server has no file at this URL.")
    (str name ": HTTP " status " from the server.")))

(defn network-error
  "The message for a fetch that failed with no HTTP status."
  [name]
  (str name ": the browser cannot read this URL. The server is not available,"
       " or it does not let other sites read the file (CORS)."))

(defn ^:async fetch-text
  "The text of the file at `url`. Throws an Error with a message for the
   reader when the fetch fails."
  [url]
  (let [name (file-name url)
        resp (try (await (js/fetch url))
                  (catch :default _
                    (throw (js/Error. (network-error name)))))]
    (if (.-ok resp)
      (await (.text resp))
      (throw (js/Error. (http-error name (.-status resp)))))))
