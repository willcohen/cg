;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.messages-test
  "The sources of each view and host against the message list. A sender
   sends only listed types, a receiver has a case for each listed type sent
   to it, and a receiver has no case for a type that nobody lists."
  (:require [cljs.test :as t :refer [deftest is]]
            ["node:fs" :as fs]
            ["node:path" :as path]
            ["node:url" :refer [fileURLToPath]]))

(def messages
  "Each cg/* message that crosses a postMessage boundary: its channel, sender
   role, receiver role and fields. Channels: canvas (the canvas and its host),
   map (the cg-vscode map webview and its host), native (the web shell and a
   React Native shell). A field that ends in ? is optional. :unhandled says why
   an entry has no receiver case; :unhandled-by names each host of an edit type
   that has no case for it."
  [{:type "cg/ready" :channel "canvas" :from "view" :to "host"}
   {:type "cg/edit" :channel "canvas" :from "view" :to "host"
    :fields ["editType" "model" "payload"]
    :edit-types {"DragNode" {:fields ["nodeId" "position"]}
                 "SetLiteral" {:fields ["name" "location" "was" "text"]}
                 "EditReportSpec"
                 {:fields ["nodeId" "spec"]
                  :unhandled "The form edits stay scratch until a report spec can be written back into the .cg text."}}}
   {:type "cg/run" :channel "canvas" :from "view" :to "host"}
   {:type "cg/select-model" :channel "canvas" :from "view" :to "host" :fields ["model"]}
   {:type "cg/open-defop" :channel "canvas" :from "view" :to "host"
    :fields ["path" "location"]}

   {:type "cg/flow" :channel "canvas" :from "host" :to "view" :fields ["flow"]}
   {:type "cg/run-model" :channel "canvas" :from "host" :to "view" :fields ["model"]}
   {:type "cg/run-started" :channel "canvas" :from "host" :to "view"
    :fields ["file?" "pid?" "execution-id?"]}
   {:type "cg/run-output" :channel "canvas" :from "host" :to "view"
    :fields ["stream" "text"]}
   {:type "cg/trace-event" :channel "canvas" :from "host" :to "view"
    :fields ["event"]}
   {:type "cg/run-ended" :channel "canvas" :from "host" :to "view"
    :fields ["outcome" "count?" "exitCode?"]}
   {:type "cg/run-error" :channel "canvas" :from "host" :to "view"
    :fields ["message"]}
   {:type "cg/cursor" :channel "canvas" :from "host" :to "view" :fields ["line"]}
   {:type "cg/reveal-line" :channel "canvas" :from "host" :to "view"
    :fields ["line"]}
   {:type "cg/theme-kind" :channel "canvas" :from "host" :to "view"
    :fields ["mode"]}
   {:type "cg/jvm-vars" :channel "canvas" :from "host" :to "view"
    :fields ["file" "vars"]
    :unhandled "The canvas has no row-count display for binding nodes yet."}

   {:type "cg/run" :channel "map" :from "host" :to "view"
    :fields ["text" "flow" "model?" "file?"]}
   {:type "cg/layers" :channel "map" :from "host" :to "view" :fields ["layers"]}
   {:type "cg/status" :channel "map" :from "host" :to "view" :fields ["text"]}
   {:type "cg/module-text" :channel "map" :from "host" :to "view"
    :fields ["id" "path" "text?" "error?"]}

   {:type "cg/ready" :channel "map" :from "view" :to "host"}
   {:type "cg/run-started" :channel "map" :from "view" :to "host" :fields ["model?"]}
   {:type "cg/read-module" :channel "map" :from "view" :to "host" :fields ["id" "path"]}
   {:type "cg/map-ready" :channel "map" :from "view" :to "host"
    :fields ["online" "layer" "count"]}
   {:type "cg/run-result" :channel "map" :from "view" :to "host"
    :fields ["ok" "count" "mapped" "layer?"]}
   {:type "cg/run-error" :channel "map" :from "view" :to "host" :fields ["error"]}
   {:type "cg/layers-rendered" :channel "map" :from "view" :to "host"
    :fields ["count" "vars"]}
   {:type "cg/viewport-settled" :channel "map" :from "view" :to "host"
    :fields ["bbox"]}
   {:type "cg/map-error" :channel "map" :from "view" :to "host" :fields ["error"]}
   {:type "cg/csp-violation" :channel "map" :from "view" :to "host"
    :fields ["directive" "blocked"]}
   {:type "cg/trace-event" :channel "map" :from "view" :to "host"
    :fields ["event"]}

   {:type "cg/backend-url" :channel "native" :from "native" :to "web"
    :fields ["url"]}
   {:type "cg/backend-flapping" :channel "native" :from "native" :to "web"}
   {:type "cg/load-file" :channel "native" :from "native" :to "web"
    :fields ["name" "content"]}
   {:type "cg/new" :channel "native" :from "native" :to "web"}
   {:type "cg/save" :channel "native" :from "native" :to "web"}
   {:type "cg/file-save-request" :channel "native" :from "web" :to "native"
    :fields ["name" "content"]}])

(def ^:private root
  (path/resolve (path/dirname (fileURLToPath (js* "import.meta.url")))
                "../../../../.."))

(def ^:private canvas-dir "cg-app/shared/squint/src/cg_gui/canvas")

(defn- canvas-files []
  (->> (fs/readdirSync (path/join root canvas-dir))
       (filter (fn [f] (.endsWith f ".cljs")))
       (mapv (fn [f] (str canvas-dir "/" f)))))

(defn- participants []
  [{:name "canvas" :channel "canvas" :role "view" :files (canvas-files)}
   {:name "cg-vscode editor" :channel "canvas" :role "host"
    :files ["cg-vscode/src/cg_vscode/editor.cljs"
            "cg-vscode/src/cg_vscode/runtime.cljs"
            "cg-vscode/src/cg_vscode/exec_webview.cljs"
            "cg-vscode/src/cg_vscode/run_relay.cljs"]}
   {:name "web shell" :channel "canvas" :role "host"
    :files ["cg-app/app/src/cg_app/ui/app.cljs"
            "cg-app/app/src/cg_app/ui/execution.cljs"]}
   {:name "map webview" :channel "map" :role "view"
    :files ["cg-vscode/src/cg_vscode/exec_html.cljs"]}
   {:name "map host" :channel "map" :role "host"
    :files ["cg-vscode/src/cg_vscode/exec_webview.cljs"
            "cg-vscode/src/cg_vscode/run_relay.cljs"]}
   {:name "web shell (native bridge)" :channel "native" :role "web"
    :files ["cg-app/app/src/cg_app/ui/app.cljs"
            "cg-app/app/src/cg_app/ui/save.cljs"]}])

;; The map webview script is JavaScript inside a squint string. Each
;; direction has a squint pattern and a JavaScript pattern.
(def ^:private sent-patterns
  [":type +\"(cg/[a-z-]+)\""
   "type: *'(cg/[a-z-]+)'"])

(def ^:private received-patterns
  ["\\(= +t +\"(cg/[a-z-]+)\"\\)"
   "^ *\"(cg/[a-z-]+)\" *$"
   "m\\.type *=== *'(cg/[a-z-]+)'"])

(def ^:private sent-edit-patterns [":editType +\"([A-Za-z]+)\""])

(def ^:private received-edit-patterns
  ["\\(= +\"([A-Za-z]+)\" +\\(aget m \"editType\"\\)\\)"
   "^ *\"([A-Z][A-Za-z]+)\" *$"])

(defn- read-source [f]
  (fs/readFileSync (path/join root f) "utf8"))

(defn- captures [pattern text]
  (mapv (fn [hit] (aget hit 1))
        (js/Array.from (.matchAll text (js/RegExp. pattern "gm")))))

(defn- scan [patterns files]
  (vec (distinct (mapcat (fn [f]
                           (let [text (read-source f)]
                             (mapcat (fn [p] (captures p text)) patterns)))
                         files))))

(defn- listed-types
  "The types on `channel` whose `end` (\"from\" or \"to\") is `role`."
  [channel end role]
  (->> messages
       (filter (fn [e] (and (= channel (get e "channel")) (= role (get e end)))))
       (mapv (fn [e] (get e "type")))))

(defn- all-files []
  (vec (distinct (mapcat :files (participants)))))

(defn- roles-of [file]
  (filterv (fn [p] (some (fn [f] (= f file)) (:files p))) (participants)))

(defn- unlisted
  "[file type] pairs that `file` sends or receives with no entry for any
   role that the file has."
  [patterns end]
  (vec (mapcat (fn [file]
                 (let [ok (mapcat (fn [p] (listed-types (:channel p) end (:role p)))
                                  (roles-of file))]
                   (->> (scan patterns [file])
                        (remove (fn [ty] (some (fn [o] (= o ty)) ok)))
                        (map (fn [ty] [file ty])))))
               (all-files))))

(deftest each-sent-type-is-listed
  (is (= [] (unlisted sent-patterns "from"))))

(deftest each-received-type-is-listed
  (is (= [] (unlisted received-patterns "to"))))

(defn- unreceived []
  (vec (mapcat (fn [e]
                 (when-not (get e "unhandled")
                   (->> (participants)
                        (filter (fn [p] (and (= (:channel p) (get e "channel"))
                                             (= (:role p) (get e "to")))))
                        (remove (fn [p] (some (fn [ty] (= ty (get e "type")))
                                              (scan received-patterns (:files p)))))
                        (map (fn [p] [(:name p) (get e "type")])))))
               messages)))

(deftest each-listed-type-has-a-receiver-case
  (is (= [] (unreceived))))

(defn- edit-types []
  (let [e (first (filter (fn [e] (= "cg/edit" (get e "type"))) messages))]
    (get e "edit-types")))

(defn- canvas-hosts []
  (filterv (fn [p] (and (= "canvas" (:channel p)) (= "host" (:role p))))
           (participants)))

(deftest edit-types-match-the-list
  (let [listed (js/Object.keys (edit-types))
        listed? (fn [k] (some (fn [l] (= l k)) listed))]
    (is (= [] (vec (remove listed? (scan sent-edit-patterns (canvas-files))))))
    (doseq [p (canvas-hosts)]
      (let [received (scan received-edit-patterns (:files p))]
        (is (= [] (vec (remove listed? received))) (:name p))
        (is (= [] (vec (remove (fn [k]
                                 (or (get (get (edit-types) k) "unhandled")
                                     (get (get (get (edit-types) k) "unhandled-by") (:name p))
                                     (some (fn [r] (= r k)) received)))
                               listed)))
            (:name p))))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
