;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.diagnostics.kondo
  "Builds the clj-kondo command for a .cg buffer and reads its JSON findings.
   The buffer goes on standard input, because clj-kondo skips .cg files.")

(defn argv
  "The clj-kondo arguments for the .cg buffer `path`. The cache is off, because
   all .cg vars go in `user` and a shared cache mixes the programs."
  [path]
  ["--lint" "-"
   "--filename" path
   "--cache" "false"
   "--config" "{:output {:format :json}}"])

(def ^:private level->severity
  {"error" :error
   "warning" :warning
   "info" :info})

(defn findings->diagnostic-data
  "clj-kondo JSON findings -> diagnostic maps with :source \"clj-kondo\".
   Lines and columns go from 1-based to 0-based."
  [findings]
  (->> (or findings [])
       (keep (fn [f]
               (let [row (:row f)
                     col (:col f)
                     end-row (max (or (:end-row f) row) row)
                     end-col (or (:end-col f) col)
                     end-col (if (= end-row row) (max end-col col) end-col)]
                 (when (and row col)
                   {:range {:start [(dec row) (dec col)]
                            :end   [(dec end-row) (dec end-col)]}
                    :code (str (:type f))
                    :severity (get level->severity (:level f) :error)
                    :message (str (:message f))
                    :source "clj-kondo"}))))
       vec))

(defn parse-output
  "Reads clj-kondo's stdout into diagnostic maps. Returns [] when the text is
   not JSON."
  [stdout]
  (if-let [parsed (try (js/JSON.parse (str stdout))
                       (catch :default _ nil))]
    (findings->diagnostic-data (:findings parsed))
    []))
