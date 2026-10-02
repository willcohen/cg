;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.doc.overlay
  "The write half of the ^:cgproj overlay codec. A key that exists keeps its
   form, and a first drag adds its key as the React Flow node id."
  (:require [clojure.string :as str]
            [cg-gui.doc.edn :as edn]))

(defn- splice-overlay
  "Replace the overlay region of `text` with `rendered`, using the
   parser-supplied 1-indexed (start-line, end-line) span."
  [text start-line end-line rendered]
  (let [lines (str/split-lines text)
        before (str/join "\n" (take (dec start-line) lines))
        after  (let [after-lines (drop end-line lines)]
                 (if (seq after-lines)
                   (str "\n" (str/join "\n" after-lines))
                   ""))]
    (str before
         (when (pos? (count before)) "\n")
         rendered
         after)))

(defn- update-overlay-positions
  "Applies a {node-id-string -> [x y]} map to the :nodes of the parsed overlay.
   Updates existing entries and adds ids that are not present."
  [parsed positions]
  (let [existing (or (edn/kget parsed "nodes") {})
        entries  (js/Object.entries existing)
        updated  (reduce (fn [acc [id-k props]]
                           (let [pos (get positions (edn/kw-name id-k))]
                             (assoc acc id-k
                                    (if pos (assoc props ":pos" (vec pos)) props))))
                         {}
                         entries)
        present  (set (map (fn [[id-k _]] (edn/kw-name id-k)) entries))
        added    (reduce (fn [acc [id pos]]
                           (if (contains? present id)
                             acc
                             (assoc acc id {":pos" (vec pos)})))
                         {}
                         (js/Object.entries positions))]
    (assoc parsed ":nodes" (merge updated added))))

(defn update-visual-metadata
  "Writes `positions` (node id -> [x y]) into the overlay block of `text`, or
   appends a new block when `flow` (the parsed CgFlow) has no overlay."
  [text flow positions]
  (let [overlay (aget flow "overlay")]
    (if overlay
      (let [location (aget overlay "location")
            parsed   (edn/read-overlay (aget overlay "raw"))
            updated  (update-overlay-positions parsed positions)
            rendered (str (edn/render-overlay updated) "\n")]
        (splice-overlay text
                        (aget location "start-line")
                        (aget location "end-line")
                        rendered))
      (let [nodes    (reduce (fn [m [id pos]] (assoc m id {":pos" (vec pos)}))
                             {}
                             (js/Object.entries positions))
            rendered (str (edn/render-overlay {":nodes" nodes}) "\n")]
        (str (str/trimr text) "\n\n" rendered)))))
