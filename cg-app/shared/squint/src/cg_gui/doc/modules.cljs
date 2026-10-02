;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.doc.modules
  "Collects the ops of each module that a flow loads, directly or not. `host` has
   :resolve (path -> id or nil), :read (id -> Promise of text) and :parse.")

(defn module-label-from-path
  "\"src/cg/my-ops.cg\" -> \"My Ops\": the tab label of a module in the toolbar."
  [path-str]
  (let [parts (.split (str path-str) "/")
        base0 (aget parts (dec (.-length parts)))
        base  (if (.endsWith base0 ".cg") (.slice base0 0 -3) base0)]
    (-> (.split base "-")
        (.map (fn [s] (if (zero? (.-length s))
                        s
                        (str (.toUpperCase (.charAt s 0)) (.slice s 1)))))
        (.join " "))))

(defn- ^:async parse-module
  "{:ops :modules} of one module: its ops tagged :source-module and the modules it
   loads. Empty on an error, because one bad module must not stop the render."
  [host id label]
  (try
    (let [text (await ((:read host) id))]
      (if-not text
        {:ops [] :modules []}
        (let [sub    (await ((:parse host) text))
              ;; The main flow calls these ops through its require alias, and
              ;; the checker matches them by the ns name of the module.
              mod-ns (some-> sub :module-ns :name)]
          {:ops (mapv (fn [op]
                        (assoc op :source-module {:path id :label label :ns mod-ns}))
                      (or (:operations sub) []))
           :modules (or (:loaded-modules sub) [])})))
    (catch :default _ {:ops [] :modules []})))

(defn- unseen-entries
  "The modules in `ms` that resolve and that are not in `seen`. Adds each one
   to `seen`: a load cycle then ends, and a shared module is read one time."
  [host ms seen]
  (vec (keep (fn [m]
               (let [p (:path m)]
                 (when-let [id ((:resolve host) p)]
                   (when-not (.has seen id)
                     (.add seen id)
                     {:id id :label (module-label-from-path p)}))))
             ms)))

(defn ^:async follow-loaded-modules!
  "Promise of the ops of each module that `flow` loads, directly or through other
   modules, each tagged :source-module {:path :label :ns}."
  [flow host]
  (let [seen (js/Set.)
        out  #js []]
    (loop [entries (unseen-entries host (or (:loaded-modules flow) []) seen)]
      (when (seq entries)
        (let [subs   (await (js/Promise.all
                             (into-array
                              (mapv (fn [e] (parse-module host (:id e) (:label e)))
                                    entries))))
              nested #js []]
          (doseq [s subs]
            (doseq [op (:ops s)] (.push out op))
            (doseq [m (:modules s)] (.push nested m)))
          (recur (unseen-entries host (vec nested) seen)))))
    (vec out)))
