;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.extract
  "Parses a flow with the tree-sitter parser and attaches the overlay
   positions as :visual {:pos [x y]}."
  (:require ["../../../../../cg/src/cg/parser/core.mjs" :as parser]
            ["../../../../shared/squint/src/cg_gui/doc/modules.mjs" :as doc-modules]
            [cg-app.ui.loader :as loader]))

(defn ^:async extract-flow
  "Promise<CgFlow>. opts must carry :grammar-wasm (path or URL) on first call."
  [text opts]
  (await (parser/extract-models text opts)))

(defn ^:async read-module-text
  "The text of the module at load path `path` from loader/module-url, or nil
   when the server does not have it."
  [path]
  (let [resp (await (js/fetch (loader/module-url path)))]
    (when (.-ok resp)
      (await (.text resp)))))

(defn ^:async follow-loaded-modules!
  "A new flow with the operations of each module that `flow` loads. The flow
   comes back unchanged when no module resolves."
  [flow opts]
  (let [extra (await (doc-modules/follow-loaded-modules!
                      flow
                      {:resolve identity
                       :read    read-module-text
                       :parse   (fn [text] (extract-flow text opts))}))]
    (if (zero? (count extra))
      flow
      ;; recheck removes the diagnostics from before the follow (the false
      ;; :unresolved-op marks for ops in other files) and resolves against the
      ;; merged registry.
      (parser/recheck
       (js/Object.assign #js {} flow
                         #js {:operations
                              (.concat (or (aget flow "operations") #js [])
                                       (into-array extra))})))))

(defn- unq [qn]
  (let [parts (.split (str qn) "/")]
    (aget parts (dec (.-length parts)))))

(defn- pos-for [positions ks]
  (loop [i 0]
    (when (< i (.-length ks))
      (or (aget positions (aget ks i))
          (recur (inc i))))))

(defn- with-visual [node p]
  (if p
    (js/Object.assign #js {} node #js {:visual #js {:pos p}})
    node))

(defn merge-positions-into-flow
  "flow + #js {key -> [x y]} -> a new flow with :visual on each matched
   function_call binding and threading op. No other node gets a position."
  [flow positions]
  (let [bindings (or (aget flow "bindings") #js [])
        merged
        (.map bindings
              (fn [b]
                (let [shape (aget b "body-shape")
                      bname (or (aget b "name") "anon")]
                  (cond
                    (= "function_call" shape)
                    (with-visual b (pos-for positions
                                            #js [(str "binding-" bname) bname]))

                    (= "threading_macro" shape)
                    (let [ops (or (aget b "ops") #js [])
                          mops (.map ops
                                     (fn [op i]
                                       (let [raw (str (unq (aget op "qualified-name")) "-" (inc i))]
                                         (with-visual op (pos-for positions
                                                                  #js [(str bname "__" raw) raw])))))]
                      (js/Object.assign #js {} b #js {:ops mops}))

                    :else b))))]
    (js/Object.assign #js {} flow #js {:bindings merged})))
