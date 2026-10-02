;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.extension.lifecycle
  (:require ["vscode" :as vscode]))

(defn push-disposable! [!state ^js disposable]
  (swap! !state update :extension/disposables conj disposable)
  (.push (.-subscriptions ^js (:extension/context @!state)) disposable))

(defn cleanup! [!state]
  (doseq [^js disposable (:extension/disposables @!state)]
    (.dispose disposable))
  (swap! !state assoc :extension/disposables []))

(defn register-command! [!state command-id f]
  (push-disposable! !state
    (vscode/commands.registerCommand command-id
      (fn [& args]
        (apply f !state args)))))

(defn make-nonce []
  (let [arr (js/Uint8Array. 16)]
    (js/crypto.getRandomValues arr)
    (.toString (js/Buffer.from arr) "base64")))

(defn set-when-context! [key value]
  (vscode/commands.executeCommand "setContext" key value))
