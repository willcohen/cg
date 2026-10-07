;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.bencode-test
  "The bencode of the nREPL wire."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../src/cg_vscode/repl/bencode.mjs" :as b]))

(defn- decode [s] (b/decode-all (js/Buffer.from s "utf8")))

(deftest a-message-goes-there-and-back
  (let [[msgs tail] (decode (b/encode #js {:op "eval" :code "(+ 1 2)" :n 3 :l #js ["a" 1]}))
        m (first msgs)]
    (is (= 1 (count msgs)))
    (is (= "eval" (aget m "op")))
    (is (= "(+ 1 2)" (aget m "code")))
    (is (= 3 (aget m "n")))
    (is (= ["a" 1] (vec (aget m "l"))))
    (is (zero? (.-length tail)))))

(deftest a-length-counts-bytes
  (is (= "5:été" (b/encode "été")))
  (is (= "été" (first (first (decode "5:été"))))))

(deftest a-dict-has-sorted-keys-and-no-nil
  (is (= "d1:ai1e1:bi2ee" (b/encode #js {:b 2 :a 1 :c nil}))))

(deftest a-partial-message-waits-for-the-next-chunk
  (let [whole (b/encode #js {:op "clone"})
        [msgs tail] (decode (str (b/encode #js {:op "a"}) (.slice whole 0 5)))]
    (is (= ["a"] (mapv (fn [m] (aget m "op")) msgs)))
    (is (= 5 (.-length tail)))))

(.then (js/Promise.resolve (t/run-tests)) #(js/process.exit (if (t/successful? %) 0 1)))
