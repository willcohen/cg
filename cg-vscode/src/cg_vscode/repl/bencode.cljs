;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.repl.bencode
  "Bencode, the wire format of nREPL. A string is text, and its length counts
   UTF-8 bytes.")

(defn encode
  "The bencode of `x`: a string, an integer, an array, or an object as a dict,
   with sorted keys and no nil value."
  [x]
  (cond
    (string? x) (str (js/Buffer.byteLength x "utf8") ":" x)
    (number? x) (str "i" (js/Math.trunc x) "e")
    (js/Array.isArray x) (str "l" (.join (.map x (fn [v] (encode v))) "") "e")
    (some? x) (str "d"
                   (.join (.map (.filter (.sort (js/Object.keys x)) (fn [k] (some? (aget x k))))
                                (fn [k] (str (encode k) (encode (aget x k)))))
                          "")
                   "e")
    :else ""))

(declare read-at)

(defn- read-list
  "#js [array next-index] of the list items from `i`, or nil when `buf` ends
   first."
  [^js buf i]
  (loop [i i out #js []]
    (cond
      (>= i (.-length buf)) nil
      (= 101 (aget buf i)) #js [out (inc i)]
      :else (when-let [r (read-at buf i)]
              (.push out (aget r 0))
              (recur (aget r 1) out)))))

(defn- read-dict
  "#js [object next-index] of the dict entries from `i`, or nil when `buf`
   ends first."
  [^js buf i]
  (loop [i i out #js {}]
    (cond
      (>= i (.-length buf)) nil
      (= 101 (aget buf i)) #js [out (inc i)]
      :else (when-let [k (read-at buf i)]
              (when-let [v (read-at buf (aget k 1))]
                (aset out (aget k 0) (aget v 0))
                (recur (aget v 1) out))))))

(defn- read-string-at [^js buf i]
  (let [colon (.indexOf buf 58 i)]
    (when (>= colon 0)
      (let [start (inc colon)
            end (+ start (js/parseInt (.toString buf "latin1" i colon) 10))]
        (when (<= end (.-length buf))
          #js [(.toString buf "utf8" start end) end])))))

(defn- read-at
  "#js [value next-index] of the bencode at `i` of the Buffer `buf`, or nil
   when `buf` ends first."
  [^js buf i]
  (when (< i (.-length buf))
    (case (aget buf i)
      105 (let [e (.indexOf buf 101 i)]
            (when (>= e 0)
              #js [(js/parseInt (.toString buf "latin1" (inc i) e) 10) (inc e)]))
      108 (read-list buf (inc i))
      100 (read-dict buf (inc i))
      (read-string-at buf i))))

(defn decode-all
  "#js [values tail]: the complete bencode values at the start of the Buffer
   `buf`, and the rest of `buf`, which a later chunk completes."
  [^js buf]
  (loop [i 0 out #js []]
    (if-let [r (read-at buf i)]
      (do (.push out (aget r 0))
          (recur (aget r 1) out))
      #js [out (.subarray buf i)])))
