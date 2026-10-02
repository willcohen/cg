;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.doc.edn
  "A minimal EDN reader and printer for the ^:cgproj overlay. A keyword reads as
   \":kw\", and the printer puts one entry on each line, for one-line drag diffs."
  (:require [clojure.string :as str]))

(defn kw-name
  "Removes the leading colon of a parsed-overlay keyword. Returns a plain
   string unchanged."
  [k]
  (if (and (string? k) (.startsWith k ":")) (subs k 1) k))

(defn kget
  "Look up keyword key `k` (given WITHOUT colon) in a parsed-overlay map."
  [m k]
  (when m (get m (str ":" k))))

(defn- ws? [ch]
  (or (= ch " ") (= ch "\n") (= ch "\t") (= ch "\r") (= ch ",")))

(defn- delim? [ch]
  (.includes "{}[]()\";" ch))

(defn- skip-ws [s i]
  (loop [j i]
    (cond
      (>= j (count s)) j
      (ws? (.charAt s j)) (recur (inc j))
      (= ";" (.charAt s j)) (let [nl (.indexOf s "\n" j)]
                              (if (neg? nl) (count s) (recur (inc nl))))
      :else j)))

(defn- token-end [s i]
  (loop [j i]
    (if (or (>= j (count s))
            (ws? (.charAt s j))
            (delim? (.charAt s j)))
      j
      (recur (inc j)))))

(defn- read-string-lit [s i]
  (loop [j (inc i) acc ""]
    (let [ch (.charAt s j)]
      (cond
        (= ch "") (throw (js/Error. "edn: unterminated string"))
        (= ch "\"") [acc (inc j)]
        (= ch "\\") (let [nx (.charAt s (inc j))]
                      (recur (+ j 2)
                             (str acc (case nx
                                        "n" "\n"
                                        "t" "\t"
                                        "r" "\r"
                                        nx))))
        :else (recur (inc j) (str acc ch))))))

(declare read-value)

(defn- read-coll [s i close]
  (loop [j (skip-ws s i) acc []]
    (cond
      (>= j (count s)) (throw (js/Error. "edn: unterminated collection"))
      (= close (.charAt s j)) [acc (inc j)]
      :else (let [[v k] (read-value s j)]
              (recur (skip-ws s k) (conj acc v))))))

(defn- read-map [s i]
  (let [[items next-i] (read-coll s i "}")]
    (when (odd? (count items))
      (throw (js/Error. "edn: map literal with odd number of forms")))
    ;; Built by mutation: squint inlines (assoc <loop-local-literal> k v) to an
    ;; object spread whose computed key has no [brackets].
    (let [m {}]
      (loop [idx 0]
        (when (< idx (count items))
          ;; aset on a JS object is squint-idiomatic; kondo types it array-only
          #_{:clj-kondo/ignore [:type-mismatch]}
          (aset m (nth items idx) (nth items (inc idx)))
          (recur (+ idx 2))))
      [m next-i])))

(def ^:private number-re #"^[+-]?(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?$")

(defn read-value
  "Reads one form from `s` at index `i` and returns [value next-index]. It drops
   metadata (which removes ^:cgproj) and reads a bare symbol as a string."
  [s i]
  (let [j (skip-ws s i)
        ch (.charAt s j)]
    (cond
      (= ch "") (throw (js/Error. "edn: unexpected EOF"))
      (= ch "^") (let [[_ k] (read-value s (inc j))]
                   (read-value s k))
      (= ch "{") (read-map s (inc j))
      (= ch "[") (read-coll s (inc j) "]")
      (= ch "(") (read-coll s (inc j) ")")
      (= ch "\"") (read-string-lit s j)
      :else
      (let [e (token-end s j)
            tok (subs s j e)]
        (cond
          (= tok "") (throw (js/Error. (str "edn: unexpected character " ch)))
          (= tok "true") [true e]
          (= tok "false") [false e]
          (= tok "nil") [nil e]
          (.test number-re tok) [(js/parseFloat tok) e]
          :else [tok e])))))

(defn read-overlay
  "Parse a raw OverlaySource block (with or without the ^:cgproj prefix)
   into the parsed-overlay representation."
  [raw]
  (let [[v _] (read-value raw 0)] v))

(defn- obj? [v]
  (and (some? v) (not (vector? v)) (= "object" (js* "typeof ~{}" v))))

(defn- print-str-lit [s]
  (str "\""
       (-> s
           (.replaceAll "\\" "\\\\")
           (.replaceAll "\"" "\\\"")
           (.replaceAll "\n" "\\n"))
       "\""))

(defn print-inline
  "Single-line EDN rendering of a parsed-overlay value."
  [v]
  (cond
    (nil? v) "nil"
    (true? v) "true"
    (false? v) "false"
    (string? v) (if (.startsWith v ":") v (print-str-lit v))
    (vector? v) (str "[" (str/join " " (map print-inline v)) "]")
    (obj? v) (str "{"
                  (str/join " "
                            (map (fn [[k vv]]
                                   (str (print-inline k) " " (print-inline vv)))
                                 (js/Object.entries v)))
                  "}")
    :else (str v)))

(declare ^:private render-overlay*)

(defn render-overlay
  "Renders a parsed-overlay map as a ^:cgproj block: one line for each top-level
   entry, and one line for each entry of a collection with more than one."
  [parsed]
  (if (zero? (count (js/Object.entries parsed)))
    "^:cgproj\n{}"
    (render-overlay* parsed)))

(defn- render-overlay* [parsed]
  (let [entries (js/Object.entries parsed)
        render-entries
        (fn [prefix open close es print-entry]
          (let [pad (.repeat " " (inc (count prefix)))]
            (str prefix open
                 (str/join (str "\n" pad) (map print-entry es))
                 close)))
        lines
        (map-indexed
         (fn [idx [k v]]
           (let [lead (if (zero? idx) "{" " ")
                 prefix (str lead (print-inline k) " ")]
             (cond
               (and (obj? v) (> (count (js/Object.entries v)) 1))
               (render-entries prefix "{" "}" (js/Object.entries v)
                               (fn [[ek ev]]
                                 (str (print-inline ek) " " (print-inline ev))))

               (and (vector? v) (> (count v) 1))
               (render-entries prefix "[" "]" v print-inline)

               :else (str prefix (print-inline v)))))
         entries)]
    (str "^:cgproj\n" (str/join "\n" lines) "}")))

(defn strip-keys
  "Converts parsed-overlay maps to squint maps, recursively: removes the colon
   of each keyword key."
  [v]
  (cond
    (vector? v) (mapv strip-keys v)
    (obj? v) (reduce (fn [acc [k vv]]
                       (assoc acc (kw-name k) (strip-keys vv)))
                     {}
                     (js/Object.entries v))
    :else v))
