;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.doc.literal
  "The edit of one scalar literal of a .cg text. A kind is the :kind of a parser
   field: string, number, keyword, boolean or nil.")

(def ^:private number-re (js/RegExp. "^[+-]?\\d+(\\.\\d+)?([eE][+-]?\\d+)?$"))
(def ^:private keyword-re (js/RegExp. "^:[\\w*+!?<>=.-][\\w*+!?<>=.'#-]*(/[\\w*+!?<>=.'#-]+)?$"))
(def ^:private string-escapes "\"\\ntrbf")

(defn field-text
  "The text that a box shows for the source text `raw`: a string without its
   quotes, each other kind as written."
  [kind raw]
  (let [s (str raw)]
    (if (and (= "string" kind) (>= (.-length s) 2))
      (.slice s 1 -1)
      s)))

(defn- string-source
  "The string literal whose content is `typed`, or nil. A bare quote gets its
   backslash. A backslash must start an escape that the reader takes."
  [typed]
  (let [n (.-length typed)]
    (loop [i 0 out "\""]
      (if (>= i n)
        (str out "\"")
        (let [c (.charAt typed i)
              nxt (.charAt typed (inc i))]
          (cond
            (= c "\"") (recur (inc i) (str out "\\\""))
            (not= c "\\") (recur (inc i) (str out c))
            (and (not= "" nxt) (.includes string-escapes nxt)) (recur (+ i 2) (str out c nxt))
            :else nil))))))

(defn- string-literal?
  [src]
  (and (>= (.-length src) 2)
       (.startsWith src "\"")
       (.endsWith src "\"")
       (= src (string-source (.slice src 1 -1)))))

(defn- scalar-source
  "`src` when it reads as one scalar literal, else nil."
  [src]
  (when (or (= src "nil") (= src "true") (= src "false")
            (.test number-re src)
            (.test keyword-re src)
            (string-literal? src))
    src))

(defn field-source
  "The source text of a literal of `kind` for the `typed` text of its box, or nil
   when the text does not read as that kind. A nil box takes each scalar kind."
  [kind typed]
  (let [s (str typed)
        t (.trim s)]
    (cond
      (= kind "string") (string-source s)
      (= kind "number") (when (.test number-re t) t)
      (= kind "keyword") (let [k (if (.startsWith t ":") t (str ":" t))]
                           (when (.test keyword-re k) k))
      (= kind "boolean") (when (or (= t "true") (= t "false")) t)
      (= kind "nil") (scalar-source t)
      :else nil)))

(defn- offset
  "The index in `text` of a 1-indexed line and column, or nil when the text
   has no such place."
  [text line column]
  (let [lines (.split text "\n")]
    (when (and (>= line 1) (<= line (.-length lines)))
      (let [row (aget lines (dec line))
            before (.reduce (.slice lines 0 (dec line))
                            (fn [n l] (+ n (.-length l) 1))
                            0)]
        (when (and (>= column 1) (<= column (inc (.-length row))))
          (+ before (dec column)))))))

(defn splice
  "`text` with the span `loc` replaced by `raw`, or nil when the span does not
   hold `was` (the text changed after the parse)."
  [text loc was raw]
  (let [a (offset text (aget loc "start-line") (aget loc "start-column"))
        b (offset text (aget loc "end-line") (aget loc "end-column"))]
    (when (and (some? a) (some? b) (= (str was) (.slice text a b)))
      (str (.slice text 0 a) raw (.slice text b)))))

(defn- string-end
  "The index of the quote that closes the string whose content starts at
   `i`, or -1."
  [text i]
  (let [n (.-length text)]
    (loop [j i]
      (cond
        (>= j n) -1
        (= "\\" (.charAt text j)) (recur (+ j 2))
        (= "\"" (.charAt text j)) j
        :else (recur (inc j))))))

(def ^:private closers #js {"(" ")" "[" "]" "{" "}"})

(defn unclosed
  "nil when each string and bracket of `text` is closed, else a short reason. The
   caller does not parse open text, because the parser gives a wrong flow for it."
  [text]
  (let [n (.-length text)
        stack #js []]
    (loop [i 0]
      (if (>= i n)
        (when (pos? (.-length stack))
          (str "an open " (aget stack (dec (.-length stack)))))
        (let [c (.charAt text i)]
          (cond
            (= c "\\") (recur (+ i 2))
            (= c ";") (let [nl (.indexOf text "\n" i)]
                        (recur (if (neg? nl) n (inc nl))))
            (= c "\"") (let [end (string-end text (inc i))]
                         (if (neg? end)
                           "an open string"
                           (recur (inc end))))
            (some? (aget closers c)) (do (.push stack c)
                                         (recur (inc i)))
            (or (= c ")") (= c "]") (= c "}"))
            (let [opener (.pop stack)]
              (cond
                (nil? opener) (str "a " c " with no open bracket")
                (= c (aget closers opener)) (recur (inc i))
                :else (str "an open " opener)))
            :else (recur (inc i))))))))
