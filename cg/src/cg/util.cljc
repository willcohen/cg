;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.util
  ;; The SCI sandbox gives .cg scripts neither the clojure.core parsers nor
  ;; Long/parseLong. These wrappers give one parser on both platforms.
  (:refer-clojure :exclude [parse-long parse-double #?(:cljs js->clj)])
  #?(:clj (:require [clojure.java.io]
                    [clojure.edn :as edn])
     :cljs (:require ["squint-cljs" :as squint-compiler])))

#?(:clj (set! *warn-on-reflection* true))

#?(:cljs
   (def fs-module
     (when (and (exists? js/process)
                (exists? js/process.versions)
                (exists? js/process.versions.node))
       ;; Use interop, not js*. js* fails in the SCI of squint when this ns
       ;; loads as a dependency (the eval-free parser requires cg.util): SCI
       ;; cannot resolve the js* special form. Member access and exists?
       ;; compile to the same JS.
       (or
        ;; An embedder that loaded fs first wins, and cg.mjs keeps control.
        (.-nodeFs js/globalThis)
        ;; ESM has no require, and a static import of node:fs would go with
        ;; this ns into browser bundles. getBuiltinModule gives node:fs
        ;; synchronously from a module (node >= 20.16). Without it, an ESM
        ;; consumer that does not set globalThis.nodeFs gets nil, and each fs
        ;; call here takes its no-fs branch.
        (when (exists? js/process.getBuiltinModule)
          (.getBuiltinModule js/process "node:fs"))
        ;; CommonJS, for a node too old for getBuiltinModule.
        (try
          (let [req (or (.-require js/globalThis)
                        (when (exists? js/module) (.-require js/module)))]
            (when req (req "fs")))
          (catch :default _ nil))))))

(defn current-time-ms
  "Return the current time in milliseconds since the epoch."
  []
  #?(:clj (System/currentTimeMillis)
     :cljs (.now js/Date)))

(defn elapsed-ms
  "Return the milliseconds from start-time, a (current-time-ms) value, to
  now."
  [start-time]
  (- (current-time-ms) start-time))

(defn parse-long
  "Parse a string as a long, or return nil, as clojure.core/parse-long does."
  [s]
  (when (string? s)
    #?(:clj (try (Long/parseLong s)
                 (catch NumberFormatException _ nil))
       :cljs (when (re-matches #"[+-]?\d+" s)
               (let [n (js/parseInt s 10)]
                 (when-not (js/isNaN n) n))))))

(defn parse-double
  "Parse a string as a double, or return nil, as clojure.core/parse-double does."
  [s]
  (when (string? s)
    #?(:clj (try (Double/parseDouble s)
                 (catch NumberFormatException _ nil))
       :cljs (cond
               (re-matches #"[+-]?NaN" s) js/NaN
               (re-matches #"[+-]?Infinity" s) (js/parseFloat s)
               (re-matches #"[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?" s)
               (js/parseFloat s)
               :else nil))))

(defn try-catch
  "Return (f), or (on-error e) when f throws. On the JVM it catches Exception
  only, and an Error propagates."
  [f on-error]
  (try (f)
       (catch #?(:clj Exception :cljs :default) e
         (on-error e))))

(defn read-file
  "Read the file at path as a string. Throws in a browser, which has no file
  system."
  [path]
  #?(:clj (slurp path)
     :cljs (if fs-module
             (.readFileSync fs-module path "utf-8")
             (throw (js/Error. "File system access not available in browser environment")))))

(defn write-file!
  "Write the string content to the file at path, replacing it. Throws in a
  browser."
  [path content]
  #?(:clj (spit path content)
     :cljs (if fs-module
             (.writeFileSync fs-module path content "utf-8")
             (throw (js/Error. "File system access not available in browser environment")))))

(defn write-edn!
  "Write `value` to the file at path as EDN, replacing it. Throws in a
  browser."
  [path value]
  (write-file! path (pr-str value)))

(defn read-edn
  "Return the value of the EDN file at path. On cljs it evaluates the text:
  read only a file that cg or its user wrote. Throws in a browser."
  [path]
  #?(:clj (edn/read-string (read-file path))
     :cljs (let [compiled (.-javascript ((.-compileStringEx squint-compiler)
                                         (read-file path)
                                         #js {:context "expr"
                                              :elide-imports true
                                              :elide-exports true}))
                 ;; An eval through a local is an indirect eval: it runs in
                 ;; the global scope. EDN compiles to plain JS literals.
                 indirect-eval js/eval]
             (indirect-eval compiled))))

(defn file-exists?
  "Return true if a file exists at path. Returns false in a browser."
  [path]
  #?(:clj (.exists (clojure.java.io/file path))
     :cljs (if fs-module
             (.existsSync fs-module path)
             false)))

#?(:clj
   (defn cache-root
     "Return the cache root of cg: cg.cache.dir, else $CG_CACHE_DIR, else
      $XDG_CACHE_HOME/cg, else ~/.cache/cg. getenv is a fn of a variable name."
     ([] (cache-root #(System/getenv ^String %)))
     ([getenv]
      (or (not-empty (System/getProperty "cg.cache.dir"))
          (not-empty (getenv "CG_CACHE_DIR"))
          (some-> (not-empty (getenv "XDG_CACHE_HOME"))
                  (clojure.java.io/file "cg")
                  str)
          (str (clojure.java.io/file (System/getProperty "user.home")
                                     ".cache" "cg"))))))

(defn make-dirs!
  "Create the directory at path, with its parents. Returns true when a
  directory was created, and false in a browser."
  [path]
  #?(:clj (.mkdirs (clojure.java.io/file path))
     :cljs (if fs-module
             ;; With :recursive, mkdirSync returns the first directory it
             ;; created, or undefined when the directory already existed.
             (try (some? (.mkdirSync fs-module path {:recursive true}))
                  (catch :default _ false))
             false)))

(defn list-file-names
  "List the entry names (not paths) of the directory dir. Returns [] when
  the directory does not exist or cannot be read, and in a browser."
  [dir]
  #?(:clj (or (some-> (.list (clojure.java.io/file dir)) vec) [])
     :cljs (if fs-module
             (try (vec (.readdirSync fs-module dir))
                  (catch :default _ []))
             [])))

(defn delete-file!
  "Delete the file at path. Returns true when a file was deleted, and false
  in a browser."
  [path]
  #?(:clj (.delete (clojure.java.io/file path))
     :cljs (if fs-module
             (try (.unlinkSync fs-module path) true
                  (catch :default _ false))
             false)))

(defn log
  "Log a message to the console or stdout."
  [& args]
  #?(:clj (apply println args)
     :cljs (apply js/console.log args)))

(defn warn
  "Log a warning message."
  [& args]
  #?(:clj (binding [*out* *err*]
            (apply println "WARNING:" args))
     :cljs (apply js/console.warn args)))

(defn error
  "Log an error message."
  [& args]
  #?(:clj (binding [*out* *err*]
            (apply println "ERROR:" args))
     :cljs (apply js/console.error args)))

(defn exception-message
  "Return the message of an exception or error."
  [e]
  #?(:clj (.getMessage ^Throwable e)
     :cljs (.-message e)))

(defn throw-ex
  "Throw an ex-info with a message and an optional data map."
  ([msg]
   (throw-ex msg {}))
  ([msg data]
   #?(:clj (throw (ex-info msg data))
      :cljs (throw (ex-info msg data)))))

(defn deep-merge
  "Deep merge maps recursively."
  [& maps]
  (apply merge-with
         (fn [v1 v2]
           (if (and (map? v1) (map? v2))
             (deep-merge v1 v2)
             v2))
         maps))

;; Under squint a keyword and a symbol are plain strings. squint-cljs/core
;; does not export `keyword` or `symbol`, and its `name` accepts only strings.
;; These shims use the real constructors on the JVM and strings on CLJS. They
;; are here because cg.macros (generate-node-id) and cg.parser.extract
;; (body-graph node ids) must share one contract.

(defn unq-name
  "Return the unqualified name of x, a symbol, keyword or string: the text
   after a leading 'ns/' prefix."
  [x]
  (let [s #?(:clj  (cond
                     (string? x) x
                     (or (symbol? x) (keyword? x)) (name x)
                     :else (str x))
             :cljs (cond
                     (string? x) x
                     (and (some? x) (string? (.-name x))) (.-name x)
                     :else (str x)))
        idx (.indexOf ^String s "/")]
    (if (neg? idx) s (subs s (inc idx)))))

(defn as-sym
  "Return s as a symbol: a plain string on CLJS."
  ([s]     #?(:clj (symbol s)     :cljs (str s)))
  ([ns nm] #?(:clj (symbol ns nm) :cljs (str ns "/" nm))))

(defn as-kw
  "Return s as a keyword: a plain string on CLJS."
  [s]
  #?(:clj (keyword s) :cljs (str s)))

;; squint-cljs/core does not export `munge`. The compiler munges identifiers
;; at compile time (`intersects?` to `intersects_QMARK_`, `item->` to
;; `item__GT_`). This code applies the same rule at run time, to read a
;; property of a squint namespace object by its munged name.
#?(:cljs
   (def ^:private reserved-js-names
     "The names that squint gives a trailing $."
     #{"abstract" "arguments" "await" "boolean" "break" "byte" "case" "catch"
       "char" "class" "const" "constructor" "continue" "debugger" "default"
       "delete" "do" "double" "else" "enum" "eval" "export" "extends" "final"
       "finally" "float" "for" "function" "goto" "if" "implements" "import"
       "in" "instanceof" "int" "interface" "let" "long" "methods" "native"
       "new" "null" "package" "private" "protected" "public" "return" "short"
       "static" "super" "switch" "synchronized" "this" "throw" "throws"
       "transient" "try" "typeof" "var" "void" "volatile" "while" "with"
       "yield"}))

#?(:cljs
   (defn munge-name
     "Munge a Clojure identifier as squint does, giving a JS reserved word a
      trailing $."
     [s]
     (let [m (-> (str s)
                 (.replaceAll "->" "__GT_")
                 (.replaceAll "<-" "__LT_")
                 (.replaceAll "?" "_QMARK_")
                 (.replaceAll "!" "_BANG_")
                 (.replaceAll "*" "_STAR_")
                 (.replaceAll "+" "_PLUS_")
                 (.replaceAll "=" "_EQ_")
                 (.replaceAll ">" "_GT_")
                 (.replaceAll "<" "_LT_")
                 (.replaceAll "-" "_"))]
       (if (and (contains? reserved-js-names m)
                (not (contains? #{"import" "await"} m)))
         (str m "$")
         m))))

#?(:cljs
   (defn munge-alias
     "munge-name for a namespace alias: the name that squint compiles x/f
      against. squint gives import and await a trailing $ there too."
     [s]
     (let [m (munge-name s)]
       (if (contains? #{"import" "await"} m) (str m "$") m))))

#?(:cljs
   (defn js->clj
     "Convert a JS object or array to Clojure data, recursively. It ignores
      `:keywordize-keys`, because a squint keyword is a plain string."
     ([obj] (js->clj obj false))
     ([obj _kw-flag & _more]
      (cond
        (nil? obj) nil
        (array? obj) (mapv js->clj obj)
        ;; Walk plain objects only (constructor Object, or none for
        ;; Object.create(null)). A class instance passes through: a Date, an
        ;; Arquero table, or a compiler-side persistent map that a macro of
        ;; the eval in the browser puts in the op params. Object.keys on such
        ;; an instance gives its internal fields.
        (and (some? obj)
             (= "object" (js/typeof obj))
             (or (nil? (.-constructor obj))
                 (identical? (.-constructor obj) js/Object)))
        (persistent!
         (reduce
          (fn [m k]
            (assoc! m k (js->clj (unchecked-get obj k))))
          (transient {})
          (js/Object.keys obj)))
        :else obj))))
