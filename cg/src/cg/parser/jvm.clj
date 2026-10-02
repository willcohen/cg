;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.jvm
  "JVM backend: web-tree-sitter in a GraalVM polyglot Context. Must stay free
   of eval: do not require cg.dsl, cg.macros, cg.run or cg.codegen."
  (:require [clojure.java.io :as io])
  (:import [org.graalvm.polyglot Context PolyglotAccess Value]
           [org.graalvm.polyglot.proxy ProxyArray ProxyExecutable]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent CompletableFuture]))

(set! *warn-on-reflection* true)

(def ^:private grammar-wasm-resource
  "node_modules/@yogthos/tree-sitter-clojure/tree-sitter-clojure.wasm")

(def ^:private runtime-wasm-resource
  "node_modules/web-tree-sitter/web-tree-sitter.wasm")

(def ^:private web-tree-sitter-cjs
  "node_modules/web-tree-sitter/web-tree-sitter.cjs")

;; bb jar puts the three files in this classpath dir, because a user of the
;; jar has no node_modules.
(def ^:private bundled-dir "cg/parser/tree-sitter/")

(defn- unpack-bundled
  "Copy the parser files of the classpath to a new temp folder in the
   node_modules layout, and return the folder. Nil when the classpath has none."
  ^String []
  (when (io/resource (str bundled-dir (.getName (io/file web-tree-sitter-cjs))))
    ;; A temp folder, because GraalJS require() reads a file path and cannot
    ;; read a jar entry.
    (let [root (.toFile (Files/createTempDirectory "cg-parser" (make-array FileAttribute 0)))]
      (doseq [path [web-tree-sitter-cjs runtime-wasm-resource grammar-wasm-resource]
              :let [target (io/file root path)]]
        (io/make-parents target)
        (with-open [in (io/input-stream (io/resource (str bundled-dir (.getName target))))]
          (io/copy in target)))
      (.addShutdownHook (Runtime/getRuntime)
                        (Thread. ^Runnable (fn []
                                             (run! #(io/delete-file % true)
                                                   (reverse (file-seq root))))))
      (str root))))

(defn- node-modules-root
  "The working directory, or else the cg checkout that holds this file, when
   only the checkout has web-tree-sitter in node_modules. cg-app and cg-vscode
   take cg by :local/root and run in their own folders."
  ^String []
  (let [has? (fn [dir] (.exists (io/file dir web-tree-sitter-cjs)))
        cwd (System/getProperty "user.dir")
        src (io/resource "cg/parser/jvm.clj")
        checkout (when (= "file" (some-> src .getProtocol))
                   ;; src/cg/parser/jvm.clj -> the folder that holds src/
                   (-> (io/file (.toURI src)) .getParentFile .getParentFile
                       .getParentFile .getParent))]
    (if (or (has? cwd) (not (and checkout (has? checkout))))
      cwd
      checkout)))

(defn- package-root
  "The folder whose node_modules holds web-tree-sitter. The files on the
   classpath come first, and node_modules second."
  ^String []
  (or (unpack-bundled) (node-modules-root)))

(defn- read-bytes [^String root ^String path]
  (with-open [in (io/input-stream (io/file root path))]
    (.readAllBytes in)))

;; The wasm files go to JS as ProxyArrays: GraalJS has no event loop to
;; resolve a Promise that depends on async file I/O.
(defn- bytes->proxy ^ProxyArray [^bytes ba]
  (ProxyArray/fromArray
   (object-array
    (map #(Long/valueOf (bit-and ^long (long %) 0xff)) ba))))

;; Keep the options equal to net.willcohen.native.wasm in clj-native, to
;; prevent Truffle version skew when both libraries load into one JVM.
(defn new-context
  "Builds a GraalVM Context for JS and WASM with CommonJS resolution from
   `root`. Host I/O is on, because the CommonJS loader must resolve require()
   paths."
  ^Context [^String root]
  (-> (Context/newBuilder (into-array String ["js" "wasm"]))
      (.allowPolyglotAccess PolyglotAccess/ALL)
      (.allowIO true)
      (.allowExperimentalOptions true)
      (.option "js.ecmascript-version" "staging")
      (.option "js.esm-eval-returns-exports" "true")
      (.option "js.webassembly" "true")
      (.option "js.commonjs-require" "true")
      (.option "js.commonjs-require-cwd" root)
      (.out System/out)
      (.err System/err)
      (.build)))

(defn- boot-source
  "The boot shim for the web-tree-sitter under `root`. The require path is
   absolute, because GraalJS resolves a require path against the process
   folder."
  ^String [^String root]
  ;; The shim returns a function that copies the ProxyArray bytes into
  ;; Uint8Arrays, to give Parser.init and Language.load the bytes with no
  ;; fetch or fs. JS runs the Promise chain on its microtask queue, and the
  ;; host thread waits on a CompletableFuture that the callbacks complete.
  (str
   "(function () {\n"
   "  const ts = require(" (pr-str (str (io/file root web-tree-sitter-cjs))) ");\n"
   "  function copyBytes(proxy) {\n"
   "    const n = proxy.length;\n"
   "    const out = new Uint8Array(n);\n"
   "    for (let i = 0; i < n; i++) out[i] = proxy[i];\n"
   "    return out;\n"
   "  }\n"
   "  return function initialize(runtimeBytes, grammarBytes, onSuccess, onError) {\n"
   "    (async () => {\n"
   "      try {\n"
   "        const r = copyBytes(runtimeBytes);\n"
   "        const g = copyBytes(grammarBytes);\n"
   "        await ts.Parser.init({ wasmBinary: r });\n"
   "        const language = await ts.Language.load(g);\n"
   "        onSuccess({ Parser: ts.Parser, language: language });\n"
   "      } catch (e) {\n"
   "        onError(String((e && e.stack) || e));\n"
   "      }\n"
   "    })();\n"
   "  };\n"
   "})()"))

(defonce ^:private state (atom nil))

(defn init!
  "Create the GraalVM Context on the first call, load the runtime and grammar
   wasm bytes, and wait for the JS side. Stores the Parser constructor and
   the Language for parse. A later call returns the stored state."
  []
  (locking state
    (when-not @state
      (let [root (package-root)
            ctx (new-context root)
            initialize-fn (.eval ctx "js" (boot-source root))
            runtime (bytes->proxy (read-bytes root runtime-wasm-resource))
            grammar (bytes->proxy (read-bytes root grammar-wasm-resource))
            fut (CompletableFuture.)
            on-success (reify ProxyExecutable
                         (execute [_ args]
                           (.complete fut (when (pos? (alength args)) (aget args 0)))
                           nil))
            on-error (reify ProxyExecutable
                       (execute [_ args]
                         (let [m (if (pos? (alength args)) (str (aget args 0)) "unknown")]
                           (.completeExceptionally fut
                                                   (ex-info (str "tree-sitter init failed: " m)
                                                            {:cause m})))
                         nil))]
        (.executeVoid initialize-fn
                      (object-array [runtime grammar on-success on-error]))
        (let [booted ^Value (.get fut)]
          (reset! state {:context ctx
                         :parser-ctor (.getMember booted "Parser")
                         :language (.getMember booted "language")})))))
  @state)

(defn parse
  "Parse Clojure source text into a tree-sitter Tree (a GraalVM Value).
   `opts` is ignored: the grammar comes from the classpath or node_modules
   (see package-root)."
  ([^String text] (parse text nil))
  ([^String text _opts]
   (let [{:keys [parser-ctor ^Value language]} (init!)
         parser (.newInstance ^Value parser-ctor (object-array []))]
     (.invokeMember parser "setLanguage" (object-array [language]))
     (.invokeMember parser "parse" (object-array [text])))))
