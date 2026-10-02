;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.wasmts-setup
  "One-time wasmts setup for the dual-runtime .cljc test suites. cljs side
   only.

   The setup has four steps. Read the wasm binary. Patch globalThis.fetch
   to serve it. Import wasmts.js, which installs globalThis.wasmts. Then
   wait for the init to complete.

   wasmts must start before any cg module that builds a geometry. wasmts fills
   globalThis.wasmts on import, but it initializes asynchronously. A geometry
   call that runs too early fails, because globalThis.wasmts.geom is not
   defined. Because of this, each suite awaits this fn in its footer, before
   run-tests.

   JVM callers do not use this ns, because JTS is on the classpath."
  #?(:cljs (:require ["node:fs" :as fs]
                     ["node:path" :as node-path])))

#?(:clj (set! *warn-on-reflection* true))

#?(:cljs
   (def wasmts-js-path
     "The wasmts loader bundle. The path is absolute, because a worker imports
      it as a module specifier and node reads a relative path as a package
      name. Paths resolve against process.cwd(). Run the test suites from the
      module root."
     (node-path/resolve "node_modules/@wcohen/wasmts/dist/wasmts.js")))

#?(:cljs
   (def wasmts-wasm-path
     (node-path/resolve "node_modules/@wcohen/wasmts/dist/wasmts.js.wasm")))

#?(:cljs
   (defn wasmts-assets
     "The loader URL and the .wasm bytes, with the keys cg.run/init-cg-workers!
      uses. A worker is a separate JS context. It cannot see the main-thread
      fetch shim below. Because of this, it needs the bytes."
     []
     {:wasmts-js-url      wasmts-js-path
      :wasmts-wasm-binary (fs/readFileSync wasmts-wasm-path)}))

#?(:cljs
   (defn ^:async init-wasmts!
     "Install globalThis.wasmts (single-threaded, main-thread). Idempotent:
      a second call returns the already-loaded module. Paths resolve against
      process.cwd(), so run test suites from the repo root."
     []
     (if (and (.-wasmts js/globalThis) (.. js/globalThis -wasmts -geom))
       js/globalThis.wasmts
       (let [wasm-js    wasmts-js-path
             wasm-path  wasmts-wasm-path
             wasm-bin   (fs/readFileSync wasm-path)
             orig-fetch (.-fetch js/globalThis)]
         (set! (.-__filename js/globalThis) wasm-js)
         (set! (.-fetch js/globalThis)
               (fn [url]
                 (if (and url
                          (or (.endsWith url ".wasm")
                              (.includes url "wasmts.js.wasm")))
                   (js/Promise.resolve
                    #js {:ok true
                         :arrayBuffer
                         (fn []
                           (js/Promise.resolve
                            (.slice (.-buffer wasm-bin)
                                    (.-byteOffset wasm-bin)
                                    (+ (.-byteOffset wasm-bin)
                                       (.-byteLength wasm-bin)))))})
                   (.call orig-fetch js/globalThis url))))
         (await (js/import "@wcohen/wasmts/dist/wasmts.js"))
         (await (js/Promise. (fn [resolve] (js/setTimeout resolve 2000))))
         (when-not (and (.-wasmts js/globalThis) (.. js/globalThis -wasmts -geom))
           (throw (js/Error. "wasmts failed to initialize (globalThis.wasmts.geom missing)")))
         js/globalThis.wasmts))))
