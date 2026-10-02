;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.pool-setup
  "Joint worker-router pool setup for the dual-runtime .cljc test suites. cljs
   side only.

   This ns installs globalThis.wasmts. Then it starts the pool with the
   wasmts asset paths. It adds the gdal handler when the caller asks for it.

   A worker is a separate JS context. It cannot see the main-thread fetch shim
   that init-wasmts! installs. Because of this, the wasmts loader URL and the
   .wasm bytes go to each worker in the pool init args.

   The gdal handler is optional, for the reason cg.run makes it optional. The
   gdal init throws without dbBytes, and a worker that throws in its init does
   not answer. JVM callers do not use this ns."
  #?(:cljs (:require ["node:fs" :as fs]
                     ["node:path" :as node-path]
                     ["node:url" :as node-url]
                     ["../../src/cg/run.mjs" :as run]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_
                                                   wasmts_assets]])))

#?(:clj (set! *warn-on-reflection* true))

#?(:cljs
   (def proj-db-path
     "The PROJ database that gdal-wasm ships. It must match the PROJ of its
      libgdal.wasm: an older proj.db makes each GDAL call throw. Paths
      resolve against process.cwd(). Run the test suites from the module
      root."
     "node_modules/gdal-wasm/proj.db"))

#?(:cljs
   (defn wasmts-handler-url
     "The file URL of the squint-compiled cg wasmts handler. Each worker
      imports this module. The bundled build has a sibling copy of it next to
      dist/cg.mjs, but the test suites import the source, which has no such
      sibling."
     []
     (.-href (node-url/pathToFileURL
              (node-path/resolve "src/cg/wasmts_handler.mjs")))))

#?(:cljs
   (defn ^:async init-cg-pool!
     "Install globalThis.wasmts and start the joint pool. Returns the pool.

      opts is a map. All keys are optional:
        :gdal — if true, register the gdal handler with the clj-gdal proj.db.
        :size — the pool size. init-cg-workers! receives it.

      init-cg-workers! keeps a live pool and does not throw on a second call.
      Because each suite runs in its own node process, the opts of the first
      call apply to that process."
     [opts]
     (await (init_wasmts_BANG_))
     (let [assets (wasmts_assets)
           base   {:wasmts-js-url      (:wasmts-js-url assets)
                   :wasmts-wasm-binary (:wasmts-wasm-binary assets)
                   ;; The suites import the unbundled src/cg/run.mjs. The
                   ;; default handler URL is the sibling of the bundle, which
                   ;; does not exist next to the source.
                   :wasmts-handler-url (wasmts-handler-url)}
           base   (if (:size opts) (assoc base :size (:size opts)) base)
           full   (if (:gdal opts)
                    (assoc base :gdal-init {:dbBytes (fs/readFileSync proj-db-path)})
                    base)]
       (await (run/init-cg-workers! full)))))

#?(:cljs
   (defn ^:async shutdown-cg-pool!
     "Stop the joint pool. This function does nothing when no pool started.

      A suite that exits through cg.test-runner does not need this function,
      because process.exit releases the workers. A suite that wants a clean
      handle count can give this function to the runner as its teardown fn."
     []
     (await (run/terminate-cg-workers!))))
