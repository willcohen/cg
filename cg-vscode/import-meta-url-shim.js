// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// esbuild inject shim. A CJS bundle has no import.meta, and
// createRequire(import.meta.url) would get undefined. build-extension.mjs
// replaces `import.meta.url` with this export. __filename and require are CJS
// globals in node output.
export const import_meta_url = require('node:url').pathToFileURL(__filename).href;
