# cg

cg is a small DSL for GIS analysis in urban planning. I wrote it to run the same analysis file on the JVM and in JavaScript.

A `.cg` file is text in a Clojure dialect. A threading macro such as `table->` describes a pipeline. A cg operation is a `defn` whose name has the metadata `^:op`. An editor canvas draws the file next to the text.

Each runtime uses these libraries:

- On the JVM: Clojure and SCI, with JTS, and with PROJ and GDAL through clj-proj and clj-gdal.
- In JavaScript: squint, with JTS as wasm through wasmts, and with proj-wasm and gdal-wasm.

## Status

cg is early work.

- The JavaScript side is on npm as `@wcohen/cg`. It needs Node.js 26 or later: `npm install @wcohen/cg`.
- The web app works. A demo is at https://willcohen.github.io/cg/. The demo runs files in JavaScript only.
- A VS Code extension is in progress. It can run a file with a JVM backend or with a JavaScript backend.
- Desktop and mobile apps are planned.

The web app and the VS Code extension can load and show the Boston example. The Examples menu of the web app does not list it. To open it in the web app, click Open URL. Then type this URL:

```text
https://raw.githubusercontent.com/willcohen/cg/main/cg/examples/boston.cg
```

This link opens it in the demo: https://willcohen.github.io/cg/?url=https://raw.githubusercontent.com/willcohen/cg/main/cg/examples/boston.cg. The app loads the file and does not run it. In VS Code, open `cg/examples/boston.cg`.

To run `boston.cg` in a browser, use a desktop browser, because one district needs several GB of memory. A phone browser reloads the tab before the run ends.

## Folders

- `cg/` is the library. It holds the two CLIs, the `.cg` modules (`cg/src/cg/*.cg`) and the examples (`cg/examples/`).
- `cg-app/` is a web app with a canvas, a code panel, a status panel, a map and a results panel. It also holds a small JVM backend.
- `cg-vscode/` is a VS Code extension. You run it from a checkout with F5. I do not supply a packaged `.vsix` file.

`cg/README.md` and `cg-app/README.md` give more detail.

## Requirements

`flake.nix` defines a Nix development shell. To start it, run `nix develop` in the repository root. The shell provides these tools:

- GraalVM CE 25 (a JDK 25) and the Clojure CLI
- Node.js 26
- Babashka (`bb`), which runs the tasks
- clj-kondo and Maven

If you use direnv, make a file `.envrc` that contains the line `use flake`. Then run `direnv allow`. Git ignores `.envrc`.

Without Nix, install the same tools. JDK 21 rejects a JVM option that the tasks use. On a JDK that is not GraalVM, PROJ and the parser run about 6 times slower. Node.js 24 has a defect that stops the process when a worker pool closes.

## Procedures

Run each block from the repository root, in the development shell.

### Install the packages

```bash
(cd cg && bb npm:install)
(cd cg-app && bb setup && bb vendor-deps)
```

The first line installs the npm packages of the library. The JVM code needs them too, because the JVM parser reads its grammar from `cg/node_modules`. The second line installs the npm packages of the web app and copies its browser runtime files. The procedures below need these packages.

### Run an example on the JVM

```bash
cd cg
bb run examples/simple.cg
```

The task prints the name of the model and the result.

### Run an example in JavaScript

```bash
cd cg
bb node-run examples/simple.cg
```

The task compiles cg with squint. Then it runs the file in Node.js.

### Run the tests

```bash
(cd cg && bb test && bb test:cljs && bb lint)
(cd cg-app && bb test)
```

In `cg/`, `bb test` uses the JVM, `bb test:cljs` uses squint and Node.js, and `bb lint` uses clj-kondo. In `cg-app/`, `bb test` runs the backend tests and the app tests. The app tests import the cg modules that `bb test:cljs` compiles.

### Start the web app

```bash
cd cg-app
bb dev:web
```

Open http://localhost:3100. To use a different port, set the environment variable `PORT`. To open a file, select it in the Examples menu or click Open URL. Then click Run. The browser runs the file in JavaScript. To run it on the JVM, see `cg-app/README.md`.

### Build the static demo

```bash
cd cg-app
bb build:demo
```

The task writes the demo to `docs/` in the repository root. A static host such as GitHub Pages can serve that folder. The demo runs files in JavaScript only. Its page loads each npm package from a CDN.

### Run the VS Code extension

```bash
(cd cg-app && bb build:vscode)
(cd cg-vscode && npm install)
```

Open the folder `cg-vscode/` in VS Code. Then press F5. The extension needs the Calva extension.

## Examples

- `cg/examples/simple.cg` puts a buffer around a point, a line and a polygon. It runs on the JVM and in JavaScript. It needs no data files.
- `cg/examples/parcel.cg` gets the parcel of Boston City Hall from the public parcels layer of the city and draws it on the map. The popup of the parcel shows its attributes. It runs on the JVM and in JavaScript. It sends one request to the network.
- `cg/examples/boston.cg` compares Boston parcels with the existing zoning. It runs on the JVM and in JavaScript. It gets its data from the network.

`boston.cg` uses two modules in `cg/src/cg/`. The module `parcel.cg` measures a lot, and it is a different file from the example `parcel.cg`. The module `zoning.cg` finds the district of a lot and judges each rule. The modules name no city: `boston.cg` gives them the Boston column names and thresholds.

A run of `boston.cg` gets public GIS layers of the city from the network. One district takes minutes. The Examples menu of the web app does not list `boston.cg`, because each run sends many requests to the GIS servers of the city. On the JVM, the command is `bb run examples/boston.cg`, and the file keeps its results as GeoPackage files in `cg/.cg-cache/`. In a browser, the results stay in memory, and each run gets the layers again.

## License

Copyright (c) 2025, 2026 Will Cohen. The license is the Apache License, Version 2.0, with LLVM Exceptions. See [`LICENSE`](LICENSE).
