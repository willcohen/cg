# cg, the library

[![CI](https://github.com/willcohen/cg/actions/workflows/ci.yml/badge.svg)](https://github.com/willcohen/cg/actions/workflows/ci.yml)
[![Pages](https://github.com/willcohen/cg/actions/workflows/pages.yml/badge.svg)](https://willcohen.github.io/cg/)
[![npm](https://img.shields.io/npm/v/@wcohen/cg)](https://www.npmjs.com/package/@wcohen/cg)
[![Clojars](https://img.shields.io/clojars/v/net.willcohen/cg.svg)](https://clojars.org/net.willcohen/cg)

This folder holds the cg library, its two CLIs, the `.cg` modules and the examples. The same `.cljc` sources run on the JVM and, through squint, in JavaScript.

A demo of the web app is at https://willcohen.github.io/cg/. The source is at https://github.com/willcohen/cg.

Run each command in this folder, in the development shell of the root README. Install the npm packages first, because the JVM code and the JavaScript code read from `node_modules`:

```bash
bb npm:install
```

## A small `.cg` file

This code is from `examples/simple.cg`, without its comments.

```clojure
(require '[cg.dataset :as ds])
(require '[cg.feature :as f])
(require '[cg.geo :as geo])
(require '[cg.macros :refer [table->]])

(def shapes
  (ds/->dataset
    {:geometry [(f/point -71.0589 42.3601)
                (f/linestring [[-71.0650 42.3550] [-71.0550 42.3580]])
                (f/polygon [[-71.0500 42.3500] [-71.0450 42.3500]
                            [-71.0450 42.3550] [-71.0500 42.3550]
                            [-71.0500 42.3500]])]
     :name ["point" "line" "polygon"]}))

(def buffered
  (table-> :shapes
    (geo/buffer 0.0005)))
```

- A dataset has one column for each property, with one value for each row.
- A threading macro (`item->`, `coll->` or `table->`) makes a model. `table->` applies each step to the dataset, one row at a time.
- `:shapes` names the binding that the model reads.
- A run executes the last threading-macro binding of the file. Here, that binding is `buffered`.
- A cg operation, or op, is a `defn` whose name has the metadata `^:op`. A model can use it as a step. For an example, see `prepare-parcel` in `src/cg/parcel.cg`.
- A sink can end a model. In `examples/parcel.cg`, the `map-layer` sink tells the map how to draw the result.
- The `:colors` option of `map-layer` gives a color to each value of the `:color-by` column. The `:popup` option sets the card that the map shows for a clicked feature. The `:opacity` option sets the fill opacity, and `:counts false` removes the feature counts from the legend. The map has no colors or cards of its own for a module. `examples/boston.cg` uses these options.

## The command line

On the JVM, Babashka tasks run a file. In Node.js, `bin/cg.mjs` runs a file on the bundle `dist/cg.mjs`, which `bb squint` builds.

```bash
bb run examples/simple.cg
bb run examples/simple.cg --model buffered
node bin/cg.mjs run examples/simple.cg
```

| Command | Result |
|---------|--------|
| `run <file> [--model NAME] [--timeout-ms N]` | Runs one model and prints its result. `--model` names the binding. The default timeout is 60000 ms. |
| `show-model <file>` | Prints the name, the node count, the edge count and the operations of each model. |
| `generate <file>` | Prints `.cg` source that it writes from the models of the file. |
| `validate <file>` | Changes the code to models and back to code. The exit code is 0 for a match and 1 for a difference. |
| `--help` | Prints the help text. |

- `bb run <file.cg> [--model NAME] [--timeout-ms N]` runs a file on the JVM.
- `bb show-model <file.cg>`, `bb generate <file.cg>` and `bb validate <file.cg>` use the JVM.
- `bb node-run <file.cg>` builds the bundle, then runs a file in Node.js.

## JVM use

`bb repl` starts an nREPL server on port 7888. In a REPL, `cg.run/run-file` runs the source text of a file:

```clojure
(require '[cg.run :as run])
(run/run-file (slurp "examples/simple.cg"))
;; a map with the keys :model and :result
```

To use cg in a different project, add the dependency from Clojars:

```clojure
net.willcohen/cg {:mvn/version "0.0.1"}
```

A git dependency with `:deps/root` also works:

```clojure
io.github.willcohen/cg {:git/url "https://github.com/willcohen/cg.git"
                        :git/sha "<commit sha>"
                        :deps/root "cg"}
```

cg logs through `clojure.tools.logging` and SLF4J, and it brings no log backend. To see the log lines, add a backend such as logback to your project. The bb tasks of this folder use the `:log` alias of `deps.edn` for that.

Each path has limits:

- tools.deps does not pass the JVM options of a dependency. Give your JVM `--enable-native-access=ALL-UNNAMED` and `--sun-misc-unsafe-memory-access=allow`.
- The jar holds the grammar of the parser. With a git dependency, the parser reads its grammar from `node_modules` in the working directory, and a git dependency brings no `node_modules`.
- GDAL has native libraries for macOS on Apple Silicon and for Linux on amd64 and arm64. Maven and Leiningen get the library of the platform through the pom of `net.willcohen/gdal`. tools.deps does not read that pom profile. In a tools.deps project, add the native jar, for example `net.willcohen/gdal-native$darwin-aarch64 {:mvn/version "0.0.1"}`. Without a native library, GDAL runs as wasm on GraalVM.

## JavaScript use

`bb squint` compiles the sources with squint and bundles them into `dist/cg.mjs` with esbuild. The npm package `@wcohen/cg` holds that bundle, the `cg` command, the `.cg` modules and the examples. It needs Node.js 26 or later:

```bash
npm install @wcohen/cg
npx cg run node_modules/@wcohen/cg/examples/simple.cg
```

`bin/cg.mjs` shows the setup that the bundle needs in Node.js. It starts wasmts and sets `globalThis.nodeFs` before it imports the bundle.

## Modules

A `.cg` module is a `.cg` file with an `ns` form. A file loads a module by its path, then requires its namespace:

```clojure
(run/load-module "src/cg/parcel.cg")
(clojure.core/require '[cg.parcel :as parcel])
```

cg looks for the path in the working folder first, and then in the cg library. Because of this, the same line loads a library module in a checkout, in a project that has the npm package, and in a project that has cg as a git dependency.

- `parcel.cg` measures a lot: its edges, its lot type and its setbacks.
- `zoning.cg` finds the zoning district of a lot and gives a verdict for each rule of that district.
- `cache.cg`, `columns.cg`, `layer.cg` and `export.cg` hold helpers for caches, columns, CRS work and exports.

`examples/boston.cg` uses these modules. It gives them the Boston column names and thresholds, and it gets the city GIS layers on its first run. Run it with `bb run examples/boston.cg`. The file keeps its results as GeoPackage files in `.cg-cache/`.

## Tests and lint

- `bb test` runs the Clojure tests on the JVM.
- `bb test:cljs` compiles the `.cljc` test suites with squint and runs them in Node.js.
- `bb lint` runs clj-kondo on the sources, the tests and the `.cg` files.
