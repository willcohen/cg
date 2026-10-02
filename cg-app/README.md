# cg-app

cg-app is the web app for cg. It shows a `.cg` file in five panels: Flow (the canvas), Code, Status, Map and Results. The browser runs the file in JavaScript. With the JVM backend, the app can also run the file on the JVM.

Desktop and mobile apps are planned.

Run each command in this folder, in the development shell of the root README.

## Setup

```bash
(cd ../cg && bb npm:install)
bb setup
bb vendor-deps
```

`bb setup` runs `npm install`. `bb vendor-deps` copies the browser runtime packages into `shared/ui/public/vendor/`, which the dev server serves as `/vendor/`. The task reads `node_modules` and `../cg/node_modules`.

## Dev server

```bash
bb dev:web
```

The task builds the cg bundle, the canvas and the app shell. Then it serves the app at http://localhost:3100. To use a different port, set the environment variable `PORT`.

In the page, click "Load .cg" or select a file in the Examples menu. Then click Run.

The task builds again when a source file of the app or the canvas changes. It does not look for changes in `../cg`. For those, run `bb squint:watch` in `../cg` and `bb watch:cg` here.

## Open a file from a URL

Click "Open URL". Type the URL of a `.cg` file. Then click Open. The app loads the file and does not run it. To run the file, click Run.

A link can open a file too. Put the URL of the file in the query parameter `url` of the page address:

```text
https://willcohen.github.io/cg/?url=https://raw.githubusercontent.com/willcohen/cg/main/cg/examples/boston.cg
```

- The server of the file must send a CORS header. raw.githubusercontent.com sends one.
- The app changes a github.com file address to its raw.githubusercontent.com address, because github.com sends no CORS header.
- A `run/load-module` path in the file resolves to the modules of the app (`modules/`), and not to the URL of the file.
- After an open, the address bar holds the `url` parameter. A load from the Examples menu or from "Load .cg" removes it.

## Backend

```bash
bb backend:dev
```

The backend is a JVM HTTP server that runs `.cg` code with the cg library. It binds to 127.0.0.1, on a port that the operating system selects. The task writes the port number to `.cg-backend-port`.

The dev server reads that file when it serves the page. Start the backend first. Then load the page again. See `backend/README.md`.

## Tests

```bash
bb squint:cg-core
bb test
```

The app tests import the compiled cg modules, and `bb squint:cg-core` compiles them. `bb test` runs two tasks:

- `bb test:backend` runs the backend tests on the JVM.
- `bb test:web` compiles the app tests with squint and runs them in Node.js.

## Probes

A probe is a Node.js script that opens the canvas or the app in headless Chromium and does checks. The probes use Playwright. If Playwright has no browser, run `npx playwright install chromium`.

```bash
bb probes
```

This task builds the canvas bundle and runs the canvas probes in `shared/squint/probes/canvas-verify.cjs`.

The app probes are in `app/probes/`. The header of each file tells you what the probe checks. Build the app first. Then run one probe with Node.js:

```bash
bb css:build
bb build:web-app
node app/probes/op-js-verify.cjs
```

## Demo build

```bash
bb build:demo
```

The task writes a static demo to `../docs/`: `index.html`, one bundle (`cg-app.mjs`), the worker file `cg-wasmts-handler.mjs`, `css/`, `examples/` and `modules/`. Git ignores this output.

- The page loads each npm package from a CDN, at the version in `node_modules`.
- The demo has no backend. It runs files in JavaScript only.
- The Examples menu lists the files in `../cg/examples/index.json`. That list does not include `boston.cg`, because a run of it sends many requests to the GIS servers of the City of Boston.
- `modules/` holds the `.cg` modules of `../cg/src/cg/`, which an example loads with `run/load-module`.

A static host such as GitHub Pages can serve `docs/`. To check the build, run `node app/probes/demo-verify.cjs`. That probe needs the network.

## More tasks

- `bb build:web-app` builds all that the dev server serves: the cg bundle, the canvas and the app shell.
- `bb build:vscode` builds the files that `cg-vscode` reads from this folder.
- `bb clean` removes the build output.
- `bb tasks` lists each task.
