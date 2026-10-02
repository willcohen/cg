# cg-app backend

The backend is a small JVM HTTP server (Ring and Jetty). It runs `.cg` code with the cg library on the JVM. The web app and `cg-vscode` use it to run files on the JVM. A run in JavaScript does not need it.

## Start

Run this command in `cg-app/`, in the development shell of the root README:

```bash
bb backend:dev
```

The task runs `clj -M:backend --port-file .cg-backend-port`.

## Port

The server binds to 127.0.0.1 only. It has no fixed port, because the operating system selects one. The task writes the port number to `cg-app/.cg-backend-port`. Read the port from that file:

```bash
curl http://127.0.0.1:$(cat .cg-backend-port)/api/health
```

Start the backend before you load the page of the web app. The dev server reads the port file when it serves the page.

## Routes

`src/cg_app/backend/server.clj` lists each route. The web app and `cg-vscode` call these routes:

- `GET /api/health` tells the client that the server is up.
- `POST /api/execution/start` starts a run of `.cg` code.
- `GET /api/execution/:id/trace` gives the trace of that run as server-sent events.
- `GET /api/execution/:id/result` gives the status of the run, then its result.
- `POST /api/session/create` makes a session. `POST /api/session/load` runs `.cg` code in it. `GET /api/session/vars` lists its vars.
- `POST /api/data/:var` gives one dataset as GeoJSON, with an optional bbox filter.

A run gives a summary and no geometries. A client gets the geometries of one dataset through `/api/data/:var`.

The clients do not call the other routes. Of those, `POST /api/execute` is deprecated, and `POST /api/project` gives HTTP 501.

## Tests

```bash
bb test:backend
```

The tests are in `cg-app/test/cg_app/backend/`.
