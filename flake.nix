{
  description = "Development shell for cg, cg-app and cg-vscode";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # JDK for all JVM work. GraalVM CE 25 is a full JDK 25 AND ships
        # libgraal, so the embedded Truffle guests (cg.parser.jvm's
        # web-tree-sitter, clj-proj's PROJ wasm) JIT-compile instead of
        # running interpreted. Stock OpenJDK
        # 25 (Zulu) cannot: its JVMCI lacks the config the community Graal
        # compiler demands (JDK-8364936 / NMethodPatchingType::conc_data_patch),
        # so the optimizing runtime only loads there by bypassing a codegen
        # correctness guard. The org.graalvm.* pins in cg/deps.edn must be the
        # release of this JDK (cg/test/cg/dependency_pins_test.clj).
        jvmJdk = pkgs.graalvmPackages.graalvm-ce;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            babashka
            clj-kondo
            # JDK 25 (GraalVM CE, see jvmJdk above) is required so the
            # embedded Truffle guests JIT via libgraal.
            #
            # Run JVM tasks (clojure -M:test, bb test) INSIDE `nix develop`. The
            # ambient nix-profile `clojure` is a different build on JDK 21 and
            # rejects the deps.edn JDK-25 flags (--sun-misc-unsafe-memory-access=allow),
            # failing with "Could not create the Java Virtual Machine".
            (clojure.override { jdk = jvmJdk; })
            clojure-lsp
            # nodejs_26, not the default `nodejs` (currently the v24 LTS): node
            # 24.x has a libuv regression that aborts the process (uv__io_poll
            # kqueue EBADF) when a multi-worker pool whose workers did network I/O
            # is torn down -- e.g. bb test:js tearing down the joint pool after
            # the gdal handler's /vsicurl reads. v22 and v26 are clean; pin v26.
            # Matches the pin clj-proj's flake already carries for the same bug.
            nodejs_26
            # npm install compiles the native binding of
            # @yogthos/tree-sitter-clojure with node-gyp, which needs Python.
            python3
            ripgrep
          ];
          shellHook = ''
            export JAVA_HOME=${jvmJdk};
            export PATH="${jvmJdk}/bin:$PATH";
          '';
        };
      }
    );
}
