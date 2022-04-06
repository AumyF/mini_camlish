{
  description = "An OCaml-like language implementation";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}; in
      rec {
        packages = flake-utils.lib.flattenTree
          {
            mini-camlish = pkgs.callPackage ./default.nix { };
          };
        defaultPackage = packages.mini-camlish;

        checks = {
          test = pkgs.runCommand "dune-test"
            {
              buildInputs = [
                pkgs.dune_2
                pkgs.ocaml
                pkgs.gcc
                pkgs.ocamlPackages.findlib
                pkgs.ocamlPackages.alcotest
                pkgs.ocamlPackages.containers
                pkgs.ocamlPackages.ppx_deriving
                pkgs.ocamlPackages.ppxlib
              ];
            } ''
            set -euo pipefail
            
            cp -r ${./.} ./test
            dune test
            touch $out
          '';
        };

        apps.mini-camlish = flake-utils.lib.mkApp { drv = packages.mini-camlish; };
        defaultApp = apps.mini-camlish;
        devShell = pkgs.callPackage ./shell.nix { };
      }
    );
}
