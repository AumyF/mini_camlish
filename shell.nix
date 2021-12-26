{ pkgs ? import <nixpkgs> { }, system ? builtins.currentSystem }: pkgs.mkShell {
  buildInputs = [
    pkgs.ocaml
    pkgs.ocamlPackages.findlib
    pkgs.dune_2
    pkgs.ocamlPackages.ocaml-lsp
    pkgs.ocamlPackages.containers
    pkgs.ocamlPackages.alcotest
    pkgs.ocamlPackages.ppx_deriving
    pkgs.ocamlPackages.ppxlib
    pkgs.ocamlPackages.utop
    pkgs.ocamlformat
    pkgs.ocamlPackages.ocamlformat-rpc-lib
  ] ++
  (if builtins.elem system pkgs.inotify-tools.meta.platforms
  then [ pkgs.inotify-tools ] else [ ]);
}
