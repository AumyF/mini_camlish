{ pkgs ? import <nixpkgs> { } }: pkgs.ocamlPackages.buildDunePackage {
  pname = "mini_ocaml";
  version = "0.1.0";
  useDune2 = true;
  src =
    ./.;
  nativeBuildInputs = [
    pkgs.ocamlPackages.alcotest
    pkgs.ocamlPackages.containers
    pkgs.ocamlPackages.ppx_deriving
    pkgs.ocamlPackages.ppxlib
  ];
}
