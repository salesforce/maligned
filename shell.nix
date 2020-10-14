let
  sources = import ./nix/sources.nix;
in
{ pkgs ? import sources.nixpkgs {} }:

  with pkgs;
  stdenv.mkDerivation {
    name = "maligned-dev-env";

    buildInputs = [
      sbt
      git # used by sbt-dynver
      graphviz # used for ScalaDoc diagrams
    ];
  }
