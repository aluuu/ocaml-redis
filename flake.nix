{
  description = "Development environment for ocaml-redis";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-24.05";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShell = with pkgs; mkShell {
          buildInputs = [
            ocaml
            ocamlPackages.utop
            ocamlPackages.ounit2
            ocamlPackages.findlib
            ocamlPackages.ocaml-lsp
            ocamlPackages.ocamlformat
            ocamlPackages.merlin
            ocamlPackages.dune_3

            ocamlPackages.camlp-streams
            ocamlPackages.stdlib-shims
            ocamlPackages.uuidm
            ocamlPackages.re
          ];
        };
      }
    );
}
