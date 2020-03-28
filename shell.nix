with import <nixpkgs> {};

let sources = import ./nix/sources.nix;
    pkgs = import sources.nixpkgs {};
in stdenv.mkDerivation {
  name = "org-trello-env";
  buildInputs = [
    pkgs.cask
  ];
  src = null;
}
