let sources = import ./nix/sources.nix;
    pkgs = import sources.nixpkgs {};
in pkgs.stdenv.mkDerivation {
  name = "org-trello-env";
  buildInputs = [
    pkgs.cask
  ];
  src = null;
}
