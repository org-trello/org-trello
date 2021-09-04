{
  description = "org-trello flake";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
      follows = "nix/nixpkgs";
    };

    flake-utils = {
      type = "github";
      owner = "numtide";
      repo = "flake-utils";
      ref = "master";
    };
  };

  outputs = { self, nix, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let lib = nixpkgs.lib;
            pkgs = nixpkgs.legacyPackages.${system};
            pname = "org-trello";
            version = "0.8.2";
        in rec {
          packages."${system}" = {
            org-trello = pkgs.stdenv.mkDerivation {
              inherit pname version;
              src = ./.;
              buildInputs = with pkgs.emacs.pkgs; [
                emacs s dash
                request-deferred deferred
              ];
              unpackPhase = ''
                cp $src/org-trello*.el .
                # those are for dev only
                rm org-trello-tools*.el
              '';
              buildPhase = ''
                emacs -L . --batch -f batch-byte-compile *.el
              '';
              installPhase =
                let install-dir = "$out/share/emacs/site-lisp/elpa/${pname}-${version}/"; in
                ''
                mkdir -p ${install-dir}
                cp -v *.el *.elc ${install-dir}
              '';

              doCheck = false;

              meta = {
                description = "Minor mode to synchronize org-mode buffer and trello board.";
                homepage = https://github.com/org-trello/org-trello/;
                license = lib.licenses.gpl2;
                maintainers = with lib.maintainers; [ ardumont ];
              };
            };
          };

          devShell = import ./shell.nix { pkgs = pkgs // packages."${system}"; };

          defaultPackage = packages."${system}".org-trello;
        });
}
