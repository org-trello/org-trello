{ pkgs, ... }:

let org-trello-emacs = pkgs.emacsWithPackages (epkgs:
      (with epkgs.melpaStablePackages; [
        s dash
        request-deferred deferred
        pkgs.org-trello
      ]) ++ (with pkgs; [ emacs org-trello])
    );
in pkgs.stdenv.mkDerivation {
  name = "org-trello-env";
  buildInputs = with pkgs; [
    org-trello-emacs
    cask
  ];
  src = null;
}
