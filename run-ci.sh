#!/bin/sh -xe

CASK_EMACS=${EMACS:-$(which emacs)}

echo "*** Emacs version ***"
echo "CASK_EMACS = $CASK_EMACS"
"$CASK_EMACS" --version
echo

# unit/integration tests
make EMACS=$CASK_EMACS install test install-file-with-deps-from-marmalade install-file-with-deps-from-melpa
