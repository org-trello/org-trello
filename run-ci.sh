#!/bin/sh -xe

CASK_EMACS=${EMACS:-$(which emacs)}

echo "*** Emacs version ***"
echo "CASK_EMACS = $CASK_EMACS"
"$CASK_EMACS" --version
echo

# unit/integration tests
make test
# testing install file in marmalade
make install-file-with-deps-from-marmalade
# testing install file in melpa
make install-file-with-deps-from-melpa
