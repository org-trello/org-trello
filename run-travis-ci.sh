#!/bin/sh -e

cd "$(dirname "$0")"

CURRENT_EMACS=${EMACS:-$(which emacs)}
CURRENT_CARTON=$(which carton)

echo "*** Emacs version ***"
echo "CURRENT_EMACS = $CURRENT_EMACS"
"$CURRENT_EMACS" --version
echo "CURRENT_CARTON = $CURRENT_CARTON"

make test
