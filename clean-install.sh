#!/bin/bash -xe

# setup

PACKAGE=${1-"org-trello"}

CLEAN_INSTALL_FOLDER=$HOME/$PACKAGE-install

# prepare

rm -rf $CLEAN_INSTALL_FOLDER
mkdir -p $CLEAN_INSTALL_FOLDER
cat <<EOF > $CLEAN_INSTALL_FOLDER/default.el
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(package-refresh-contents)
(package-install '$PACKAGE)
(package-install '$PACKAGE)

EOF

# execute

HOME=$CLEAN_INSTALL_FOLDER emacs --batch -nw -l $CLEAN_INSTALL_FOLDER/default.el
