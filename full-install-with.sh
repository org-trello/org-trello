#!/bin/bash -xe

# setup

REPO=$1
PACKAGE=org-trello

CLEAN_INSTALL_FOLDER=$HOME/$PACKAGE-install-$REPO

# prepare

rm -rf $CLEAN_INSTALL_FOLDER
mkdir -p $CLEAN_INSTALL_FOLDER
cp install.el $CLEAN_INSTALL_FOLDER

# execute
HOME=$CLEAN_INSTALL_FOLDER emacs -Q --batch -nw -l $CLEAN_INSTALL_FOLDER/install.el -- $REPO
