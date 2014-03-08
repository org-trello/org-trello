#!/bin/bash -xe

# setup

REPO=$1
VERSION=$2

CLEAN_INSTALL_FOLDER=$HOME/$PACKAGE-install-$REPO

# prepare

rm -rf $CLEAN_INSTALL_FOLDER
mkdir -p $CLEAN_INSTALL_FOLDER

# execute

HOME=$CLEAN_INSTALL_FOLDER emacs -Q --batch -l ./build.el -- $REPO org-trello-$VERSION.tar
