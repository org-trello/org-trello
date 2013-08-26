#!/bin/bash -xe

# params

USER=${1-"ardumont"}
TOKEN_FILE=${2-"$HOME/.marmalade/token"}
VERSION=$3

# compute

TOKEN=$(cat $TOKEN_FILE)
ARCHIVE=../org-trello-$VERSION.tar

# post

curl -v -F "name=ardumont" -F "token=my-token" -F "package=@$ARCHIVE" http://www.marmalade-repo.org/v1/packages
