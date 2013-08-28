#!/bin/bash -xe

VERSION=$1

# from the current dev branch

git fetch -p --all

git checkout master

git merge origin/master

git tag $VERSION

git push --tag

make package

# FIXME

# automate the release to marmalade (rest api)
# automate the release page from github (rest api too)
