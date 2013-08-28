#!/bin/bash -xe

if [ $# -ne 2 ]; then
    cat <<EOF
Use: $0 <VERSION> <USER>"
- USER       your marmalade login
- VERSION    version to release (0.1.6 for example)

To install the token, execute the install-marmalade-token.sh.
EOF
    exit 1;
fi

USER=$1
VERSION=$2

# from the current dev branch

git fetch -p --all

git checkout master

git merge origin/master

git tag $VERSION

git push --tag

make package

$WDIR/push-to-marmalade.sh $USER $HOME/.marmalade/token $VERSION

# FIXME

# automate the release page from github (rest api too)
