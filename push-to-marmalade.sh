#!/usr/bin/env bash

if [ $# -ne 3 ]; then
    cat <<EOF
Use: $0 <USER> <TOKEN-FILE> <VERSION>"
- USER       your marmalade login
- TOKEN-FILE path to your token file (~/.marmalade/token seems like a good idea)
- VERSION    version to release (0.1.6 for example)

To install the token, execute the install-marmalade-token.sh.
EOF
    exit 1;
fi

# params

USER=$1
TOKEN_FILE=$2
VERSION=$3

# compute

if [ ! -f $TOKEN_FILE ]; then
    cat <<EOF
The token file '$TOKEN_FILE' must be installed.
Use the script install-marmalade-token.sh first and relaunch this script.
EOF
    exit 1;

fi

TOKEN=$(cat $TOKEN_FILE)
ARCHIVE=org-trello-$VERSION.tar

if [ ! -f $ARCHIVE ]; then
cat <<EOF
The archive '$ARCHIVE' does not exist!.
Create the archive first! Then relaunch this script.
EOF
    exit 1;

fi

# post

curl -v -F "name=$USER" -F "token=$TOKEN" -F "package=@$ARCHIVE" http://www.marmalade-repo.org/v1/packages

[ $? -eq 0 ] && echo "Release $VERSION done!" || echo "Release $VERSION failed!"
