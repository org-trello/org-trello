#!/usr/bin/env bash

# This will request your token file and install it on ~/.marmalade/token
# This will overwrite it if already present. This is highly dependent on the marmalade api!

TOKEN_DIR=$HOME/.marmalade
TOKEN_FILE=$TOKEN_DIR/token

echo "### Welcome to the marmalade token installer ###"
echo "Please enter your login/password (no registering nor seeing the pass), just recording/overwriting your token in $TOKEN_FILE."
echo

read -p "User to marmalade: " user

stty -echo
read -p "Password: " pass
stty echo

mkdir -p $TOKEN_DIR

curl -XPOST "http://www.marmalade-repo.org/v1/users/login?name=$user&password=$pass" | cut -d':' -f 4 | cut -d'"' -f 2 > $TOKEN_FILE

( [ -f $TOKEN_FILE ] && echo "Token $TOKEN_FILE:" && cat $TOKEN_FILE ) || echo "Problem during saving the token, this fails!"
