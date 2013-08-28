#!/bin/bash -xe

echo "User: "
read user

echo "Pass: "
read pass

curl -XPOST "http://www.marmalade-repo.org/v1/users/login?name=$user&password=$pass"
