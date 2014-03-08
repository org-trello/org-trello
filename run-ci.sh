#!/bin/sh -xe

# unit/integration tests
make test
# testing install file in marmalade
make install-file-with-deps-from-marmalade
# testing install file in melpa
make install-file-with-deps-from-melpa
