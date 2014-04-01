README for the org-trello developer
===================================

This will describe org-trello's current state of affairs.

# Sources

The sources are splitted in `namespaces` in the [src/](./src/) folder.
This is described in the [namespaces.el](./namespaces) file.

## Namespaces

The namespaces are in loading order:

Namespace File    | Description of the namespace
------------------|------------------------------------------------------------------------
header.el         | Main information about org-trello (version, licence, deps, etc...)
log.el            | Provide log facilities
setup.el          | Main variable definition that permits internal org-trello functions customization
hash.el           | Hash-map manipulation utilities
action.el         | Higher-order functions helper
data.el           | Internal org-trello data manipulation
cbx.el            | Checkbox manipulation utilities
api.el            | Trello API abstraction DSL
query.el          | HTTP query utilities
backend.el        | Deals with trello requests
elnode.el         | Elnode utilities (common namespace between proxy.el and webadmin.el)
proxy.el          | Proxy utilities - Namespace in charge of dealing with the trello requests
webadmin.el       | Web admin front utilities. Namespace in charge of web admin interface
server.el         | Main namespace that hides the webadmin and the proxy
buffer.el         | Buffer manipulation functions
input.el          | Text input functions
controller.el     | Controller used by org-trello.el
org-trello.el     | Main org-trello mode which defines interactive command and the mode to be used
footer.el         | End of the org-trello packaging

## Loading

Use the [load-namespaces.el](./load-namespaces.el) file to load org-trello for developing purposes and keep the emacs way of browsing source code.

Open the file and `M-x eval-buffer`.

# Makefile

the [Makefile](./Makefile) is your ally for:
- package
- test
- release

## Package

The sources are built from the [src/](./src/) folder and generated into one [org-trello.el](./org-trello.el) file which is the only file packaged.

This will trigger:
- the org-trello.el generation from the [src/](./src/) source files
- trigger the launch-test.el script that runs the tests

```sh
make package
```

## Test

```sh
make test
```

This will trigger:
- the loading of the source code from the **src/** folder.
- trigger the **launch-test.el** script that runs the unit/integration tests (stored in **test/** folder)

## Install

To test that the package, once created, can be installed (using the repository to fetch the dependencies).

```sh
make install-file-with-deps-from-marmalade install-file-with-deps-from-melpa
```

## Full install testing

As we deploy both in marmalade and in melpa, we need to ensure once delivered that the installation is ok.

```sh
make install-package-from-marmalage install-package-from-melpa
```

## Release

The release process is done through 2 steps:
- Self Pull Request from the feature branch inside master
- Then trigger the release through the call to the release target from the Makefile

```sh
make release
```

This will:
- fetch the latest modification on your repository
- go to the master branch
- update to the latest master commit
- tag using the $VERSION you submit to the script (defaulting to the version from the org-trello.el header)
- push the tag to your repository
- trigger the package from the Makefile
- push the package to marmalade (if you did the configuration for this, otherwise it will fail)

Note:
- this is an orchestration of the [release.sh](./release.sh) script
- the packaging for MELPA is automatically done from org-trello/org-trello repository
