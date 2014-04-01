README for the org-trello developer
===================================

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](http://doctoc.herokuapp.com/)*

- [Sources](#sources)
	- [Conventions](#conventions)
		- [Rules](#rules)
		- [Exception](#exception)
	- [Namespaces](#namespaces)
	- [Loading](#loading)
- [Makefile](#makefile)
	- [Package](#package)
	- [Test](#test)
	- [Install](#install)
	- [Full install testing](#full-install-testing)
	- [Release](#release)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

This will describe org-trello's current state of affairs.

# Sources

The sources are splitted in `namespaces` in the [src/](./src/) folder.
This is described in the [namespaces.el](./namespaces.el) file.

## Conventions

As there exists no namespace in emacs-lisp, I use:
- convention in function names to determine the nature private/public
- splitted file that represents namespaces.

### Rules

The conventions enforced in org-trello:

- functions are prefixed with `orgtrello-<NAMESPACE-FILENAME>/<FUNCTION-NAME>`
  - `<NAMESPACE-FILENAME>` is the filename without its path nor its extension. For example, `src/buffer.el` renders `buffer`
  - `<FUNCTION-NAME>` is an alphanumeric symbol with `-` as separator. For example, `orgtrello-api/add-board` is a public function which does create a request to add a board.
- private functions are prefixed with `--`. For example, `orgtrello-api/--deal-with-optional-values` which is a private utility function to help in creating api request.
- side-effecty (interacts with the world, e.g read from user, read/write from/to buffer, etc...) functions are suffixed with `!`. For example, `orgtrello-buffer/back-to-card!` which does move the caret to the beginning of the card in the org buffer.
- predicate functions are suffixed by `-p`. For example, `orgtrello-data/entity-card-p` which checks if the parameter entity is a card or not.

For example:
- in `src/buffer.el`, all pure public functions are named `orgtrello-buffer/some-fn-name`
- in `src/input.el`, all pure private functions are named `orgtrello-input/--some-private-fn`
- in `src/cbx.el`, all public side-effecty functions are named `orgtrello-cbx/some-fn-name!`

### Exception

`src/org-trello.el` is the main namespace that declares the:
- interactive commands used throughout all of org-trello
- minor mode

Every exposed interactive command is named `org-trello/fn-name`.

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

Use the [load-namespaces.el](./load-namespaces.el) file to load org-trello for development purposes and keep the emacs way of browsing source code (`M-.`, `M-,`).

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

*Note*:
This will trigger the installation from a local package `org-trello-<VERSION>.tar`.
First the installation is used with the dependencies fetch from marmalade `make install-file-with-deps-from-marmalade`.
Second with melpa's - `make install-file-with-deps-from-marmalade`.

*Note 2*
These are the targets used by the CI (cf. [.travis.yml](./.travis.yml))

## Full install testing

As we deploy both in marmalade and in melpa, we can ensure that once delivered, the installation is ok using those targets.

```sh
make install-package-from-marmalade install-package-from-melpa
```

## Release

The release process is done through 2 steps:
- Self Pull Request from the feature branch inside master

    ```sh
    make pr
    ```

    *Note* You need `hub` installed for this target to work.

- Then trigger the release through the call to the release target from the Makefile

    ```sh
    make release
    ```

This will:
- fetch the latest modifications on your repository
- checkout the master branch
- fast-forward to the latest master commit
- tag the latest commit from master using the $VERSION you submit to the script (defaulting to the version from the org-trello.el header)
- push the tag to the upstream branch repository
- trigger the package target from the Makefile (thus building a new package to the latest version)
- push the package to marmalade (if you did the configuration for this, otherwise it will fail)

Note:
- this is an orchestration of the [release.sh](./release.sh) script
- the packaging for MELPA is automatically done from `org-trello/org-trello` repository
