org-trello
==========

[![Build Status](https://travis-ci.org/ardumont/org-trello.png?branch=master)](https://travis-ci.org/ardumont/org-trello)

Minor emacs mode for org-mode - 2-way synchronization between org-mode file and trello board

**Table of Contents**  *generated with [DocToc](http://doctoc.herokuapp.com/)*

- [org-trello](#org-trello)
- [why?](#why)
- [Demo](#demo)
- [Contributions](#contributions)
- [Release notes](./release-notes.md)
- [Install](#install)
	- [marmalade - Stable version](#marmalade---stable-version)
	- [melpa - ~snapshot](#melpa---snapshot)
	- [org-trello](#org-trello-1)
	- [github](#github)
	- [Example](#example)
- [Setup](#setup)
	- [Emacs related](#emacs-related)
	- [Trello related](#trello-related)
		- [keys](#keys)
		- [Sync org to trello](#sync-org-to-trello)
			- [pre-requisite](#pre-requisite)
			- [Sync your org-mode buffer](#sync-your-org-mode-buffer)
		- [Create a board](#create-a-board)
- [Bindings](#bindings)
- [Use cases](#use-cases)
- [License](#license)

# why?

- org-mode is what I need.
- Trello is what my team need.
- org-trello may satisfy everybody.

# Demo

- [Install](http://youtu.be/e3NzllAHbHY)
- [Setup key and token](http://youtu.be/ReUp1Wn5scc)
- [Attach org-mode file to one trello board](http://youtu.be/2PT8K1HG-eY)
- [Synchronize one entity](http://youtu.be/ILPs74L5LFU)
- [Synchronize one complete entity and move entity according to status](http://youtu.be/H8DXm5BLaD0)
- [Synchronize from org-mode file to trello](http://youtu.be/d6SATWzhQhs)
- [Synchronize from trello to org-mode files](http://youtu.be/-ldo8gvhaTY)
- [Create board and sync](http://youtu.be/6k4zRm6t8ZY)
- Delete entity - no video yet
- Setup org-mode keyword list + create board - no video yet

# Contributions

- Pull Requests welcome (cf. [todo](./TODO.org) - better read on emacs's org-mode buffer)
- Appreciate any feedback
- Open issues if you want something to be done
- Open issues for bugs too (please describe maximum inputs, version, *Messages* buffer, stacktrace, etc...)
- [What has been done and remains to be done](./TODO.org)

# Release notes

[Release notes](./release-notes.md)

# Install

## marmalade - Stable version

Add this to your emacs's init file (~/.emacs, ~/.emacs.d/init.el, or *scratch*, or whatnot...)

``` lisp
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
```

Then hit `M-x eval-buffer` to evaluate the buffer's contents.

## melpa - ~snapshot

Add this to your emacs's init file (~/.emacs, ~/.emacs.d/init.el, or *scratch*, or whatnot...)

``` lisp
(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)
```

Then hit `M-x eval-buffer` to evaluate the buffer's contents.

## org-trello

To install org-trello:

``` lisp
(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(org-trello)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
```

Again hit `M-x eval-buffer`.

## github

Download org-trello from GitHub

```sh
git clone http://github.com/ardumont/org-trello.git
```

Add the org-trello directory to your load path and then add

``` lisp
(add-to-list 'load-path "/path/to/org-trello/"))
(require 'org-trello)
```

## Example

- [Adding emacs repository](https://github.com/ardumont/install-packages-pack/blob/master/init.el)
- [Installing org-trello](https://github.com/ardumont/orgmode-pack/blob/master/init.el#L1)

# Setup

## Emacs related

Org-trello is a minor mode for org-mode to sync.
Simply, require it somewhere in your load file (~/.emacs or ~/.emacs.d/init.el).

``` lisp
(require 'orgtrello)
```

For example, here is my [startup file](https://github.com/ardumont/orgmode-pack/blob/master/init.el#L3).

## Trello related

### keys

Install the consumer-key and the read-write token for org-trello to be able to work in your name with your trello boards.

`C-c o i`

or

``` lisp
M-x org-trello/install-key-and-token
```

### Sync org to trello

#### pre-requisite

Beware, this step implicitely requires you to prepare your trello board by creating the needed list (in accordance to your org-mode keywords, cf. [org-todo-keywords](https://www.gnu.org/software/emacs/manual/html_node/org/Per_002dfile-keywords.html)).

For example, you could either use the following keywords globally:

```lisp
(setq org-todo-keywords
   '((sequence "TODO" "IN-PROGRESS" "PENDING" "|"  "DONE" "FAIL" "DELEGATED" "CANCELLED")))
```

Or, you could also setup your org-mode file locally, adding this as a org-mode header in your file (cf. [previous link](https://www.gnu.org/software/emacs/manual/html_node/org/Per_002dfile-keywords.html)):

```orgmode
#+TODO: TODO IN-PROGRESS PENDING | DONE FAIL DELEGATED CANCELLED
```

Either way, you need to create the list "TODO", "IN-PROGRESS", "PENDING", "DONE", "FAIL", "DELEGATED", and "CANCELLED" in your trello board before launching the routine to setup your org-mode buffer.

#### Sync your org-mode buffer

For each org-mode file, you need to connect your org-mode file with a trello board.

`C-c o I`

or

``` lisp
M-x org-trello/install-board-and-lists-ids
```

A simpler way could be to create a board from scratch (if you can).

### Create a board

You can avoid the previous step and create directly a board from your org-mode file.
This will create the list from the keywords you use in your org-mode (cf. [org-todo-keywords](http://orgmode.org/manual/In_002dbuffer-settings.html)).

`C-c o b`

or

``` lisp
M-x org-trello/create-board
```

# Bindings

Actual bindings (not definitive, suggestions regarding those bindings are welcome):
- C-c o i - Interactive command to install the key and the read/write access-token.
- C-c o I - Interactive command to setup your org-mode file to work with your remote trello board
- C-c o b - Create interactively a board and attach the org-mode file to this trello board.
- C-c o c - Create/Update asynchronously an entity (card/checklist/item) depending on its level and status. Do not deal with level superior to 4.
- C-c o C - Create/Update a complete entity card/checklist/item and its subtree (depending on its level).
- C-c o s - Synchronize the org-mode file to the trello board (org-mode -> trello).
- C-c o S - Synchronize the org-mode file from the trello board (trello -> org-mode).
- C-c o k - Kill the entity (and its arborescence tree).
- C-c o h - This help message.

# Use cases

1. open an org-mode file

2. Install the key and the token file (`C-c o i` or `M-x org-trello/install-key-and-token`).
This will open your browser to retrieve the needed informations (`consumer-key` then the `access-token`) and wait for your input in emacs.
*Remark:* This need to done once and for all time until you revoke such token.

3. Setup your org-mode file with your trello board (`C-c o I` or `M-x org-trello/install-board-and-lists-ids`).
This will present you a list of your actual boards. Select the one you want and hit enter.
This will edit your org-mode file with properties needed.

*Remarks:*
- This need to be done once for each org-mode file you want to sync with a trello board.
- You can create directly a board (`C-c o b` or `M-x orgtrello/do-create-board-and-lists`)

4. Now you are ready to use org-mode as usual.

The idea is this:
- 3 levels:
  level 1 - Card
  level 2 - Checklist
  level 3 - Item

For example:

```org-mode
* card-identity
** checklist
*** task1
*** task2
*** task3
```

Creation step by step:
- Card:
  - Place yourself on the `card-identity` and hit the binding *BINDING-SIMPLE-CREATION*, this will create the card in the `TODO` column in your trello board
  - You can edit the title and hit *BINDING-SIMPLE-CREATION*, this will update the title in trello
  - Change the status from TODO to any intermediary status, then hit the binding, this will move the card to the list `DOING`.
  - Once done, move the status of the card from anything to DONE, hit the binding, this will move the card to the list `DONE`.
- Checklist:
  - Place yourself on the checklist `checklist`, hit the binding, this will add `checklist` as a checklist to your card `card-identity`
  - Rename your checklist and hit again the binding to update its label
- Task:
  - Place yourself on your task and hit *BINDING-SIMPLE-CREATION*, this will add the item to such checklist.
  - Change the name of the task and hit *BINDING-SIMPLE-CREATION*, this will update its label
  - Change the status of the task to `DONE` and hit the binding, this will check such item in trello.

5. You can sync all of the entity and its arborescence once:
Place yourself on the entity (card or checklist) and hit `C-c o C`.

At the moment, this action is synchonous.

6. You can sync all your org-mode file to trello too.
Hit `C-c o s`.

At the moment, this action is synchonous.

7. You can sync the content of your board into an org-mode file too.
Hit `C-c o S`.

At the moment, this action is synchronous.

8. You can remove the entity and its arborescence with `C-c o k`.

# License

org-trello is free software under GPL v3. See COPYING file for details.
