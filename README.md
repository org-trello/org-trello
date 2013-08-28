# org-trello [![Build Status](https://travis-ci.org/ardumont/org-trello.png?branch=master)](https://travis-ci.org/ardumont/org-trello)

Minor emacs mode for org-mode - 2-way synchronization between org and trello board

**Table of Contents**  *generated with [DocToc](http://doctoc.herokuapp.com/)*

- [rational](#rational)
- [Emacs version](#emacs-version)
- [Migration](#migration)
- [TL;DR](#tl;dr)
	- [Fast help](#fast-help)
	- [Demo](#demo)
		- [Synchronize one entity](#synchronize-one-entity)
		- [Install](#install)
		- [Setup key and token](#setup-key-and-token)
		- [Attach org file to trello board](#attach-org-file-to-trello-board)
		- [Synchronize one complete entity](#synchronize-one-complete-entity)
		- [Synchronize from org to trello](#synchronize-from-org-to-trello)
		- [Synchronize from trello to org](#synchronize-from-trello-to-org)
		- [Create board and sync](#create-board-and-sync)
		- [Create board with your keywords](#create-board-with-your-keywords)
		- [Use the checklist Luke!](#use-the-checklist-luke!)
		- [Kill entities](#kill-entities)
		- [Simple help routine](#simple-help-routine)
		- [Check your Setup](#check-your-setup)
- [Contributions](#contributions)
- [Release notes](#release-notes)
- [Install](#install-1)
	- [marmalade - Stable version](#marmalade---stable-version)
	- [melpa - ~snapshot](#melpa---~snapshot)
	- [org-trello](#org-trello)
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
	- [Setup](#setup-1)
	- [Formats](#formats)
		- [natural org format (from 0.1.6 onwards)](#natural-org-format-from-016-onwards)
			- [Migrate to 0.1.6](#migrate-to-016)
			- [Reactivate](#reactivate)
		- [Original format (previous to 0.1.6)](#original-format-previous-to-016)
			- [Activate](#activate)
	- [Creation step-by-step](#creation-step-by-step)
	- [Card and deadline/due date](#card-and-deadlinedue-date)
	- [Checklist and transitivity](#checklist-and-transitivity)
	- [Creation full entity](#creation-full-entity)
	- [Sync org-mode file to trello board](#sync-org-mode-file-to-trello-board)
	- [Sync org-mode file from trello board](#sync-org-mode-file-from-trello-board)
	- [Remove entity](#remove-entity)
- [Errors](#errors)
- [proxy-admin](#proxy-admin)
- [Mailing list](#mailing-list)
- [License](#license)

# rational

- [org-mode](http://orgmode.org/) is what I need.
- [Trello](http://trello.com/) is what my team need.
- [org-trello](https://github.com/ardumont/org-trello) is middle ground!

This will satisfy org-modians and trelloans won't see any difference!

# Emacs version

Tested on:
- GNU Emacs 24.1.1 (x86_64-pc-linux-gnu, X toolkit, Xaw3d scroll bars) of 2012-09-22 on batsu, modified by Debian (<= 0.0.9)
- GNU Emacs 24.3.1 (x86_64-pc-linux-gnu, X toolkit, Xaw3d scroll bars) of 2013-04-14 on marid, modified by Debian (from 0.1.0)

# Migration

0.1.5 -> 0.1.6

Org-trello does use more natural ways of dealing with checklist using checkboxes!

cf. [natural org format (from 0.1.6 onwards)](#natural-org-format-from-016-onwards) for more details.

0.1.1 -> 0.1.2:
- From the version 0.1.1, some of the http requests will be asynchronous.
For this, we use elnode as a proxy server before requesting trello.
elnode server is started on the port 9876.
You can always change this port

``` lisp
(setq *ORGTRELLO-PROXY-PORT* 9876)
```
Then `M-x orgtrello-proxy/reload`

# TL;DR

## Fast help

Yank this into a scratch buffer:
``` lisp
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; or (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(package-install 'org-trello)
(require 'org-trello)
;; to trigger org-trello for each org file
(add-hook 'org-mode-hook 'org-trello-mode)

```
then `M-x eval-buffer`

Now open an org-mode buffer, then hit: `C-c o h`

## Demo

### Synchronize one entity

[<img src="https://i1.ytimg.com/vi/ILPs74L5LFU/0.jpg" />](http://youtu.be/ILPs74L5LFU)

### Install

[<img src="https://i1.ytimg.com/vi/e3NzllAHbHY/0.jpg" />](http://youtu.be/e3NzllAHbHY)

### Setup key and token

[<img src="https://i1.ytimg.com/vi/ReUp1Wn5scc/0.jpg" />](http://youtu.be/ReUp1Wn5scc)

### Attach org file to trello board

[<img src="https://i1.ytimg.com/vi/2PT8K1HG-eY/0.jpg" />](http://youtu.be/2PT8K1HG-eY)

### Synchronize one complete entity

[<img src="https://i1.ytimg.com/vi/H8DXm5BLaD0/0.jpg" />](http://youtu.be/H8DXm5BLaD0)

### Synchronize from org to trello

[<img src="https://i1.ytimg.com/vi/d6SATWzhQhs/0.jpg" />](http://youtu.be/d6SATWzhQhs)

### Synchronize from trello to org

[<img src="https://i1.ytimg.com/vi/-ldo8gvhaTY/0.jpg" />](http://youtu.be/-ldo8gvhaTY)

### Create board and sync

[<img src="https://i1.ytimg.com/vi/6k4zRm6t8ZY/0.jpg" />](http://youtu.be/6k4zRm6t8ZY)

### Create board with your keywords

[<img src="https://i1.ytimg.com/vi/1UYYXjCwshs/0.jpg" />](http://youtu.be/1UYYXjCwshs)

### Use the checklist Luke!

[<img src="https://i1.ytimg.com/vi/M9xAvO3m_mU/0.jpg" />](http://youtu.be/M9xAvO3m_mU)

### Kill entities

[<img src="https://i1.ytimg.com/vi/C1c_m9LHyC0/0.jpg" />](http://youtu.be/C1c_m9LHyC0)

### Simple help routine

[<img src="https://i1.ytimg.com/vi/g-lW0lDFU5Y/0.jpg" />](http://youtu.be/g-lW0lDFU5Y)

### Check your Setup

[<img src="https://i1.ytimg.com/vi/1UYYXjCwshs/0.jpg" />](http://youtu.be/2OlmPMwtCAs)

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
Add this somewhere in your load file (~/.emacs or ~/.emacs.d/init.el).

``` lisp
(require 'orgtrello)
;; to have org-trello activated for all org file, uncomment this
;; (add-hook 'org-mode-hook 'org-trello-mode)
;; otherwise, M-x org-trello-mode
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

Keybindings        | Interactive commands                         | Description
-------------------|----------------------------------------------|-----------------------------------------------------------------------------------------------------------------------
<kbd>C-c o i</kbd> | `M-x org-trello/install-key-and-token`       | Install the keys and the access-token.
<kbd>C-c o I</kbd> | `M-x org-trello/install-board-and-lists-ids` | Select the board and attach the todo, doing and done list.
<kbd>C-c o d</kbd> | `M-x org-trello/check-setup`                 | Check that the setup is ok. If everything is ok, will simply display 'Setup ok!'
<kbd>C-c o x</kbd> | `M-x org-trello/delete-setup`                | Clean up the org buffer from all org-trello informations
<kbd>C-c o b</kbd> | `M-x org-trello/create-board`                | Create interactively a board and attach the org-mode file to this trello board.
<kbd>C-c o c</kbd> | `M-x org-trello/sync-entity`                 | Create/Update an entity (card/checklist/item) depending on its level and status. Do not deal with level superior to 4.
<kbd>C-c o C</kbd> | `M-x org-trello/sync-full-entity`            | Create/Update a complete entity card/checklist/item and its subtree (depending on its level).
<kbd>C-c o s</kbd> | `M-x org-trello/sync-to-trello`              | Synchronize the org-mode file to the trello board (org-mode -> trello).
<kbd>C-c o S</kbd> | `M-x org-trello/sync-from-trello`            | Synchronize the org-mode file from the trello board (trello -> org-mode).
<kbd>C-c o k</kbd> | `M-x org-trello/kill-entity`                 | Kill the entity (and its arborescence tree) from the trello board and the org buffer.
<kbd>C-c o K</kbd> | `M-x org-trello/kill-all-entities`           | Kill all the entities (and their arborescence tree) from the trello board and the org buffer.
<kbd>C-c o h</kbd> | `M-x org-trello/help-describing-bindings`    | This help message.

# Use cases

## Setup

1. open an org-mode file

2. Install the key and the token file (`C-c o i` or `M-x org-trello/install-key-and-token`).
This will open your browser to retrieve the needed informations (`consumer-key` then the `access-token`) and wait for your input in emacs.

*Remark:* This need to be done once and for all until you revoke such token.

3. Setup your org-mode file with your trello board (`C-c o I` or `M-x org-trello/install-board-and-lists-ids`).
This will present you a list of your actual boards. Select the one you want and hit enter.
This will edit your org-mode file with properties needed.

*Remarks:*
- This need to be done once for each org-mode file you want to sync with a trello board.
- You can create directly a board (`C-c o b` or `M-x orgtrello/do-create-board-and-lists`)

Now you are ready to use org-mode as usual.

## Formats

There is a new setup which is activated by default, using the natural org [checkboxes](http://orgmode.org/manual/Checkboxes.html).
Thanks to @sw1nn [for showing me this org feature, this is awesome!](https://github.com/ardumont/org-trello/issues/14).

### natural org format (from 0.1.6 onwards)

Activated by default.

```org-mode
- [-] LISP
  - [X] Emacs-Lisp
  - [X] Common-Lisp
  - [ ] Scheme
  - [X] Clojure
- [X] ML
  - [X] Haskell
  - [X] Ocaml
- [X] Hybrid
  - [X] Scala
- [ ] little more detail, this is level 2, so checklist in trello board
  - [ ] item 3
    - [ ] any entities with level superior to 4 are considered level 3
```

For example, once sync to trello, this looks like:

```
* IN-PROGRESS Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 521dc9f3edeabee47600401e
:END:
- [-] LISP                                                    :PROPERTIES: {"orgtrello-id":"521dc9f48e95d74636004107"}
  - [X] Emacs-Lisp                                            :PROPERTIES: {"orgtrello-id":"521dc9f7487c2e9b250047a5"}
  - [X] Common-Lisp                                           :PROPERTIES: {"orgtrello-id":"521dc9f7ae27842a36003b26"}
  - [ ] Scheme                                                :PROPERTIES: {"orgtrello-id":"521dc9f834f52df935003b15"}
  - [X] Clojure                                               :PROPERTIES: {"orgtrello-id":"521dc9f9c1b85c905f006b4e"}
- [X] ML                                                      :PROPERTIES: {"orgtrello-id":"521dc9f5d49a919614000266"}
  - [X] Haskell                                               :PROPERTIES: {"orgtrello-id":"521dc9fa5699f00b25003bd0"}
  - [X] Ocaml                                                 :PROPERTIES: {"orgtrello-id":"521dc9fb7ef4310554003ab3"}
- [X] Hybrid                                                  :PROPERTIES: {"orgtrello-id":"521dc9f6238d072770007217"}
  - [X] Scala                                                 :PROPERTIES: {"orgtrello-id":"521dc9fc8e95d74636004109"}
```

#### Migrate to 0.1.6

To migrate your 0.1.6 org trello buffer to the new format:
- simply push the content to trello (`C-c o s`)
- Erase the content of your buffer except for the org-trello properties (`#+` entries at the beginning of the file)
- and sync from trello again (`C-c o S`).

#### Reactivate

This is activated by default but if you change this and you want to get back:

```lisp
(org-trello/activate-natural-org-checkboxes)
```

### Original format (previous to 0.1.6)

```org-mode
* IN-PROGRESS Joy of FUN(ctional) LANGUAGES
** TODO LISP
*** DONE Emacs-Lisp
*** DONE Common-Lisp
*** Scheme
*** TODO Clojure
** DONE ML
*** DONE Haskell
*** DONE Ocaml
** DONE Hybrid
*** DONE Scala
** A little more explanation, this is level 2 so checklist on trello
*** item (level 3)
**** Any entity superior to level 4 are not considered for sync
```

Once synchronized, this looks like (largely prettier in emacs org buffer):

```org-mode
* IN-PROGRESS Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 521dc9f3edeabee47600401e
:END:
** TODO LISP
:PROPERTIES:
:orgtrello-id: 521dc9f48e95d74636004107
:END:
*** DONE Emacs-Lisp
:PROPERTIES:
:orgtrello-id: 521dc9f7487c2e9b250047a5
:END:
*** DONE Common-Lisp
:PROPERTIES:
:orgtrello-id: 521dc9f7ae27842a36003b26
:END:
*** TODO Scheme
:PROPERTIES:
:orgtrello-id: 521dc9f834f52df935003b15
:END:
*** DONE Clojure
:PROPERTIES:
:orgtrello-id: 521dc9f9c1b85c905f006b4e
:END:
** TODO ML
:PROPERTIES:
:orgtrello-id: 521dc9f5d49a919614000266
:END:
*** DONE Haskell
:PROPERTIES:
:orgtrello-id: 521dc9fa5699f00b25003bd0
:END:
*** DONE Ocaml
:PROPERTIES:
:orgtrello-id: 521dc9fb7ef4310554003ab3
:END:
** TODO Hybrid
:PROPERTIES:
:orgtrello-id: 521dc9f6238d072770007217
:END:
*** DONE Scala
:PROPERTIES:
:orgtrello-id: 521dc9fc8e95d74636004109
:END:

```

#### Activate

From 0.1.6 onwards, if you want to deactivate the default way, and get back to the original way, set this snippet somewhere in your init file:

```lisp
(org-trello/deactivate-natural-org-checkboxes)
```

## Creation step-by-step

The idea is this, you have 3 levels (cf. [possible format](#formats))
- level 1 - Card
- level 2 - Checklist
- level 3 - Item

- Card:
  - Place yourself on the `card-identity` and hit the binding `C-c o c`, this will sync the card (create) in the `TODO` column in your trello board.
  - You can edit the label and hit `C-c o c` again, this time, this will sync the card again (update) in trello
  - Change the status from TODO to any intermediary status, then hit the binding, this will move the card to the list `DOING` (depending on your todo keywords list).
  - Once done, move the status of the card from anything to DONE, hit the binding, this will move the card to the list `DONE`.

- Checklist:
  - Place yourself on the checklist `checklist`, hit the binding, this will add `checklist` as a checklist to your card `card-identity`
  - Rename your checklist and hit again the binding to update its label.

- Item:
  - Place yourself on your item and hit the binding, this will add the item to such checklist.
  - Change the label of the item and hit the binding, this will update its label.
  - Change the status of the item to `DONE` and hit the binding, this will check such item in trello.

## Card and deadline/due date

You can use [org-mode's deadline](http://orgmode.org/manual/Inserting-deadline_002fschedule.html), this is mapped to trello's due date notion during the synchronize step.

## Checklist and transitivity

By default now, the status of the checklist does transit to the items.
Thus:
- if the status of the checklist is DONE, every item will be synced to DONE and the org-mode buffer will be updated accordingly.
- if the status of the checklist is TODO, every item will be synced to TODO and the org-mode buffer will be updated accordingly.

If you do not want this, you can disable it by adding those lines to your emacs's startup file:

```lisp
(require 'org-trello)
(setq *ORGTRELLO-CHECKLIST-UPDATE-ITEMS* nil)
```

## Creation full entity

You can sync all the entities and their arborescence once.
Place yourself on the entity (card or checklist or item) and hit `C-c o C`.

## Sync org-mode file to trello board

You can sync all your org-mode file to trello.
Hit `C-c o s`.

## Sync org-mode file from trello board

You can sync the content of your trello board into an org-mode file.
Hit `C-c o S`.

This will update any already present entry in the org-mode file and create the one not created yet.

## Remove entity

You can remove any entity and its arborescence with `C-c o k`.
This will also remove the entry from the org-mode buffer.

# Errors

Here are the possible error messages you can have if trying to sync in bad conditions:

- without setuping the consumer-key and the access-token:
```
 - Setup problem - You need to install the consumer-key and the read/write access-token - C-c o i or M-x org-trello/install-board-and-lists-ids
```

- without setuping the org-mode buffer:
```
 - Setup problem.
Either you did not connect your org-mode buffer with a trello board, to correct this:
  * attach to a board through C-c o I or M-x org-trello/install-board-and-lists-ids
  * or create a board from scratch with C-c o b or M-x org-trello/create-board).
Either your org-mode's todo keyword list and your trello board lists are not named the same way (which they must).
For this, connect to trello and rename your board's list according to your org-mode's todo list.
Also, you can specify on your org-mode buffer the todo list you want to work with, for example: #+TODO: TODO DOING | DONE FAIL (hit C-c C-c to refresh the setup)
```

- If the board's list names are different from your org-mode's keyword list:

```
 - Setup problem.
Either you did not connect your org-mode buffer with a trello board, to correct this:
  * attach to a board through C-c o I or M-x org-trello/install-board-and-lists-ids
  * or create a board from scratch with C-c o b or M-x org-trello/create-board).
Either your org-mode's todo keyword list and your trello board lists are not named the same way (which they must).
For this, connect to trello and rename your board's list according to your org-mode's todo list.
Also, you can specify on your org-mode buffer the todo list you want to work with, for example: #+TODO: TODO DOING | DONE FAIL (hit C-c C-c to refresh the setup)
```

- no label on the card:
```
Cannot synchronize the card - missing mandatory label. Skip it...
```

- no label on the checklist:
```
Cannot synchronize the checklist - missing mandatory label. Skip it...
```

- no label on the item:
```
Cannot synchronize the item - missing mandatory label. Skip it...
```

- syncing the checklist without syncing the card first:
```
Cannot synchronize the checklist - the card must synchronized first. Skip it...
```

- syncing the item without syncing the checklist first:
```
Cannot synchronize the item - the checklist must be synchronized first. Skip it...
```

# proxy-admin

From the version 0.1.2 onwards, there is a proxy which is in charge of:
- consuming the synchronization of the entities
- requesting Trello for other actions too
- proxy admin web page

I also use it to make at disposition an admin web server - http://localhost:9876/proxy/admin/ - to be able to know which sync actions are running:
![screenshot_2013-08-23_10-03-54](https://f.cloud.github.com/assets/718812/1014903/c208ff34-0bca-11e3-895a-432f66453208.png)

# Mailing list

- subscripe: Send a mail to emacs.org.trello [AT] librelist.com to subscribe.
- unsubscribe: Send a mail to emacs.org.trello-unsubscribe [AT] librelist.com to unsubscribe.

# License

org-trello is free software under GPL v3. See COPYING file for details.
