# org-trello [![Build Status](https://travis-ci.org/ardumont/org-trello.png?branch=master)](https://travis-ci.org/ardumont/org-trello)

[Org](http://orgmode.org/) minor mode - 2-way sync [org](http://orgmode.org/) & [trello](http://trello.com/)

**Table of Contents**  *generated with [DocToc](http://doctoc.herokuapp.com/)*

- [Rationale](#rationale)
- [Emacs version](#emacs-version)
- [Migration](#migration)
	- [0.3.1](#031)
	- [0.2.9](#029)
	- [0.2.8](#028)
	- [0.2.1 -> 0.2.2](#021-->-022)
	- [0.1.5 -> 0.1.6](#015-->-016)
	- [0.1.1 -> 0.1.2](#011-->-012)
- [TL;DR](#tl;dr)
	- [Fast setup](#fast-setup)
	- [Prefix binding override in one shot](#prefix-binding-override-in-one-shot)
	- [Demo](#demo)
		- [Synchronize one entity](#synchronize-one-entity)
		- [Install](#install)
		- [Setup key and token](#setup-key-and-token)
		- [Attach org file to trello board](#attach-org-file-to-trello-board)
		- [Synchronize one complete entity](#synchronize-one-complete-entity)
		- [Synchronize from org to trello](#synchronize-from-org-to-trello)
		- [Sync from trello to org (new format)](#sync-from-trello-to-org-new-format)
		- [Synchronize from trello to org (old format)](#synchronize-from-trello-to-org-old-format)
		- [Create board and sync](#create-board-and-sync)
		- [Create board with your keywords](#create-board-with-your-keywords)
		- [Use the checkbox Luke](#use-the-checkbox-luke)
		- [Kill entities](#kill-entities)
		- [Cleanup routine](#cleanup-routine)
		- [org-trello - webadmin](#org-trello---webadmin)
		- [Simple help routine](#simple-help-routine)
		- [Check your Setup](#check-your-setup)
		- [Assign oneself to the card](#assign-oneself-to-the-card)
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
	- [Creation of a full entity](#creation-of-a-full-entity)
	- [Sync org-mode file to trello board](#sync-org-mode-file-to-trello-board)
	- [Sync org-mode file from trello board](#sync-org-mode-file-from-trello-board)
	- [Remove entity](#remove-entity)
	- [Remove entities](#remove-entities)
	- [Cleanup org-trello setup](#cleanup-org-trello-setup)
- [Errors](#errors)
- [proxy-admin](#proxy-admin)
- [Mailing list](#mailing-list)
- [License](#license)

# Rationale

- [org-mode](http://orgmode.org/) is what I need.
- [Trello](http://trello.com/) is what my team need.
- [org-trello](https://github.com/ardumont/org-trello) is the middle ground!

This will satisfy org-modians and trelloans won't see any difference!

# Emacs version

Tested on:
- GNU Emacs 24.3.1 (x86_64-pc-linux-gnu, X toolkit, Xaw3d scroll bars) of 2013-04-14 on marid, modified by Debian (from 0.1.0)
- GNU Emacs 24.3.1 (x86_64-pc-linux-gnu, GTK+ Version 3.6.0) of 2014-01-03 on chindi02, modified by Debian
- GNU Emacs 24.3.50.1 (x86_64-unknown-linux-gnu, GTK+ Version 2.24.22) of 2014-03-08 on dagobah

# Migration

## 0.3.1

The old checklist/item format has been removed - https://github.com/ardumont/org-trello/issues/105.
Now the org checkbox way is the standard one.

## 0.2.9

For information, from 0.2.9 onward, the property "orgtrello-id" from the checkbox (checklists, items) will be hidden.

*Note*
- Upon activating org-trello minor mode, all existing checkbox will be migrated and should disappear before your eyes.
- Symmetrically, when deactivating org-trello, all checkbox will appear.
- For this, org-trello use [overlays](https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlays.html) (implementation detail which permits to hide buffer region).

If you began to use org-trello, nothing to do.

## 0.2.8

From 0.2.8 onward, the card description can be synchronized too.
Just synchronize as usual.

## 0.2.1 -> 0.2.2

From the 0.2.2 version onward, we can assign people to card.
As a pre-requisite, we need to re-install the board (C-c o I), so that new properties will be installed (users currently assigned to the board we attach to).

This way, you will be able to use the assign (C-c o a) / unassign (C-c o u) yourself to the card.

## 0.1.5 -> 0.1.6

Org-trello now uses more natural ways of dealing with checklists using checkboxes!

cf. [natural org format (from 0.1.6 onwards)](#natural-org-format-from-016-onwards) for more details.

## 0.1.1 -> 0.1.2

- From version 0.1.1, some http requests will be asynchronous.
For this, we use elnode as a proxy server to make requests to trello.
The elnode server is started on the port 9876.
You can always change this port

``` lisp
(setq *ORGTRELLO-PROXY-PORT* 9876)
```
Then `M-x orgtrello-proxy/reload`

# TL;DR

## Fast setup

Yank this into a scratch buffer:
``` lisp
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(package-install 'elnode) ;; for some obscure reason, letting org-trello install elnode's dependencies fail!
(package-install 'org-trello)
(require 'org-trello)
;; to trigger org-trello for each org file
(add-hook 'org-mode-hook 'org-trello-mode)

```
then `M-x eval-buffer`

Now open an org-mode buffer, then hit: `C-c o h`

## Prefix binding override in one shot

If you are not happy with "C-c o" as default prefix, you can now override your default prefix keybinding.
For this, you need to install the following hook specifying the <prefix-key>:

``` lisp
(require 'org-trello)

(add-hook 'org-trello-mode-hook (lambda () (org-trello/install-local-prefix-mode-keybinding! <prefix-key>)))
```

For example, installing using the "C-c x" as prefix key:
``` lisp
(require 'org-trello)

(add-hook 'org-trello-mode-hook (lambda () (org-trello/install-local-prefix-mode-keybinding! "C-c x")))
```

*Note* If org-trello was already running, you will need to relaunch the mode (`M-x org-trello-mode` twice).

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

### Sync from trello to org (new format)

[<img src="https://i1.ytimg.com/vi/taIiWD-_zKY/0.jpg" />](http://youtu.be/taIiWD-_zKY)

### Synchronize from trello to org (old format)

[<img src="https://i1.ytimg.com/vi/-ldo8gvhaTY/0.jpg" />](http://youtu.be/-ldo8gvhaTY)

### Create board and sync

[<img src="https://i1.ytimg.com/vi/6k4zRm6t8ZY/0.jpg" />](http://youtu.be/6k4zRm6t8ZY)

### Create board with your keywords

[<img src="https://i1.ytimg.com/vi/1UYYXjCwshs/0.jpg" />](http://youtu.be/1UYYXjCwshs)

### Use the checkbox Luke

[<img src="https://i1.ytimg.com/vi/Sc9EUW67I7A/0.jpg" />](http://youtu.be/Sc9EUW67I7A)

### Kill entities

[<img src="https://i1.ytimg.com/vi/Cz0JikxKx4I/0.jpg" />](http://youtu.be/Cz0JikxKx4I)

### Cleanup routine

[<img src="https://i1.ytimg.com/vi/Wp7BXF3m9rA/0.jpg" />](http://youtu.be/Wp7BXF3m9rA)

### org-trello - webadmin

[<img src="https://i1.ytimg.com/vi/dp8S7VTwHCc/0.jpg" />](http://youtu.be/dp8S7VTwHCc)

### Simple help routine

[<img src="https://i1.ytimg.com/vi/g-lW0lDFU5Y/0.jpg" />](http://youtu.be/g-lW0lDFU5Y)

### Check your Setup

[<img src="https://i1.ytimg.com/vi/1UYYXjCwshs/0.jpg" />](http://youtu.be/2OlmPMwtCAs)

### Assign oneself to the card

TODO

# Contributions

- Pull Requests surely welcome (cf. [todo](./TODO.org) - use emacs org-mode buffer to read it)
- Appreciate any feedback
- Open issues if you want something to be done
- Open issues for bugs too (please describe maximum inputs, emacs version, org-trello version, *Messages* buffer, stacktrace, etc...)
- [What has been done and remains to be done](./TODO.org)

# Release notes

[Release notes](./release-notes.md)

# Install

Marmalade one is recommended.

## marmalade - Stable version

Add this to your emacs's init file (~/.emacs, ~/.emacs.d/init.el, or *scratch*, or whatnot...)

``` lisp
(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)
```

Then hit `M-x eval-buffer` to evaluate the buffer's contents.

## melpa - ~snapshot

Add this to your emacs's init file (~/.emacs, ~/.emacs.d/init.el, or *scratch*, or whatnot...)

``` lisp
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
```

Then hit `M-x eval-buffer` to evaluate the buffer's contents.

*Note*
Melpa and Marmalade repositories package the same org-trello code (I try to only merge in master through PR and both release comes from the master branch).
The divergence comes from the org-trello dependencies which are not the same version. So surely, the most stable remains marmalade.

## org-trello

To install org-trello, use this:

``` lisp
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(elnode org-trello)
  "A list of packages installed once at launch.")

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
(add-to-list 'load-path "/path/to/org-trello/")
(require 'org-trello)
```

## Example

- [Adding emacs repository](https://github.com/ardumont/install-packages-pack/blob/master/init.el)
- [Installing org-trello](https://github.com/ardumont/orgmode-pack/blob/master/init.el#L1)

# Setup

## Emacs related

Org-trello is a minor mode for org-mode to sync.
Add this somewhere in your load file (`~/.emacs` or `~/.emacs.d/init.el`).

``` lisp
(require 'org-trello)
;; to have org-trello activated for all org file, uncomment this
;; (add-hook 'org-mode-hook 'org-trello-mode)
;; otherwise, M-x org-trello-mode
```

For example, here is my [startup file](https://github.com/ardumont/orgmode-pack/blob/master/init.el#L3).

## Trello related

### keys

Install the consumer-key and the read-write token for org-trello to be able to work in your name with your trello boards.
First, ensure that the web browser that emacs is configured to use is started, and you are logged in to Trello in it.

Then, from an org-mode buffer:

`C-c o i`

or in any mode:

``` lisp
M-x org-trello/install-key-and-token
```

### Sync org to trello

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

Keybindings        | Interactive commands                             | Description
-------------------|--------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------
                   | `M-x org-trello/version`                         | org-trello's current version
<kbd>C-c o i</kbd> | `M-x org-trello/install-key-and-token`           | Install the keys and the access-token.
<kbd>C-c o I</kbd> | `M-x org-trello/install-board-and-lists-ids`     | Select the board and attach the todo, doing and done list.
<kbd>C-c o d</kbd> | `M-x org-trello/check-setup`                     | Check that the setup is ok. If everything is ok, will simply display 'Setup ok!'
<kbd>C-c o D</kbd> | `M-x org-trello/delete-setup`                    | Clean up the org buffer from all org-trello informations
<kbd>C-c o b</kbd> | `M-x org-trello/create-board`                    | Create interactively a board and attach the org-mode file to this trello board.
<kbd>C-c o c</kbd> | `M-x org-trello/sync-entity`                     | Create/Update an entity (card/checklist/item) depending on its level and status. Do not deal with level superior to 4.
<kbd>C-c o C</kbd> | `M-x org-trello/sync-full-entity`                | Create/Update a complete entity card/checklist/item and its subtree (depending on its level).
<kbd>C-c o s</kbd> | `M-x org-trello/sync-to-trello`                  | Synchronize the org-mode file to the trello board (org-mode -> trello).
<kbd>C-c o S</kbd> | `M-x org-trello/sync-from-trello`                | Synchronize the org-mode file from the trello board (trello -> org-mode).
<kbd>C-c o k</kbd> | `M-x org-trello/kill-entity`                     | Kill the entity (and its arborescence tree) from the trello board and the org buffer.
<kbd>C-c o K</kbd> | `M-x org-trello/kill-all-entities`               | Kill all the entities (and their arborescence tree) from the trello board and the org buffer.
<kbd>C-c o a</kbd> | `M-x org-trello/assign-me`                       | Assign one-self to the current card
<kbd>C-c o u</kbd> | `M-x org-trello/unassign-me`                     | Unassign one-self from the current card
<kbd>C-c o j</kbd> | `M-x org-trello/jump-to-card`                    | Jump to current trello card
<kbd>C-c o J</kbd> | `M-x org-trello/jump-to-trello-board`            | Jump to current trello board
<kbd>C-c o h</kbd> | `M-x org-trello/help-describing-bindings`        | This help message.

# Use cases

## Setup

1. open an org-mode file

2. Install the key and the token file (`C-c o i` or `M-x org-trello/install-key-and-token`).
This will open your browser to retrieve the needed information (`consumer-key` then the `access-token`) and wait for your input in emacs.

*Remark:* This only needs to be done once, until you revoke the token.

3. Setup your org-mode file with your trello board (`C-c o I` or `M-x org-trello/install-board-and-lists-ids`).
This will present you with a list of your actual boards. Select the one you want and hit enter.
This will edit your org-mode file to add the properties needed.

*Remarks:*
- This need to be done once for each org-mode file you want to sync with a trello board.
- You can directly create a board (`C-c o b` or `M-x orgtrello/do-create-board-and-lists`)

Now you are ready to use org-mode as usual.

## Formats

[Natural org checkboxes](http://orgmode.org/manual/Checkboxes.html).
Thanks to @sw1nn [for showing me this org feature, this is awesome!](https://github.com/ardumont/org-trello/issues/14).

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

For example, once synced to trello, this looks like:

```
* IN-PROGRESS Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 521dc9f3edeabee47600401e
:END:
- [-] LISP :PROPERTIES: {"orgtrello-id":"521dc9f48e95d74636004107"}
  - [X] Emacs-Lisp :PROPERTIES: {"orgtrello-id":"521dc9f7487c2e9b250047a5"}
  - [X] Common-Lisp :PROPERTIES: {"orgtrello-id":"521dc9f7ae27842a36003b26"}
  - [ ] Scheme :PROPERTIES: {"orgtrello-id":"521dc9f834f52df935003b15"}
  - [X] Clojure :PROPERTIES: {"orgtrello-id":"521dc9f9c1b85c905f006b4e"}
- [X] ML :PROPERTIES: {"orgtrello-id":"521dc9f5d49a919614000266"}
  - [X] Haskell :PROPERTIES: {"orgtrello-id":"521dc9fa5699f00b25003bd0"}
  - [X] Ocaml :PROPERTIES: {"orgtrello-id":"521dc9fb7ef4310554003ab3"}
- [X] Hybrid :PROPERTIES: {"orgtrello-id":"521dc9f6238d072770007217"}
  - [X] Scala :PROPERTIES: {"orgtrello-id":"521dc9fc8e95d74636004109"}
  ```

*Note* In org-trello buffer, the :PROPERTIES: for the checklists/items won't be visible for the user.

## Creation step-by-step

Basically, you have 3 levels (cf. [possible formats](#formats))
- level 1 - Card
- level 2 - Checklist
- level 3 - Item

Steps:
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

## Creation of a full entity

You can sync all the entities and their arborescence once.
Place yourself on the entity (card or checklist or item) and hit `C-c o C`.

## Sync org-mode file to trello board

You can sync all your org-mode file to trello.
Hit `C-c o s`.

## Sync org-mode file from trello board

You can sync the content of your trello board into an org-mode file.
Hit `C-c o S`.

This will update any entries that were already present in the org-mode file and create any entries that were not created yet.

## Remove entity

You can remove any entity and its arborescence from the board with `C-c o k`.
This will also remove the entry from the org-mode buffer.

## Remove entities

You can remove all entities from the board `C-c o K`.
This will also remove the entries from the org-mode buffer.

## Cleanup org-trello setup

You can remove all data from the org-mode buffer with `C-c o x`.
This will remove any org-trello related entries in your file (headers included).

# Errors

Here are the possible error messages you can get if trying to sync in bad conditions:

- without setting up the consumer-key and the access-token:
```
 - Setup problem - You need to install the consumer-key and the read/write access-token - C-c o i or M-x org-trello/install-board-and-lists-ids
```

- without setting up the org-mode buffer:
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

From version 0.1.2 onwards, there is a proxy which is in charge of:
- consuming the synchronization of the entities
- requesting that Trello do other actions too
- proxy admin web page

I also used it to put at your disposal an admin web server - [http://localhost:9876/proxy/admin/](http://localhost:9876/proxy/admin/) - so that you can know which sync actions are running:
![screenshot_2013-08-23_10-03-54](https://f.cloud.github.com/assets/718812/1014903/c208ff34-0bca-11e3-895a-432f66453208.png)

# Mailing list

- subscribe: Send a mail to emacs.org.trello [AT] librelist.com to subscribe.
- unsubscribe: Send a mail to emacs.org.trello-unsubscribe [AT] librelist.com to unsubscribe.

# License

org-trello is free software under the GPL v3. See the COPYING file for details.
