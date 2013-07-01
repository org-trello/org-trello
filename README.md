org-trello
==========

Sync your org-mode files with your trello boards.

# under heavy development

Not ready :D

# why?

- org-mode is what I need.
- Trello is what my team need.
- org-trello may satisfy everybody.

# Setup

## Trello related

1) Retrieve your trello api key from https://trello.com/1/appKey/generate
Then add those entries inside the file `~/.trello/config.el`:

```emacs-lisp
;; -*- lisp -*-
(defvar consumer-key "your-consumer-key")
```

2) then connect to this url with your browser
https://trello.com/1/authorize?response_type=token&name=org-trello&scope=read,write&expiration=never&key=<consumer-key>
Add another entry inside the `~/.trello/config.el`

```emacs-lisp
(defvar access-token "your-access-token")
```

Then you're good to go.

## emacs related

orgtrello is a minor mode for org-mode to sync.

``` emacs-lisp
;; orgtrello depends on dash, org-mode

(require 'orgtrello)
```

# Use case

1. open an org-mode file
2. edit the identity of the desired board (must have been presetup with the at least 3 columns - todo, doing, done)
3. create a todo list following this line:

```org-mode
* card-identity
** checklist
*** task1
*** task2
*** task3
```

*BINDING* C-c H (will change)

Trello:
- Card:
  - Place yourself on the `card-identity` and hit the binding *BINDING*, this will create the card in the `TODO` column in your trello board
  - You can edit the title and hit *BINDING*, this will update the title in trello
  - Change the status from TODO to any intermediary status, then hit the binding, this will move the card to the list `DOING`.
  - Once done, move the status of the card from anything to DONE, hit the binding, this will move the card to the list `DONE`.
- Checklist:
  - Place yourself on the checklist `checklist`, hit the binding, this will add `checklist` as a checklist to your card `card-identity`
  - Rename your checklist and hit again the binding to update its label

- Task:
  - Place yourself on your task and hit *BINDING*, this will add the item to such checklist.
  - Change the name of the task and hit *BINDING*, this will update its label
  - Change the status of the task to `DONE` and hit the binding, this will check such item in trello.

# Release note

[What has been done and remains to be done](./TODO.org)

# License

org-trello is free software under GPL v3. See COPYING file for details.
