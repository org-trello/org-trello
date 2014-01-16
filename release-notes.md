# 0.2.8.1

- [X] Fix error on synchronization from trello
- [X] Update doc
- [X] Release notes

# 0.2.8

- [X] Backlog
- [X] Synchronize description - https://github.com/ardumont/org-trello/issues/80
- [X] Use --reduce-from from dash instead of cl-reduce to reduce the cl deps
- [X] Version
- [X] Update doc
- [X] Release notes

# 0.2.7

- [X] Backlog
- [X] Create TODO template for the backlog start
- [X] Clean obsolete files (org-trello.org is no longer maintained, TODO-tests.org is no longer used)
- [X] Retry to split into `namespace` files - https://github.com/ardumont/org-trello/issues/93
  - [X] Split into `namespace` files
  - [X] Ensure packaging is generated and ok for marmalade
  - [X] Ensure packaging is generated and ok for melpa - yes, ensure that the root `org-trello.el` is generated before pushing on master.
  - [X] Ensure the tests are still ok
- [X] README-dev - https://github.com/ardumont/org-trello/issues/99
- [X] Rewrite the abstraction around fetched results to unify the `data model` (in and out identical) - https://github.com/ardumont/org-trello/issues/100
- [X] Unify the terms around users-assigned (org-trello) and members (trello)
- [X] Split the last org-trello namespace into 2 (`controller` for the orchestration function call triggered by interactive commands and `org-trello` for the interactive commands)
- [X] Unfold every entries before triggering the sync from trello to avoid problems similar as https://github.com/ardumont/org-trello/issues/53
- [X] Version
- [X] Release notes

# 0.2.6

- [X] Jump to card - https://github.com/ardumont/org-trello/issues/88
- [X] Rewrite convention for the "goto board" action into "jump to board"
- [X] Update README.md
- [X] Version
- [X] Release Note

# 0.2.5

- [X] Backlog updates
- [X] Global properties in upper case - https://github.com/ardumont/org-trello/issues/83
- [X] Hide the global properties - https://github.com/ardumont/org-trello/issues/77
- [X] Use of checkbox convention - https://github.com/ardumont/org-trello/issues/78
- [X] Jumping from emacs to the current trello board - https://github.com/ardumont/org-trello/issues/76
- [X] Simplify the update of the help menu to avoid possible desynchronisation with code
- [X] Update documentation + TOC
- [X] Version
- [X] Release notes

# 0.2.4

- [X] Sprint backlog
- [X] Some refactoring + tests coverage
- [X] Fix sync-from-trello - Merge org card's users assigned list and the trello one
- [X] Fix sync-from-trello - if new entities are referenced but not yet sync'ed on trello, they will disappear from the buffer when sync-from-trello (they should not) - https://github.com/ardumont/org-trello/issues/71
  - [X] Compute the entities without sync'ed properties
  - [X] Write them silly at the end of the computation of the sync'ed data (trello and org merge) to the org buffer
- [X] Upgrade version
- [X] Release notes

# 0.2.3

- [X] Version
- [X] Fix discrepancy between docstring and binding to delete the setup - https://github.com/ardumont/org-trello/issues/74
- [X] Use a prefix binding which does not override the emacs's default user prefix (keep the original for the moment to avoid disturbing people which already use it) - https://github.com/ardumont/org-trello/issues/72
- [X] Update documentation about the possibility to change the default prefix key
- [X] Fix ci-travis build due to change in cask installation command
- [X] Release notes

# 0.2.2

- [X] Version
- [X] Improve abstraction around data from trello (not complete yet)
- [X] Show people assigned to card - https://github.com/ardumont/org-trello/issues/67
  - [X] Compute user properties part from the board's informations
  - [X] Install board setup routine (C-c o I) also install board users
  - [X] Create board routine (C-c o i) should also install user boards (only the current user should then appear)
  - [X] Setuping properties before doing any actions
  - [X] Cleanup routine should also remove user global properties
  - [X] User assigns oneself to the card
  - [X] User unassigns oneself to the card
  - [X] sync-to-trello also assign users
  - [X] sync-to-trello also unassign users
  - [X] sync-from-trello also retrieve user informations and update the org buffer
  - [X] cleanup routine must cleanup card properties regarding user assigned
- [X] Update README about new command
- [X] Update README TOC
- [X] Release notes

# 0.2.1.2

- [X] Version
- [X] Release notes
- [X] Fix typography in naming windows-nt system

# 0.2.1.1

- [X] Version
- [X] Release notes
- [X] Fix error in packaging manipulation

# 0.2.1

- [X] Version
- [X] Improve sync from trello - In case of new item/checklist, the entry will be added at the end of the buffer instead of its rightful place.
- [X] Problem regarding some function on windows system - https://github.com/ardumont/org-trello/issues/62
- [X] Release notes

# 0.2.0

- [X] Version
- [X] Improve cleanup of the org-trello metadata
- [X] Videos about org-trello
  - [X] Webadmin
  - [X] Checkbox
  - [X] Sync from trello
  - [X] Cleanup trello board
  - [X] Cleanup org-trello buffe
- [X] Reference videos to README
- [X] Improve sync to trello
- [X] Release notes

# 0.1.9

- [X] Clean install org-trello
- [X] Ensure sync-from-trello keep the order of the checklists clean
- [X] Version
- [X] Release notes

# 0.1.8

- [X] Simplify Cask file
- [X] webadmin: Improve rendering [2/2]
  - [X] Better display for the delete buttons.
  - [X] Use css for the play/pause entities
- [X] Ensure checks before sync request to the proxy.
- [X] Fix sync full entity (related to map-checkbox) - https://github.com/ardumont/org-trello/issues/53
- [X] Improve the justify policy once and for all (this blinks at the moment!)
- [X] Version
- [X] Fix problem with archive/unarchive file routine
- [X] Fix problem with delete file routine (does not take place)
- [X] Add missing callback for the delete action
- [X] Improve the post-actions
  - [X] Justify once
  - [X] Saving once
  - [X] Unify the saving behaviour with the delete action

# 0.1.7.1

- [X] Fix problem requiring cl-lib - https://github.com/ardumont/org-trello/issues/51
- [X] Fix error on when-let - https://github.com/ardumont/org-trello/issues/54
- [X] Version
- [X] release notes

# 0.1.7

- [X] webadmin: Add an action button on action to stop it
- [X] webadmin: Add an action button to stop every running actions on entities
- [X] URLencode/Protect the data from the query before executing the query - https://github.com/ardumont/org-trello/issues/46
- [X] Some refactoring about:
  - [X] Docstring position (same level as function definition, this way, when toggling sexp, we see the docstring too)
  - [X] Simplification of cond statement
  - [X] Adding some missing tests
  - [X] Simplifying some code function (removing let when not needed)
- [X] Fix the scan problem with level 3 (must have been introduced with the refactoring from number to variable level)
- [X] Remove the marker notion and use the identifier in its place (we already use it as marker)
- [X] Fix the :PROPERTIES: font lock (which is not painted as keyword)
- [X] Improve the justify policy (trim the content before computing the justifying) -> There remains erratic behaviour
- [X] Improve map-checkbox to deal with limit (map over checkbox inferior to current level, at the moment, we scan all checkboxes)
- [X] Version
- [X] Release notes

# 0.1.6

- [X] Use the native org checklist to sync to trello
- [X] Sync entity and arborescence tree
- [X] Sync to trello must sync the native checklist if the flag is activated
- [X] Sync from trello must create native checklists if the flag is activated
- [X] Cleanup routine must cleanup the new checklist.
- [X] Delete entity must remove entity on point
- [X] Upgrade version
- [X] Automate the release to marmalade
- [X] Justify the #PROPERTIES# to the left for a better rendering
- [X] Keywordify the #PROPERTIES#
- [X] Merge the org :PROPERTIES: and the org-trello #PROPERTIES# into one.
- [X] Improve the activation/deactivation of the new way
- [X] Update the readme about the new checkbox mode
- [X] Release notes
- [X] Release

# 0.1.5

- Symmetry in the architecture - the deletion must pass through the consumer too.
- Upgrade version
- webadmin: Improve the rendering of the webadmin page to add headers and action
- webadmin: Add a current scanning entry in the webadmin page
- Defining log level using variable
- Redefine main function to sync an entity (they are badly named).
- Release notes
- Release

# 0.1.4

- Prepare the hierarchy files to avoid problem like https://github.com/ardumont/org-trello/issues/37#issuecomment-23151353
- Adding a version interactive command (it will help for asking people the version they use).
- Upgrade version
- Refactoring - simplify code
- Improve message labels
- Improve failure dealing regarding the sync
- Improve the marker computation
- Remove the label/title notion and use the name notion (same as trello), this will reduce error reasoning.
- Fix the format with missing argument on the callback sync success
- Improve the test regarding the synchro completion of a level
- Order when syncing to trello
- Release notes
- Release

# 0.1.3

- Leverage elnode's webserver capacity to display some basic monitoring about the synchronization
- Install bootstrap and jquery.js directly to avoid the user's manual installation (the first query to static files install bootstrap and jquery now)
- Simple end to end test - simple entity creation
- Improve the logging policy
- Improve the saving policy
- Upgrade version
- Release notes
- Release

# 0.1.2

- DONE Use an intermediary server between org and trello to permit asynchronous actions
- DONE Change org-trello's loading policy + update the readme about it
- DONE Find a way to make the proxy less verbose
- DONE Fix the loss of todo keywords after the synchronization -> no idea what's wrong here
- DONE Improve the attachment board routine to add the missing | keyword (hardcode with DONE for example)
- DONE Improve the clearing of already present heading metadata
- DONE Improve the help message to categorize the bindings
- DONE Improve the starting/stopping of org-trello
- DONE Remove end to end test as this can no longer works with asynchronous
- DONE Update the readme to explain the migration from 0.1.1 to 0.1.2
- DONE Render the sync to trello routine asynchronous
- DONE Render the sync complex entity routine asynchronous
- DONE Render the sync from trello asynchronous
- DONE Fix check setup routine
- DONE Interactive command to cleanup the org-trello data from the current buffer
- DONE Interactive command to delete all entities from the board and the org buffer
- DONE Update version
- DONE Release notes
- DONE Release

# 0.1.1

- DONE Literate org-trello
- DONE Testing the packaging, at the moment, this is done manually (make package & M-x package-install-file)
- DONE Introducing C-c o h at the loading of the buffer when ot is loaded
- DONE Update readme/main page with images on videos
- DONE Replace already present properties regarding boards before injecting new ones.
- DONE Improve the current attach board and list routine to avoid manual setup for the user
- DONE Update version
- DONE Update dependencies lib to more recent version.
- DONE Remove some warnings
- DONE Add some message for the user to warn the user to use UTF-8
- DONE Force utf-8 for the routine of board installation (create aend attach)
- DONE Some more refactoring
- DONE Refactoring: formatting code, install consume-key and access token routine improved, ...
- DONE Refactoring: UTF-8 warning for every routine + Forcing UTF-8 at setup time (create board and attach board routine)
- DONE Refactoring: Adding ^L break line inside the code to delimit the `namespace`
- DONE Refactoring: Improve the callback use by avoiding to specify them
- DONE Refactoring: Restart org-mode after synchronization to avoid losing the user's setup.
- DONE Refactoring: Fix some problems (version, checklist mechanism update, warning on format, create complex entity which sync only the current entity)
- DONE Release notes
- DONE Release

# 0.1.0

- DONE Fix importing of cards with due date - https://github.com/ardumont/org-trello/pull/22
- DONE Migrate carton to card.el (travis-ci builds break because of this)
- DONE Order of creation of list (from keywords) in trello does not match the order of the keyword
- DONE Abstract away the query (:method, :uri, ...) implementation
- DONE Abstract away the implementation of the org-data some more (access of the information from the map are embedded in the function instead of delegating to dedicated function, thus not hiding the implementation)
- DONE Version
- DONE Order when synchronizing from trello does not match trello's order
- DONE Refactor the sync from trello routine function (2 reduce embedded, only one is sufficient)
- DONE Enforce the order of the card accord to the org keywords after dumping the new entries from trello
- DONE Abstract away the return data from the http request
- DONE Update org version to the latest stable
- DONE Release notes

# v0.0.9

- DONE Update videos with the new features in the readme
- DONE Fix bug regarding the saving buffer routine
- DONE Refactoring action code regarding the message to display in the minibuffer (ATM we do not see what is done or not any longer)
- DONE Refactoring the http and http-sync functions
- DONE Improve the delete entity action by forcing the caret to go at the begin of the line of the heading before killing anything.
- DONE Update version
- DONE Release notes

# v0.0.8

- DONE Fix the cl-lib dependency
- DONE Improve the message when an action is done to let the user know when the action is done!
- DONE Better saving buffer policy
- DONE Sync org DEADLINE with trello card due - https://github.com/ardumont/org-trello/issues/17
- DONE Done, Todo on "check lists" - Once the user sets a "check list" to DONE or TODO, then check or uncheck all the tasks it contains. - https://github.com/ardumont/org-trello/issues/16
- DONE A simple and parametrable setup to make the user choose to use the checklist system or not
- DONE Upgrade version
- DONE Release notes
- DONE Release
- DONE Update documentation about deadline, checklist
- DONE Cleanup the debugging functions

# v0.0.7

- DONE Reference missing bindings in documentation
- DONE Upgrade version
- DONE Improve the error message around the setup (the setup message is too generic and must be more detailed)
- DONE Save the buffer after the synchronization is done (after all we write the trello ids in the buffer)
- DONE Release

# v0.0.6.1

- DONE Fix the json-1.3 dependency, sometimes only 1.2 is available
- DONE Update version
- DONE Release

# v0.0.6

- DONE Test that the title/label of the entity is set when syncing to avoid a 400 error
- DONE Before making the request, ensure that everything is ok (checklist needs the card id, items needs the checklist id, etc...)
- DONE Up the demo video in the beginning of the readme
- DONE Improve readme with links to org-mode and trello
- DONE Improve readme's use cases
- DONE Mention the possible errors
- DONE Mention the emacs version
- DONE Update release notes
- DONE Update the version
- DONE Make a release

# v0.0.5

- DONE Synchronizing from the trello board must simply computes and add/remove what's changed [100%]
- DONE Rewrite the release notes in their own file
- DONE Look on how to make a toc in the README.md in github
- DONE Update the release notes
- DONE Release on github, marmalade, melpa

# v0.0.4.1

- DONE Fix the creation board routine regarding the org-mode keywords (dynamic typing hell!)
- DONE Improve documentation regarding the possibilities to setup the org-mode keyword
- DONE Upgrade version to 0.0.4.1
- DONE Release on github, melpa, marmalade

# v0.0.4

- DONE Permit the user to deal with his/her own trello list (based on his/her org-mode keywords - cf. http://orgmode.org/manual/In_002dbuffer-settings.html)
- DONE Deploy on marmalade the stable version (and update the readme about it)
- DONE Rewrite tests using `expectations`
- DONE Simplify some code regarding destructuring for example
- DONE Remove useless code
- DONE Improve documentations and sync the routine check message with the documentation.
- DONE Update documentation
- DONE Release on github, melpa, marmalade

# v0.0.3

- DONE Syncing complex entities
- DONE cleanup useless tests
- DONE Namespace cleanup
- DONE Building package is now able to deal with the right version
- DONE Create a board from org-mode
- DONE Display the name of the board as a property file
- DONE Cleanup the useless controls
- DONE Given a org-mode file, fill in the trello board
- DONE Announce in emacs mailing list
- DONE Filter out the closed boards from the "choose board list"
- DONE filter out level > 4 when syncing.
- DONE Given a trello board, sync into a org-mode file
- cf. [todo/done](./TODO.org) for the remains
- DONE Release on github (and the previous version too)

# v0.0.2

- Technical release fixing technical details
- Fixing the packaging (inlining into org-trello.el)
- Adding ci-travis
- Local packaging to help testing

# v0.0.1

- write only mode at the moment (org-mode pushes to trello, no reading yet)
- simple entity creation (card, checklist, item/task), the request is asynchroneous
- entity deletion (card, checklist, item/task)
- Interactive command to ease the setup of the consumer-key and the access-token
- Interactive command to ease the setup of one org-mode file (which represents one trello board). I assume there exists
  a 'todo', 'doing', and 'done' list (named that way too)
- Control that the setup file (consumer-key and access-token) are rightly generated (to avoid later problem)
- Control that the properties on the current org-mode file are rightly setuped to access a trello board
- packaging for melpa
