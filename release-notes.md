# 0.1.4

- Prepare the hierarchy files to avoid problem like https://github.com/ardumont/org-trello/issues/37#issuecomment-23151353
- Adding a version interactive command (it will help for asking people the version they use).
- Upgrade version
- Refactoring - simplify code
- Improve message labels
- Improve failure dealing regarding the sync
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
