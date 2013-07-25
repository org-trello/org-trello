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
