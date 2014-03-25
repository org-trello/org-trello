(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(expectations (desc "ORGTRELLO-MODE-PREFIX-KEYBINDING*")
 (expect "C-c o a - M-x some-action - some-description
C-c o 2 - M-x action2 - some other description" (org-trello/--help-describing-bindings-template "C-c o" '((some-action "a" "some-description")
                                                                                                          (action2 "2" "some other description")))))
(expectations (desc "org-trello/--help-describing-bindings-template")
  (expect
      "C-c o v - M-x org-trello/version - Display the current version installed.
C-c o i - M-x org-trello/install-key-and-token - Install the keys and the access-token.
C-c o I - M-x org-trello/install-board-and-lists-ids - Select the board and attach the todo, doing and done list.
C-c o d - M-x org-trello/check-setup - Check that the setup is ok. If everything is ok, will simply display 'Setup ok!'.
C-c o a - M-x org-trello/assign-me - Assign oneself to the card.
C-c o u - M-x org-trello/unassign-me - Unassign oneself from the card
C-c o D - M-x org-trello/delete-setup - Clean up the org buffer from all org-trello informations.
C-c o b - M-x org-trello/create-board - Create interactively a board and attach the org-mode file to this trello board.
C-c o c - M-x org-trello/sync-entity - Create/Update an entity (card/checklist/item) depending on its level and status. Do not deal with level superior to 4.
C-c o C - M-x org-trello/sync-full-entity - Create/Update a complete entity card/checklist/item and its subtree (depending on its level).
C-c o k - M-x org-trello/kill-entity - Kill the entity (and its arborescence tree) from the trello board and the org buffer.
C-c o K - M-x org-trello/kill-all-entities - Kill all the entities (and their arborescence tree) from the trello board and the org buffer.
C-c o s - M-x org-trello/sync-buffer - Synchronize the org-mode file to the trello board (org-mode -> trello). With prefix C-u, sync-from-trello (org-mode <- trello).
C-c o j - M-x org-trello/jump-to-card - Jump to card in browser.
C-c o J - M-x org-trello/jump-to-trello-board - Open the browser to your current trello board.
C-c o o - M-x org-trello/show-card-comments - Display the card's comments in a pop-up buffer.
C-c o A - M-x org-trello/add-card-comments - Add a comment to the card.
C-c o l - M-x org-trello/show-board-labels - Display the board's labels in a pop-up buffer.
C-c o U - M-x org-trello/update-board-metadata - Update the buffer's trello board metadata.
C-c o g - M-x org-trello/abort-sync - Abort synchronization activities.
C-c o h - M-x org-trello/help-describing-bindings - This help message."
      (org-trello/--help-describing-bindings-template *ORGTRELLO-MODE-PREFIX-KEYBINDING* org-trello/--list-of-interactive-command-binding-couples)))
