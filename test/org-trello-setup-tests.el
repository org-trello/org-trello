(require 'org-trello-setup)
(require 'ert)
(require 'el-mock)

(ert-deftest test-orgtrello-setup/help-describing-bindings-template ()
  (should (equal "C-c o a - M-x some-action - some-description
C-c o 2 - M-x action2 - some other description" (orgtrello-setup/help-describing-bindings-template "C-c o" '((some-action "a" "some-description")
                                                                                                             (action2 "2" "some other description")))))
  (should (equal
           "C-c z v - M-x org-trello/version - Display the current version installed.
C-c z i - M-x org-trello/install-key-and-token - Install the keys and the access-token.
C-c z I - M-x org-trello/install-board-metadata - Select the board and attach the todo, doing and done list.
C-c z u - M-x org-trello/update-board-metadata - Update the buffer's trello board metadata.
C-c z b - M-x org-trello/create-board-and-install-metadata - Create interactively a board and attach the newly created trello board with the current org file.
C-c z d - M-x org-trello/check-setup - Check that the setup is ok. If everything is ok, will simply display 'Setup ok!'.
C-c z D - M-x org-trello/delete-setup - Clean up the org buffer from all org-trello informations.
C-c z c - M-x org-trello/sync-card - Create/Update a complete card.
C-c z s - M-x org-trello/sync-buffer - Synchronize the org-mode file to the trello board (org-mode -> trello). With prefix C-u, sync-from-trello (org-mode <- trello).
C-c z A - M-x org-trello/archive-cards - Archive all DONE cards.
C-c z g - M-x org-trello/abort-sync - Abort synchronization activities.
C-c z k - M-x org-trello/kill-entity - Kill the entity (and its arborescence tree) from the trello board and the org buffer.
C-c z K - M-x org-trello/kill-cards - Kill all the entities (and their arborescence tree) from the trello board and the org buffer.
C-c z a - M-x org-trello/assign-me - Assign oneself to the card. With C-u modifier, unassign oneself from the card.
C-c z C - M-x org-trello/add-card-comments - Add a comment to the card. With C-u modifier, display the current card's comments in a pop-up buffer.
C-c z l - M-x org-trello/show-board-labels - Display the board's labels in a pop-up buffer.
C-c z j - M-x org-trello/jump-to-trello-card - Jump to card in browser.
C-c z J - M-x org-trello/jump-to-trello-board - Open the browser to your current trello board.
C-c z h - M-x org-trello/help-describing-bindings - This help message."
           (orgtrello-setup/help-describing-bindings-template *ORGTRELLO/MODE-PREFIX-KEYBINDING* *org-trello-interactive-command-binding-couples*))))

(ert-deftest test-orgtrello-setup/startup-message ()
  (should (equal "org-trello/ot is on! To begin with, hit C-c o h or M-x 'org-trello/help-describing-bindings" (orgtrello-setup/startup-message "C-c o"))))
