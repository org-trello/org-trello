;;; org-trello-setup.el --- Constants + Variables setup for org-trello.
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-trello-utils)

(defgroup org-trello nil " Org-trello customisation group."
  :tag "Org-trello"
  :version "0.5.9"
  :group 'org)

(defconst org-trello-consumer-key nil
  "Id representing the user.")

(defalias '*consumer-key* 'org-trello-consumer-key) ;; for retro compatibility

(defconst org-trello-access-token nil
  "Read/write access token to use trello on behalf of the user.")

(defalias '*access-token* 'org-trello-access-token)

(defconst org-trello--label-key-marker "orgtrello-marker"
  "A marker used inside the org buffer to synchronize entries.")

(defconst org-trello--card-level 1
  "Card level.")

(defconst org-trello--checklist-level 2
  "Checkbox level.")

(defconst org-trello--item-level 3
  "Item level.")

(defconst org-trello--comment-level -2
  "Comment level.")

(defconst org-trello--out-of-bounds-level 4
  "Out of bounds level.")

(defconst org-trello--label-key-user-prefix "orgtrello-user-"
  "Org-trello prefix to define user to a 'org-mode' level.")

(defconst org-trello--property-users-entry "orgtrello-users"
  "Org-trello property entry to store the users assigned to a card.")

(defconst org-trello--property-user-me "orgtrello-user-me"
  "Current user's property id.")

(defconst org-trello--user-logged-in    nil
  "Current user logged in.")

(defconst org-trello--label-key-local-checksum "orgtrello-local-checksum"
  "Current card's checksum property.")

(defconst org-trello--title-buffer-information "*org-trello-information*"
  "Title for the org-trello buffers that display information.")

(defconst org-trello--property-deadline-prefix   "DEADLINE:"
  "Deadline (org's equivalent to trello's due date property) prefix.")

(defconst org-trello--https "https://trello.com"
  "URL https to help in browsing.")

(defconst org-trello--error-sync-card-missing-name
  "Cannot synchronize the card - missing mandatory name. Skip it...")

(defconst org-trello--error-sync-checklist-sync-card-first
  "Cannot synchronize the checklist - the card must be synchronized first. Skip it...")

(defconst org-trello--error-sync-checklist-missing-name
  "Cannot synchronize the checklist - missing mandatory name. Skip it...")

(defconst org-trello--error-sync-item-sync-card-first
  "Cannot synchronize the item - the card must be synchronized first. Skip it...")

(defconst org-trello--error-sync-item-sync-checklist-first
  "Cannot synchronize the item - the checklist must be synchronized first. Skip it...")

(defconst org-trello--error-sync-item-missing-name
  "Cannot synchronize the item - missing mandatory name. Skip it...")

(defconst org-trello--todo "TODO"
  "Org-mode todo state.")

(defconst org-trello--done "DONE"
  "Org-mode done state.")

(defconst org-trello--property-board-id "board-id"
  "Org-trello property board-id entry.")

(defconst org-trello--property-board-name "board-name"
  "Org-trello property board-name entry.")

(defvar org-trello--org-keyword-trello-list-names nil
  "Org-trello property names of the different lists.
This use the standard 'org-todo-keywords property from 'org-mode'.
This is intended as a buffer local variable.")

(defvar org-trello--hmap-list-orgkeyword-id-name       nil
  "Org-trello hash map containing for each id, the associated org keyword.
This is intended as a buffer local variable.")

(defvar org-trello--hmap-users-id-name nil
  "Org-trello hash map containing for each user name, the associated id.
This is intended as a buffer local variable.")

(defvar org-trello--hmap-users-name-id nil
  "Org-trello hash map containing for each user id, the associated name.
This is intended as a buffer local variable.")

(defvar org-trello--mode-activated-p nil
  "Flag to notify that the mode is activated or not.
This is intended as a buffer local variable.")

(defconst org-trello--checklist-indent 2 "Indentation for checklist.")

(defconst org-trello--item-indent 4 "Indentation for item.")

;; make variable buffer-local
(mapc (lambda (var)
        (make-variable-buffer-local var))
      '(org-trello-consumer-key
        org-trello-access-key
        org-trello--org-keyword-trello-list-names
        org-trello--hmap-list-orgkeyword-id-name
        org-trello--hmap-users-id-name
        org-trello--hmap-users-name-id
        org-trello--user-logged-in
        org-trello--mode-activated-p))

(defconst org-trello--config-dir "~/.trello"
  "Default trello directory for the configuration files.")

(defconst org-trello--config-file
  (concat org-trello--config-dir "/config.el"))

(defconst org-trello--label-key-id "orgtrello-id"
  "Key entry used for the trello identifier and the trello marker (the first sync).")

(defconst org-trello-buffer--indent-description 2
  "The default card description's indentation column.")

(defvar org-trello-interactive-command-binding-couples '() "List of commands and default bindings without the prefix key.")
(defalias '*org-trello-interactive-command-binding-couples* 'org-trello-interactive-command-binding-couples)

(setq org-trello-interactive-command-binding-couples
      '((org-trello/version                           "v" "Display the current version installed.")
        (org-trello/install-key-and-token             "i" "Install the keys and the access-token.")
        (org-trello/install-board-metadata            "I" "Select the board and attach the todo, doing and done list.")
        (org-trello/update-board-metadata             "u" "Update the buffer's trello board metadata.")
        (org-trello/create-board-and-install-metadata "b" "Create interactively a board and attach the newly created trello board with the current org file.")
        (org-trello/check-setup                       "d" "Check that the setup is ok. If everything is ok, will simply display 'Setup ok!'.")
        (org-trello/delete-setup                      "D" "Clean up the org buffer from all org-trello informations.")
        (org-trello/sync-card                         "c" "Create/Update a complete card.")
        (org-trello/sync-buffer                       "s" "Synchronize the org-mode file to the trello board (org-mode -> trello). With prefix C-u, sync-from-trello (org-mode <- trello).")
        (org-trello/archive-cards                     "A" "Archive all DONE cards.")
        (org-trello/abort-sync                        "g" "Abort synchronization activities.")
        (org-trello/kill-entity                       "k" "Kill the entity (and its arborescence tree) from the trello board and the org buffer.")
        (org-trello/kill-cards                        "K" "Kill all the entities (and their arborescence tree) from the trello board and the org buffer.")
        (org-trello/assign-me                         "a" "Assign oneself to the card. With C-u modifier, unassign oneself from the card.")
        (org-trello/add-card-comment                  "C" "Add a comment to the card. With C-u modifier, remove the current card's comment.")
        (org-trello/sync-comment                      "U" "Sync a comment to trello. With C-u modifier, remove the current card's comment.")
        (org-trello/show-board-labels                 "l" "Display the board's labels in a pop-up buffer.")
        (org-trello/jump-to-trello-card               "j" "Jump to card in browser.")
        (org-trello/jump-to-trello-board              "J" "Open the browser to your current trello board.")
        (org-trello/help-describing-bindings          "h" "This help message.")))

(defvar org-trello-mode-map (make-sparse-keymap)
  "Org-trello's mode map.")

(defun org-trello/compute-url (url-without-base-uri)
  "An helper method to compute the uri to trello from URL-WITHOUT-BASE-URI."
  (concat org-trello--https url-without-base-uri))

(defun org-trello/require-cl ()
  "Require cl lib."
  (if (version< "24.3" emacs-version)
      (require 'cl-lib)
    (progn ;; need to alias the call
      (require 'cl)
      (defalias 'cl-defun 'defun*)
      (defalias 'cl-destructuring-bind 'destructuring-bind))))

(defun orgtrello-setup/startup-message (prefix-keybinding)
  "Compute org-trello's startup message with the PREFIX-KEYBINDING."
  (orgtrello-utils/replace-in-string "#PREFIX#" prefix-keybinding "org-trello/ot is on! To begin with, hit #PREFIX# h or M-x 'org-trello/help-describing-bindings"))

(defun orgtrello-setup/help-describing-bindings-template (keybinding list-command-binding-description)
  "Standard Help message template from KEYBINDING and LIST-COMMAND-BINDING-DESCRIPTION."
  (->> list-command-binding-description
       (--map (let ((command        (car it))
                    (prefix-binding (cadr it))
                    (help-msg       (cadr (cdr it))))
                (concat keybinding " " prefix-binding " - M-x " (symbol-name command) " - " help-msg)))
       (s-join "\n")))

(defun orgtrello-setup/install-local-keybinding-map! (previous-org-trello-mode-prefix-keybinding org-trello-mode-prefix-keybinding interactive-command-binding-to-install)
  "Install locally the default binding map with the prefix binding of org-trello-mode-prefix-keybinding."
  (mapc (lambda (command-and-binding)
          (let ((command (car command-and-binding))
                (binding (cadr command-and-binding)))
            ;; unset previous binding
            (define-key org-trello-mode-map (kbd (concat previous-org-trello-mode-prefix-keybinding binding)) nil)
            ;; set new binding
            (define-key org-trello-mode-map (kbd (concat org-trello-mode-prefix-keybinding binding)) command)))
        interactive-command-binding-to-install))

(defun orgtrello-setup/remove-local-keybinding-map! (previous-org-trello-mode-prefix-keybinding interactive-command-binding-to-install)
  "Remove the default org-trello bindings."
  (mapc (lambda (command-and-binding)
          (let ((command (car command-and-binding))
                (binding (cadr command-and-binding)))
            (define-key org-trello-mode-map (kbd (concat previous-org-trello-mode-prefix-keybinding binding)) nil)))
        interactive-command-binding-to-install))

(defun orgtrello-setup/remove-local-prefix-mode-keybinding! (keybinding)
  "Install the new default org-trello mode keybinding."
  (orgtrello-setup/remove-local-keybinding-map! keybinding org-trello-interactive-command-binding-couples))

(defun orgtrello-setup/install-local-prefix-mode-keybinding! (keybinding)
  "Install the default org-trello mode keybinding."
  (orgtrello-setup/install-local-keybinding-map! keybinding keybinding org-trello-interactive-command-binding-couples))

(defvar org-trello--previous-prefix-keybinding "C-c o" "Previous or current mode prefix keybinding.")
(defcustom org-trello-current-prefix-keybinding "C-c o"
  "The default prefix keybinding to execute org-trello commands."
  :type 'string
  :require 'org-trello
  :set (lambda (variable prefix-keybinding)
         "Install the new default org-trello mode keybinding."
         (orgtrello-setup/install-local-keybinding-map! org-trello--previous-prefix-keybinding prefix-keybinding org-trello-interactive-command-binding-couples)
         `(set org-trello--previous-prefix-keybinding ,variable)
         (set variable prefix-keybinding))
  :group 'org-trello)

(defalias '*ORGTRELLO/MODE-PREFIX-KEYBINDING* 'org-trello-current-prefix-keybinding)

(provide 'org-trello-setup)
;;; org-trello-setup.el ends here
