;;; org-trello-setup.el --- Constants + Variables setup for org-trello.
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-trello-utils)
(require 'org-trello-log)

(defgroup org-trello nil " Org-trello customisation group."
  :tag "Org-trello"
  :version "0.5.9"
  :group 'org)

(defconst org-trello-consumer-key nil
  "Id representing the user.")

(defvar *consumer-key*) ;; for retro compatibility

(defconst org-trello-access-token nil
  "Read/write access token to use trello on behalf of the user.")

(defvar *access-token*)  ;; for retro compatibility

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

(defvar org-trello--user-logged-in nil
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

(defvar org-trello--hmap-list-orgkeyword-id-name nil
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

(defcustom orgtrello-setup-use-position-in-checksum-computation nil
  "Let the user decide if (s)he wants to use the position in the checksum.
The checksum is a hash computation of the current entity's data to prevent
sending too many sync requests if unnecessary (no real change since last time).

If t, when sync to trello, any change for the current entity will be reflected
in trello's board.  However, a global sync to trello, will trigger many
unnecessary sync to trello queries for any entity below the current entity
changed...  Indeed, the position changes anytime you insert or delete a char.
This renders almost useless the checksum computation.

Cf.  https://github.com/org-trello/org-trello-issues/271.
Please, do not hesitate to provide a better idea or a better implementation.

If nil, the default, the sync to trello will be limited to what's really changed
\(except for the position\).  So the entity's position in trello's board can be
slightly different than the one from the buffer."
  :group 'org-trello
  :version "0.7.1")

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
        org-trello--mode-activated-p
        ;; orgtrello-setup-use-position-in-checksum-computation ;; -> should ideally be that way, need to fix tests before that
        ))

(defconst org-trello--old-config-dir "~/.trello"
  "Old default trello directory.
As of 0.7.0, org-trello now follows Emacs's conventions.")

(defconst org-trello--config-dir (format "%s%s/" user-emacs-directory ".trello")
  "Default trello directory for the configuration files.")

(defconst org-trello--config-filename "%s.el"
  "Default org-trello's config filename.")

(defconst org-trello--old-config-file (expand-file-name (format "%s/%s" org-trello--old-config-dir "config.el"))
  "Absolute path to the old org-trello's config file.")

(defconst org-trello--config-file (expand-file-name (format "%s/%s" org-trello--config-dir org-trello--config-filename))
  "Absolute path to org-trello's config file.")

(defconst org-trello--label-key-id "orgtrello-id"
  "Key entry used for the trello identifier and the trello marker (the first sync).")

(defconst org-trello-buffer--indent-description 2
  "The default card description's indentation column.")

(defvar org-trello-interactive-command-binding-couples '()
  "List of commands and default bindings without the prefix key.")
(defalias '*org-trello-interactive-command-binding-couples*
  'org-trello-interactive-command-binding-couples)

(setq org-trello-interactive-command-binding-couples
      '((org-trello-version                           "v" "Display the current version installed.")
        (org-trello-install-key-and-token             "i" "Install the keys and the access-token.")
        (org-trello-install-board-metadata            "I" "Select the board and attach the todo, doing and done list.")
        (org-trello-update-board-metadata             "u" "Update the buffer's trello board metadata.")
        (org-trello-create-board-and-install-metadata "b" "Create interactively a board and attach the newly created trello board with the current org file.")
        (org-trello-check-setup                       "d" "Check that the setup is ok. If everything is ok, will simply display 'Setup ok!'.")
        (org-trello-delete-setup                      "D" "Clean up the org buffer from all org-trello informations.")
        (org-trello-sync-card                         "c" "Create/Update a complete card.")
        (org-trello-sync-buffer                       "s" "Synchronize the org-mode file to the trello board (org-mode -> trello). With prefix C-u, sync-from-trello (org-mode <- trello).")
        (org-trello-archive-cards                     "A" "Archive all DONE cards.")
        (org-trello-abort-sync                        "g" "Abort synchronization activities.")
        (org-trello-kill-entity                       "k" "Kill the entity (and its arborescence tree) from the trello board and the org buffer.")
        (org-trello-kill-cards                        "K" "Kill all the entities (and their arborescence tree) from the trello board and the org buffer.")
        (org-trello-assign-me                         "a" "Assign oneself to the card. With C-u modifier, unassign oneself from the card.")
        (org-trello-add-card-comment                  "C" "Add a comment to the card. With C-u modifier, remove the current card's comment.")
        (org-trello-sync-comment                      "U" "Sync a comment to trello. With C-u modifier, remove the current card's comment.")
        (org-trello-show-board-labels                 "l" "Display the board's labels in a pop-up buffer.")
        (org-trello-jump-to-trello-card               "j" "Jump to card in browser.")
        (org-trello-jump-to-trello-board              "J" "Open the browser to your current trello board.")
        (org-trello-bug-report                        "B" "Prepare a bug report message. With C-u modifier, opens a new issue in org-trello's github tracker too.")
        (org-trello-help-describing-bindings          "h" "This help message.")))

(defvar org-trello-mode-map (make-sparse-keymap)
  "Org-trello's mode map.")

(defun orgtrello-setup-compute-url (url-without-base-uri)
  "An helper method to compute the uri to trello from URL-WITHOUT-BASE-URI."
  (concat org-trello--https url-without-base-uri))

(defun orgtrello-setup-startup-message (prefix-keybinding)
  "Compute org-trello's startup message with the PREFIX-KEYBINDING."
  (orgtrello-utils-replace-in-string
   "#PREFIX#"
   prefix-keybinding
   "Hello master, help is `M-x org-trello-help-describing-bindings RET' or `#PREFIX# h'."))

(defun orgtrello-setup-help-describing-bindings-template (keybinding
                                                          command-binding-desc)
  "Standard Help message template from KEYBINDING and COMMAND-BINDING-DESC."
  (->> command-binding-desc
       (--map (let ((command        (car it))
                    (prefix-binding (cadr it))
                    (help-msg       (cadr (cdr it))))
                (concat keybinding " "
                        prefix-binding " - M-x "
                        (symbol-name command) " - "
                        help-msg)))
       (s-join "\n")))

(defun orgtrello-setup-install-local-keybinding-map (prev-mode-prefix-keybind
                                                     mode-prefix-keybind
                                                     interactive-command-binding)
  "PREV-MODE-PREFIX-KEYBIND old prefix keybinding.
MODE-PREFIX-KEYBIND new prefix keybinding
INTERACTIVE-COMMAND-BINDING the list of commands to install
Supercede the old prefix keybinding with the new one."
  (--map (-let (((command binding _) it))
           ;; unset previous binding
           (define-key org-trello-mode-map
             (kbd (concat prev-mode-prefix-keybind " " binding)) nil)
           ;; set new binding
           (define-key org-trello-mode-map
             (kbd (concat mode-prefix-keybind " " binding)) command))
         interactive-command-binding))

(defun orgtrello-setup-remove-local-keybinding-map (prev-mode-prefix-keybind
                                                    interactive-command-binding)
  "Remove the default org-trello bindings.
PREV-MODE-PREFIX-KEYBIND old prefix keybinding
INTERACTIVE-COMMAND-BINDING the list of commands to install."
  (--map (-let (((command binding _) it))
           (define-key org-trello-mode-map (kbd (concat prev-mode-prefix-keybind " " binding)) nil))
         interactive-command-binding))

(defun orgtrello-setup-remove-local-prefix-mode-keybinding (keybinding)
  "Install the new default org-trello mode KEYBINDING."
  (orgtrello-setup-remove-local-keybinding-map
   keybinding
   org-trello-interactive-command-binding-couples))

(defun orgtrello-setup-install-local-prefix-mode-keybinding (keybinding)
  "Install the default org-trello mode KEYBINDING."
  (orgtrello-setup-install-local-keybinding-map
   keybinding
   keybinding
   org-trello-interactive-command-binding-couples))

(defun orgtrello-setup-display-current-buffer-setup ()
  "Display current buffer's setup."
  (list :users-id-name org-trello--hmap-users-id-name
        :users-name-id org-trello--hmap-users-name-id
        :user-logged-in org-trello--user-logged-in
        :org-keyword-trello-list-names org-trello--org-keyword-trello-list-names
        :org-keyword-id-name org-trello--hmap-list-orgkeyword-id-name))

(defun orgtrello-setup-set-binding (current-prefix-binding-variable
                                    prefix-keybinding)
  "Install the default org-trello mode keybinding.
CURRENT-PREFIX-BINDING-VARIABLE is the current prefix binding variable to set.
PREFIX-KEYBINDING is the new binding."
  (when prefix-keybinding
    (let ((prev-prefix-keybinding (eval current-prefix-binding-variable)))
      (orgtrello-log-msg orgtrello-log-trace
                         "variable: %s\ncurrent value: %s\nnew binding: %s"
                         current-prefix-binding-variable
                         prev-prefix-keybinding
                         prefix-keybinding)
      ;; goal is to set the current-prefix-binding-variable with the value prefix-keybinding
      (orgtrello-setup-install-local-keybinding-map
       prev-prefix-keybinding
       prefix-keybinding
       org-trello-interactive-command-binding-couples)
      ;; update the current-prefix-binding-variable with the new binding
      (set current-prefix-binding-variable prefix-keybinding))))

(defconst org-trello-default-prefix-keybinding "C-c o"
  "Default org-trello's prefix keybinding.")

;;;###autoload
(defcustom org-trello-current-prefix-keybinding nil
  "The default prefix keybinding to execute org-trello commands."
  :type 'string
  :require 'org-trello
  :set 'orgtrello-setup-set-binding
  :group 'org-trello)

(custom-set-variables `(org-trello-current-prefix-keybinding ,org-trello-default-prefix-keybinding))

(defalias '*ORGTRELLO/MODE-PREFIX-KEYBINDING* 'org-trello-current-prefix-keybinding)

(defun orgtrello-setup-user-logged-in ()
  "Return the user logged in's name."
  org-trello--user-logged-in)

(defun orgtrello-setup-set-user-logged-in (user-logged-in)
  "Set the user logged in USER-LOGGED-IN."
  (setq org-trello--user-logged-in user-logged-in))

(provide 'org-trello-setup)
;;; org-trello-setup.el ends here
