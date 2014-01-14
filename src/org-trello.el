(require 'org-trello-log)
(require 'org-trello-setup)
(require 'org-trello-action)
(require 'org-trello-hash)
(require 'org-trello-api)
(require 'org-trello-data)
(require 'org-trello-cbx)
(require 'org-trello-query)
(require 'org-trello-controller)

;; #################### org-trello

(defun org-trello/sync-entity () "Control first, then if ok, create a simple entity."
  (interactive)
  (orgtrello-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting entity sync"
     '(orgtrello-controller/--setup-properties orgtrello-controller/--control-keys orgtrello-controller/--control-properties orgtrello-controller/--control-encoding)
     'orgtrello-controller/do-sync-entity))

(defun org-trello/sync-full-entity () "Control first, then if ok, create an entity and all its arborescence if need be."
  (interactive)
  (orgtrello-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting entity and structure sync"
     '(orgtrello-controller/--setup-properties orgtrello-controller/--control-keys orgtrello-controller/--control-properties orgtrello-controller/--control-encoding)
     'orgtrello-controller/do-sync-full-entity))

(defun org-trello/sync-to-trello () "Control first, then if ok, sync the org-mode file completely to trello."
  (interactive)
  (orgtrello-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting sync org buffer to trello board"
     '(orgtrello-controller/--setup-properties orgtrello-controller/--control-keys orgtrello-controller/--control-properties orgtrello-controller/--control-encoding)
     'orgtrello-controller/do-sync-full-file))

(defun org-trello/sync-from-trello () "Control first, then if ok, sync the org-mode file from the trello board."
  (interactive)
  ;; execute the action
  (orgtrello-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting sync org buffer from trello board"
     '(orgtrello-controller/--setup-properties orgtrello-controller/--control-keys orgtrello-controller/--control-properties orgtrello-controller/--control-encoding)
     'orgtrello-controller/do-sync-full-from-trello
     *do-save-buffer*))

(defun org-trello/kill-entity () "Control first, then if ok, delete the entity and all its arborescence."
  (interactive)
  (orgtrello-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting deleting entity"
     '(orgtrello-controller/--setup-properties orgtrello-controller/--control-keys orgtrello-controller/--control-properties orgtrello-controller/--control-encoding)
     'orgtrello-controller/do-delete-simple))

(defun org-trello/kill-all-entities () "Control first, then if ok, delete the entity and all its arborescence."
  (interactive)
  (orgtrello-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting deleting entities"
     '(orgtrello-controller/--setup-properties orgtrello-controller/--control-keys orgtrello-controller/--control-properties orgtrello-controller/--control-encoding)
     'orgtrello-controller/do-delete-entities))

(defun org-trello/install-key-and-token () "No control, trigger the setup installation of the key and the read/write token."
  (interactive)
  (orgtrello-action/--deal-with-consumer-msg-controls-or-actions-then-do
   "Setup key and token"
   nil
   'orgtrello-controller/do-install-key-and-token
   *do-save-buffer*
   *do-reload-setup*))

(defun org-trello/install-board-and-lists-ids () "Control first, then if ok, trigger the setup installation of the trello board to sync with."
  (interactive)
  (orgtrello-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Install boards and lists"
     '(orgtrello-controller/--setup-properties orgtrello-controller/--control-keys)
     'orgtrello-controller/do-install-board-and-lists
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/jump-to-card () "Jump to current card in browser."
  (interactive)
  (orgtrello-action/--controls-or-actions-then-do
     '(orgtrello-controller/--setup-properties orgtrello-controller/--control-keys orgtrello-controller/--control-properties orgtrello-controller/--control-encoding)
     (lambda ()
       (let* ((full-meta       (orgtrello-data/entry-get-full-metadata))
              (entity          (orgtrello-data/current full-meta))
              (right-entity-fn (cond ((orgtrello-data/entity-item-p entity)      'orgtrello-data/grandparent)
                                     ((orgtrello-data/entity-checklist-p entity) 'orgtrello-data/parent)
                                     ((orgtrello-data/entity-card-p entity)      'orgtrello-data/current))))
         (-if-let (card-id (->> full-meta (funcall right-entity-fn) orgtrello-data/entity-id))
                  (browse-url (org-trello/https-trello (format "/c/%s" card-id))))))))

(defun org-trello/jump-to-trello-board () "Jump to current trello board."
  (interactive)
  (orgtrello-action/--controls-or-actions-then-do
     '(orgtrello-controller/--setup-properties orgtrello-controller/--control-keys orgtrello-controller/--control-properties orgtrello-controller/--control-encoding)
     (lambda () (browse-url (org-trello/https-trello (format "/b/%s" (orgtrello-controller/--board-id)))))))

(defun org-trello/create-board () "Control first, then if ok, trigger the board creation."
  (interactive)
  (orgtrello-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Create board and lists"
     '(orgtrello-controller/--setup-properties orgtrello-controller/--control-keys)
     'orgtrello-controller/do-create-board-and-lists
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/assign-me () "Assign oneself to the card."
  (interactive)
  (orgtrello-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Create board and lists"
     '(orgtrello-controller/--setup-properties orgtrello-controller/--control-keys)
     'orgtrello-controller/do-assign-me
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/unassign-me () "Unassign oneself of the card."
  (interactive)
  (orgtrello-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Create board and lists"
     '(orgtrello-controller/--setup-properties orgtrello-controller/--control-keys)
     'orgtrello-controller/do-unassign-me
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/check-setup () "Check the current setup."
  (interactive)
  (orgtrello-action/--controls-or-actions-then-do
     '(orgtrello-controller/--setup-properties orgtrello-controller/--control-keys orgtrello-controller/--control-properties orgtrello-controller/--control-encoding)
     (lambda () (orgtrello-log/msg *OT/NOLOG* "Setup ok!"))))

(defun org-trello/delete-setup () "Delete the current setup."
  (interactive)
  (orgtrello-action/--deal-with-consumer-msg-controls-or-actions-then-do
   "Deleting current org-trello setup"
     '(orgtrello-controller/--setup-properties orgtrello-controller/--control-keys orgtrello-controller/--control-properties orgtrello-controller/--control-encoding)
     (lambda ()
       (orgtrello-controller/--remove-properties-file! *LIST-NAMES* *HMAP-USERS-NAME-ID* *ORGTRELLO-USER-LOGGED-IN* t) ;; remove any orgtrello relative entries
       (orgtrello-controller/--delete-property *ORGTRELLO-ID*)          ;; remove all properties orgtrello-id from the buffer
       (orgtrello-controller/--delete-property *ORGTRELLO-USERS-ENTRY*) ;; remove all properties users-assigned/member-ids
       (orgtrello-log/msg *OT/NOLOG* "Cleanup done!")) ;; a simple message to tell the user that the work is done!
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/activate-natural-org-checkboxes () "Activate the natural org-checkboxes - http://orgmode.org/manual/Checkboxes.html"
  (interactive)
  (setq *ORGTRELLO-NATURAL-ORG-CHECKLIST* t)
  (setq *ORGTRELLO-CHECKLIST-UPDATE-ITEMS* nil))

(defun org-trello/deactivate-natural-org-checkboxes () "Activate the natural org-checkboxes - http://orgmode.org/manual/Checkboxes.html"
  (interactive)
  (setq *ORGTRELLO-NATURAL-ORG-CHECKLIST* nil)
  (setq *ORGTRELLO-CHECKLIST-UPDATE-ITEMS* t))

(defun org-trello/--replace-string-prefix-in-string (keybinding string-to-replace)
  (replace-regexp-in-string "#PREFIX#" keybinding string-to-replace t))

(defun org-trello/--startup-message (keybinding)
  (let ((template-string "org-trello/ot is on! To begin with, hit #PREFIX# h or M-x 'org-trello/help-describing-bindings"))
    (replace-regexp-in-string "#PREFIX#" keybinding template-string t)))

(defun org-trello/--help-describing-bindings-template (keybinding list-command-binding-description) "Standard Help message template"
  (->> list-command-binding-description
       (--map (let ((command        (first it))
                    (prefix-binding (second it))
                    (help-msg       (third it)))
                (concat keybinding " " prefix-binding " - M-x " (symbol-name command) " - " help-msg)))
       (s-join "\n")))

(defun org-trello/help-describing-bindings () "A simple message to describe the standard bindings used."
  (interactive)
  (orgtrello-log/msg 0 (org-trello/--help-describing-bindings-template *ORGTRELLO-MODE-PREFIX-KEYBINDING* org-trello/--list-of-interactive-command-binding-couples)))

(defvar org-trello/--list-of-interactive-command-binding-couples
  '((org-trello/version                     "v" "Display the current version installed.")
    (org-trello/install-key-and-token       "i" "Install the keys and the access-token.")
    (org-trello/install-board-and-lists-ids "I" "Select the board and attach the todo, doing and done list.")
    (org-trello/check-setup                 "d" "Check that the setup is ok. If everything is ok, will simply display 'Setup ok!'.")
    (org-trello/assign-me                   "a" "Assign oneself to the card.")
    (org-trello/unassign-me                 "u" "Unassign oneself of the card")
    (org-trello/delete-setup                "D" "Clean up the org buffer from all org-trello informations.")
    (org-trello/create-board                "b" "Create interactively a board and attach the org-mode file to this trello board.")
    (org-trello/sync-from-trello            "S" "Synchronize the org-mode file from the trello board (trello -> org-mode).")
    (org-trello/sync-entity                 "c" "Create/Update an entity (card/checklist/item) depending on its level and status. Do not deal with level superior to 4.")
    (org-trello/sync-full-entity            "C" "Create/Update a complete entity card/checklist/item and its subtree (depending on its level).")
    (org-trello/kill-entity                 "k" "Kill the entity (and its arborescence tree) from the trello board and the org buffer.")
    (org-trello/kill-all-entities           "K" "Kill all the entities (and their arborescence tree) from the trello board and the org buffer.")
    (org-trello/sync-to-trello              "s" "Synchronize the org-mode file to the trello board (org-mode -> trello).")
    (org-trello/jump-to-card                "j" "Jump to card in browser.")
    (org-trello/jump-to-trello-board        "J" "Open the browser to your current trello board.")
    (org-trello/help-describing-bindings    "h" "This help message."))
  "List of command and default binding without the prefix key.")

(defun org-trello/--install-local-keybinding-map! (previous-org-trello-mode-prefix-keybinding org-trello-mode-prefix-keybinding interactive-command-binding-to-install)
  "Install locally the default binding map with the prefix binding of org-trello-mode-prefix-keybinding."
  (mapc (lambda (command-and-binding)
          (let ((command (first command-and-binding))
                (binding (second command-and-binding)))
            ;; unset previous binding
            (local-unset-key (kbd (concat previous-org-trello-mode-prefix-keybinding binding)))
            ;; set new binding
            (local-set-key (kbd (concat org-trello-mode-prefix-keybinding binding)) command)))
        interactive-command-binding-to-install))

(defvar *ORGTRELLO-MODE-PREFIX-KEYBINDING*          "C-c o" "The default prefix keybinding.")
(defvar *PREVIOUS-ORGTRELLO-MODE-PREFIX-KEYBINDING* "C-c o" "The memory default prefix keybinding.")

(defun org-trello/install-local-prefix-mode-keybinding! (keybinding) "Install the new default org-trello mode keybinding."
  (setq *PREVIOUS-ORGTRELLO-MODE-PREFIX-KEYBINDING* *ORGTRELLO-MODE-PREFIX-KEYBINDING*)
  (setq *ORGTRELLO-MODE-PREFIX-KEYBINDING* keybinding)
  (org-trello/--install-local-keybinding-map! *PREVIOUS-ORGTRELLO-MODE-PREFIX-KEYBINDING* *ORGTRELLO-MODE-PREFIX-KEYBINDING* org-trello/--list-of-interactive-command-binding-couples))

;;;###autoload
(define-minor-mode org-trello-mode "Sync your org-mode and your trello together."
  :lighter    " ot"
  :after-hook (org-trello/install-local-prefix-mode-keybinding! *ORGTRELLO-MODE-PREFIX-KEYBINDING*))

(add-hook 'org-trello-mode-on-hook
          (lambda ()
            ;; buffer-invisibility-spec
            (add-to-invisibility-spec '(org-trello-cbx-property)) ;; for an ellipsis (...) change to '(org-trello-cbx-property . t)
            ;; start the proxy
            (orgtrello-proxy/start)
            ;; a little message in the minibuffer to notify the user
            (orgtrello-log/msg *OT/NOLOG* (org-trello/--startup-message *ORGTRELLO-MODE-PREFIX-KEYBINDING*))))

(add-hook 'org-trello-mode-off-hook
          (lambda ()
            (remove-from-invisibility-spec '(org-trello-cbx-property)) ;; for an ellipsis (...) change to '(org-trello-cbx-property . t)
            ;; remove the highlight
            ;; stop the proxy
            (orgtrello-proxy/stop)
            ;; a little message in the minibuffer to notify the user
            (orgtrello-log/msg *OT/NOLOG* "org-trello/ot is off!")))

(orgtrello-log/msg *OT/DEBUG* "org-trello loaded!")

(provide 'org-trello)


