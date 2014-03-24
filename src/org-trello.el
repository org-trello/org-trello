(defun org-trello/add-card-comments ()
  "Control first, then if ok, add a comment to the current card."
  (interactive)
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
   "Add card comment"
   '(orgtrello-controller/setup-properties orgtrello-controller/control-keys orgtrello-controller/control-properties orgtrello-controller/control-encoding)
   'orgtrello-controller/do-add-card-comment!))

(defun org-trello/show-card-comments ()
  "Control first, then if ok, show a simple buffer with the current card's last comments."
  (interactive)
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
   "Display current card's last comments"
   '(orgtrello-controller/setup-properties orgtrello-controller/control-keys orgtrello-controller/control-properties orgtrello-controller/control-encoding)
   'orgtrello-controller/do-show-card-comments!))

(defun org-trello/show-board-labels ()
  "Control first, then if ok, show a simple buffer with the current board's labels."
  (interactive)
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
   "Display current board's labels"
   '(orgtrello-controller/setup-properties orgtrello-controller/control-keys orgtrello-controller/control-properties orgtrello-controller/control-encoding)
   'orgtrello-controller/do-show-board-labels!))

(defun org-trello/sync-entity ()
  "Control first, then if ok, create a simple entity."
  (interactive)
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
     "Request 'sync entity'"
     '(orgtrello-controller/setup-properties orgtrello-controller/control-keys orgtrello-controller/control-properties orgtrello-controller/control-encoding)
     'orgtrello-controller/do-sync-entity))

(defun org-trello/sync-full-entity ()
  "Control first, then if ok, create an entity and all its arborescence if need be."
  (interactive)
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
     "Request 'sync entity with structure"
     '(orgtrello-controller/setup-properties orgtrello-controller/control-keys orgtrello-controller/control-properties orgtrello-controller/control-encoding)
     'orgtrello-controller/do-sync-full-entity))

(defun org-trello/sync-to-trello ()
  "Control first, then if ok, sync the org-mode file completely to trello."
  (interactive)
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
     "Request 'sync org buffer to trello board'"
     '(orgtrello-controller/setup-properties orgtrello-controller/control-keys orgtrello-controller/control-properties orgtrello-controller/control-encoding)
     'orgtrello-controller/do-sync-full-file))

(defun org-trello/sync-from-trello ()
  "Control first, then if ok, sync the org-mode file from the trello board."
  (interactive)
  ;; execute the action
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
     "Request 'sync org buffer from trello board'"
     '(orgtrello-controller/setup-properties orgtrello-controller/control-keys orgtrello-controller/control-properties orgtrello-controller/control-encoding)
     'orgtrello-controller/do-sync-full-from-trello
     *do-save-buffer*))

(defun org-trello/kill-entity ()
  "Control first, then if ok, delete the entity and all its arborescence."
  (interactive)
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
     "Request 'delete entity'"
     '(orgtrello-controller/setup-properties orgtrello-controller/control-keys orgtrello-controller/control-properties orgtrello-controller/control-encoding)
     'orgtrello-controller/do-delete-simple))

(defun org-trello/kill-all-entities ()
  "Control first, then if ok, delete the entity and all its arborescence."
  (interactive)
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
     "Request - 'delete entities'"
     '(orgtrello-controller/setup-properties orgtrello-controller/control-keys orgtrello-controller/control-properties orgtrello-controller/control-encoding)
     'orgtrello-controller/do-delete-entities))

(defun org-trello/install-key-and-token ()
  "No control, trigger the setup installation of the key and the read/write token."
  (interactive)
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
   "Setup key and token"
   nil
   'orgtrello-controller/do-install-key-and-token
   *do-save-buffer*
   *do-reload-setup*))

(defun org-trello/install-board-and-lists-ids ()
  "Control first, then if ok, trigger the setup installation of the trello board to sync with."
  (interactive)
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
     "Install boards and lists"
     '(orgtrello-controller/setup-properties orgtrello-controller/control-keys)
     'orgtrello-controller/do-install-board-and-lists
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/update-board-metadata ()
  "Control first, then if ok, trigger the update of the informations about the board."
  (interactive)
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
   "Update board information"
   '(orgtrello-controller/setup-properties orgtrello-controller/control-keys)
   'orgtrello-controller/do-update-board-metadata!
   *do-save-buffer*
   *do-reload-setup*))

(defun org-trello/jump-to-card ()
  "Jump to current card in browser."
  (interactive)
  (orgtrello-action/controls-or-actions-then-do
     '(orgtrello-controller/setup-properties orgtrello-controller/control-keys orgtrello-controller/control-properties orgtrello-controller/control-encoding)
     (lambda ()
       (let* ((full-meta       (orgtrello-data/entry-get-full-metadata!))
              (entity          (orgtrello-data/current full-meta))
              (right-entity-fn (cond ((orgtrello-data/entity-item-p entity)      'orgtrello-data/grandparent)
                                     ((orgtrello-data/entity-checklist-p entity) 'orgtrello-data/parent)
                                     ((orgtrello-data/entity-card-p entity)      'orgtrello-data/current))))
         (-if-let (card-id (->> full-meta (funcall right-entity-fn) orgtrello-data/entity-id))
                  (browse-url (org-trello/https-trello (format "/c/%s" card-id))))))))

(defun org-trello/jump-to-trello-board ()
  "Jump to current trello board."
  (interactive)
  (orgtrello-action/controls-or-actions-then-do
     '(orgtrello-controller/setup-properties orgtrello-controller/control-keys orgtrello-controller/control-properties orgtrello-controller/control-encoding)
     (lambda () (browse-url (org-trello/https-trello (format "/b/%s" (orgtrello-buffer/board-id!)))))))

(defun org-trello/create-board ()
  "Control first, then if ok, trigger the board creation."
  (interactive)
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
     "Create board and lists"
     '(orgtrello-controller/setup-properties orgtrello-controller/control-keys)
     'orgtrello-controller/do-create-board-and-lists
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/assign-me ()
  "Assign oneself to the card."
  (interactive)
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
     "Assign myself to card"
     '(orgtrello-controller/setup-properties orgtrello-controller/control-keys)
     'orgtrello-controller/do-assign-me
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/unassign-me ()
  "Unassign oneself of the card."
  (interactive)
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
     "Unassign me from card"
     '(orgtrello-controller/setup-properties orgtrello-controller/control-keys)
     'orgtrello-controller/do-unassign-me
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/check-setup ()
  "Check the current setup."
  (interactive)
  (orgtrello-action/controls-or-actions-then-do
     '(orgtrello-controller/setup-properties orgtrello-controller/control-keys orgtrello-controller/control-properties orgtrello-controller/control-encoding)
     (lambda () (orgtrello-log/msg *OT/NOLOG* "Setup ok!"))))

(defun org-trello/delete-setup ()
  "Delete the current setup."
  (interactive)
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
   "Delete current org-trello setup"
     '(orgtrello-controller/setup-properties orgtrello-controller/control-keys orgtrello-controller/control-properties orgtrello-controller/control-encoding)
     (lambda ()
       (orgtrello-controller/do-cleanup-from-buffer! t) ;; will do a global cleanup
       (orgtrello-log/msg *OT/NOLOG* "Cleanup done!")) ;; a simple message to tell the user that the work is done!
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/--replace-string-prefix-in-string (keybinding string-to-replace)
  (replace-regexp-in-string
   "#PREFIX#" keybinding string-to-replace t))

(defun org-trello/--startup-message (keybinding)
  (let ((template-string
         "org-trello/ot is on! To begin with, hit #PREFIX# h or M-x 'org-trello/help-describing-bindings"))
    (replace-regexp-in-string "#PREFIX#" keybinding template-string t)))

(defun org-trello/--help-describing-bindings-template (keybinding list-command-binding-description)
  "Standard Help message template"
  (->> list-command-binding-description
       (--map (let ((command        (first it))
                    (prefix-binding (second it))
                    (help-msg       (third it)))
                (concat keybinding " " prefix-binding " - M-x " (symbol-name command) " - " help-msg)))
       (s-join "\n")))

(defun org-trello/help-describing-bindings ()
  "A simple message to describe the standard bindings used."
  (interactive)
  (orgtrello-log/msg 0 (org-trello/--help-describing-bindings-template *ORGTRELLO-MODE-PREFIX-KEYBINDING* org-trello/--list-of-interactive-command-binding-couples)))

(defvar org-trello/--list-of-interactive-command-binding-couples
  '((org-trello/version                      "v" "Display the current version installed.")
    (org-trello/install-key-and-token        "i" "Install the keys and the access-token.")
    (org-trello/install-board-and-lists-ids  "I" "Select the board and attach the todo, doing and done list.")
    (org-trello/check-setup                  "d" "Check that the setup is ok. If everything is ok, will simply display 'Setup ok!'.")
    (org-trello/assign-me                    "a" "Assign oneself to the card.")
    (org-trello/unassign-me                  "u" "Unassign oneself of the card")
    (org-trello/delete-setup                 "D" "Clean up the org buffer from all org-trello informations.")
    (org-trello/create-board                 "b" "Create interactively a board and attach the org-mode file to this trello board.")
    (org-trello/sync-from-trello             "S" "Synchronize the org-mode file from the trello board (trello -> org-mode).")
    (org-trello/sync-entity                  "c" "Create/Update an entity (card/checklist/item) depending on its level and status. Do not deal with level superior to 4.")
    (org-trello/sync-full-entity             "C" "Create/Update a complete entity card/checklist/item and its subtree (depending on its level).")
    (org-trello/kill-entity                  "k" "Kill the entity (and its arborescence tree) from the trello board and the org buffer.")
    (org-trello/kill-all-entities            "K" "Kill all the entities (and their arborescence tree) from the trello board and the org buffer.")
    (org-trello/sync-to-trello               "s" "Synchronize the org-mode file to the trello board (org-mode -> trello).")
    (org-trello/jump-to-card                 "j" "Jump to card in browser.")
    (org-trello/jump-to-trello-board         "J" "Open the browser to your current trello board.")
    (org-trello/show-card-comments           "o" "Show the card's comments.")
    (org-trello/add-card-comments            "A" "Add a comment to the card.")
    (org-trello/help-describing-bindings     "h" "This help message."))
  "List of command and default binding without the prefix key.")

(defun org-trello/--install-local-keybinding-map! (previous-org-trello-mode-prefix-keybinding org-trello-mode-prefix-keybinding interactive-command-binding-to-install)
  "Install locally the default binding map with the prefix binding of org-trello-mode-prefix-keybinding."
  (mapc (lambda (command-and-binding)
          (let ((command (first command-and-binding))
                (binding (second command-and-binding)))
            ;; unset previous binding
            (define-key org-trello-mode-map (kbd (concat previous-org-trello-mode-prefix-keybinding binding)) nil)
            ;; set new binding
            (define-key org-trello-mode-map (kbd (concat org-trello-mode-prefix-keybinding binding)) command)))
        interactive-command-binding-to-install))

(defun org-trello/--remove-local-keybinding-map! (previous-org-trello-mode-prefix-keybinding interactive-command-binding-to-install)
  "Remove the default org-trello bindings."
  (mapc (lambda (command-and-binding)
          (let ((command (first command-and-binding))
                (binding (second command-and-binding)))
            (define-key org-trello-mode-map (kbd (concat previous-org-trello-mode-prefix-keybinding binding)) nil)))
        interactive-command-binding-to-install))

(defvar *ORGTRELLO-MODE-PREFIX-KEYBINDING*          "C-c o" "The default prefix keybinding.")
(defvar *PREVIOUS-ORGTRELLO-MODE-PREFIX-KEYBINDING* "C-c o" "The memory default prefix keybinding.")

(defun org-trello/install-local-prefix-mode-keybinding! (keybinding)
  "Install the new default org-trello mode keybinding."
  (setq *PREVIOUS-ORGTRELLO-MODE-PREFIX-KEYBINDING* *ORGTRELLO-MODE-PREFIX-KEYBINDING*)
  (setq *ORGTRELLO-MODE-PREFIX-KEYBINDING* keybinding)
  (org-trello/--install-local-keybinding-map! *PREVIOUS-ORGTRELLO-MODE-PREFIX-KEYBINDING* *ORGTRELLO-MODE-PREFIX-KEYBINDING* org-trello/--list-of-interactive-command-binding-couples))

(defun org-trello/remove-local-prefix-mode-keybinding! (keybinding)
  "Install the new default org-trello mode keybinding."
  (org-trello/--remove-local-keybinding-map! *PREVIOUS-ORGTRELLO-MODE-PREFIX-KEYBINDING* org-trello/--list-of-interactive-command-binding-couples))

;;;###autoload
(define-minor-mode org-trello-mode "Sync your org-mode and your trello together."
  :lighter " ot"
  :keymap  (make-sparse-keymap))

(defun org-trello-mode-on-hook-fn (&optional partial-mode)
  "Actions to do when org-trello starts."
  (unless partial-mode
          (org-trello/install-local-prefix-mode-keybinding! *ORGTRELLO-MODE-PREFIX-KEYBINDING*)
          (orgtrello-proxy/start)
          ;; buffer-invisibility-spec
          (add-to-invisibility-spec '(org-trello-cbx-property)) ;; for an ellipsis (...) change to '(org-trello-cbx-property . t)
          ;; installing hooks
          (add-hook 'before-save-hook 'orgtrello-controller/install-overlays!) ;; before-change-functions
          ;; migrate all checkbox at org-trello mode activation
          (orgtrello-controller/install-overlays!)
          ;; a little message in the minibuffer to notify the user
          (orgtrello-log/msg *OT/NOLOG* (org-trello/--startup-message *ORGTRELLO-MODE-PREFIX-KEYBINDING*))
          ;; Overwrite the org-mode-map
          (define-key org-trello-mode-map [remap org-end-of-line] 'org-trello/end-of-line!)))

(defun org-trello-mode-off-hook-fn (&optional partial-mode)
  "Actions to do when org-trello stops."
  (unless partial-mode
          (org-trello/remove-local-prefix-mode-keybinding! *ORGTRELLO-MODE-PREFIX-KEYBINDING*)
          (orgtrello-proxy/stop)
          ;; remove the invisible property names
          (remove-from-invisibility-spec '(org-trello-cbx-property)) ;; for an ellipsis (...) change to '(org-trello-cbx-property . t)
          ;; installing hooks
          (remove-hook 'before-save-hook 'orgtrello-controller/install-overlays!)
          ;; remove org-trello overlays
          (orgtrello-controller/remove-overlays!)
          ;; remove mapping override
          (define-key org-trello-mode-map [remap org-end-of-line] nil)
          ;; a little message in the minibuffer to notify the user
          (orgtrello-log/msg *OT/NOLOG* "org-trello/ot is off!")))

(defun org-trello/end-of-line! ()
  "Move the cursor at the end of the line. For a checkbox, move to the 1- point (because of overlays)."
  (interactive)
  (let* ((pt (save-excursion (org-end-of-line) (point)))
         (entity-level (-> (orgtrello-data/entry-get-full-metadata!) orgtrello-data/current orgtrello-data/entity-level)))
    (goto-char (if (or (= *CHECKLIST-LEVEL* entity-level) (= *ITEM-LEVEL* entity-level))
                   (-if-let (s (org-trello/compute-overlay-size!))
                            (- pt s 1)
                            pt)
                   pt))))

(defun org-trello/compute-overlay-size! ()
  "Compute the overlay size to the current position"
  (-when-let (o (first (overlays-in (point-at-bol) (point-at-eol))))
             (- (overlay-end o) (overlay-start o))))

(add-hook 'org-trello-mode-on-hook 'org-trello-mode-on-hook-fn)

(add-hook 'org-trello-mode-off-hook 'org-trello-mode-off-hook-fn)

(orgtrello-log/msg *OT/DEBUG* "org-trello loaded!")

(provide 'org-trello)


