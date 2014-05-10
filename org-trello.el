;;; org-trello.el --- Minor mode to synchronize org-mode buffer and trello board

;; Copyright (C) 2013 Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.4.3
;; Package-Requires: ((dash "2.5.0") (request "0.2.0") (elnode "0.9.9.7.6") (esxml "0.3.0") (s "1.7.0") (db "0.0.6"))
;; Keywords: org-mode trello sync org-trello
;; URL: https://github.com/org-trello/org-trello

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Minor mode to sync org-mode buffer and trello board
;;
;; 1) Add the following to your emacs init file
;; (require 'org-trello)
;; (add-hook 'org-mode-hook 'org-trello-mode)
;;
;; 2) Once - Install the consumer-key and the read-write token for org-trello to be able to work in your name with your trello boards (C-c o i)
;; M-x org-trello/install-key-and-token
;;
;; You may want:
;; - to connect your org buffer to an existing board (C-c o I). Beware that this will only install properties needed to speak with trello board (nothing else).
;; M-x org-trello/install-board-and-lists-ids
;;
;; - to create an empty board directly from a org-mode buffer (C-c o b)
;; M-x org-trello/create-board
;;
;; 3) Now check your setup is ok (C-c o d)
;; M-x org-trello/check-setup
;;
;; 6) For some more help (C-c o h)
;; M-x org-trello/help-describing-setup
;;
;; 7) If you attached to an existing trello board, you may want to bootstrap your org-buffer (C-u C-c o s)
;; C-u M-x org-trello/sync-buffer
;;
;; Now you can work with trello from the comfort of org-mode and emacs
;; 8) Sync an entity from org to trello (C-c o c)
;; M-x org-trello/sync-entity
;;
;; 9) Sync an entity and its structure from org to trello (C-c o C)
;; M-x org-trello/sync-full-entity
;;
;; 10) Sync an entity from trello to org (C-u C-c o c)
;; C-u M-x org-trello/sync-entity
;;
;; 11) Sync an entity and its structure from trello to org (C-u C-c o C)
;; C-u M-x org-trello/sync-full-entity
;;
;; 12) Sync all the org buffer to trello (C-c o s)
;; M-x org-trello/sync-buffer
;;
;; 13) As already mentionned, you can sync all the org buffer from trello (C-u C-c o s)
;; C-u M-x org-trello/sync-buffer
;;
;; Enjoy!
;;
;; More informations on https://org-trello.github.io/org-trello

;;; Code:

(defconst *ORGTRELLO/ERROR-INSTALL-MSG* (format "Oops - your emacs isn't supported. org-trello only works on Emacs 24.3+ and you're running version: %s.
Please consider upgrading Emacs." emacs-version) "Error message when installing org-trello with an unsupported emacs version.")

(when (version< emacs-version "24") (error *ORGTRELLO/ERROR-INSTALL-MSG*))

;; Dependency on internal Emacs libs
(require 'org)
(require 'json)
(require 'parse-time)
(require 'timer)
(require 'align)

;; Dependency on external Emacs libs
(require 'dash)
(require 'request)
(require 'elnode)
(require 's)
(require 'esxml)
(require 'db)

(if (version< "24.3" emacs-version)
    (require 'cl-lib)
  (progn ;; need to alias the call
    (require 'cl)
    (defalias 'cl-defun 'defun*)
    (defalias 'cl-destructuring-bind 'destructuring-bind)))

(defconst *ORGTRELLO/VERSION* "0.4.3" "current org-trello version installed.")



(require 'org-trello-log)
(require 'org-trello-utils)
(require 'org-trello-setup)
(require 'org-trello-hash)
(require 'org-trello-action)
(require 'org-trello-db)
(require 'org-trello-data)
(require 'org-trello-cbx)
(require 'org-trello-api)
(require 'org-trello-query)
(require 'org-trello-backend)
(require 'org-trello-elnode)
(require 'org-trello-proxy)
(require 'org-trello-webadmin)
(require 'org-trello-server)
(require 'org-trello-buffer)
(require 'org-trello-input)
(require 'org-trello-controller)



(defun org-trello/proxy-do (action-label action-fn &optional with-save-flag)
  "Execute sync action."
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
   action-label
   '(orgtrello-controller/load-keys orgtrello-controller/control-keys orgtrello-controller/setup-properties orgtrello-controller/control-properties orgtrello-controller/control-encoding)
   action-fn
   (when with-save-flag 'do-save-buffer)
   (when with-save-flag 'do-reload-setup)))

(defun org-trello/proxy-do-and-save (action-label action-fn &optional no-check-flag)
  "Execute action and then save the buffer."
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
   action-label
   (if no-check-flag nil '(orgtrello-controller/load-keys orgtrello-controller/control-keys orgtrello-controller/setup-properties))
   action-fn
   'do-save-buffer
   'do-reload-setup))

(defun org-trello/do (action-fn)
  "First checks, then if controls ok, execute"
  (orgtrello-action/controls-or-actions-then-do
   '(orgtrello-controller/load-keys orgtrello-controller/control-keys orgtrello-controller/setup-properties orgtrello-controller/control-properties orgtrello-controller/control-encoding)
   action-fn))

(defun org-trello/reload-server ()
  "Reload the proxy and the webadmin server."
  (interactive)
  (orgtrello-server/reload))

(defun org-trello/abort-sync ()
  "Control first, then if ok, add a comment to the current card."
  (interactive)
  (org-trello/proxy-do "Abort sync activities" 'orgtrello-webadmin/delete-entities!))

(defun org-trello/add-card-comments ()
  "Control first, then if ok, add a comment to the current card."
  (interactive)
  (org-trello/proxy-do "Add card comment" 'orgtrello-controller/do-add-card-comment!))

(defun org-trello/show-card-comments ()
  "Control first, then if ok, show a simple buffer with the current card's last comments."
  (interactive)
  (org-trello/proxy-do "Display current card's last comments" 'orgtrello-controller/do-show-card-comments!))

(defun org-trello/show-board-labels ()
  "Control first, then if ok, show a simple buffer with the current board's labels."
  (interactive)
  (org-trello/proxy-do "Display current board's labels" 'orgtrello-controller/do-show-board-labels!))

(defun org-trello/sync-entity (&optional modifier)
  "Control first, then if ok, sync a simple entity (without its structure)."
  (interactive "P")
  (if modifier
      (org-trello/proxy-do "Request 'sync entity from trello'" 'orgtrello-controller/do-sync-entity-from-trello!)
    (org-trello/proxy-do "Request 'sync entity to trello'" 'orgtrello-controller/do-sync-entity-to-trello!)))

(defun org-trello/sync-full-entity (&optional modifier)
  "Control first, then if ok, create an entity and all its arborescence if need be. If modifier is nil, will sync *TO* trello, otherwise (called with C-u), will sync *FROM* trello."
  (interactive "P")
  (if modifier
      (org-trello/proxy-do "Request 'sync entity with structure from trello" 'orgtrello-controller/do-sync-entity-and-structure-from-trello!)
    (org-trello/proxy-do "Request 'sync entity with structure to trello" 'orgtrello-controller/do-sync-full-entity-to-trello!)))

(defun org-trello/sync-buffer (&optional modifier)
  "Will trigger a buffer sync action. If modifier is nil, will sync *TO* trello, otherwise (called with C-u), will sync *FROM* trello."
  (interactive "P")
  (if modifier
      (org-trello/proxy-do "Request 'sync org buffer from trello board'" 'orgtrello-controller/do-sync-full-file-from-trello!)
    (org-trello/proxy-do "Request 'sync org buffer to trello board'" 'orgtrello-controller/do-sync-full-file-to-trello!)))

(defun org-trello/kill-entity (&optional modifier)
  "Control first, then if ok, delete the entity and all its arborescence. If used with C-u, kill all buffer entities."
  (interactive "P")
  (if modifier
      (org-trello/kill-all-entities)
    (org-trello/proxy-do "Request 'delete entity'" 'orgtrello-controller/do-delete-simple)))

(defun org-trello/kill-all-entities ()
  "Control first, then if ok, delete the entity and all its arborescence."
  (interactive)
  (org-trello/proxy-do "Request - 'delete entities'" 'orgtrello-controller/do-delete-entities))

(defun org-trello/install-key-and-token ()
  "No control, trigger the setup installation of the key and the read/write token."
  (interactive)
  (org-trello/proxy-do-and-save "Setup key and token" 'orgtrello-controller/do-install-key-and-token 'do-no-checks))

(defun org-trello/install-board-and-lists-ids ()
  "Control first, then if ok, trigger the setup installation of the trello board to sync with."
  (interactive)
  (org-trello/proxy-do-and-save "Install boards and lists" 'orgtrello-controller/do-install-board-and-lists))

(defun org-trello/update-board-metadata ()
  "Control first, then if ok, trigger the update of the informations about the board."
  (interactive)
  (org-trello/proxy-do-and-save "Update board information" 'orgtrello-controller/do-update-board-metadata!))

(defun org-trello/jump-to-card (&optional modifier)
  "Jump to current card in browser. If C-u modifier is used, jump to board."
  (interactive "P")
  (if modifier
      (org-trello/jump-to-trello-board)
    (org-trello/do 'orgtrello-controller/jump-to-card!)))

(defun org-trello/jump-to-trello-board ()
  "Jump to current trello board."
  (interactive)
  (org-trello/do 'orgtrello-controller/jump-to-board!))

(defun org-trello/create-board ()
  "Control first, then if ok, trigger the board creation."
  (interactive)
  (org-trello/proxy-do-and-save "Create board and lists" 'orgtrello-controller/do-create-board-and-lists))

(defun org-trello/assign-me (&optional modifier)
  "Assign oneself to the card. With C-u modifier, unassign form the card."
  (interactive "P")
  (if modifier
      (org-trello/proxy-do-and-save "Unassign me from card" 'orgtrello-controller/do-unassign-me)
    (org-trello/proxy-do-and-save "Assign myself to card" 'orgtrello-controller/do-assign-me)))

(defun org-trello/check-setup ()
  "Check the current setup."
  (interactive)
  (org-trello/do (lambda () (orgtrello-log/msg *OT/NOLOG* "Setup ok!"))))

(defun org-trello/delete-setup ()
  "Delete the current setup."
  (interactive)
  (org-trello/proxy-do "Delete current org-trello setup" 'orgtrello-controller/delete-setup! 'do-save-buffer))

(defun org-trello/--startup-message (keybinding)
  "Compute org-trello's startup message."
  (orgtrello-utils/replace-in-string "#PREFIX#" keybinding "org-trello/ot is on! To begin with, hit #PREFIX# h or M-x 'org-trello/help-describing-bindings"))

(defun org-trello/--help-describing-bindings-template (keybinding list-command-binding-description)
  "Standard Help message template"
  (->> list-command-binding-description
    (--map (let ((command        (car it))
                 (prefix-binding (cadr it))
                 (help-msg       (caddr it)))
             (concat keybinding " " prefix-binding " - M-x " (symbol-name command) " - " help-msg)))
    (s-join "\n")))

(defun org-trello/help-describing-bindings ()
  "A simple message to describe the standard bindings used."
  (interactive)
  (orgtrello-log/msg 0 (org-trello/--help-describing-bindings-template *ORGTRELLO/MODE-PREFIX-KEYBINDING* *org-trello-interactive-command-binding-couples*)))

(defvar *org-trello-interactive-command-binding-couples*
  '((org-trello/version                      "v" "Display the current version installed.")
    (org-trello/install-key-and-token        "i" "Install the keys and the access-token.")
    (org-trello/install-board-and-lists-ids  "I" "Select the board and attach the todo, doing and done list.")
    (org-trello/check-setup                  "d" "Check that the setup is ok. If everything is ok, will simply display 'Setup ok!'.")
    (org-trello/assign-me                    "a" "Assign oneself to the card. With C-u modifier, unassign oneself from the card.")
    (org-trello/delete-setup                 "D" "Clean up the org buffer from all org-trello informations.")
    (org-trello/create-board                 "b" "Create interactively a board and attach the org-mode file to this trello board.")
    (org-trello/sync-entity                  "c" "Create/Update an entity (card/checklist/item) depending on its level and status. Do not deal with level superior to 4.")
    (org-trello/sync-full-entity             "C" "Create/Update a complete entity card/checklist/item and its subtree (depending on its level).")
    (org-trello/kill-entity                  "k" "Kill the entity (and its arborescence tree) from the trello board and the org buffer.")
    (org-trello/kill-all-entities            "K" "Kill all the entities (and their arborescence tree) from the trello board and the org buffer.")
    (org-trello/sync-buffer                  "s" "Synchronize the org-mode file to the trello board (org-mode -> trello). With prefix C-u, sync-from-trello (org-mode <- trello).")
    (org-trello/jump-to-card                 "j" "Jump to card in browser.")
    (org-trello/jump-to-trello-board         "J" "Open the browser to your current trello board.")
    (org-trello/show-card-comments           "o" "Display the card's comments in a pop-up buffer.")
    (org-trello/add-card-comments            "A" "Add a comment to the card.")
    (org-trello/show-board-labels            "l" "Display the board's labels in a pop-up buffer.")
    (org-trello/update-board-metadata        "u" "Update the buffer's trello board metadata.")
    (org-trello/abort-sync                   "g" "Abort synchronization activities.")
    (org-trello/help-describing-bindings     "h" "This help message."))
  "List of commands and default bindings without the prefix key.")

(defun org-trello/--install-local-keybinding-map! (previous-org-trello-mode-prefix-keybinding org-trello-mode-prefix-keybinding interactive-command-binding-to-install)
  "Install locally the default binding map with the prefix binding of org-trello-mode-prefix-keybinding."
  (mapc (lambda (command-and-binding)
          (let ((command (car command-and-binding))
                (binding (cadr command-and-binding)))
            ;; unset previous binding
            (define-key org-trello-mode-map (kbd (concat previous-org-trello-mode-prefix-keybinding binding)) nil)
            ;; set new binding
            (define-key org-trello-mode-map (kbd (concat org-trello-mode-prefix-keybinding binding)) command)))
        interactive-command-binding-to-install))

(defun org-trello/--remove-local-keybinding-map! (previous-org-trello-mode-prefix-keybinding interactive-command-binding-to-install)
  "Remove the default org-trello bindings."
  (mapc (lambda (command-and-binding)
          (let ((command (car command-and-binding))
                (binding (cadr command-and-binding)))
            (define-key org-trello-mode-map (kbd (concat previous-org-trello-mode-prefix-keybinding binding)) nil)))
        interactive-command-binding-to-install))

(defvar *ORGTRELLO/MODE-PREFIX-KEYBINDING*          "C-c o" "The default prefix keybinding.")
(defvar *PREVIOUS-ORGTRELLO/MODE-PREFIX-KEYBINDING* "C-c o" "The memory default prefix keybinding.")

(defun org-trello/install-local-prefix-mode-keybinding! (keybinding)
  "Install the new default org-trello mode keybinding."
  (setq *PREVIOUS-ORGTRELLO/MODE-PREFIX-KEYBINDING* *ORGTRELLO/MODE-PREFIX-KEYBINDING*)
  (setq *ORGTRELLO/MODE-PREFIX-KEYBINDING* keybinding)
  (org-trello/--install-local-keybinding-map! *PREVIOUS-ORGTRELLO/MODE-PREFIX-KEYBINDING* *ORGTRELLO/MODE-PREFIX-KEYBINDING* *org-trello-interactive-command-binding-couples*))

(defun org-trello/remove-local-prefix-mode-keybinding! (keybinding)
  "Install the new default org-trello mode keybinding."
  (org-trello/--remove-local-keybinding-map! *PREVIOUS-ORGTRELLO/MODE-PREFIX-KEYBINDING* *org-trello-interactive-command-binding-couples*))

;;;###autoload
(define-minor-mode org-trello-mode "Sync your org-mode and your trello together."
  :lighter " ot"
  :keymap  (make-sparse-keymap))

(defvar org-trello-mode-hook '() "org-trello-hook for user to extend org-trello with their own behavior.")

(defun org-trello-mode-on-hook-fn (&optional partial-mode)
  "Actions to do when org-trello starts."
  (unless partial-mode
    ;; install the bindings
    (org-trello/install-local-prefix-mode-keybinding! *ORGTRELLO/MODE-PREFIX-KEYBINDING*)
    ;; start the server which does some initialization on its own
    (orgtrello-server/start)
    ;; increment the number of buffers with org-trello mode on
    (orgtrello-db/increment-buffer-size *ORGTRELLO-SERVER/DB*)
    ;; buffer-invisibility-spec
    (add-to-invisibility-spec '(org-trello-cbx-property)) ;; for an ellipsis (...) change to '(org-trello-cbx-property . t)
    ;; installing hooks
    (add-hook 'before-save-hook 'orgtrello-buffer/install-overlays!) ;; before-change-functions
    ;; migrate all checkbox at org-trello mode activation
    (orgtrello-buffer/install-overlays!)
    ;; a little message in the minibuffer to notify the user
    (orgtrello-log/msg *OT/NOLOG* (org-trello/--startup-message *ORGTRELLO/MODE-PREFIX-KEYBINDING*))
    ;; Overwrite the org-mode-map
    (define-key org-trello-mode-map [remap org-end-of-line] 'org-trello/end-of-line!)
    ;; run hook at startup
    (run-hooks 'org-trello-mode-hook)))

(defun org-trello-mode-off-hook-fn (&optional partial-mode)
  "Actions to do when org-trello stops."
  (unless partial-mode
    ;; remove the bindings when org-trello mode off
    (org-trello/remove-local-prefix-mode-keybinding! *ORGTRELLO/MODE-PREFIX-KEYBINDING*)
    ;; decrement the number of buffers of 1
    (orgtrello-db/decrement-buffer-size *ORGTRELLO-SERVER/DB*)
    ;; stop the proxy server and webadmin
    (orgtrello-server/stop)
    ;; remove the invisible property names
    (remove-from-invisibility-spec '(org-trello-cbx-property)) ;; for an ellipsis (...) change to '(org-trello-cbx-property . t)
    ;; installing hooks
    (remove-hook 'before-save-hook 'orgtrello-buffer/install-overlays!)
    ;; remove org-trello overlays
    (orgtrello-buffer/remove-overlays!)
    ;; remove mapping override
    (define-key org-trello-mode-map [remap org-end-of-line] nil)
    ;; a little message in the minibuffer to notify the user
    (orgtrello-log/msg *OT/NOLOG* "org-trello/ot is off!")))

(defun org-trello/end-of-line! ()
  "Move the cursor at the end of the line. For a checkbox, move to the 1- point (because of overlays)."
  (interactive)
  (let* ((pt (save-excursion (org-end-of-line) (point)))
         (entity-level (-> (orgtrello-buffer/entry-get-full-metadata!) orgtrello-data/current orgtrello-data/entity-level)))
    (goto-char (if (or (= *ORGTRELLO/CHECKLIST-LEVEL* entity-level) (= *ORGTRELLO/ITEM-LEVEL* entity-level))
                   (-if-let (s (org-trello/compute-overlay-size!))
                       (- pt s 1)
                     pt)
                 pt))))

(defun org-trello/compute-overlay-size! ()
  "Compute the overlay size to the current position"
  (-when-let (o (car (overlays-in (point-at-bol) (point-at-eol))))
    (- (overlay-end o) (overlay-start o))))

(add-hook 'org-trello-mode-on-hook 'org-trello-mode-on-hook-fn)

(add-hook 'org-trello-mode-off-hook 'org-trello-mode-off-hook-fn)

(orgtrello-log/msg *OT/DEBUG* "org-trello loaded!")

(provide 'org-trello)
;;; org-trello.el ends here
