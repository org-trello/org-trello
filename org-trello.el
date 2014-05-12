;;; org-trello.el --- Minor mode to synchronize org-mode buffer and trello board

;; Copyright (C) 2013 Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.4.4
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
;; 1) Add the following to your Emacs init file
;; (require 'org-trello)
;; (add-hook 'org-mode-hook 'org-trello-mode)
;;
;; 2) Once - Install the consumer-key and the read-write token for org-trello to be able to work in your name with your trello boards (C-c o i)
;; M-x org-trello/install-key-and-token
;;
;; You may want:
;; - to connect your org buffer to an existing board (C-c o I).  Beware that this will only install properties needed to speak with trello board (nothing else).
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
;; Now you can work with trello from the comfort of org-mode and Emacs
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

(defconst *ORGTRELLO/ERROR-INSTALL-MSG* (format "Oops - your Emacs isn't supported. org-trello only works on Emacs 24.3+ and you're running version: %s.
Please consider upgrading Emacs." emacs-version) "Error message when installing org-trello with an unsupported Emacs version.")

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

(defconst *ORGTRELLO/VERSION* "0.4.4" "Current org-trello version installed.")

(defun org-trello/version ()
  "Org-trello version."
  (interactive)
  (message "org-trello version: %s" *ORGTRELLO/VERSION*))



(require 'org-trello-log)
(require 'org-trello-utils)
(require 'org-trello-setup)
(require 'org-trello-action)
(require 'org-trello-server)
(require 'org-trello-proxy)
(require 'org-trello-controller)
(require 'org-trello-db)
(require 'org-trello-buffer)

(org-trello/require-cl)



(defun org-trello/proxy-do (action-label action-fn &optional with-save-flag)
  "Given an ACTION-LABEL and an ACTION-FN, execute sync action.
If WITH-SAVE-FLAG is set, will do a buffer save and reload the org setup."
  (orgtrello-proxy/deal-with-consumer-msg-controls-or-actions-then-do
   action-label
   '(orgtrello-controller/load-keys orgtrello-controller/control-keys orgtrello-controller/setup-properties orgtrello-controller/control-properties orgtrello-controller/control-encoding)
   action-fn
   (when with-save-flag 'do-save-buffer)
   (when with-save-flag 'do-reload-setup)))

(defun org-trello/proxy-do-and-save (action-label action-fn &optional no-check-flag)
  "Given an ACTION-LABEL and an ACTION-FN, execute sync action.
If NO-CHECK-FLAG is set, no controls are done."
  (orgtrello-proxy/deal-with-consumer-msg-controls-or-actions-then-do
   action-label
   (if no-check-flag nil '(orgtrello-controller/load-keys orgtrello-controller/control-keys orgtrello-controller/setup-properties))
   action-fn
   'do-save-buffer
   'do-reload-setup))

(defun org-trello/do (action-fn)
  "Check and if controls are ok, execute ACTION-FN."
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
  "Execute the sync of an entity to trello.
If MODIFIER is non nil, execute the sync entity from trello."
  (interactive "P")
  (if modifier
      (org-trello/proxy-do "Request 'sync entity from trello'" 'orgtrello-controller/do-sync-entity-from-trello!)
    (org-trello/proxy-do "Request 'sync entity to trello'" 'orgtrello-controller/do-sync-entity-to-trello!)))

(defun org-trello/sync-full-entity (&optional modifier)
  "Execute the sync of an entity and its structure to trello.
If MODIFIER is non nil, execute the sync entity and its structure from trello."
  (interactive "P")
  (if modifier
      (org-trello/proxy-do "Request 'sync entity with structure from trello" 'orgtrello-controller/do-sync-entity-and-structure-from-trello!)
    (org-trello/proxy-do "Request 'sync entity with structure to trello" 'orgtrello-controller/do-sync-full-entity-to-trello!)))

(defun org-trello/sync-buffer (&optional modifier)
  "Execute the sync of the entire buffer to trello.
If MODIFIER is non nil, execute the sync of the entire buffer from trello."
  (interactive "P")
  (if modifier
      (org-trello/proxy-do "Request 'sync org buffer from trello board'" 'orgtrello-controller/do-sync-full-file-from-trello!)
    (org-trello/proxy-do "Request 'sync org buffer to trello board'" 'orgtrello-controller/do-sync-full-file-to-trello!)))

(defun org-trello/kill-entity (&optional modifier)
  "Execute the entity removal from trello and the buffer.
If MODIFIER is non nil, execute all entities removal from trello and buffer."
  (interactive "P")
  (if modifier
      (org-trello/kill-all-entities)
    (org-trello/proxy-do "Request 'delete entity'" 'orgtrello-controller/do-delete-simple)))

(defun org-trello/kill-all-entities ()
  "Execute all entities removal from trello and buffer."
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
  "Jump from current card to trello card in browser.
If MODIFIER is not nil, jump from current card to board."
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
  "Assign oneself to the card.
If MODIFIER is not nil, unassign oneself from the card."
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

(defun org-trello/--startup-message (prefix-keybinding)
  "Compute org-trello's startup message with the PREFIX-KEYBINDING."
  (orgtrello-utils/replace-in-string "#PREFIX#" prefix-keybinding "org-trello/ot is on! To begin with, hit #PREFIX# h or M-x 'org-trello/help-describing-bindings"))

(defun org-trello/--help-describing-bindings-template (keybinding list-command-binding-description)
  "Standard Help message template from KEYBINDING and LIST-COMMAND-BINDING-DESCRIPTION."
  (->> list-command-binding-description
    (--map (let ((command        (car it))
                 (prefix-binding (cadr it))caddr
                 (help-msg       (cadr (cdr it))))
             (concat keybinding " " prefix-binding " - M-x " (symbol-name command) " - " help-msg)))
    (s-join "\n")))

(defun org-trello/help-describing-bindings ()
  "A simple message to describe the standard bindings used."
  (interactive)
  (orgtrello-log/msg 0 (org-trello/--help-describing-bindings-template *ORGTRELLO/MODE-PREFIX-KEYBINDING* *org-trello-interactive-command-binding-couples*)))

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
  :keymap org-trello-mode-map)

(defvar org-trello-mode-hook '()
  "Define one org-trello hook for user to extend org-trello with their own behavior.")

(add-hook 'org-trello-mode-on-hook 'orgtrello-controller/mode-on-hook-fn)

(add-hook 'org-trello-mode-on-hook (lambda ()
                                     ;; install the bindings
                                     (org-trello/install-local-prefix-mode-keybinding! *ORGTRELLO/MODE-PREFIX-KEYBINDING*)
                                     ;; Overwrite the org-mode-map
                                     (define-key org-trello-mode-map [remap org-end-of-line] 'orgtrello-buffer/end-of-line!)
                                     (define-key org-trello-mode-map [remap org-return] 'orgtrello-buffer/org-return!)
                                     (define-key org-trello-mode-map [remap org-ctrl-c-ret] 'orgtrello-buffer/org-ctrl-c-ret!)
                                     ;; a little message in the minibuffer to notify the user
                                     (orgtrello-log/msg *OT/NOLOG* (org-trello/--startup-message *ORGTRELLO/MODE-PREFIX-KEYBINDING*)))
          'do-append)

(add-hook 'org-trello-mode-off-hook 'orgtrello-controller/mode-off-hook-fn)

(add-hook 'org-trello-mode-off-hook (lambda ()
                                      ;; remove the bindings when org-trello mode off
                                      (org-trello/remove-local-prefix-mode-keybinding! *ORGTRELLO/MODE-PREFIX-KEYBINDING*)
                                      ;; remove mapping override
                                      (define-key org-trello-mode-map [remap org-end-of-line] nil)
                                      (define-key org-trello-mode-map [remap org-return] nil)
                                      (define-key org-trello-mode-map [remap org-ctrl-c-ret] nil)
                                      ;; a little message in the minibuffer to notify the user
                                      (orgtrello-log/msg *OT/NOLOG* "org-trello/ot is off!"))
          'do-append)

(orgtrello-log/msg *OT/DEBUG* "org-trello loaded!")

(provide 'org-trello)
;;; org-trello.el ends here
