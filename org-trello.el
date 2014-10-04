;;; org-trello.el --- Minor mode to synchronize org-mode buffer and trello board

;; Copyright (C) 2013 Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.6.2
;; Package-Requires: ((emacs "24") (dash "2.8.0") (s "1.9.0") (deferred "0.3.2") (request-deferred "0.1.0"))
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
;; - Either activate org-trello-mode in an org-buffer - M-x org-trello-mode
;;
;; - Or add this in your Emacs setup
;; (require 'org-trello)
;; (add-hook 'org-mode-hook 'org-trello-mode)
;;
;; 2) Once - Install the consumer-key and read/write access-token for org-trello to work in your name with your boards (C-c o i)
;; M-x org-trello/install-key-and-token
;; (See http://org-trello.github.io/trello-setup.html#credentials for more details)
;;
;; You may want:
;; - to connect your org buffer to an existing board (C-c o I).  Beware that this will only install properties needed to speak with trello board (nothing else).
;; M-x org-trello/install-board-metadata
;;
;; - to update an existing org-buffer connected to a trello board (C-c o u).
;; M-x org-trello/update-board-metadata
;;
;; - to create an empty board directly from a org-mode buffer (C-c o b)
;; M-x org-trello/create-board-and-install-metadata
;;
;; 3) Now check your setup is ok (C-c o d)
;; M-x org-trello/check-setup
;;
;; 6) For some more help (C-c o h)
;; M-x org-trello/help-describing-setup
;;
;; 7) The first time you attached your buffer to an existing trello board, you may want to bootstrap your org-buffer (C-u C-c o s)
;; C-u M-x org-trello/sync-buffer
;;
;; 8) Sync a card from Org to Trello (C-c o c / C-c o C)
;; M-x org-trello/sync-card
;;
;; 9) Sync a card from Trello to Org (C-u C-c o c / C-u C-c o C)
;; C-u M-x org-trello/sync-card
;;
;; 10) Sync complete org buffer to trello (C-c o s)
;; M-x org-trello/sync-buffer
;;
;; 11) As already mentioned, you can sync all the org buffer from trello (C-u C-c o s)
;; C-u M-x org-trello/sync-buffer
;;
;; 12) You can delete an entity, card/checklist/item at point (C-c o k)
;; M-x org-trello/kill-entity
;;
;; 13) You can delete all the cards (C-c o K / C-u C-c o k)
;; M-x org-trello/kill-cards / C-u M-x org-trello/kill-entity
;;
;; 14) You can directly jump to the trello card in the browser (C-c o j)
;; M-x org-trello/jump-to-trello-card
;;
;; 15) You can directly jump to the trello board in the browser (C-c o J / C-u C-c o j)
;; M-x org-trello/jump-to-trello-board / C-u M-x org-trello/jump-to-trello-card
;;
;; Now you can work with trello from the comfort of org-mode and Emacs
;;
;; Enjoy!
;;
;; More informations: https://org-trello.github.io
;; Issue tracker: https://github.com/org-trello/org-trello/issues

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

(defconst *ORGTRELLO/VERSION* "0.6.2" "Current org-trello version installed.")



(require 'org-trello-utils)
(require 'org-trello-setup)

(require 'org-trello-log)
(require 'org-trello-action)
(require 'org-trello-controller)
(require 'org-trello-buffer)

(org-trello/require-cl)



(defun org-trello/version ()
  "Org-trello version."
  (interactive)
  (orgtrello-log/msg *OT/NOLOG* "version: %s" *ORGTRELLO/VERSION*))



(defun org-trello/apply-deferred (computation)
  "Apply the deferred COMPUTATION."
  (with-current-buffer (current-buffer)
    (save-excursion
      (apply (car computation) (cdr computation)))))

(defun org-trello/apply (comp &optional current-buffer-to-save reload-org-setup nolog-p)
  "Apply org-trello computation COMP.
When CURRENT-BUFFER-TO-SAVE (buffer name) is provided, save such buffer.
When RELOAD-ORG-SETUP is provided, reload the org setup.
when NOLOG-P is specified, no output log."
  (lexical-let ((computation        comp)
                (prefix-log-message (cadr comp))
                (buffer-to-save     current-buffer-to-save)
                (reload-setup       reload-org-setup)
                (nolog-flag         nolog-p))
    (deferred:$
      (deferred:next (lambda () (save-excursion
                                  (with-local-quit
                                    (apply (car computation) (cdr computation))))))
      (deferred:nextc it (lambda ()
                           (when buffer-to-save (orgtrello-buffer/save-buffer buffer-to-save))
                           (when reload-setup (orgtrello-action/reload-setup!))
                           (unless nolog-flag (orgtrello-log/msg *OT/INFO* "%s - Done!" prefix-log-message))))
      (deferred:error it (lambda (x) (orgtrello-log/msg *OT/ERROR* "Main apply function - Problem during execution - '%s'!" x))))))

(defun org-trello/log-strict-checks-and-do (action-label action-fn &optional with-save-flag)
  "Given an ACTION-LABEL and an ACTION-FN, execute sync action.
If WITH-SAVE-FLAG is set, will do a buffer save and reload the org setup."
  (orgtrello-action/msg-controls-or-actions-then-do
   action-label
   '(orgtrello-controller/load-keys!
     orgtrello-controller/control-keys!
     orgtrello-controller/setup-properties!
     orgtrello-controller/control-properties!)
   action-fn))

(defun org-trello/log-light-checks-and-do (action-label action-fn &optional no-check-flag)
  "Given an ACTION-LABEL and an ACTION-FN, execute sync action.
If NO-CHECK-FLAG is set, no controls are done."
  (orgtrello-action/msg-controls-or-actions-then-do
   action-label
   (if no-check-flag nil '(orgtrello-controller/load-keys! orgtrello-controller/control-keys! orgtrello-controller/setup-properties!))
   action-fn))

(defun org-trello/abort-sync ()
  "Control first, then if ok, add a comment to the current card."
  (interactive)
  (deferred:clear-queue)
  (orgtrello-log/msg *OT/INFO* "Cancel actions done!"))

(defun org-trello/add-card-comments (&optional modifier)
  "Control first, then if ok, add a comment to the current card.
When MODIFIER is set, this will show the current card's comments."
  (interactive "P")
  (org-trello/apply (cons 'org-trello/log-strict-checks-and-do
                          (if modifier
                              '("Display current card's last comments" orgtrello-controller/do-show-card-comments!)
                            '("Add card comment" orgtrello-controller/do-add-card-comment!)))))

(defun org-trello/show-card-comments ()
  "Control first, then if ok, show a simple buffer with the current card's last comments."
  (interactive)
  (org-trello/apply '(org-trello/log-strict-checks-and-do "Display current card's last comments" orgtrello-controller/do-show-card-comments!)))

(defun org-trello/show-board-labels ()
  "Control first, then if ok, show a simple buffer with the current board's labels."
  (interactive)
  (org-trello/apply '(org-trello/log-strict-checks-and-do "Display current board's labels" orgtrello-controller/do-show-board-labels!)))

(defun org-trello/sync-card (&optional modifier)
  "Execute the sync of an entity and its structure to trello.
If MODIFIER is non nil, execute the sync entity and its structure from trello."
  (interactive "P")
  (org-trello/apply-deferred
   (cons 'org-trello/log-strict-checks-and-do
         (if modifier
             '("Request 'sync entity with structure from trello" orgtrello-controller/checks-then-sync-card-from-trello!)
           '("Request 'sync entity with structure to trello" orgtrello-controller/checks-then-sync-card-to-trello!)))))

(defun org-trello/sync-buffer (&optional modifier)
  "Execute the sync of the entire buffer to trello.
If MODIFIER is non nil, execute the sync of the entire buffer from trello."
  (interactive "P")
  (org-trello/apply-deferred
   (cons 'org-trello/log-strict-checks-and-do
         (if modifier
             '("Request 'sync org buffer from trello board'" orgtrello-controller/do-sync-buffer-from-trello!)
           '("Request 'sync org buffer to trello board'" orgtrello-controller/do-sync-buffer-to-trello!)))))

(defun org-trello/kill-entity (&optional modifier)
  "Execute the entity removal from trello and the buffer.
If MODIFIER is non nil, execute all entities removal from trello and buffer."
  (interactive "P")
  (org-trello/apply-deferred
   (cons 'org-trello/log-strict-checks-and-do
         (if modifier
             '("Delete all cards" orgtrello-controller/do-delete-entities)
           '("Delete entity at point (card/checklist/item)" orgtrello-controller/checks-then-delete-simple)))))

(defun org-trello/kill-cards ()
  "Execute all entities removal from trello and buffer."
  (interactive)
  (org-trello/apply-deferred '(org-trello/log-strict-checks-and-do "Delete Cards" orgtrello-controller/do-delete-entities)))

(defun org-trello/archive-card ()
  "Execute archive card at point."
  (interactive)
  (org-trello/apply-deferred '(org-trello/log-strict-checks-and-do "Archive Card at point" orgtrello-controller/checks-and-do-archive-card)))

(defun org-trello/archive-cards ()
  "Execute archive all the DONE cards from buffer."
  (interactive)
  (org-map-entries 'org-trello/archive-card "/DONE" 'file))

(defun org-trello/install-key-and-token ()
  "No control, trigger the setup installation of the key and the read/write token."
  (interactive)
  (org-trello/apply-deferred '(org-trello/log-light-checks-and-do "Setup key and token" orgtrello-controller/do-install-key-and-token 'do-no-checks)))

(defun org-trello/install-board-metadata ()
  "Control first, then if ok, trigger the setup installation of the trello board to sync with."
  (interactive)
  (org-trello/apply-deferred '(org-trello/log-light-checks-and-do "Install boards and lists" orgtrello-controller/do-install-board-and-lists)))

(defun org-trello/update-board-metadata ()
  "Control first, then if ok, trigger the update of the informations about the board."
  (interactive)
  (org-trello/apply-deferred '(org-trello/log-light-checks-and-do "Update board information" orgtrello-controller/do-update-board-metadata!)))

(defun org-trello/jump-to-trello-card (&optional modifier)
  "Jump from current card to trello card in browser.
If MODIFIER is not nil, jump from current card to board."
  (interactive "P")
  (org-trello/apply (cons 'org-trello/log-strict-checks-and-do
                          (if modifier
                              '("Jump to board" orgtrello-controller/jump-to-board!)
                            '("Jump to card" orgtrello-controller/jump-to-card!)))))

(defun org-trello/jump-to-trello-board ()
  "Jump to current trello board."
  (interactive)
  (org-trello/apply '(org-trello/log-strict-checks-and-do "Jump to board" orgtrello-controller/jump-to-board!)))

(defun org-trello/create-board-and-install-metadata ()
  "Control first, then if ok, trigger the board creation."
  (interactive)
  (org-trello/apply-deferred '(org-trello/log-light-checks-and-do "Create board and lists" orgtrello-controller/do-create-board-and-install-metadata)))

(defun org-trello/assign-me (&optional modifier)
  "Assign oneself to the card.
If MODIFIER is not nil, unassign oneself from the card."
  (interactive "P")
  (org-trello/apply (cons 'org-trello/log-light-checks-and-do
                          (if modifier
                              '("Unassign me from card" orgtrello-controller/do-unassign-me)
                            '("Assign myself to card" orgtrello-controller/do-assign-me)))
                    (current-buffer)))

(defun org-trello/check-setup ()
  "Check the current setup."
  (interactive)
  (org-trello/apply '(org-trello/log-strict-checks-and-do "Checking setup." orgtrello-controller/check-trello-connection!) nil nil 'no-log))

(defun org-trello/delete-setup ()
  "Delete the current setup."
  (interactive)
  (org-trello/apply '(org-trello/log-strict-checks-and-do "Delete current org-trello setup" orgtrello-controller/delete-setup!) (current-buffer)))

(defun org-trello/help-describing-bindings ()
  "A simple message to describe the standard bindings used."
  (interactive)
  (org-trello/apply `(message ,(orgtrello-setup/help-describing-bindings-template *ORGTRELLO/MODE-PREFIX-KEYBINDING* *org-trello-interactive-command-binding-couples*)) nil nil 'no-log))



;;;###autoload
(define-minor-mode org-trello-mode "Sync your org-mode and your trello together."
  :lighter " ot"
  :keymap org-trello-mode-map
  :group 'org-trello)

(defcustom org-trello-mode-hook nil
  "Define one org-trello hook for user to extend org-trello with their own behavior."
  :type 'hook
  :group 'org-trello)

(add-hook 'org-trello-mode-on-hook 'orgtrello-controller/mode-on-hook-fn)

(add-hook 'org-trello-mode-on-hook (lambda ()
                                     ;; install the bindings
                                     (orgtrello-setup/install-local-prefix-mode-keybinding! *ORGTRELLO/MODE-PREFIX-KEYBINDING*)
                                     ;; Overwrite the org-mode-map
                                     (define-key org-trello-mode-map [remap org-end-of-line] 'orgtrello-buffer/end-of-line!)
                                     (define-key org-trello-mode-map [remap org-return] 'orgtrello-buffer/org-return!)
                                     (define-key org-trello-mode-map [remap org-ctrl-c-ret] 'orgtrello-buffer/org-ctrl-c-ret!)
                                     (define-key org-trello-mode-map [remap org-archive-subtree] 'org-trello/archive-card)
                                     ;; a little message in the minibuffer to notify the user
                                     (orgtrello-log/msg *OT/NOLOG* (orgtrello-setup/startup-message *ORGTRELLO/MODE-PREFIX-KEYBINDING*)))
          'do-append)

(add-hook 'org-trello-mode-off-hook 'orgtrello-controller/mode-off-hook-fn)

(add-hook 'org-trello-mode-off-hook (lambda ()
                                      ;; remove the bindings when org-trello mode off
                                      (orgtrello-setup/remove-local-prefix-mode-keybinding! *ORGTRELLO/MODE-PREFIX-KEYBINDING*)
                                      ;; remove mapping override
                                      (define-key org-trello-mode-map [remap org-end-of-line] nil)
                                      (define-key org-trello-mode-map [remap org-return] nil)
                                      (define-key org-trello-mode-map [remap org-ctrl-c-ret] nil)
                                      (define-key org-trello-mode-map [remap org-archive-subtree] nil)
                                      ;; a little message in the minibuffer to notify the user
                                      (orgtrello-log/msg *OT/NOLOG* "org-trello/ot is off!"))
          'do-append)

(orgtrello-log/msg *OT/DEBUG* "org-trello loaded!")

(provide 'org-trello)
;;; org-trello.el ends here
