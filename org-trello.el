;;; org-trello.el -- Minor mode for org-mode to sync org-mode and trello

;; Copyright (C) 2013 Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Keywords: org-mode trello sync

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

;; Minor mode for org-mode to sync org-mode and trello
;;
;; 1) Add the following to your emacs init file
;; (require 'org-trello)
;;
;; Automatically
;; 2) Install the consumer-key and the read-write token for org-trello to be able to work in your name with your trello boards
;; M-x orgtrello-do-install-keys-and-token
;;
;; 3) For each org-mode file, you want to connect your org-mode file with a trello board
;; M-x orgtrello-do-install-board-and-lists
;;
;; Manually
;; 2) retrieve your trello api key https://trello.com/1/appKey/generate
;; Then add those entries inside the ~/.trello/config.el:
;; (defvar *consumer-key* "consumer-key")
;;
;; 3) then connect to this url with your browser
;; https://trello.com/1/authorize?response_type=token&name=org-trello&scope=read,write&expiration=never&key=<consumer-key>
;; Add another entry inside the `~/.trello/config.el`
;; (defvar *access-token* "your-access-token")
;;
;; 4) You need to make your org-mode buffer aware of trello.
;;
;; Add this to the top of your org-mode file
;;
;; #+property: board-id      <BOARD-ID>
;; #+property: todo-list-id  <TODO-LIST-ID>
;; #+property: doing-list-id <DOING-LIST-ID>
;; #+property: done-list-id  <DONE-LIST-ID>
;;
;;; Code:

(require 'org)
(require 'json)
(require 'dash)
(require 'request)
(require 'cl-lib)

(add-to-list 'load-path (concat default-directory  "/lib"))

(load-library "orgtrello-hash")
(load-library "orgtrello-data")
(load-library "orgtrello-api")
(load-library "orgtrello-query")
(load-library "orgtrello")

(require 'orgtrello)

(defun org-trello/--control-and-do (control-fns fn-to-control-and-execute)
  "Execute the function fn if control-fns is nil or if the result of apply every function to fn is ok."
  (if control-fns
      (progn
        (if (--all? (identity it) (--map (funcall it) control-fns));; beware, i'm calling control functions which have
            ;; side effects, not a good idea with mapcar but i need their respective result
            ;; ok, we call the function
            (funcall fn-to-control-and-execute)
          ;; there is some trouble, trying to help the user
          (message "You need to setup your:\n- consumer-key and your access-token for org-trello to work ok. Use M-x orgtrello-do-install-keys-and-token\n- org-mode file and connect it to trello. Use M-x orgtrello-do-install-board-and-lists")))
    (funcall fn-to-control-and-execute)))

(defun org-trello/create-simple-entity ()
  "Control first, then if ok, create a simple entity."
  (interactive)
  (message "Syncing entity...")
  (org-trello/--control-and-do '(orgtrello/--control-keys orgtrello/--control-properties) 'orgtrello-do-create-simple))

(defun org-trello/create-entity ()
  "Control first, then if ok, create an entity and all its arborescence if need be."
  (interactive)
  (message "Syncing complex entity...")
  (org-trello/--control-and-do '(orgtrello/--control-keys orgtrello/--control-properties) 'orgtrello-do-create-full-card))

(defun org-trello/kill-entity ()
  "Control first, then if ok, delete the entity and all its arborescence."
  (interactive)
  (message "Delete entity...")
  (org-trello/--control-and-do '(orgtrello/--control-keys orgtrello/--control-properties) 'orgtrello-do-delete-simple))

(defun org-trello/install-key-and-token ()
  "No control, trigger the setup installation of the key and the read/write token."
  (interactive)
  (message "Setup key and token...")
  (org-trello/--control-and-do nil 'orgtrello-do-install-keys-and-token))

(defun org-trello/install-board-and-lists-ids ()
  "Control first, then if ok, trigger the setup installation of the trello board to sync with."
  (interactive)
  (message "Install boards and lists...")
  (org-trello/--control-and-do '(orgtrello/--control-keys) 'orgtrello-do-install-board-and-lists))

(defun org-trello/help-describing-bindings ()
  "A simple message to describe the standard bindings used."
  (interactive)
  (message "C-c o c - Create/Update asynchronously simple a card/checklist/item depending on the level and status. Do not deal with level superior to 4.
C-c o k - Kill the arborescence tree and the corresponding entity.
C-c o i - Interactive command to install the keys and the access-token.
C-c o I - Interactive command to select the board and attach the todo, doing and done list.
C-c o h - This very binding to display this help menu."))

;;;###autoload
(define-minor-mode org-trello-mode "Sync your org-mode and your trello together."
  :lighter " ot" ;; the name on the modeline
  :keymap  (let ((map (make-sparse-keymap)))
             ;; binding will change
             (define-key map (kbd "C-c o c") 'org-trello/create-simple-entity)
             ;; (define-key map (kbd "C-c o C") 'org-trello/create-entity) ;; does not work yet
             (define-key map (kbd "C-c o k") 'org-trello/kill-entity)
             (define-key map (kbd "C-c o i") 'org-trello/install-key-and-token)
             (define-key map (kbd "C-c o I") 'org-trello/install-board-and-lists-ids)
             (define-key map (kbd "C-c o h") 'org-trello/help-describing-bindings)
             ;; for debugging purposes (I do not know any better yet)
             ;; (define-key map (kbd "C-c z") 'orgtrello-describe-heading)
             ;; (define-key map (kbd "C-c x") 'orgtrello-describe-headings)
             ;; (define-key map (kbd "C-c F") 'orgtrello-find-block)
             ;; define other bindings...
             map))

(add-hook 'org-mode-hook 'org-trello-mode)

(message "org-trello loaded!")

(provide 'org-trello)

;;; org-trello.el ends here
