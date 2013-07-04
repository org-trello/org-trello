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

;; 1) Add the following to your emacs init file
;; (require 'org-trello)

;; Automatically
;; 2) Install the consumer-key and the read-write token for org-trello to be able to work in your name with your trello boards
;; M-x orgtrello-do-install-keys-and-token
;;
;; 3) For each org-mode file, you want to connect your org-mode file with a trello board
;; M-x orgtrello-do-install-board-and-lists

;; Manually
;; 2) retrieve your trello api key https://trello.com/1/appKey/generate
;; Then add those entries inside the ~/.trello/config.el:
;; ;; -*- lisp -*-
;; (defvar consumer-key "consumer-key")

;; 3) then connect to this url with your browser
;; https://trello.com/1/authorize?response_type=token&name=org-trello&scope=read,write&expiration=never&key=<consumer-key>
;; Add another entry inside the `~/.trello/config.el`
;; (defvar access-token "your-access-token")

;; 4) You need to make your org-mode buffer aware of trello.

;; Add this to the top of your org-mode file

;; #+property: board-id      <BOARD-ID>
;; #+property: todo-list-id  <TODO-LIST-ID>
;; #+property: doing-list-id <DOING-LIST-ID>
;; #+property: done-list-id  <DONE-LIST-ID>

;; Example:

;; #+title: todo orgtrello's dev progress
;; #+author: Antoine R. Dumont
;; #+property: board-id      50bcfd2f033110476000e768
;; #+property: todo-list-id  51d15c319c93af375200155f
;; #+property: doing-list-id 51d15c319c93af3752001500
;; #+property: done-list-id  51d15c319c93ag375200155f

;;; Code:

(add-to-list 'load-path "./utils")

;; query
(require 'org)
(require 'json)
(require 'dash)
(require 'request)
(require 'orgtrello)

(defvar *CONFIG-DIR* (concat (getenv "HOME") "/" ".trello"))
(defvar *CONFIG-FILE* (concat *CONFIG-DIR* "/config.el")
"1) retrieve your trello api key https://trello.com/1/appKey/generate
Then add those entries inside the ~/.trello/config.el:
\(defvar consumer-key 'consumer-key'\)
2) then connect to this url with your browser
https://trello.com/1/authorize?response_type=token&name=org-trello&scope=read,write&expiration=never&key=<consumer-key>
Add another entry inside the '~/.trello/config.el'
\(defvar access-token 'your-access-token'\)")

(defvar consumer-key nil)
(defvar access-token nil)

;; Properties key for the orgtrello headers #+PROPERTY board-id, etc...
(defvar *BOARD-ID*      "board-id"      "orgtrello property board-id entry")
(defvar *TODO-LIST-ID*  "todo-list-id"  "orgtrello property todo list id")
(defvar *DOING-LIST-ID* "doing-list-id" "orgtrello property doing list id")
(defvar *DONE-LIST-ID*  "done-list-id"  "orgtrello property done list id")

(defun org-trello/--control-properties ()
  "org-trello needs the properties board-id, todo-list-id, doing-list-id, done-list-id to be able to work ok."
  (and (assoc-default *BOARD-ID*      org-file-properties)
       (assoc-default *TODO-LIST-ID*  org-file-properties)
       (assoc-default *DOING-LIST-ID* org-file-properties)
       (assoc-default *DONE-LIST-ID*  org-file-properties)))

(defun org-trello/--control-keys ()
  "org-trello needs the consumer-key and the access-token to access the trello resources. Return t if everything is ok."
  (if (and consumer-key access-token)
      ;; everything is ok
      t
    ;; the data are not set,
    (progn
      (and (file-exists-p *CONFIG-FILE*)
           ;; trying to load them
           (load *CONFIG-FILE*)
           ;; still not loaded, something is not right!
           (and consumer-key access-token)))))

(defun org-trello/control-and-do (control-fns fn-to-control-and-execute)
  "Execute the function fn if control-fns is nil or if the result of apply every function to fn is ok."
  (if control-fns
      (progn
        (if (--all? (identity it) (--map (funcall it) control-fns))
            ;; ok, we call the function
            (funcall fn-to-control-and-execute)
          ;; there is some trouble, trying to help the user
          (message "You need to setup your:\n- consumer-key and your access-token for org-trello to work ok. Use M-x orgtrello-do-install-keys-and-token\n- org-mode file and connect it to trello. Use M-x orgtrello-do-install-board-and-lists")))
    (funcall fn-to-control-and-execute)))

;;;###autoload
(define-minor-mode org-trello-mode "Sync your org-mode and your trello together."
  :lighter " ot" ;; the name on the modeline
  :keymap  (let ((map (make-sparse-keymap)))
             ;; binding will change
             (define-key map (kbd "C-c H") (lambda () (interactive) (org-trello/control-and-do '(org-trello/--control-keys org-trello/--control-properties) 'orgtrello-do-create-simple)))
             (define-key map (kbd "C-c j") (lambda () (interactive) (org-trello/control-and-do '(org-trello/--control-keys org-trello/--control-properties) 'orgtrello-do-create-full-card)))
             (define-key map (kbd "C-c k") (lambda () (interactive) (org-trello/control-and-do '(org-trello/--control-keys org-trello/--control-properties) 'orgtrello-do-delete-simple)))
             (define-key map (kbd "C-c I") (lambda () (interactive) (org-trello/control-and-do nil                                                          'orgtrello-do-install-keys-and-token)))
             (define-key map (kbd "C-c J") (lambda () (interactive) (org-trello/control-and-do '(org-trello/--control-keys)                                 'orgtrello-do-install-board-and-lists)))
             ;; for debugging purposes (I do not know any better yet)
             ;; (define-key map (kbd "C-c z") (lambda () (org-trello/control-and-do nil 'orgtrello-describe-heading)))
             ;; (define-key map (kbd "C-c x") (lambda () (org-trello/control-and-do nil 'orgtrello-describe-headings)))
             ;; (define-key map (kbd "C-c F") (lambda () (org-trello/control-and-do nil 'orgtrello-find-block)))
             ;; define other bindings...
             map))
;;;###autoload

(add-hook 'org-mode-hook 'org-trello-mode)

(provide 'org-trello)

;;; org-trello.el ends here
