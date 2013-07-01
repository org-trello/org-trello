;;; orgtrello.el -- Minor mode for org-mode to sync org-mode and trello

;; Copyright (C) 2013 Antoine R. Dumont

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.1
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

;; 1) retrieve your trello api key https://trello.com/1/appKey/generate
;; Then add those entries inside the ~/.trello/config.el:
;; ;; -*- lisp -*-
;; (defvar consumer-key "consumer-key")
;; 2) then connect to this url with your browser
;; https://trello.com/1/authorize?response_type=token&name=org-trello&scope=read,write&expiration=never&key=<consumer-key>
;; Add another entry inside the `~/.trello/config.el`
;; (defvar access-token "your-access-token")

;; Add the following to your emacs init file
;; (require 'orgtrello)

;;; Code:

(require 'json)
(defvar app-name "org-trello")
(add-to-list 'load-path "./emacs-request")
(add-to-list 'load-path "./utils")

;; query
(require 'dash)
(require 'orgtrello-api)
(require 'orgtrello-query)
(require 'orgtrello-data)

(defvar *BOARD-ID*      "50bcfd2f033110476000e768")
(defvar *TODO-LIST-ID*  "51d15c319c93af375200155f")
;;(defvar *DOING-LIST-ID* "51d15c98741fd4673a0014b5")

(defun orgtrello--card (card-meta &optional parent-meta grandparent-meta)
  "Deal with create/update card"
  ;; parent and grandparent are useless here
  (let* ((orgtrello--card-id   (gethash :id    card-meta))
         (orgtrello--card-name (gethash :title card-meta))
         (orgtrello--action    (if orgtrello--card-id
                                   ;; update
                                   (orgtrello-api--move-card orgtrello--card-id *TODO-LIST-ID* orgtrello--card-name)
                                 ;; create
                                 (orgtrello-api--add-card orgtrello--card-name *TODO-LIST-ID*))))
    (orgtrello-query-http orgtrello--action)))

(defun orgtrello--checklist (checklist-meta &optional card-meta grandparent-meta)
  "Deal with create/update checklist"
  ;; grandparent is useless here
  (let* ((orgtrello--checklist-id   (gethash :id checklist-meta))
         (orgtrello--card-id        (gethash :id card-meta))
         (orgtrello--checklist-name (gethash :title checklist-meta))
         (orgtrello--action         (if orgtrello--checklist-id
                                        ;; update
                                        (orgtrello-api--update-checklist orgtrello--checklist-id orgtrello--checklist-name)
                                      ;; create
                                      (orgtrello-api--add-checklist orgtrello--card-id orgtrello--checklist-name))))
    (orgtrello-query-http orgtrello--action)))

(defun orgtrello--task (task-meta &optional checklist-meta card-meta)
  "Deal with create/update task"
  ;; card-meta is only usefull for the update part
  (let* ((orgtrello--task-id      (gethash :id task-meta))
         (orgtrello--checklist-id (gethash :id checklist-meta))
         (orgtrello--card-id      (gethash :id card-meta))
         (orgtrello--task-name    (gethash :title task-meta))
         ;; the trello api is strange
         (orgtrello--task-state   (if (string= "DONE" (gethash :keyword task-meta)) "complete" "incomplete")) ;; update api call
         (orgtrello--task-check   (if (string= "DONE" (gethash :keyword task-meta)) 't nil))                  ;; create api call
         (orgtrello--action       (if orgtrello--task-id
                                      ;; update - rename, check or uncheck the task
                                      (orgtrello-api--update-task orgtrello--card-id orgtrello--checklist-id orgtrello--task-id orgtrello--task-name orgtrello--task-state)
                                    ;; create
                                    (orgtrello-api--add-tasks orgtrello--checklist-id orgtrello--task-name orgtrello--task-check))))
    (orgtrello-query-http orgtrello--action)))

(defun orgtrello--too-deep-level (meta &optional parent-meta grandparent-meta)
  "Deal with too deep level."
  "Do not deal with level superior to 4.")

(defun orgtrello--dispatch-map-creation ()
  "Dispatch map for the creation of card/checklist/item."
  (let* ((dispatch-map (make-hash-table :test 'equal)))
    (puthash 1 'orgtrello--card      dispatch-map)
    (puthash 2 'orgtrello--checklist dispatch-map)
    (puthash 3 'orgtrello--task      dispatch-map)
    dispatch-map))

(defvar *MAP-DISPATCH-CREATE-UPDATE* (orgtrello--dispatch-map-creation))

(defun orgtrello--dispatch-create (meta &optional parent-meta grandparent-meta)
  (let* ((level       (gethash :level meta))
         (dispatch-fn (gethash level *MAP-DISPATCH-CREATE-UPDATE* 'orgtrello--too-deep-level)))
    (funcall dispatch-fn meta parent-meta grandparent-meta)))

(defun orgtrello--do-action ()
  "Compute the action needed to be done depending on the current heading's level."
  (interactive)
  (orgtrello--dispatch-create (orgtrello-data-metadata) (orgtrello-data-parent-metadata) (orgtrello-data-grandparent-metadata)))

(defun orgtrello--describe-heading ()
  "Describe the current heading's metadata"
  (interactive)
  (let* ((org-heading (org-get-heading))
         ;; retrieve data from the current heading
         (org-metadata (org-heading-components))
         (meta (orgtrello-data--get-metadata org-metadata))
         ;; retrieve data from the current heading's parent
         (org-parent-metadata (save-excursion
                                (org-up-heading-safe)
                                (org-heading-components)))
         (parent-meta (orgtrello-data--get-metadata org-parent-metadata))
         (org-grandparent-metadata (save-excursion
                                     (org-up-heading-safe)
                                     (org-up-heading-safe)
                                     (org-heading-components)))
         (grandparent-meta (orgtrello-data--get-metadata org-grandparent-metadata)))
    (message "org-heading: %s\norg-metadata: %s\norgtrello-metadata: %s\norg-parent-metadata: %s\norgtrello-parent-metadata: %s\norg-grandparent-metadata: %s\norgtrello-grandparent-metadata: %s"
             org-heading
             org-metadata
             meta
             org-parent-metadata
             parent-meta
             org-grandparent-metadata
             grandparent-meta)))

;;;###autoload
(define-minor-mode orgtrello-mode "Sync your org-mode and your trello together."
  :lighter " ot" ;; the name on the modeline
  :keymap  (let ((map (make-sparse-keymap)))
             (define-key map (kbd "C-c H") 'orgtrello--do-action)
             (define-key map (kbd "C-c z") 'orgtrello--describe-heading)
             ;; define other bindings...
             map))
;;;###autoload

(add-hook 'org-mode-hook 'orgtrello-mode)

(provide 'orgtrello)

;;; orgtrello.el ends here
