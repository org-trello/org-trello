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

(defun orgtrello--card (meta)
  "Deal with create/update card"
  "create/update card"
  (let* ((orgtrello--id     (gethash :id    meta))
         (orgtrello--name   (gethash :title meta))
         (orgtrello--action (if orgtrello--id
                                ;; update
                                (orgtrello-api--move-card orgtrello--id *TODO-LIST-ID* orgtrello--name)
                              ;; create
                              (orgtrello-api--add-card orgtrello--name *TODO-LIST-ID*))))
    (orgtrello-query-http orgtrello--action)))

(defun orgtrello--checklist (meta)
  "Deal with create/update checklist"
  "create/update checklist"
  )

(defun orgtrello--task (meta)
  "Deal with create/update task"
  "create/update task"
  )

(defun orgtrello--too-deep-level (meta)
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
;; (setq *MAP-DISPATCH-CREATE-UPDATE* (orgtrello--dispatch-map-creation))

(defun orgtrello--dispatch-create (meta)
  (let* ((level       (gethash :level meta))
         (dispatch-fn (gethash level *MAP-DISPATCH-CREATE-UPDATE* 'orgtrello--too-deep-level)))
    (funcall dispatch-fn meta)))

(defun orgtrello--do-action ()
  "Compute the action needed to be done depending on the current heading's level."
  (interactive)
  (orgtrello--dispatch-create (orgtrello-data-metadata)))

(defun orgtrello--describe-heading ()
  "Describe the current heading's metadata"
  (interactive)
  (let* ((org-heading (org-get-heading))
         (org-metadata (org-heading-components))
         (meta (orgtrello-data--get-metadata org-metadata)))
    (message "org-heading: %s\norg-metadata: %s\norgtrello-metadata: %s" org-heading org-metadata meta)))

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
