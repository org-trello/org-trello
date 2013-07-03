;;; orgtrello.el -- Minor mode for org-mode to sync org-mode and trello

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

(add-to-list 'load-path "./utils")

;; query
(require 'org)
(require 'json)
(require 'dash)
(require 'request)

(require 'orgtrello-api)
(require 'orgtrello-query)
(require 'orgtrello-data)

;; Properties key for the orgtrello headers #+PROPERTY board-id, etc...
(defvar *BOARD-ID*      "board-id"      "orgtrello property board-id entry")
(defvar *TODO-LIST-ID*  "todo-list-id"  "orgtrello property todo list id")
(defvar *DOING-LIST-ID* "doing-list-id" "orgtrello property doing list id")
(defvar *DONE-LIST-ID*  "done-list-id"  "orgtrello property done list id")

;; Specific state - FIXME check if they do not already exist on org-mode to avoid potential collisions
(defvar *TODO* "TODO" "org-mode todo state")
(defvar *DONE* "DONE" "org-mode done state")

(defun orgtrello--compute-list-key (state)
  "Given a state, compute the list id for the creation of a card"
  (cond ((string= state *TODO*) *TODO-LIST-ID*)
        ((string= state *DONE*) *DONE-LIST-ID*)
        (t                      *DOING-LIST-ID*)))

(ert-deftest testing-orgtrello--compute-list-key ()
  (should (equal (orgtrello--compute-list-key *TODO*)        *TODO-LIST-ID*))
  (should (equal (orgtrello--compute-list-key *DONE*)        *DONE-LIST-ID*))
  (should (equal (orgtrello--compute-list-key "otherwise")   *DOING-LIST-ID*))
  (should (equal (orgtrello--compute-list-key "IN PROGRESS") *DOING-LIST-ID*)))

(defun orgtrello--card (card-meta &optional parent-meta grandparent-meta)
  "Deal with create/update card query build"
  ;; parent and grandparent are useless here
  (let* ((orgtrello--card-kwd  (gethash :keyword card-meta))
         (orgtrello--list-id   (assoc-default (orgtrello--compute-list-key orgtrello--card-kwd) org-file-properties))
         (orgtrello--card-id   (gethash :id      card-meta))
         (orgtrello--card-name (gethash :title   card-meta)))
    (if orgtrello--card-id
        ;; update
        (orgtrello-api--move-card orgtrello--card-id orgtrello--list-id orgtrello--card-name)
      ;; create
      (orgtrello-api--add-card orgtrello--card-name orgtrello--list-id))))

(defun orgtrello--checklist (checklist-meta &optional card-meta grandparent-meta)
  "Deal with create/update checklist query build"
  ;; grandparent is useless here
  (let* ((orgtrello--checklist-id   (gethash :id checklist-meta))
         (orgtrello--card-id        (gethash :id card-meta))
         (orgtrello--checklist-name (gethash :title checklist-meta)))
    (if orgtrello--checklist-id
        ;; update
        (orgtrello-api--update-checklist orgtrello--checklist-id orgtrello--checklist-name)
      ;; create
      (orgtrello-api--add-checklist orgtrello--card-id orgtrello--checklist-name))))

(defun orgtrello--task (task-meta &optional checklist-meta card-meta)
  "Deal with create/update task query build"
  ;; card-meta is only usefull for the update part
  (let* ((orgtrello--task-id      (gethash :id task-meta))
         (orgtrello--checklist-id (gethash :id checklist-meta))
         (orgtrello--card-id      (gethash :id card-meta))
         (orgtrello--task-name    (gethash :title task-meta))
         ;; FIXME - the trello api is strange - extract those calls into function
         (orgtrello--task-state   (if (string= *DONE* (gethash :keyword task-meta)) "complete" "incomplete")) ;; update api call
         (orgtrello--task-check   (if (string= *DONE* (gethash :keyword task-meta)) 't nil))) ;; create api call
    (if orgtrello--task-id
        ;; update - rename, check or uncheck the task
        (orgtrello-api--update-task orgtrello--card-id orgtrello--checklist-id orgtrello--task-id orgtrello--task-name orgtrello--task-state)
      ;; create
      (orgtrello-api--add-tasks orgtrello--checklist-id orgtrello--task-name orgtrello--task-check))))

(defun orgtrello--too-deep-level (meta &optional parent-meta grandparent-meta)
  "Deal with too deep level."
  "Your arborescence depth is too deep. We only support up to depth 3.\nLevel 1 - card\nLevel 2 - checklist\nLevel 3 - items/tasks")

(defun orgtrello--dispatch-map-creation ()
  "Dispatch map for the creation of card/checklist/item."
  (let* ((dispatch-map (make-hash-table :test 'equal)))
    (puthash 1 'orgtrello--card      dispatch-map)
    (puthash 2 'orgtrello--checklist dispatch-map)
    (puthash 3 'orgtrello--task      dispatch-map)
    dispatch-map))

(defvar *MAP-DISPATCH-CREATE-UPDATE* (orgtrello--dispatch-map-creation) "Dispatch map for the creation/update of card/checklist/task")

(defun orgtrello--dispatch-create (meta &optional parent-meta grandparent-meta)
  (let* ((level       (gethash :level meta))
         (dispatch-fn (gethash level *MAP-DISPATCH-CREATE-UPDATE* 'orgtrello--too-deep-level)))
    (funcall dispatch-fn meta parent-meta grandparent-meta)))

(defun orgtrello--do-create-simple ()
  "Do the actual simple creation of a card, checklist or task."
  (interactive)
  (let* ((entry-metadata (orgtrello-data-entry-get-full-metadata))
         (query-http     (orgtrello--dispatch-create (gethash :current entry-metadata) (gethash :parent entry-metadata) (gethash :grandparent entry-metadata))))
    ;; FIXME? can't we do better that this?
    (if (hash-table-p query-http)
        (orgtrello-query-http query-http)
      (message query-http))))

(defun orgtrello--merge-map (entry map-ids-by-name)
  "Given a map of (id . name) and an entry, return the entry updated with the id if not already present."
  (let* ((orgtrello--merge-map-id   (gethash :id entry))
         (orgtrello--merge-map-name (gethash :title entry)))
    (if orgtrello--merge-map-id
        ;; already identified, return the entry without any modification
        entry
      ;; not present, we add the entry :id with its value and return such value
      (progn
        (puthash :id (gethash orgtrello--merge-map-name map-ids-by-name) entry)
        entry))))

(ert-deftest testing-orgtrello--merge-map ()
  (let* ((entry   (orgtrello-hash--make-hash-org :level :method "the name of the entry" nil))
         (map-ids (make-hash-table :test 'equal)))
    (puthash "the name of the entry" :some-id map-ids)
    (should (equal (gethash :id (orgtrello--merge-map entry map-ids)) :some-id))))

(ert-deftest testing-orgtrello--merge-map2 ()
  (let* ((entry   (orgtrello-hash--make-hash-org :level :method :title :id-already-there))
         (map-ids (make-hash-table :test 'equal)))
    (puthash :title :some-id map-ids)
    (should (equal (gethash :id (orgtrello--merge-map entry map-ids)) :id-already-there))))

(ert-deftest testing-orgtrello--merge-map3 ()
  (let* ((entry   (orgtrello-hash--make-hash-org :level :method :title :id-already-there))
         (map-ids (make-hash-table :test 'equal)))
    (should (equal (gethash :id (orgtrello--merge-map entry map-ids)) :id-already-there))))

(defun orgtrello--do-create-full-card ()
  "Do the actual full card creation - from card to task. Beware full side effects..."
  (interactive)
  ;; beware, the list-entries-metadata is stored once and not updated after each http call, thus do not possess the
  ;; newly created id
  (defvar orgtrello--do-create-full-card-response-http-data nil)
  (let* ((list-entries-metadata (orgtrello-data-compute-full-metadata))
         (map-ids               (make-hash-table :test 'equal)))
    (mapcar (lambda (mapdata)
              (let* ((current     (gethash :current     mapdata))
                     (parent      (gethash :parent      mapdata))
                     (grandparent (gethash :grandparent mapdata))
                     (query-http (orgtrello--dispatch-create
                                  (orgtrello--merge-map current map-ids)
                                  (orgtrello--merge-map parent map-ids)
                                  (orgtrello--merge-map grandparent map-ids))))
                ;; side effect, sniffffff
                ;; the query is synchronous as there is order in the current list - FIXME any better way? queues?
                (puthash :sync 't query-http)
                ;; execute and retrieve the result of the request
                (setq orgtrello--do-create-full-card-response-http-data (request-response-data (orgtrello-query-http query-http)))
                ;; keep the last id
                (puthash (assoc-default 'name orgtrello--do-create-full-card-response-http-data)
                         (assoc-default 'id orgtrello--do-create-full-card-response-http-data)
                         map-ids)))
            list-entries-metadata)))

(defun orgtrello--describe-heading ()
  "Describe the current heading's metadata"
  (interactive)
  (let* ((entry-metadata (orgtrello-data-entry-get-full-metadata)))
    (message "entry metadata: %S" entry-metadata)))

(defun orgtrello--describe-headings ()
  "Describe the heading and its sublist."
  (interactive)
  (let* ((meta       (orgtrello-data-compute-full-metadata))
         (count-meta (length meta)))
    (message "meta: %S\ncount: %s" meta count-meta)))

(defun orgtrello--find-block ()
  (interactive)
  (message "found: %s" (org-entry-get (point) "orgtrello-id")))

(defun orgtrello--card-delete (card-meta &optional parent-meta)
  "Deal with the deletion query of a card"
  ;; parent is useless here
  (orgtrello-api--delete-card (gethash :id card-meta)))

(defun orgtrello--checklist-delete (checklist-meta &optional parent-meta)
  "Deal with the deletion query of a checklist"
  ;; parent is useless here
  (orgtrello-api--delete-checklist (gethash :id checklist-meta)))

(defun orgtrello--task-delete (task-meta &optional checklist-meta)
  "Deal with create/update task query build"
  (let* ((orgtrello--task-id      (gethash :id task-meta))
         (orgtrello--checklist-id (gethash :id checklist-meta)))
    (orgtrello-api--delete-task orgtrello--checklist-id orgtrello--task-id)))

(defun orgtrello--dispatch-map-delete ()
  "Dispatch map for the deletion of card/checklist/item."
  (let* ((dispatch-map (make-hash-table :test 'equal)))
    (puthash 1 'orgtrello--card-delete      dispatch-map)
    (puthash 2 'orgtrello--checklist-delete dispatch-map)
    (puthash 3 'orgtrello--task-delete      dispatch-map)
    dispatch-map))

(defvar *MAP-DISPATCH-DELETE* (orgtrello--dispatch-map-delete) "Dispatch map for the deletion query of card/checklist/task.")

(defun orgtrello--dispatch-delete (meta &optional parent-meta)
  (let* ((level       (gethash :level meta))
         (dispatch-fn (gethash level *MAP-DISPATCH-DELETE* 'orgtrello--too-deep-level)))
    (funcall dispatch-fn meta parent-meta)))

(defun orgtrello--do-delete-simple ()
  "Do the simple deletion of a card, checklist or task."
  (interactive)
  (let* ((entry-metadata   (orgtrello-data-entry-get-full-metadata))
         (current-metadata (gethash :current entry-metadata))
         (id               (gethash :id current-metadata)))
    (if (and current-metadata id)
        (let ((query-http (orgtrello--dispatch-delete (gethash :current entry-metadata) (gethash :parent entry-metadata))))
          (if (hash-table-p query-http)
              (orgtrello-query-http query-http)
            (message query-http)))
      (message "Entity not synchronized on trello yet!"))))

;;;###autoload
(define-minor-mode orgtrello-mode "Sync your org-mode and your trello together."
  :lighter " ot" ;; the name on the modeline
  :keymap  (let ((map (make-sparse-keymap)))
             ;; binding will change
             (define-key map (kbd "C-c H") 'orgtrello--do-create-simple)
             (define-key map (kbd "C-c j") 'orgtrello--do-create-full-card)
             (define-key map (kbd "C-c k") 'orgtrello--do-delete-simple)
             ;; for debugging purposes (I do not know any better yet)
             (define-key map (kbd "C-c z") 'orgtrello--describe-heading)
             (define-key map (kbd "C-c x") 'orgtrello--describe-headings)
             (define-key map (kbd "C-c F") 'orgtrello--find-block)
             ;; define other bindings...
             map))
;;;###autoload

(add-hook 'org-mode-hook 'orgtrello-mode)

(provide 'orgtrello)

;;; orgtrello.el ends here
