;;; org-trello.el --- Minor mode to synchronize org-mode buffer and trello board

;; Copyright (C) 2013 Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.3.9
;; Package-Requires: ((dash "2.5.0") (request "0.2.0") (elnode "0.9.9.7.6") (esxml "0.3.0") (s "1.7.0") (kv "0.0.19"))
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
;; 3) Once per org-mode file/board you want to connect to (C-c o I)
;; M-x org-trello/install-board-and-lists-ids
;;
;; 4) You can also create a board directly from a org-mode buffer (C-c o b)
;; M-x org-trello/create-board
;;
;; 5) Check your setup (C-c o d)
;; M-x org-trello/check-setup
;;
;; 6) Help (C-c o h)
;; M-x org-trello/help-describing-setup
;;
;; Now you can work with trello from the comfort of emacs.
;; Enjoy!

;; More informations on https://ardumont.github.io/org-trello

;;; Code:


(defconst *ERROR-INSTALL-MSG* (format "Oops - your emacs isn't supported. org-trello only works on Emacs 24.3+ and you're running version: %s.
Please consider upgrading Emacs." emacs-version) "Error message when installing org-trello with an unsupported emacs version.")

(when (version< emacs-version "24") (error *ERROR-INSTALL-MSG*))

;; Dependency on internal Emacs libs
(require 'org)
(require 'json)
(require 'parse-time)
(require 'timer)
(require 'align)

;; Depdendency on external Emacs libs
(require 'dash)
(require 'request)
(require 'elnode)
(require 's)
(require 'kv)
(require 'esxml)

(if (version< "24.3" emacs-version)
    (require 'cl-lib)
  (progn ;; need to alias the call
    (require 'cl)
    (defalias 'cl-defun 'defun*)
    (defalias 'cl-destructuring-bind 'destructuring-bind)))

(defconst *ORGTRELLO-VERSION* "0.3.9" "current org-trello version installed.")


(defconst *OT/NOLOG* 0)
(defconst *OT/ERROR* 1)
(defconst *OT/WARN*  2)
(defconst *OT/INFO*  3)
(defconst *OT/DEBUG* 4)
(defconst *OT/TRACE* 5)

(defvar *orgtrello-log/level* *OT/INFO*
  "Set log level.
Levels:
0 - no logging   (*OT/NOLOG*)
1 - log errors   (*OT/ERROR*)
2 - log warnings (*OT/WARN*)
3 - log info     (*OT/INFO*)
4 - log debug    (*OT/DEBUG*)
5 - log trace    (*OT/TRACE*)
To change such level, add this to your init.el file: (setq *orgtrello-log/level* *OT/TRACE*)") ;;(setq *orgtrello-log/level* *OT/TRACE*) (setq *orgtrello-log/level* *OT/INFO*)

(defun orgtrello-log/msg (level &rest args) "Log message."
  (when (<= level *orgtrello-log/level*)
    (apply 'message args)))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-log loaded!")


(defconst *consumer-key*                nil                                               "Id representing the user.")
(defconst *access-token*                nil                                               "Read/write access token to use trello on behalf of the user.")
(defconst *ORGTRELLO-MARKER*            "orgtrello-marker"                                "A marker used inside the org buffer to synchronize entries.")
(defconst *CARD-LEVEL*                  1                                                 "card level")
(defconst *CHECKLIST-LEVEL*             2                                                 "checkbox level")
(defconst *ITEM-LEVEL*                  3                                                 "item level")
(defconst *OUTOFBOUNDS-LEVEL*           4                                                 "Out of bounds level")
(defconst *ORGTRELLO-LEVELS*            `(,*CARD-LEVEL* ,*CHECKLIST-LEVEL* ,*ITEM-LEVEL*) "Current levels 1 is card, 2 is checklist, 3 is item.")
(defconst *ORGTRELLO-ACTION-SYNC*       "sync-entity"                                     "Possible action regarding the entity synchronization.")
(defconst *ORGTRELLO-ACTION-DELETE*     "delete"                                          "Possible action regarding the entity deletion.")
(defconst *ORGTRELLO-USER-PREFIX*       "orgtrello-user-"                                 "orgtrello prefix to define user to a org-mode level.")
(defconst *ORGTRELLO-USERS-ENTRY*       "orgtrello-users"                                 "orgtrello property entry to store the users assigned to a card.")
(defconst *ORGTRELLO-USER-ME*           "orgtrello-user-me"                               "Current user's property id.")
(defconst *ORGTRELLO-USER-LOGGED-IN*    nil                                               "Current user logged in.")
(defconst *ORGTRELLO-CARD-COMMENTS*     "orgtrello-card-comments"                         "Current card's comments property.")
(defconst *ORGTRELLO-CARD-COMMENTS-DELIMITER* "###"                                       "Current card's comments delimiter.")
(defconst *ORGTRELLO-CARD-COMMENTS-DELIMITER-PRINT* "\n\n"                                "Current card's comments delimiter to print.")
(defconst *ORGTRELLO-DO-SHOW-CARD-COMMENTS-AFTER-ADDING* nil                              "Show the comment buffer after adding one comment")
(defconst *ORGTRELLO-TITLE-BUFFER-INFORMATION* "*org-trello-information*"                 "Title for the org-trello buffers that display information.")

(defconst *ORGTRELLO-HTTPS*               "https://trello.com"                            "URL https to help in browsing")

(defconst *ERROR-SYNC-CARD-MISSING-NAME* "Cannot synchronize the card - missing mandatory name. Skip it...")
(defconst *ERROR-SYNC-CHECKLIST-SYNC-CARD-FIRST* "Cannot synchronize the checklist - the card must be synchronized first. Skip it...")
(defconst *ERROR-SYNC-CHECKLIST-MISSING-NAME* "Cannot synchronize the checklist - missing mandatory name. Skip it...")
(defconst *ERROR-SYNC-ITEM-SYNC-CARD-FIRST* "Cannot synchronize the item - the card must be synchronized first. Skip it...")
(defconst *ERROR-SYNC-ITEM-SYNC-CHECKLIST-FIRST* "Cannot synchronize the item - the checklist must be synchronized first. Skip it...")
(defconst *ERROR-SYNC-ITEM-MISSING-NAME* "Cannot synchronize the item - missing mandatory name. Skip it...")
(defconst *ERROR-SYNC-ITEM-SYNC-UPPER-LAYER-FIRST* "The card and the checklist must be synced before syncing the item. Skip it...")

(defun org-trello/https-trello (url-without-base-uri) "An helper method to compute the uri to trello"
  (concat *ORGTRELLO-HTTPS* url-without-base-uri))

;; #################### orgtrello-version

(defun org-trello/version () (interactive) "Version of org-trello"
  (message "org-trello version: %s" *ORGTRELLO-VERSION*))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-setup loaded!")


(defun orgtrello-hash/empty-hash () "Empty hash table with test 'equal"
  (make-hash-table :test 'equal))

(defun orgtrello-hash/make-hash-org (member-ids level keyword name id due position buffer-name desc comments tags)
  "Utility function to ease the orgtrello-metadata creation"
  (let ((h (orgtrello-hash/empty-hash)))
    (puthash :buffername     buffer-name h)
    (puthash :position       position    h)
    (puthash :level          level       h)
    (puthash :keyword        keyword     h)
    (puthash :name           name        h)
    (puthash :id             id          h)
    (puthash :due            due         h)
    (puthash :member-ids     member-ids  h)
    (puthash :desc           desc        h)
    (puthash :comments       comments    h)
    (puthash :tags           tags        h)
    h))

(defun orgtrello-hash/make-properties (properties) "Given a list of key value pair, return a hash table."
  (--reduce-from (progn (puthash (car it) (cdr it) acc) acc)
                 (orgtrello-hash/empty-hash)
                 properties))

(defun orgtrello-hash/make-transpose-properties (properties) "Given a list of key value pair, return a hash table with key/value transposed."
  (--reduce-from (progn (puthash (cdr it) (car it) acc) acc)
                 (orgtrello-hash/empty-hash)
                 properties))

(defun orgtrello-hash/make-hierarchy (current &optional parent grandparent) "Helper constructor for the hashmap holding the full metadata about the current-entry."
  (orgtrello-hash/make-properties `((:current . ,current)
                                    (:parent . ,parent)
                                    (:grandparent . ,grandparent))))

(defun orgtrello-hash/key (s) "Given a string, compute its key format."
  (format ":%s:" s))

(defun orgtrello-hash/init-map-from (data)
  "Init a map from a given data. If data is nil, return an empty hash table."
  (if data data (orgtrello-hash/empty-hash)))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-hash loaded!")


(defun trace (label e) "Decorator for some inaccessible code to easily 'message'."
  (message "TRACE: %s: %S" label e)
  e)

(defun -trace (e &optional label) "Decorator for some inaccessible code to easily 'message'."
  (progn
    (if label
        (trace label e)
        (message "TRACE: %S" e))
    e))

(defun orgtrello-action/reload-setup () "Reload orgtrello setup."
  (org-set-regexps-and-options))

(defmacro orgtrello-action/safe-wrap (fn &rest clean-up) "A macro to deal with intercept uncaught error when executing the fn call and cleaning up using the clean-up body."
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "### org-trello ### Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))

(defun orgtrello-action/--execute-controls (controls-or-actions-fns &optional entity) "Given a series of controls, execute them and return the results."
  (--map (funcall it entity) controls-or-actions-fns))

(defun orgtrello-action/--filter-error-messages (control-or-actions) "Given a list of control or actions done, filter only the error message. Return nil if no error message."
  (--filter (not (equal :ok it)) control-or-actions))

(defun orgtrello-action/--compute-error-message (error-msgs) "Given a list of error messages, compute them as a string."
  (apply 'concat (--map (concat "- " it "\n") error-msgs)))

(defun orgtrello-action/controls-or-actions-then-do (control-or-action-fns fn-to-execute &optional nolog-p) "Execute the function fn-to-execute if control-or-action-fns is nil or display the error message if problems."
  (if control-or-action-fns
      (let ((org-trello/--error-messages (-> control-or-action-fns orgtrello-action/--execute-controls orgtrello-action/--filter-error-messages)))
        (if org-trello/--error-messages
            (unless nolog-p
                    ;; there are some trouble, we display all the error messages to help the user understand the problem
                    (orgtrello-log/msg *OT/ERROR* "List of errors:\n %s" (orgtrello-action/--compute-error-message org-trello/--error-messages)))
            ;; ok execute the function as the controls are ok
            (funcall fn-to-execute)))
      ;; no control, we simply execute the function
      (funcall fn-to-execute)))

(defun orgtrello-action/functional-controls-then-do (control-fns entity fn-to-execute args) "Execute the function fn if control-fns is nil or if the result of apply every function to fn-to-execute is ok."
  (if control-fns
      (let ((org-trello/--error-messages (-> control-fns (orgtrello-action/--execute-controls entity) orgtrello-action/--filter-error-messages)))
        (if org-trello/--error-messages
            ;; there are some trouble, we display all the error messages to help the user understand the problem
            (orgtrello-log/msg *OT/ERROR* "List of errors:\n %s" (orgtrello-action/--compute-error-message org-trello/--error-messages))
            ;; ok execute the function as the controls are ok
            (funcall fn-to-execute entity args)))
      ;; no control, we simply execute the function
      (funcall fn-to-execute entity args)))

(defun orgtrello-action/msg-controls-or-actions-then-do (msg control-or-action-fns fn-to-execute &optional save-buffer-p reload-setup-p nolog-p) "A decorator fn to execute some action before/after the controls."
  (unless nolog-p (orgtrello-log/msg *OT/INFO* (concat msg "...")))
  ;; now execute the controls and the main action
  (orgtrello-action/safe-wrap
   (orgtrello-action/controls-or-actions-then-do control-or-action-fns fn-to-execute nolog-p)
   (progn
     (when save-buffer-p  (save-buffer))
     (when reload-setup-p (orgtrello-action/reload-setup))
     (unless nolog-p (orgtrello-log/msg *OT/INFO* (concat msg " - done!"))))))

(defun orgtrello-action/delete-file! (file-to-remove) "Remove metadata file."
  (when (file-exists-p file-to-remove) (delete-file file-to-remove)))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-action loaded!")


(defvar *ORGTRELLO-ID* "orgtrello-id" "Key entry used for the trello identifier and the trello marker (the first sync).")

(defun orgtrello-data/merge-2-lists-without-duplicates (a-list b-list)
  "Merge 2 lists together (no duplicates)."
  (-> a-list
      (append b-list)
      (delete-dups)))

(defun orgtrello-data/--compute-fn (entity list-dispatch-fn)
  "Given an entity, compute the result"
  (funcall (if (hash-table-p entity) (first list-dispatch-fn) (second list-dispatch-fn)) entity))

(defun orgtrello-data/--entity-with-level-p (entity level) "Is the entity with level level?" (-> entity orgtrello-data/entity-level (eq level)))
(defun orgtrello-data/entity-card-p      (entity) "Is this a card?"      (orgtrello-data/--entity-with-level-p entity *CARD-LEVEL*))
(defun orgtrello-data/entity-checklist-p (entity) "Is this a checklist?" (orgtrello-data/--entity-with-level-p entity *CHECKLIST-LEVEL*))
(defun orgtrello-data/entity-item-p      (entity) "Is this an item?"     (orgtrello-data/--entity-with-level-p entity *ITEM-LEVEL*))

(defun orgtrello-data/gethash-data (key map &optional default-value) "Retrieve the map from some query-map" (when map (gethash key map default-value)))
(defun orgtrello-data/puthash-data (key value map)                   "Update the map at key with value"     (when map (puthash key value map)))

(defun orgtrello-data/entity-id (entity)
  "Dispatch to the rightful function to get the id"
  (let ((id (orgtrello-data/entity-id-or-marker entity))) (when (orgtrello-data/id-p id) id)))

(defun orgtrello-data/entity-keyword (entity &optional default-value)
  "Retrieve the keyword from the entity."
  (orgtrello-data/gethash-data :keyword entity default-value))

(defun orgtrello-data/entity-name         (entity) (orgtrello-data/gethash-data :name           entity))
(defun orgtrello-data/entity-memberships  (entity) (orgtrello-data/gethash-data :memberships    entity))
(defun orgtrello-data/entity-member       (entity) (orgtrello-data/gethash-data :member         entity))
(defun orgtrello-data/entity-username     (entity) (orgtrello-data/gethash-data :username       entity))
(defun orgtrello-data/entity-action       (entity) (orgtrello-data/gethash-data :action         entity))
(defun orgtrello-data/entity-board-id     (entity) (orgtrello-data/gethash-data :board-id       entity))
(defun orgtrello-data/entity-card-id      (entity) (orgtrello-data/gethash-data :card-id        entity))
(defun orgtrello-data/entity-list-id      (entity) (orgtrello-data/gethash-data :list-id        entity))
(defun orgtrello-data/entity-member-ids   (entity) (orgtrello-data/gethash-data :member-ids     entity))
(defun orgtrello-data/entity-description  (entity) (orgtrello-data/gethash-data :desc           entity))
(defun orgtrello-data/entity-checklists   (entity) (orgtrello-data/gethash-data :checklists     entity))
(defun orgtrello-data/entity-items        (entity) (orgtrello-data/gethash-data :items          entity))
(defun orgtrello-data/entity-position     (entity) (orgtrello-data/gethash-data :position       entity))
(defun orgtrello-data/entity-buffername   (entity) (orgtrello-data/gethash-data :buffername     entity))
(defun orgtrello-data/entity-checked      (entity) (orgtrello-data/gethash-data :checked        entity))
(defun orgtrello-data/entity-due          (entity) (orgtrello-data/gethash-data :due            entity))
(defun orgtrello-data/entity-id-or-marker (entity) (orgtrello-data/gethash-data :id             entity))
(defun orgtrello-data/entity-level        (entity) (orgtrello-data/gethash-data :level          entity))
(defun orgtrello-data/entity-closed       (entity) (orgtrello-data/gethash-data :closed         entity))
(defun orgtrello-data/entity-callback     (entity) (orgtrello-data/gethash-data :callback       entity))
(defun orgtrello-data/entity-start        (entity) (orgtrello-data/gethash-data :start          entity))
(defun orgtrello-data/entity-comments     (entity) (orgtrello-data/gethash-data :comments       entity))
(defun orgtrello-data/entity-labels       (entity) (orgtrello-data/gethash-data :labels         entity))
(defun orgtrello-data/entity-tags         (entity) (orgtrello-data/gethash-data :tags           entity))

(defun orgtrello-data/entity-method (query-map) (orgtrello-data/gethash-data :method query-map))
(defun orgtrello-data/entity-uri    (query-map) (orgtrello-data/gethash-data :uri    query-map))
(defun orgtrello-data/entity-sync   (query-map) (orgtrello-data/gethash-data :sync   query-map))
(defun orgtrello-data/entity-params (query-map) (orgtrello-data/gethash-data :params query-map))

(defun orgtrello-data/current     (entry-meta) (orgtrello-data/gethash-data :current     entry-meta))
(defun orgtrello-data/parent      (entry-meta) (orgtrello-data/gethash-data :parent      entry-meta))
(defun orgtrello-data/grandparent (entry-meta) (orgtrello-data/gethash-data :grandparent entry-meta))

(defun orgtrello-data/--compute-level (entity-map) "Given a map, compute the entity level"
  (cond ((gethash :list-id entity-map) *CARD-LEVEL*)
        ((gethash :card-id entity-map) *CHECKLIST-LEVEL*)
        ((gethash :checked entity-map) *ITEM-LEVEL*)
        (t nil)))

(defvar *ORGTRELLO-DATA-MAP-KEYWORDS* (orgtrello-hash/make-properties `((url            . :url)
                                                                        (id             . :id)
                                                                        (name           . :name)
                                                                        (idMembers      . :member-ids)
                                                                        (idList         . :list-id)
                                                                        (idChecklists   . :checklists)
                                                                        (idBoard        . :board-id)
                                                                        (due            . :due)
                                                                        (desc           . :desc)
                                                                        (closed         . :closed)
                                                                        (idCard         . :card-id)
                                                                        (checkItems     . :items)
                                                                        (state          . :checked)
                                                                        (status         . :status)
                                                                        (pos            . :position)
                                                                        (keyword        . :keyword)
                                                                        (member-ids     . :member-ids)
                                                                        (member         . :member)
                                                                        (memberships    . :memberships)
                                                                        (username       . :username)
                                                                        (fullName       . :full-name)
                                                                        (actions        . :comments)
                                                                        (labelNames     . :labels)
                                                                        (red            . :red)
                                                                        (yellow         . :yellow)
                                                                        (blue           . :blue)
                                                                        (green          . :green)
                                                                        (orange         . :orange)
                                                                        (purple         . :purple)
                                                                        (labels         . :labels)
                                                                        (color          . :color))))

(defun orgtrello-data/--deal-with-key (key)
  "Given a key, return it as is if it's a keyword or return its mapped version from *ORGTRELLO-DATA-MAP-KEYWORDS*"
  (cond ((keywordp key) key)
        (t             (gethash key *ORGTRELLO-DATA-MAP-KEYWORDS*))))

(defun orgtrello-data/--dispatch-parse-data-fn (key)
  "Given a key, return the function to call to execute the parsing (parse-actions or parse-data)"
  (cond ((eq :comments key) 'orgtrello-data/--parse-actions)
        (t                  'orgtrello-data/parse-data)))

(defun orgtrello-data/--parse-actions (data &optional size)
  "Given a 'comment' (trello action limited to comment in org-trello) structure, return a list of map representing a comment."
  (->> data
    (mapcar (lambda (it)
              (progn
                (let ((amap (orgtrello-hash/empty-hash)))
                  (puthash :comment-id (assoc-default 'id it)                                 amap)
                  (puthash :comment-text (->> it (assoc-default 'data) (assoc-default 'text)) amap)
                  (puthash :comment-user (->> it car (assoc-default 'username))               amap)
                  amap))))))

(defun orgtrello-data/parse-data (entities)
  "Given a trello entity, convert into org-trello entity"
  (cond ((eq :json-false entities)                                           nil)
        ((--any? (funcall it entities) '(stringp symbolp numberp functionp)) entities)
        ((arrayp entities)                                                   (mapcar 'orgtrello-data/parse-data entities))
        (t
         (let ((hmap (--reduce-from (let ((key (car it))
                                          (val (cdr it)))
                                      (-when-let (new-key (orgtrello-data/--deal-with-key key))
                                        (puthash new-key
                                                 (funcall (orgtrello-data/--dispatch-parse-data-fn new-key) val)
                                                 acc))
                                      acc)
                                    (orgtrello-hash/empty-hash)
                                    entities)))
           (-when-let (level (orgtrello-data/--compute-level hmap)) (puthash :level level hmap))
           hmap))))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-data loaded!")

(defun orgtrello-data/comments-to-list (comments-hash)
  "Given a list of comments hashmap, return the serialized string comment."
  (->> comments-hash
    (--map (s-join ": " (list (gethash :comment-user it) (gethash :comment-text it))))
    (s-join *ORGTRELLO-CARD-COMMENTS-DELIMITER*)))

(defun orgtrello-data/format-labels (labels)
  "Given an assoc list of labels, serialize it."
  (->> labels
    (--map (s-join ": " (list (car it) (cdr it))))
    (s-join "\n\n")))

(defun orgtrello-data/unformat-comments (comments)
  "Given a string of comments human readable, transform it into a property format."
  (->> comments
    (s-split *ORGTRELLO-CARD-COMMENTS-DELIMITER-PRINT*)
    (s-join *ORGTRELLO-CARD-COMMENTS-DELIMITER*)))

(defun orgtrello-data/format-comments (comments)
  "Given a property string of comments, work it to permit a human readable display."
  (if comments
    (->> comments
      (s-split *ORGTRELLO-CARD-COMMENTS-DELIMITER*)
      (s-join *ORGTRELLO-CARD-COMMENTS-DELIMITER-PRINT*))
    "No comments to display!"))

(defun orgtrello-data/id-p (id)
  "Is the string a trello identifier?"
  (and id (not (string-match-p (format "^%s-" *ORGTRELLO-MARKER*) id))))

(defun orgtrello-data/merge-item (trello-item org-item)
  "Merge trello and org item together."
  (if (null trello-item)
      org-item
    (let ((org-item-to-merge (orgtrello-hash/init-map-from org-item)))
      (puthash :level *ITEM-LEVEL*                             org-item-to-merge)
      (puthash :id    (orgtrello-data/entity-id trello-item)   org-item-to-merge)
      (puthash :name  (orgtrello-data/entity-name trello-item) org-item-to-merge)
      ;; FIXME find how to populate keyword
      (--> trello-item
        (orgtrello-data/entity-checked it)
        (orgtrello-data/--compute-state-item it)
        (puthash :keyword it org-item-to-merge))
      org-item-to-merge)))

(defun orgtrello-data/--compute-state-item-checkbox (state)
  "Compute the status of the item checkbox"
  (orgtrello-data/--compute-state-generic state '("[X]" "[ ]")))

(defun orgtrello-data/--compute-state-item (state)
  "Compute the status of the checkbox"
  (orgtrello-data/--compute-state-generic state `(,*DONE* ,*TODO*)))

(defun orgtrello-data/--merge-checklist (trello-checklist org-checklist)
  "Merge trello and org checklist together."
  (if (null trello-checklist)
      org-checklist
      (let ((org-checklist-to-merge (orgtrello-hash/init-map-from org-checklist)))
        (puthash :level *CHECKLIST-LEVEL*                            org-checklist-to-merge)
        (puthash :name (orgtrello-data/entity-name trello-checklist) org-checklist-to-merge)
        (puthash :id   (orgtrello-data/entity-id trello-checklist)   org-checklist-to-merge)
        org-checklist-to-merge)))

(defun orgtrello-data/entity-member-ids-as-list (entity)
  "Retrieve the users assigned to the entity."
  (-> entity
    orgtrello-data/entity-member-ids
    orgtrello-data/--users-from))

(defun orgtrello-data/--merge-member-ids (trello-card org-card)
  "Merge users assigned from trello and org."
  (--> trello-card
       (orgtrello-data/entity-member-ids it)
       (orgtrello-data/merge-2-lists-without-duplicates it (orgtrello-data/entity-member-ids-as-list org-card))
       (orgtrello-data/--users-to it)))

(defun orgtrello-data/--labels-to-tags (labels)
  (when labels
    (-when-let (tags (s-join ":" (--map (gethash :color it) labels)))
      (concat ":" tags ":"))))

(defun orgtrello-data/--merge-card (trello-card org-card)
  "Merge trello and org card together."
  (if (null trello-card)
      org-card
    (let ((org-card-to-merge (orgtrello-hash/init-map-from org-card)))
      (puthash :tags     (orgtrello-data/--labels-to-tags (orgtrello-data/entity-labels trello-card))   org-card-to-merge)
      (puthash :comments (orgtrello-data/entity-comments trello-card)                              org-card-to-merge)
      (puthash :level   *CARD-LEVEL*                                                               org-card-to-merge)
      (puthash :id      (orgtrello-data/entity-id trello-card)                                     org-card-to-merge)
      (puthash :name    (orgtrello-data/entity-name trello-card)                                   org-card-to-merge)
      (puthash :keyword (-> trello-card
                          orgtrello-data/entity-list-id
                          orgtrello-data/--compute-card-status)                            org-card-to-merge)
      (puthash :member-ids (orgtrello-data/--merge-member-ids trello-card org-card-to-merge) org-card-to-merge)
      (puthash :desc    (orgtrello-data/entity-description trello-card)                            org-card-to-merge)
      org-card-to-merge)))

(defun orgtrello-data/--dispatch-merge-fn (entity)
  "Dispatch the function fn to merge the entity."
  (cond ((orgtrello-data/entity-card-p entity)      'orgtrello-data/--merge-card)
        ((orgtrello-data/entity-checklist-p entity) 'orgtrello-data/--merge-checklist)
        ((orgtrello-data/entity-item-p entity)      'orgtrello-data/merge-item)))

(defun orgtrello-data/merge-entities-trello-and-org (trello-data org-data)
  "Merge the org-entity entities inside the trello-entities."
  (let ((trello-entities  (first trello-data))
        (trello-adjacency (second trello-data))
        (org-entities     (first org-data))
        (org-adjacency    (second org-data)))

    (maphash (lambda (id trello-entity)
               (puthash id (funcall (orgtrello-data/--dispatch-merge-fn trello-entity) trello-entity (orgtrello-data/--get-entity id org-entities)) trello-entities) ;; updating entity to trello
               (puthash id (orgtrello-data/merge-2-lists-without-duplicates (gethash id trello-adjacency) (gethash id org-adjacency))     trello-adjacency)) ;; update entity adjacency to trello
             trello-entities)

    ;; copy the entities only present on org files to the trello entities.
    (maphash (lambda (id org-entity)
               (unless (gethash id trello-entities)
                       (puthash id org-entity trello-entities)
                       (puthash id (gethash id org-adjacency) trello-adjacency)))
             org-entities)

    (list trello-entities trello-adjacency)))

(defun orgtrello-data/--compute-card-status (card-id-list)
  "Given a card's id, compute its status."
  (gethash card-id-list *HMAP-ID-NAME*))

(defun orgtrello-data/--get-entity (id entities-hash)
  "Update the card entry inside the hash."
  (gethash id entities-hash))

(defun orgtrello-data/--compute-state-generic (state list-state)
  "Computing generic."
  (if (or (string= "complete" state)
          (string= *DONE* state)) (first list-state) (second list-state)))

(defun orgtrello-data/--users-from (string-users)
  "Compute the users name from the comma separated value in string."
  (when string-users (split-string string-users "," t)))

(defun orgtrello-data/--users-to (users) "Given a list of users, compute the comma separated users."
  (if users (mapconcat 'identity users ",") ""))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-data loaded!")


(defun orgtrello-cbx/checkbox-p () "Is there a checkbox at point?" (org-at-item-checkbox-p))

(defun orgtrello-cbx/--to-properties (alist) "Serialize an association list to json."
  (json-encode-hash-table (orgtrello-hash/make-properties alist)))

(defun orgtrello-cbx/--from-properties (string) "Deserialize from json to list."
  (when string (json-read-from-string string)))

(defun orgtrello-cbx/--checkbox-split (s) "Split the checkbox into the checkbox data and the checkbox metadata."
  (s-split ":PROPERTIES:" s))

(defun orgtrello-cbx/--checkbox-metadata (s) "Retrieve the checkbox's metadata."
  (-when-let (res (-> s
                      orgtrello-cbx/--checkbox-split
                      second))
             (s-trim-left res)))

(defun orgtrello-cbx/--checkbox-data (s) "Retrieve the checkbox's data."
    (-> s
      orgtrello-cbx/--checkbox-split
      first
      s-trim-right))

(defun orgtrello-cbx/--read-properties (s) "Read the properties from the current string."
  (->> s
       orgtrello-cbx/--checkbox-metadata
       orgtrello-cbx/--from-properties))

(defun orgtrello-cbx/--read-checkbox! () "Read the full checkbox's content"
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun orgtrello-cbx/--read-properties-from-point (pt) "Read the properties from the current point."
  (save-excursion
    (goto-char pt)
    (orgtrello-cbx/--read-properties (orgtrello-cbx/--read-checkbox!))))

(defun orgtrello-cbx/--make-properties-as-string (properties)
  (format ":PROPERTIES: %s" (orgtrello-cbx/--to-properties properties)))

(defun orgtrello-cbx/remove-overlays! (start end) "Remove the overlays presents between start and end in the current buffer"
  (remove-overlays start end 'invisible 'org-trello-cbx-property))

(defun orgtrello-cbx/install-overlays! (start-position) "Install org-trello overlays (first remove the current overlay on line)."
  ;; remove overlay present on current position
  (orgtrello-cbx/remove-overlays! (point-at-bol) (point-at-eol))
  ;; build an overlay to hide the cbx properties
  (overlay-put (make-overlay start-position (point-at-eol) (current-buffer) t nil)
               'invisible 'org-trello-cbx-property))

(defun orgtrello-cbx/--write-properties-at-point (pt properties) "Given the new properties, update the current entry."
  (save-excursion
    (goto-char pt)
    (let* ((checkbox-title   (-> (orgtrello-cbx/--read-checkbox!) orgtrello-cbx/--checkbox-data))
           (updated-property (orgtrello-cbx/--make-properties-as-string properties))
           (text-to-insert   (format "%s %s" checkbox-title updated-property)))
      (beginning-of-line)
      (kill-line)
      (insert text-to-insert)
      text-to-insert)))

(defun orgtrello-cbx/--key-to-search (key) "Search the key key as a symbol"
  (if (stringp key) (intern key) key))

(defun orgtrello-cbx/--org-get-property (key properties) "Internal accessor to the key property."
  (-> key
      orgtrello-cbx/--key-to-search
      (assoc-default properties)))

(defun orgtrello-cbx/--org-update-property (key value properties) "Internal accessor to the key property."
  (->> properties
       (orgtrello-cbx/--org-delete-property key)
       (cons `(,(orgtrello-cbx/--key-to-search key) . ,value))))

(defun orgtrello-cbx/--org-delete-property (key properties) "Delete the key from the properties."
  (-> key
      orgtrello-cbx/--key-to-search
      (assq-delete-all properties)))

(defun orgtrello-cbx/org-set-property (key value) "Read the properties. Add the new property key with the value value. Write the new properties."
  (let ((current-point (point)))
    (->> current-point
         orgtrello-cbx/--read-properties-from-point
         (orgtrello-cbx/--org-update-property key value)
         (orgtrello-cbx/--write-properties-at-point current-point))))

(defun orgtrello-cbx/org-get-property (point key) "Retrieve the value for the key key."
  (->> point
       orgtrello-cbx/--read-properties-from-point
       (orgtrello-cbx/--org-get-property key)))

(defun orgtrello-cbx/org-delete-property (key) "Delete the property key from the properties."
  (let ((current-point (point)))
    (->> current-point
         orgtrello-cbx/--read-properties-from-point
         (orgtrello-cbx/--org-delete-property key)
         (orgtrello-cbx/--write-properties-at-point current-point))))

(defun orgtrello-cbx/--org-split-data (s) "Split the string into meta data with -."
  (->> s
       (s-replace "[ ]" "[]")
       (s-split " ")))

(defun orgtrello-cbx/--list-is-checkbox-p (l) "Is this a checkbox?"
  (string= "-" (first (--drop-while (string= "" it) l))))

(defun orgtrello-cbx/--level (l)
  "Given a list of strings, compute the level (starts at 2).
String look like:
- ('- '[X] 'call 'people '[4/4])
- (' '  '- '[X] 'call 'people '[4/4]).
To ease the computation, we consider level 4 if no - to start with, and to avoid missed typing, we consider level 2 if there is no space before the - and level 3 otherwise."
  (if (orgtrello-cbx/--list-is-checkbox-p l)
      (if (string= "-" (car l)) *CHECKLIST-LEVEL* *ITEM-LEVEL*)
      *OUTOFBOUNDS-LEVEL*))

(defun orgtrello-cbx/--retrieve-status (l) "Given a list of metadata, return the status"
  (car (--drop-while (not (or (string= "[]" it)
                              (string= "[X]" it)
                              (string= "[-]" it)
                              (string= "[ ]" it))) l)))

(defun orgtrello-cbx/--status (s) "Given a checklist status, return the TODO/DONE for org-trello to work."
  (if (string= "[X]" s) "DONE" "TODO"))

(defun orgtrello-cbx/--name (s status) "Retrieve the name of the checklist"
  (->> s
       (s-replace "[ ]" "[]")
       s-trim-left
       (s-chop-prefix "-")
       s-trim-left
       (s-chop-prefix status)
       s-trim))

(defun orgtrello-cbx/--metadata-from-checklist (full-checklist) "Given a checklist string, extract the list of metadata"
  (let* ((oc/--checklist-data   (orgtrello-cbx/--checkbox-data full-checklist))
         (oc/--meta             (orgtrello-cbx/--org-split-data oc/--checklist-data))
         (oc/--status-retrieved (orgtrello-cbx/--retrieve-status oc/--meta)))
    (list nil (orgtrello-cbx/--status oc/--status-retrieved) nil (orgtrello-cbx/--name oc/--checklist-data oc/--status-retrieved) nil)))

(defun orgtrello-cbx/--level! () "Compute the levels from the current position (which is `bol`)"
  (if (org-at-item-bullet-p) *CHECKLIST-LEVEL* *ITEM-LEVEL*))

(defun orgtrello-cbx/org-checkbox-metadata! ()
  "Extract the metadata about the checklist - this is the symmetrical with `org-heading-components` but for the checklist.
Return the components of the current heading.
This is a list with the following elements:
- the level as an integer                                          - (begins at 2)
- the reduced level                                                - always nil
- the TODO keyword, or nil                                         - [], [-] map to TODO, [X] map to DONE
- the priority character, like ?A, or nil if no priority is given  - nil
- the headline text itself, or the tags string if no headline text - the name of the checkbox
- the tags string, or nil.                                         - nil"
  (save-excursion
    (beginning-of-line)
    (cons (orgtrello-cbx/--level!)
          (orgtrello-cbx/--metadata-from-checklist (orgtrello-cbx/--read-checkbox!)))))

(defun orgtrello-cbx/--get-level (meta) "Retrieve the level from the meta describing the checklist"
  (car meta))

(defun orgtrello-cbx/--org-up! (destination-level) "An internal function to get back to the current entry's parent - return the level found or nil if the level found is a card."
  (let ((current-level (orgtrello-cbx/--get-level (orgtrello-cbx/org-checkbox-metadata!))))
    (cond ((= *CARD-LEVEL*      current-level) nil)
          ((= destination-level current-level) destination-level)
          ((= *CHECKLIST-LEVEL* current-level) (org-up-heading-safe))
          (t                                   (progn
                                                 (forward-line -1)
                                                 (orgtrello-cbx/--org-up! destination-level))))))

(defun orgtrello-cbx/org-up! () "A function to get back to the current entry's parent."
  (-> (orgtrello-cbx/org-checkbox-metadata!)
      orgtrello-cbx/--get-level
      1-
      orgtrello-cbx/--org-up!))

(defun orgtrello-cbx/compute-next-card-point! () "Compute the next card's position. Does preserve position. If a sibling is found, return the point-at-bol, otherwise return the max point in buffer."
  (save-excursion
    (org-back-to-heading)
    (if (org-goto-sibling) (point-at-bol) (point-max))))

(defun orgtrello-cbx/--goto-next-checkbox () "Compute the next checkbox's beginning of line. Does not preserve the current position. If hitting a heading or the end of the file, return nil."
  (forward-line)
  (when (and (not (org-at-heading-p)) (< (point) (point-max)) (not (orgtrello-cbx/checkbox-p)))
        (orgtrello-cbx/--goto-next-checkbox)))

(defun orgtrello-cbx/--goto-next-checkbox-with-same-level! (level) "Compute the next checkbox's beginning of line (with the same level). Does not preserve the current position. If hitting a heading or the end of the file, return nil. Otherwise, return the current position."
  (forward-line)
  (if (= level (orgtrello-buffer/current-level!))
      (point)
      (if (or (org-at-heading-p) (<= (point-max) (point)))
          nil
          (orgtrello-cbx/--goto-next-checkbox-with-same-level! level))))

(defun orgtrello-cbx/--map-checkboxes (level fn-to-execute) "Map over the checkboxes and execute fn when in checkbox. Does not preserve the cursor position. Do not exceed the point-max."
  (orgtrello-cbx/--goto-next-checkbox)
  (when (< level (orgtrello-buffer/current-level!))
        (funcall fn-to-execute)
        (orgtrello-cbx/--map-checkboxes level fn-to-execute)))

(defun orgtrello-cbx/map-checkboxes (fn-to-execute) "Map over the current checkbox and sync them."
  (let ((level (orgtrello-buffer/current-level!)))
    (when (= level *CHECKLIST-LEVEL*) (funcall fn-to-execute))
    (save-excursion (orgtrello-cbx/--map-checkboxes level fn-to-execute)))) ;; then map over the next checkboxes and sync them

(defun orgtrello-cbx/next-checklist-point! ()
  "Compute the next checklist position"
  (let ((next-checklist-point (save-excursion (orgtrello-cbx/--goto-next-checkbox-with-same-level! *CHECKLIST-LEVEL*) (point))))
    (if next-checklist-point
        next-checklist-point
      (orgtrello-cbx/compute-next-card-point!))))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-cbx loaded!")


(defun orgtrello-api/make-query (method uri &optional params) "Utility function to ease the creation of the map - wait, where are my clojure data again!?"
  (let ((h (orgtrello-hash/empty-hash)))
    (puthash :method method h)
    (puthash :uri    uri    h)
    (if params (puthash :params params h))
    h))

(defun orgtrello-api/--deal-with-optional-value (optional-entry value entries)
  "Add the optional value depending on the entry. Return entries updated with value if entry, entries untouched otherwise."
  (if optional-entry (cons value entries) entries))

(defun orgtrello-api/--deal-with-optional-values (optional-entries-values entries)
  "Add the optional entry/value depending on their entry. Return entries updated with value if entry, entries untouched otherwise."
  (--reduce-from (orgtrello-api/--deal-with-optional-value (car it) (cdr it) acc)
                  entries
                  optional-entries-values))

(defun orgtrello-api/add-board (name &optional description)
  "Create a board."
  (orgtrello-api/make-query "POST" "/boards" (orgtrello-api/--deal-with-optional-value description `("desc" . ,description) `(("name" . ,name)))))

(defun orgtrello-api/get-boards ()
  "Retrieve the boards of the current user."
  (orgtrello-api/make-query "GET" "/members/me/boards"))

(defun orgtrello-api/get-board (id)
  "Retrieve the boards of the current user."
  (orgtrello-api/make-query "GET" (format "/boards/%s" id) '(("memberships" . "active")
                                                             ("memberships_member" . "true")
                                                             ("fields" . "name,memberships,closed,labelNames"))))

(defun orgtrello-api/get-cards (board-id)
  "cards of a board"
  (orgtrello-api/make-query "GET" (format "/boards/%s/cards" board-id)
                            '(("actions" .  "commentCard")
                              ("fields" . "closed,desc,due,idBoard,idChecklists,idList,idMembers,name,pos"))))

(defun orgtrello-api/get-card (card-id)
  "Detail of a card with id card-id."
  (orgtrello-api/make-query "GET" (format "/cards/%s" card-id)
                            '(("actions" . "commentCard")
                              ("action_fields" . "data")
                              ("action_memberCreator_fields" . "username")
                              ("fields" .  "closed,dateLastActivity,desc,due,idChecklists,idList,idMembers,labels,name,pos"))))

(defun orgtrello-api/delete-card (card-id)
  "Delete a card with id card-id."
  (orgtrello-api/make-query "DELETE" (format "/cards/%s" card-id)))

(defun orgtrello-api/get-lists (board-id)
  "Display the lists of the board"
  (orgtrello-api/make-query "GET" (format "/boards/%s/lists" board-id)))

(defun orgtrello-api/close-list (list-id)
  "'Close' the list with id list-id."
  (orgtrello-api/make-query "PUT" (format "/lists/%s/closed" list-id) '((value . t))))

(defun orgtrello-api/get-list (list-id)
  "Get a list by id"
  (orgtrello-api/make-query "GET" (format "/lists/%s" list-id)))

(defun orgtrello-api/add-list (name idBoard)
  "Add a list - the name and the board id are mandatory (so i say!)."
  (orgtrello-api/make-query "POST" "/lists/" `(("name" . ,name) ("idBoard" . ,idBoard))))

(defun orgtrello-api/add-card (name idList &optional due id-members desc labels)
  "Add a card to a board, optional due date (formatted string date), id-members (csv id members) and description desc."
  (orgtrello-api/make-query "POST" "/cards/"
                            (orgtrello-api/--deal-with-optional-values `((,id-members . ("idMembers" . ,id-members))
                                                                         (,due . ("due" . ,due))
                                                                         (,desc . ("desc" . ,desc))
                                                                         (,labels . ("labels" . ,labels)))
                                                                       `(("name" . ,name)
                                                                         ("idList" . ,idList)))))

(defun orgtrello-api/get-cards-from-list (list-id)
  "List all the cards"
  (orgtrello-api/make-query "GET" (format "/lists/%s/cards" list-id)))

(defun orgtrello-api/move-card (card-id idList &optional name due id-members desc labels)
  "Move a card to another list - optional entries (name, due date, id-members, desc)"
  (->> (orgtrello-api/--deal-with-optional-values `((,name . ("name" . ,name))
                                                    (,id-members . ("idMembers" . ,id-members))
                                                    (,due . ("due" . ,due))
                                                    (,desc . ("desc" . ,desc))
                                                    (,labels . ("labels" . ,labels)))
                                                  `(("idList" . ,idList)))
       (orgtrello-api/make-query "PUT" (format "/cards/%s" card-id))))

(defun orgtrello-api/add-checklist (card-id name)
  "Add a checklist to a card"
  (orgtrello-api/make-query "POST" (format "/cards/%s/checklists" card-id) `(("name" . ,name))))

(defun orgtrello-api/update-checklist (checklist-id name)
  "Update the checklist's name"
  (orgtrello-api/make-query "PUT" (format "/checklists/%s" checklist-id) `(("name" . ,name))))

(defun orgtrello-api/get-checklists (card-id)
  "List the checklists of a card"
  (orgtrello-api/make-query "GET" (format "/cards/%s/checklists" card-id)))

(defun orgtrello-api/get-checklist (checklist-id &optional without-items)
  "Retrieve all the information from a checklist"
  (let ((default-params '(("fields" . "name,pos,idCard") ;; the id card is useful for us to determine if we deal with a checklist
                          ("checkItem_fields" . "name,pos,state"))))
    (orgtrello-api/make-query "GET"
                              (format "/checklists/%s" checklist-id)
                              (if without-items (cons '("checkItems" . "none") default-params) default-params))))

(defun orgtrello-api/delete-checklist (checklist-id)
  "Delete a checklist with checklist-id"
  (orgtrello-api/make-query "DELETE" (format "/checklists/%s" checklist-id)))

(defun orgtrello-api/add-items (checklist-id name &optional checked)
  "Add todo items (trello items) to a checklist with id 'id'"
  (->> (orgtrello-api/--deal-with-optional-value checked `("checked" . ,checked) `(("name" . ,name)))
       (orgtrello-api/make-query "POST" (format "/checklists/%s/checkItems" checklist-id) )))

(defun orgtrello-api/update-item (card-id checklist-id item-id name &optional state)
  "Update an item"
  (->> (orgtrello-api/--deal-with-optional-value state `("state" . ,state) `(("name" . ,name)))
       (orgtrello-api/make-query "PUT" (format "/cards/%s/checklist/%s/checkItem/%s" card-id checklist-id item-id))))

(defun orgtrello-api/get-items (checklist-id)
  "List the checklist items."
  (orgtrello-api/make-query "GET" (format "/checklists/%s/checkItems/" checklist-id)))

(defun orgtrello-api/get-item (checklist-id item-id)
  "List the checklist items."
  (orgtrello-api/make-query "GET" (format "/checklists/%s/checkItems/%s" checklist-id item-id) '(("fields" . "name,pos,state"))))

(defun orgtrello-api/delete-item (checklist-id item-id)
  "Delete a item with id item-id"
  (orgtrello-api/make-query "DELETE" (format "/checklists/%s/checkItems/%s" checklist-id item-id)))

(defun orgtrello-api/get-member (member-id)
  "Retrieve the member by its identifier."
  (orgtrello-api/make-query "GET" (format "/members/%s" member-id)))

(defun orgtrello-api/get-me ()
  "Retrieve the current user's member informations."
  (orgtrello-api/make-query "GET" "/members/me"))

(defun orgtrello-api/add-card-comment (card-id comment-text)
  "Add a comment to a card"
  (orgtrello-api/make-query "POST" (format "/cards/%s/actions/comments" card-id) `(("text" . ,comment-text))))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-api loaded!")


(defconst *TRELLO-URL* "https://api.trello.com/1" "The needed prefix url for trello")

(defun orgtrello-query/--compute-url (server uri)
  "Compute the trello url from the given uri."
  (format "%s%s" server uri))

(cl-defun orgtrello-query/--standard-error-callback (&key error-thrown symbol-status response &allow-other-keys)
  "Standard error callback. Simply displays a message in the minibuffer with the error code."
  (orgtrello-log/msg *OT/DEBUG* "client - Problem during the request to the proxy- error-thrown: %s" error-thrown))

(cl-defun orgtrello-query/--standard-success-callback (&key data &allow-other-keys)
  "Standard success callback. Simply displays a \"Success\" message in the minibuffer."
  (orgtrello-log/msg *OT/DEBUG* "client - Proxy received and acknowledged the request%s" (if data (format " - response data: %S." data) ".")))

(defun orgtrello-query/--authentication-params ()
  "Generates the list of http authentication parameters"
  `((key . ,*consumer-key*) (token . ,*access-token*)))

(defun orgtrello-query/--http-parse ()
  "Parse the http response into an org-trello entity."
  (->> (json-read)
    orgtrello-data/parse-data))

(defun orgtrello-query/--get (server query-map &optional success-callback error-callback authentication-p)
  "GET"
  (request (->> query-map orgtrello-data/entity-uri (orgtrello-query/--compute-url server))
           :sync    (orgtrello-data/entity-sync   query-map)
           :type    (orgtrello-data/entity-method query-map)
           :params  (orgtrello-data/merge-2-lists-without-duplicates (when authentication-p (orgtrello-query/--authentication-params)) (orgtrello-data/entity-params query-map))
           :parser  'orgtrello-query/--http-parse
           :success (if success-callback success-callback 'orgtrello-query/--standard-success-callback)
           :error   (if error-callback error-callback 'orgtrello-query/--standard-error-callback)))

(defun orgtrello-query/--post-or-put (server query-map &optional success-callback error-callback authentication-p)
  "POST or PUT"
  (request (->> query-map orgtrello-data/entity-uri (orgtrello-query/--compute-url server))
           :sync    (orgtrello-data/entity-sync   query-map)
           :type    (orgtrello-data/entity-method query-map)
           :params  (when authentication-p (orgtrello-query/--authentication-params))
           :headers '(("Content-type" . "application/json"))
           :data    (->> query-map orgtrello-data/entity-params json-encode)
           :parser  'orgtrello-query/--http-parse
           :success (if success-callback success-callback 'orgtrello-query/--standard-success-callback)
           :error   (if error-callback error-callback 'orgtrello-query/--standard-error-callback)))

(defun orgtrello-query/--delete (server query-map &optional success-callback error-callback authentication-p)
  "DELETE"
  (request (->> query-map orgtrello-data/entity-uri (orgtrello-query/--compute-url server))
           :sync    (orgtrello-data/entity-sync   query-map)
           :type    (orgtrello-data/entity-method query-map)
           :params  (when authentication-p (orgtrello-query/--authentication-params))
           :success (if success-callback success-callback 'orgtrello-query/--standard-success-callback)
           :error   (if error-callback error-callback 'orgtrello-query/--standard-error-callback)))

(defun orgtrello-query/--dispatch-http-query (method)
  "Dispatch the function to call depending on the method key."
  (cond ((string= "GET" method)                              'orgtrello-query/--get)
        ((or (string= "POST" method) (string= "PUT" method)) 'orgtrello-query/--post-or-put)
        ((string= "DELETE" method)                           'orgtrello-query/--delete)))

(defvar orgtrello-query/--hexify (if (version< emacs-version "24.3") 'orgtrello-query/url-hexify-string 'url-hexify-string)
  "Function to use to hexify depending on emacs version.")

(defun orgtrello-query/url-hexify-string (value)
  "Wrapper around url-hexify-string (older emacs 24 version do not map ! to %21)."
  (->> value
    url-hexify-string
    (replace-regexp-in-string "!" "%21")
    (replace-regexp-in-string "'" "%27")
    (replace-regexp-in-string "(" "%28")
    (replace-regexp-in-string ")" "%29")
    (replace-regexp-in-string "*" "%2A")))

(defun orgtrello-query/--prepare-params-assoc! (params)
  "Prepare params as association list (deal with nested list too)."
  (--map (let ((key   (car it))
               (value (cdr it)))
           (cond ((and value (stringp value)) `(,key . ,(funcall orgtrello-query/--hexify value)))
                 ((and value (listp value))   `(,key . ,(orgtrello-query/--prepare-params-assoc! value)))
                 (t                            it)))
         params))

(defun orgtrello-query/read-data (data)
  "Prepare params as association list (deal with nested list too)."
  (--map (let ((key   (car it))
               (value (cdr it)))
           (cond ((and value (stringp value)) `(,key . ,(url-unhex-string value)))
                 ((and value (listp value))   `(,key . ,(orgtrello-query/read-data value)))
                 (t  it)))
         data))

(defun orgtrello-query/--prepare-query-params! (params)
  "Given an association list of data, prepare the values of the params."
  (-> params
    json-encode                               ;; hashtable and association list renders the same result in json
    json-read-from-string                     ;; now getting back an association list
    orgtrello-query/--prepare-params-assoc!))

(defun orgtrello-query/--http (server query-map &optional sync success-callback error-callback authentication-p)
  "HTTP query the server with the query-map."
  (let* ((dispatch-http-query-fn (-> query-map
                                   orgtrello-data/entity-method
                                   orgtrello-query/--dispatch-http-query)))
    (if sync
        (progn ;; synchronous request
          (puthash :sync t query-map)
          (request-response-data (funcall dispatch-http-query-fn server query-map success-callback error-callback authentication-p)))
      (funcall dispatch-http-query-fn server query-map success-callback error-callback authentication-p))))

(defun orgtrello-query/http-trello (query-map &optional sync success-callback error-callback)
  "Query the trello api."
  (orgtrello-query/--http *TRELLO-URL* query-map sync success-callback error-callback t))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-query loaded!")


(defun orgtrello-backend/compute-items-from-checklist! (checklist entities adjacency)
  "Given a checklist, retrieve its items and update the entities hash and the adjacency list."
  (let ((checklist-id (orgtrello-data/entity-id checklist)))
    (--reduce-from (cl-destructuring-bind (entities adjacency) acc
                     (list (orgtrello-backend/--add-entity-to-entities it entities)
                           (orgtrello-backend/--add-entity-to-adjacency it checklist adjacency)))
                   (list entities adjacency)
                   (orgtrello-data/entity-items checklist))))

(defun orgtrello-backend/retrieve-checklist-from-card! (card)
  "Given a card, retrieve the checklist of the card (using trello). This gives a list of checklist in the trello order."
  (--> card
       (orgtrello-data/entity-checklists it)                                                            ;; retrieve checklist ids
       (-reduce-from (lambda (acc-list checklist-id)
                       (cons (-> checklist-id
                                 orgtrello-api/get-checklist
                                 (orgtrello-query/http-trello 'do-sync-query)) acc-list))
                     nil
                     it)                                                                         ;; retrieve the trello checklist
       (sort it (lambda (a b) (when (<= (orgtrello-data/entity-position a) (orgtrello-data/entity-position b)) 1)))))          ;; sort them by pos to get back to the right order (reversed)

(defun orgtrello-backend/compute-checklist-entities-from-card! (card entities adjacency)
  "Given a card, retrieve its checklists (with their items) in the right order."
  (let ((card-id (orgtrello-data/entity-id card)))
    (--> card
         (orgtrello-backend/retrieve-checklist-from-card! it)
         (-reduce-from (lambda (acc-entities-adj checklist)
                          (cl-destructuring-bind (entities adjacency) acc-entities-adj
                            (orgtrello-backend/compute-items-from-checklist! checklist (orgtrello-backend/--add-entity-to-entities checklist entities) (orgtrello-backend/--add-entity-to-adjacency checklist card adjacency))))
                       (list entities adjacency)
                       it))));; at last complete checklist with item

;; one map for each complete entity: {entity-id entity} (entity in {card, checklist, item}
;; adjacency list {card-id (checklist-id)
;;                 checklist-id (item-id)}

(defun orgtrello-backend/compute-full-cards-from-trello! (cards)
  "Given a list of cards, compute the full cards data from the trello board. The order from the trello board is kept. Hash result is of the form: {entity-id '(entity-card {checklist-id (checklist (item))})}"
  (--reduce-from (progn
                   (orgtrello-log/msg *OT/INFO* "Computing card '%s' data..."
                                      (orgtrello-data/entity-name it))
                   (cl-destructuring-bind (entities adjacency) acc
                     (orgtrello-backend/compute-checklist-entities-from-card! it (orgtrello-backend/--add-entity-to-entities it entities) adjacency)))
                 (list (orgtrello-hash/empty-hash) (orgtrello-hash/empty-hash))
                 cards))

(defun orgtrello-backend/compute-full-checklist-from-trello! (checklist)
  "Given a checklist, compute the full items data from trello. The order from the trello board is kept. Return result is the list of entities and adjacency in this order."
  (let ((entities (orgtrello-hash/empty-hash))
        (adjacency (orgtrello-hash/empty-hash)))
    (orgtrello-log/msg *OT/INFO* "Computing checklist '%s' data..." (orgtrello-data/entity-name checklist))
    (puthash (orgtrello-data/entity-id checklist) checklist entities)
    (orgtrello-backend/compute-items-from-checklist! checklist entities adjacency)))

(defun orgtrello-backend/--add-entity-to-entities (entity entities)
  "Adding entity to the hash entities."
  (let ((entity-id (orgtrello-data/entity-id-or-marker entity)))
    (puthash entity-id entity entities)
    entities))

(defun orgtrello-backend/--add-entity-to-adjacency (current-entity parent-entity adjacency)
  "Adding entity to the adjacency entry."
  (let* ((current-id (orgtrello-data/entity-id-or-marker current-entity))
         (parent-id  (orgtrello-data/entity-id-or-marker parent-entity)))
    (puthash parent-id (orgtrello-backend/--add-to-last-pos current-id (gethash parent-id adjacency)) adjacency)
    adjacency))

(defun orgtrello-backend/--put-entities-with-adjacency (current-meta entities adjacency)
  "Deal with adding a new item to entities."
  (let ((current-entity (orgtrello-data/current current-meta))
        (parent-entity  (orgtrello-data/parent current-meta)))
    (list (orgtrello-backend/--add-entity-to-entities current-entity entities) (orgtrello-backend/--add-entity-to-adjacency current-entity parent-entity adjacency))))

;; FIXME find an already existing implementation.
(defun orgtrello-backend/--add-to-last-pos (value list)
  "Adding the value to the list in last position."
  (--> list
    (reverse it)
    (cons value it)
    (reverse it)))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-backend loaded!")


(defun orgtrello-elnode/compute-entity-level-dir (level) "Given a level, compute the folder onto which the file will be serialized."
  (format "%s%s/%s/" elnode-webserver-docroot "org-trello" level))

(defun orgtrello-elnode/archived-scanning-dir (dir-name) "Given a filename, return the archived scanning directory"
  (format "%s.scanning" dir-name))

(defun orgtrello-elnode/--dictionary-lessp (str1 str2) "return t if STR1 is < STR2 when doing a dictionary compare (splitting the string at numbers and doing numeric compare with them)"
  (orgtrello-elnode/--dict-lessp (orgtrello-elnode/--dict-split str1) (orgtrello-elnode/--dict-split str2)))

(defun orgtrello-elnode/--dict-lessp (slist1 slist2) "compare the two lists of strings & numbers"
  (cond ((null slist1)                                       (not (null slist2)))
        ((null slist2)                                       nil)
        ((and (numberp (car slist1)) (stringp (car slist2))) t)
        ((and (numberp (car slist2)) (stringp (car slist1))) nil)
        ((and (numberp (car slist1)) (numberp (car slist2))) (or (< (car slist1) (car slist2))
                                                                 (and (= (car slist1) (car slist2))
                                                                      (orgtrello-elnode/--dict-lessp (cdr slist1) (cdr slist2)))))
        (t                                                   (or (string-lessp (car slist1) (car slist2))
                                                                 (and (string-equal (car slist1) (car slist2))
                                                                      (orgtrello-elnode/--dict-lessp (cdr slist1) (cdr slist2)))))))

(defun orgtrello-elnode/--dict-split (str) "split a string into a list of number and non-number components"
  (save-match-data
    (let ((res nil))
      (while (and str (not (string-equal "" str)))
        (let ((p (string-match "[0-9]*\\.?[0-9]+" str)))
          (cond ((null p) (setq res (cons str res))
                          (setq str nil))
                ((= p 0)  (setq res (cons (string-to-number (match-string 0 str)) res))
                          (setq str (substring str (match-end 0))))
                (t        (setq res (cons (substring str 0 (match-beginning 0)) res))
                          (setq str (substring str (match-beginning 0)))))))
      (reverse res))))

(defun orgtrello-elnode/list-files (directory &optional sort-lexicographically) "Compute list of regular files (no directory . and ..). List is sorted lexicographically if sort-flag-lexicographically is set, naturally otherwise."
  (let ((orgtrello-elnode/--list-files-result (--filter (file-regular-p it) (directory-files directory t))))
    (unless sort-lexicographically
            orgtrello-elnode/--list-files-result
            (sort orgtrello-elnode/--list-files-result 'orgtrello-elnode/--dictionary-lessp))))


(defun orgtrello-proxy/http (query-map &optional sync success-callback error-callback)
  "Query the proxy for the trello api."
  (--> query-map
    (orgtrello-query/--prepare-query-params! it)
    (orgtrello-api/make-query "POST" "/trello/" it)
    (orgtrello-query/--http *ORGTRELLO-SERVER-URL* it sync success-callback error-callback)))

(defun orgtrello-proxy/http-producer (query-map &optional sync)
  "Query the proxy producer"
  (--> query-map
    (orgtrello-query/--prepare-query-params! it)
    (orgtrello-api/make-query "POST" "/producer/" it)
    (orgtrello-query/--http *ORGTRELLO-SERVER-URL* it sync)))

(defun orgtrello-proxy/http-consumer (start)
  "Query the http-consumer process once to make it trigger a timer"
  (--> `((start . ,start))
    (orgtrello-api/make-query "POST" "/timer/" it)
    (orgtrello-query/--http *ORGTRELLO-SERVER-URL* it 'do-sync-query)))

(defun orgtrello-proxy/--json-read-from-string (data)
  "Read the json data and unhexify them."
  (-> data json-read-from-string orgtrello-query/read-data))

(defun orgtrello-proxy/--unhexify-data (params &optional unhexify-flag)
  "Given a params object, unhexify the content if need be."
  (funcall (if unhexify-flag 'orgtrello-proxy/--json-read-from-string 'json-read-from-string) params))

(defun orgtrello-proxy/--extract-trello-query (http-con &optional unhexify-flag)
  "Given an httpcon object, extract the params entry which corresponds to the real trello query."
  (-> http-con
    elnode-http-params
    caar
    (orgtrello-proxy/--unhexify-data unhexify-flag)))

(defun orgtrello-proxy/--compute-trello-query (query-map-wrapped)
  "Build a trello query from the control of query-map-wrapped."
  (orgtrello-api/make-query (orgtrello-data/entity-method query-map-wrapped) (orgtrello-data/entity-uri query-map-wrapped) (orgtrello-data/entity-params query-map-wrapped)))

(defun orgtrello-proxy/--response (http-con data)
  "A response wrapper"
  (elnode-http-start http-con 201 '("Content-type" . "application/json"))
  (elnode-http-return http-con (json-encode data)))

(defun orgtrello-proxy/response-ok (http-con)
  "OK response from the proxy to the client." ;; all is good
  (orgtrello-proxy/--response http-con '((status . "ok"))))

(defconst *ORGTRELLO-PROXY-QUERY-KEY* (orgtrello-hash/make-properties '((params . :params)
                                                                        (method . :method)
                                                                        (uri    . :uri)
                                                                        (callback . :callback)
                                                                        (buffername . :buffername)
                                                                        (position . :position)
                                                                        (sync . :sync)
                                                                        (name . :name)
                                                                        (id . :id)
                                                                        (action . :action)
                                                                        (level . :level)
                                                                        (start . :start)))
  "Constant to map from assoc list to org-trello query map.")

(defun orgtrello-proxy/--transcode-key (key)
  "Retrieve the corresponding key."
  (gethash key *ORGTRELLO-PROXY-QUERY-KEY*))

(defun orgtrello-proxy/parse-query (entities)
  "Given a query wrapped, convert into org-trello entity. 'params key stayed as is."
  (cond ((eq :params entities)  entities) ;; return params as is
        (t                      (--reduce-from (let ((key (car it))
                                                     (val (cdr it)))
                                                 (-when-let (new-key (orgtrello-proxy/--transcode-key key))
                                                   (puthash new-key val acc))
                                                 acc)
                                               (orgtrello-hash/empty-hash)
                                               entities))))

(defun orgtrello-proxy/--elnode-proxy (http-con)
  "Deal with request to trello (for creation/sync request, use orgtrello-proxy/--elnode-proxy-producer)."
  (orgtrello-log/msg *OT/TRACE* "Proxy - Request received. Transmitting...")
  (let* ((query-map-wrapped    (orgtrello-proxy/--extract-trello-query http-con 'unhexify)) ;; wrapped query is mandatory, we unhexify the wrapped query
         (query-map-data       (orgtrello-proxy/parse-query query-map-wrapped))
         (position             (orgtrello-data/entity-position query-map-data)) ;; position is mandatory
         (buffer-name          (orgtrello-data/entity-buffername query-map-data)) ;; buffer-name is mandatory
         (standard-callback    (orgtrello-data/entity-callback query-map-data)) ;; there is the possibility to transmit the callback from the client to the proxy
         (standard-callback-fn (when standard-callback (symbol-function (intern standard-callback)))) ;; the callback is passed as a string, we want it as a function when defined
         (sync                 (orgtrello-data/entity-sync query-map-data)) ;; there is a possibility to enforce the sync between proxy and client
         (query-map            (orgtrello-proxy/--compute-trello-query query-map-data)) ;; extracting the query
         (name                 (orgtrello-data/entity-name query-map-data))) ;; extracting the name of the entity (optional)
    (orgtrello-query/http-trello query-map sync (when standard-callback-fn (funcall standard-callback-fn buffer-name position name))) ;; callback-fn is not a callback, it creates one
    (orgtrello-proxy/response-ok http-con)))

(defun orgtrello-proxy/--compute-metadata-filename (root-dir buffer-name position)
  "Compute the metadata entity filename"
  (format "%s%s-%s.el" root-dir buffer-name position))

(defun orgtrello-proxy/--elnode-proxy-producer (http-con)
  "A handler which is an entity informations producer on files under the docroot/level-entities/"
  (orgtrello-log/msg *OT/TRACE* "Proxy-producer - Request received. Generating entity file...")
  (let* ((query-map-wrapped    (orgtrello-proxy/--extract-trello-query http-con 'unhexify)) ;; wrapped query is mandatory
         (query-map-data       (orgtrello-proxy/parse-query query-map-wrapped))
         (position             (orgtrello-data/entity-position query-map-data))          ;; position is mandatory
         (buffer-name          (orgtrello-data/entity-buffername query-map-data))        ;; buffer-name is mandatory
         (level                (orgtrello-data/entity-level query-map-data))
         (root-dir             (orgtrello-elnode/compute-entity-level-dir level)))
    ;; generate a file with the entity information
    (with-temp-file (orgtrello-proxy/--compute-metadata-filename root-dir buffer-name position)
      (insert (format "%S\n" query-map-wrapped)))
    (orgtrello-proxy/response-ok http-con)))

(defun orgtrello-proxy/--read-file-content (fPath)
  "Return a list of lines of a file at FPATH."
  (with-temp-buffer
    (insert-file-contents fPath)
    (buffer-string)))

(defun orgtrello-proxy/--update-buffer-to-save (buffer-name buffers-to-save)
  "Add the buffer-name to the list if not already present"
  (if (member buffer-name buffers-to-save)
      buffers-to-save
    (cons buffer-name buffers-to-save)))

(defvar *ORGTRELLO-LIST-BUFFERS-TO-SAVE* nil "A simple flag to order the saving of buffer when needed.")

(defun orgtrello-proxy/update-buffer-to-save! (buffer-name)
  "Side-effect - Mutate the *ORGTRELLO-LIST-BUFFERS-TO-SAVE* by adding buffer-name to it if not already present."
  (setq *ORGTRELLO-LIST-BUFFERS-TO-SAVE* (orgtrello-proxy/--update-buffer-to-save buffer-name *ORGTRELLO-LIST-BUFFERS-TO-SAVE*)))

(defun orgtrello-proxy/--cleanup-and-save-buffer-metadata (archive-file buffer-name)
  "To cleanup metadata after the all actions are done!"
  (orgtrello-action/delete-file! archive-file) ;; cleanup archive file
  (orgtrello-proxy/update-buffer-to-save! buffer-name)) ;; register the buffer for later saving

(defun orgtrello-proxy/batch-save (buffers)
  "Save sequentially a list of buffers."
  (-each buffers 'save-buffer))

(defun orgtrello-proxy/batch-save! ()
  "Save sequentially the org-trello list of modified buffers."
  (setq *ORGTRELLO-LIST-BUFFERS-TO-SAVE* (orgtrello-proxy/batch-save *ORGTRELLO-LIST-BUFFERS-TO-SAVE*)))

(defmacro orgtrello-proxy/--safe-wrap-or-throw-error (fn)
  "A specific macro to deal with interception of uncaught error when executing the fn call. If error is thrown, send the 'org-trello-timer-go-to-sleep flag."
  `(condition-case ex
       (progn ,fn)
     ('error
      (orgtrello-log/msg *OT/ERROR* (concat "### org-trello - consumer ### Caught exception: [" ex "]"))
      (throw 'org-trello-timer-go-to-sleep t))))

(defun orgtrello-proxy/--getting-back-to-headline (data)
  "Trying another approach to getting back to header computing the normal form of an entry in the buffer."
  (orgtrello-proxy/--getting-back-to-marker (orgtrello-buffer/--compute-entity-to-org-entry data)))

(defun orgtrello-proxy/--compute-pattern-search-from-marker (marker)
  "Given a marker, compute the pattern to look for in the file."
  marker)

(defun orgtrello-proxy/--getting-back-to-marker (marker)
  "Given a marker, getting back to marker function. Move the cursor position."
  (goto-char (point-min))
  (re-search-forward (orgtrello-proxy/--compute-pattern-search-from-marker marker) nil t))

(defun orgtrello-proxy/--get-back-to-marker (marker data)
  "Getting back to the marker. Move the cursor position."
  (-if-let (goto-ok (orgtrello-proxy/--getting-back-to-marker marker))
      goto-ok
    (orgtrello-proxy/--getting-back-to-headline data)))

(defun orgtrello-proxy/--standard-post-or-put-success-callback (entity-to-sync file-to-cleanup)
  "Return a callback function able to deal with the update of the buffer at a given position."
  (lexical-let ((orgtrello-proxy/--entry-position    (orgtrello-data/entity-position entity-to-sync))
                (orgtrello-proxy/--entry-buffer-name (orgtrello-data/entity-buffername entity-to-sync))
                (orgtrello-proxy/--entry-file        file-to-cleanup)
                (orgtrello-proxy/--marker-id         (orgtrello-data/entity-id-or-marker entity-to-sync))
                (orgtrello-proxy/--entity-name       (orgtrello-data/entity-name entity-to-sync)))
    (function* (lambda (&key data &allow-other-keys)
                 (orgtrello-action/safe-wrap
                  (let* ((orgtrello-proxy/--entry-new-id (orgtrello-data/entity-id data)))
                    (set-buffer orgtrello-proxy/--entry-buffer-name) ;; switch to the right buffer
                    ;; will update via tag the trello id of the new persisted data (if needed)
                    (save-excursion
                      ;; get back to the buffer and update the id if need be
                      (let ((str-msg (when (orgtrello-proxy/--get-back-to-marker orgtrello-proxy/--marker-id data)
                                       ;; now we extract the data
                                       (let ((orgtrello-proxy/--entry-id (when (orgtrello-data/id-p orgtrello-proxy/--marker-id) orgtrello-proxy/--marker-id)))
                                         (if orgtrello-proxy/--entry-id ;; id already present in the org-mode file
                                             ;; no need to add another
                                             (concat "Entity '" orgtrello-proxy/--entity-name "' with id '" orgtrello-proxy/--entry-id "' synced!")
                                           (let ((orgtrello-proxy/--entry-name (orgtrello-data/entity-name data)))
                                             ;; not present, this was just created, we add a simple property
                                             (orgtrello-buffer/set-property *ORGTRELLO-ID* orgtrello-proxy/--entry-new-id)
                                             (concat "Newly entity '" orgtrello-proxy/--entry-name "' with id '" orgtrello-proxy/--entry-new-id "' synced!")))))))
                        (when str-msg (orgtrello-log/msg *OT/INFO* str-msg)))))
                  (orgtrello-proxy/--cleanup-and-save-buffer-metadata orgtrello-proxy/--entry-file orgtrello-proxy/--entry-buffer-name))))))

(defun orgtrello-proxy/--archived-scanning-file (file)
  "Given a filename, return its archived filename if we were to move such file."
  (format "%s/%s" (orgtrello-elnode/archived-scanning-dir (file-name-directory file)) (file-name-nondirectory file)))

(defun orgtrello-proxy/--archive-entity-file-when-scanning (file-to-archive file-archive-name)
  "Move the file to the running folder to specify a sync is running."
  (rename-file file file-archive-name t))

(defun orgtrello-proxy/--dispatch-action (action)
  "Dispatch action function depending on the flag action"
  (cond ((string= *ORGTRELLO-ACTION-DELETE* action) 'orgtrello-proxy/--delete)
        ((string= *ORGTRELLO-ACTION-SYNC*   action) 'orgtrello-proxy/--sync-entity)))

(defun orgtrello-proxy/--cleanup-meta (entity-full-metadata)
  (unless (-> entity-full-metadata
            orgtrello-data/current
            orgtrello-data/entity-id)
    (orgtrello-cbx/org-delete-property *ORGTRELLO-ID*)))

(defun orgtrello-proxy/--sync-entity (entity-data entity-full-metadata entry-file-archived)
  "Execute the entity synchronization."
  (lexical-let ((orgtrello-query/--query-map (orgtrello-controller/--dispatch-create entity-full-metadata))
                (oq/--entity-full-meta       entity-full-metadata)
                (oq/--entry-file-archived    entry-file-archived))
    (if (hash-table-p orgtrello-query/--query-map)
        ;; execute the request
        (orgtrello-query/http-trello orgtrello-query/--query-map 'do-sync-query
                                     (orgtrello-proxy/--standard-post-or-put-success-callback entity-data entry-file-archived)
                                     (function* (lambda (&key error-thrown &allow-other-keys)
                                                  (orgtrello-log/msg *OT/ERROR* "client - Problem during the sync request to the proxy- error-thrown: %s" error-thrown)
                                                  (orgtrello-proxy/--cleanup-meta oq/--entity-full-meta)
                                                  (orgtrello-action/delete-file! oq/--entry-file-archived)
                                                  (throw 'org-trello-timer-go-to-sleep t))))
      ;; cannot execute the request
      (progn
        (orgtrello-log/msg *OT/INFO* orgtrello-query/--query-map)
        (orgtrello-proxy/--cleanup-meta entity-full-metadata)
        (throw 'org-trello-timer-go-to-sleep t)))))

(defun orgtrello-proxy/--deal-with-entity-action (entity-data file-to-archive)
  "Compute the synchronization of an entity (retrieving latest information from buffer)"
  (let* ((op/--position            (orgtrello-data/entity-position entity-data)) ;; position is mandatory
         (op/--buffer-name         (orgtrello-data/entity-buffername entity-data)) ;; buffer-name too
         (op/--entry-file-archived (orgtrello-proxy/--archived-scanning-file file-to-archive))
         (op/--marker              (orgtrello-data/entity-id-or-marker entity-data))) ;; retrieve the id (which serves as a marker too)
    (orgtrello-log/msg *OT/TRACE* "Proxy-consumer - Searching entity metadata from buffer '%s' at point '%s' to sync..." op/--buffer-name op/--position)
    (set-buffer op/--buffer-name)                                                                  ;; switch to the right buffer
    (orgtrello-proxy/--safe-wrap-or-throw-error                                                    ;; will update via tag the trello id of the new persisted data (if needed)
     (save-excursion
       (when (orgtrello-proxy/--get-back-to-marker op/--marker entity-data)
         (orgtrello-proxy/--archive-entity-file-when-scanning file-to-archive op/--entry-file-archived) ;; archive the scanned file
         (-> entity-data
           orgtrello-data/entity-action
           orgtrello-proxy/--dispatch-action
           (funcall entity-data (orgtrello-buffer/entry-get-full-metadata!) op/--entry-file-archived)))))))

(defun orgtrello-action/org-delete-property (key)
  "Delete a property depending on the nature of the current entry (org heading or checkbox)."
  (funcall (if (orgtrello-cbx/checkbox-p) 'orgtrello-cbx/org-delete-property 'org-delete-property) key))

(defun orgtrello-action/--delete-region (start end)
  "Delete a region defined by start and end bound."
  (remove-overlays start end) ;; remove overlays on the card region
  (delete-region start end))

(defun orgtrello-action/--delete-card-region ()
  "Delete the card region (including overlays and line)"
  (org-back-to-heading)
  (let ((orgtrello-action/--starting-point (point))
        (orgtrello-action/--ending-point   (save-excursion (if (org-goto-sibling) (point) (point-max))))) ;; next card or point-max
    (orgtrello-action/--delete-region orgtrello-action/--starting-point orgtrello-action/--ending-point)))

(defun orgtrello-action/--delete-checkbox-checklist-region ()
  "Delete the checklist region"
  (let ((orgtrello-action/--starting-point (point-at-bol))
        (orgtrello-action/--ending-point (save-excursion (-if-let (result (orgtrello-cbx/--goto-next-checkbox-with-same-level! *CHECKLIST-LEVEL*))
                                                             result
                                                           (orgtrello-cbx/compute-next-card-point!))))) ;; next checkbox or next card or point-max
    (orgtrello-action/--delete-region orgtrello-action/--starting-point orgtrello-action/--ending-point)))

(defun orgtrello-action/--delete-checkbox-item-region ()
  "Delete the item region"
  (let ((orgtrello-action/--starting-point (point-at-bol))
        (orgtrello-action/--ending-point (1+ (point-at-eol))))
    (orgtrello-action/--delete-region orgtrello-action/--starting-point orgtrello-action/--ending-point)))

(defun orgtrello-action/delete-region (entity)
  "Delete the region"
  (cond ((orgtrello-data/entity-card-p entity) 'orgtrello-action/--delete-card-region)
        ((orgtrello-data/entity-checklist-p entity) 'orgtrello-action/--delete-checkbox-checklist-region)
        ((orgtrello-data/entity-item-p entity) 'orgtrello-action/--delete-checkbox-item-region)))

(defun orgtrello-proxy/--standard-delete-success-callback (entity-to-del file-to-cleanup)
  "Return a callback function able to deal with the position."
  (lexical-let ((op/--entry-position    (orgtrello-data/entity-position entity-to-del))
                (op/--entry-buffer-name (orgtrello-data/entity-buffername entity-to-del))
                (op/--entry-level       (orgtrello-data/entity-level entity-to-del))
                (op/--entry-file        file-to-cleanup)
                (op/--marker            (orgtrello-data/entity-id entity-to-del)))
    (lambda (&rest response)
      (orgtrello-action/safe-wrap
       (progn
         (set-buffer op/--entry-buffer-name)
         (save-excursion
           (when (orgtrello-proxy/--getting-back-to-marker op/--marker)
             (-> (orgtrello-buffer/entry-get-full-metadata!)
               orgtrello-data/current
               orgtrello-action/delete-region
               funcall))))
       (orgtrello-proxy/--cleanup-and-save-buffer-metadata op/--entry-file op/--entry-buffer-name)))))

(defun orgtrello-proxy/--delete (entity-data entity-full-metadata entry-file-archived)
  "Execute the entity deletion."
  (lexical-let ((orgtrello-query/--query-map (orgtrello-controller/--dispatch-delete (orgtrello-data/current entity-full-metadata) (orgtrello-data/parent entity-full-metadata)))
                (oq/--entity-full-meta       entity-full-metadata)
                (oq/--entry-file-archived    entry-file-archived))
    (if (hash-table-p orgtrello-query/--query-map)
        (orgtrello-query/http-trello orgtrello-query/--query-map 'do-sync-query
                                     (orgtrello-proxy/--standard-delete-success-callback entity-data entry-file-archived)
                                     (function* (lambda (&key error-thrown &allow-other-keys)
                                                  (orgtrello-log/msg *OT/ERROR* "client - Problem during the deletion request to the proxy- error-thrown: %s" error-thrown)
                                                  (orgtrello-proxy/--cleanup-meta oq/--entity-full-meta)
                                                  (orgtrello-action/delete-file! oq/--entry-file-archived)
                                                  (throw 'org-trello-timer-go-to-sleep t))))
      (progn
        (orgtrello-log/msg *OT/INFO* orgtrello-query/--query-map)
        (throw 'org-trello-timer-go-to-sleep t)))))

(defun orgtrello-proxy/--deal-with-entity-file-action (file)
  "Given an entity file, load it and run a query action through trello"
  (when (file-exists-p file)
    (orgtrello-proxy/--deal-with-entity-action (-> file
                                                 orgtrello-proxy/--read-file-content
                                                 read
                                                 orgtrello-proxy/parse-query) file)))

(defun orgtrello-proxy/--deal-with-directory-action (level directory)
  "Given a directory, list the files and take the first one (entity) and do some action on it with trello. Call again if it remains other entities."
  (-when-let (orgtrello-proxy/--files (orgtrello-elnode/list-files directory))
    (orgtrello-proxy/--deal-with-entity-file-action (car orgtrello-proxy/--files))
    ;; if it potentially remains files, recall recursively this function
    (when (< 1 (length orgtrello-proxy/--files)) (orgtrello-proxy/--deal-with-level level directory))))

(defun orgtrello-proxy/--level-done-p (level)
  "Does all the entities for the level are their actions done?"
  (-> level
    orgtrello-elnode/compute-entity-level-dir
    orgtrello-elnode/list-files
    null))

(defun orgtrello-proxy/--level-inf-done-p (level)
  "Ensure the actions of the lower level is done (except for level 1 which has no deps)!"
  (cond ((= *CARD-LEVEL*      level) t)
        ((= *CHECKLIST-LEVEL* level) (orgtrello-proxy/--level-done-p *CARD-LEVEL*))
        ((= *ITEM-LEVEL*      level) (and (orgtrello-proxy/--level-done-p *CARD-LEVEL*) (orgtrello-proxy/--level-done-p *CHECKLIST-LEVEL*)))))

(defun orgtrello-proxy/--deal-with-level (level directory)"Given a level, retrieve one file (which represents an entity) for this level and sync it, then remove such file. Then recall the function recursively."
       (if (orgtrello-proxy/--level-inf-done-p level)
           (orgtrello-proxy/--deal-with-directory-action level directory)
         (throw 'org-trello-timer-go-to-sleep t)))

(defun orgtrello-proxy/--deal-with-archived-files (level)
  "Given a level, move all the remaining archived files into the scan folder from the same level."
  (let ((level-dir (orgtrello-elnode/compute-entity-level-dir level)))
    (mapc (lambda (file) (rename-file file (format "%s%s" level-dir (file-name-nondirectory file)) t)) (-> level-dir
                                                                                                         orgtrello-elnode/archived-scanning-dir
                                                                                                         orgtrello-elnode/list-files))))

(defun orgtrello-proxy/--consumer-entity-files-hierarchically-and-do ()
  "A handler to extract the entity informations from files (in order card, checklist, items)."
  (with-local-quit
    (dolist (l *ORGTRELLO-LEVELS*) (orgtrello-proxy/--deal-with-archived-files l))  ;; if archived file exists, get them back in the queue before anything else
    (catch 'org-trello-timer-go-to-sleep     ;; if some check regarding order fails, we catch and let the timer sleep. The next time, the trigger will get back normally to the upper level in order
      (dolist (l *ORGTRELLO-LEVELS*) (orgtrello-proxy/--deal-with-level l (orgtrello-elnode/compute-entity-level-dir l))))
    (orgtrello-proxy/batch-save!))) ;; we need to save the modified buffers

(defun orgtrello-proxy/--compute-lock-filename ()
  "Compute the name of a lock file"
  (format "%s%s/%s" elnode-webserver-docroot "org-trello" "org-trello-already-scanning.lock"))

(defvar *ORGTRELLO-LOCK* (orgtrello-proxy/--compute-lock-filename)
  "Lock file to ensure one timer is running at a time.")

(defun orgtrello-proxy/--timer-put-lock (lock-file)
  "Start triggering the timer."
  (with-temp-file lock-file
    (insert "Timer - Scanning entities...")))

(defun orgtrello-proxy/--timer-delete-lock (lock-file)
  "Cleanup after the timer has been triggered."
  (orgtrello-action/delete-file! lock-file))

(defun orgtrello-proxy/--consumer-lock-and-scan-entity-files-hierarchically-and-do ()
  "A handler to extract the entity informations from files (in order card, checklist, items)."
  (undo-boundary)
  ;; only one timer at a time
  (orgtrello-action/safe-wrap
   (progn
     (orgtrello-proxy/--timer-put-lock *ORGTRELLO-LOCK*)
     (orgtrello-proxy/--consumer-entity-files-hierarchically-and-do))
   (orgtrello-proxy/--timer-delete-lock *ORGTRELLO-LOCK*))
  ;; undo boundary, to make a unit of undo
  (undo-boundary))

(defun orgtrello-proxy/--windows-system-considered-always-with-network ()
  "function 'network-interface-list is not defined on windows system, so we avoid checking this and always return ok." :ok)

(defun orgtrello-proxy/--check-network-ok ()
  "Ensure network exists!" (if (< 1 (length (network-interface-list))) :ok "No network!"))

(defun orgtrello-proxy/--check-network-connection (&optional args)
  "Ensure there is some network running (simply check that there is more than the lo interface)."
  (funcall (if (string-equal system-type "windows-nt") 'orgtrello-proxy/--windows-system-considered-always-with-network 'orgtrello-proxy/--check-network-ok)))

(defun orgtrello-proxy/--check-no-running-timer (&optional args)
  "Ensure there is not another running timer already."
  (if (file-exists-p (orgtrello-proxy/--compute-lock-filename)) "Timer already running!" :ok))

(defun orgtrello-proxy/--controls-and-scan-if-ok ()
  "Execution of the timer which consumes the entities and execute the sync to trello."
  (orgtrello-action/msg-controls-or-actions-then-do
   "Scanning entities to sync"
   '(orgtrello-proxy/--check-network-connection orgtrello-proxy/--check-no-running-timer)
   'orgtrello-proxy/--consumer-lock-and-scan-entity-files-hierarchically-and-do
   nil ;; cannot save the buffer
   nil ;; do not need to reload the org-trello setup
   'do-not-display-log));; do no want to log

(defun orgtrello-proxy/--prepare-filesystem ()
  "Prepare the filesystem for every level."
  (dolist (l *ORGTRELLO-LEVELS*)
    (-> l
      orgtrello-elnode/compute-entity-level-dir
      orgtrello-elnode/archived-scanning-dir
      (mkdir t))))

(defvar *ORGTRELLO-TIMER* nil "A timer run by elnode")

(defun orgtrello-proxy/--elnode-timer (http-con)
  "A process on elnode to trigger even regularly."
  (let* ((query-map     (-> http-con orgtrello-proxy/--extract-trello-query orgtrello-proxy/parse-query))
         (start-or-stop (orgtrello-data/entity-start query-map)))
    (if start-or-stop
        ;; cleanup before starting anew
        (progn
          (orgtrello-log/msg *OT/DEBUG* "Proxy-timer - Request received. Start timer.")
          ;; cleanup anything that the timer possibly left behind
          (orgtrello-proxy/--timer-delete-lock *ORGTRELLO-LOCK*)
          ;; Prepare the filesystem with the right folders
          (orgtrello-proxy/--prepare-filesystem)
          ;; start the timer
          (setq *ORGTRELLO-TIMER* (run-with-timer 0 5 'orgtrello-proxy/--controls-and-scan-if-ok)))
      ;; otherwise, stop it
      (when *ORGTRELLO-TIMER*
        (orgtrello-log/msg *OT/DEBUG* "Proxy-timer - Request received. Stop timer.")
        ;; stop the timer
        (cancel-timer *ORGTRELLO-TIMER*)
        ;; nil the orgtrello reference
        (setq *ORGTRELLO-TIMER* nil)))
    ;; ok in any case
    (orgtrello-proxy/response-ok http-con)))

(defun orgtrello-proxy/timer-start ()
  "Start the orgtrello-timer."
  (orgtrello-proxy/http-consumer t))

(defun orgtrello-proxy/timer-stop ()
  "Stop the orgtrello-timer."
  (orgtrello-proxy/http-consumer nil))

(defun orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do (msg control-or-action-fns fn-to-execute &optional save-buffer-p reload-setup-p nolog-p)
  "Decorator fn to execute actions before/after the controls."
  (orgtrello-proxy/timer-stop)
  (orgtrello-action/msg-controls-or-actions-then-do msg control-or-action-fns fn-to-execute save-buffer-p reload-setup-p nolog-p)   ;; Execute as usual
  (orgtrello-proxy/timer-start))

(defvar *ORGTRELLO-QUERY-APP-ROUTES-PROXY*
  '(;; proxy to request trello
    ("^localhost//proxy/trello/\\(.*\\)" . orgtrello-proxy/--elnode-proxy)
    ;; proxy producer to receive async creation request
    ("^localhost//proxy/producer/\\(.*\\)" . orgtrello-proxy/--elnode-proxy-producer)
    ;; proxy to request trello
    ("^localhost//proxy/timer/\\(.*\\)" . orgtrello-proxy/--elnode-timer))
  "Org-trello dispatch routes for the proxy")

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-proxy loaded!")


(defun orgtrello-webadmin/--compute-root-static-files () "Root files under which css and js files are installed."
  (format "%s%s" elnode-webserver-docroot "org-trello/bootstrap"))

(defun orgtrello-webadmin/--installation-needed-p () "Determine if the installation is needed."
  (let ((dir (orgtrello-webadmin/--compute-root-static-files)))
    (not (and (file-exists-p dir)
              (< 3 (-> dir
                       directory-files
                       length)))))) ;; . and .. are returned by default

(defvar *ORGTRELLO-FILES* (let ((tmp (orgtrello-hash/empty-hash)))
                            ;;                    url                                                  temp file            install destination
                            (puthash :bootstrap `("http://getbootstrap.com/2.3.2/assets/bootstrap.zip" "/tmp/bootstrap.zip" ,(orgtrello-webadmin/--compute-root-static-files)) tmp)
                            (puthash :jquery    `("http://code.jquery.com/jquery-2.0.3.min.js"         "/tmp/jquery.js"     ,(format "%s/js" (orgtrello-webadmin/--compute-root-static-files))) tmp)
                            tmp))

(defun orgtrello-webadmin/--unzip-and-install (file dest) "Execute the unarchive command. Dependency on unzip on the system."
  (shell-command (format "unzip -o %s -d %s" file dest)))

(defun orgtrello-webadmin/--install-file (file file-dest) "Install the file from temporary location to the final destination."
  (when (file-exists-p file)
        (rename-file file file-dest t)))

(defun orgtrello-webadmin/--download-and-install-file (key-file) "Download the file represented by the parameter. Also, if the archive downloaded is a zip, unzip it."
  (let* ((url-tmp-dest (gethash key-file *ORGTRELLO-FILES*))
         (url          (first  url-tmp-dest))
         (tmp-dest     (second url-tmp-dest))
         (final-dest   (third  url-tmp-dest))
         (extension    (file-name-extension url)))
    ;; download the file
    (url-copy-file url tmp-dest t)
    (if (equal "zip" extension)
        (orgtrello-webadmin/--unzip-and-install tmp-dest (file-name-directory final-dest))
        (orgtrello-webadmin/--install-file tmp-dest final-dest))))

(defun orgtrello-webadmin/--install-css-js-files-once () "Install bootstrap and jquery if need be."
  (when (orgtrello-webadmin/--installation-needed-p)
        (mapc (lambda (key-file) (orgtrello-webadmin/--download-and-install-file key-file)) '(:bootstrap :jquery))))

(defun orgtrello-webadmin/--render-html (data) "Render the data in html."
  (esxml-to-xml data))

(defun orgtrello-webadmin/html (project-name author-name description) "Main html page"
  `(html
    ()
    ,(orgtrello-webadmin/head project-name author-name description)
    ,(orgtrello-webadmin/body project-name)))

(defun orgtrello-webadmin/head (project-name author-name description) "Generate html <head>"
  `(head ()
         (meta ((charset . "utf-8")))
         (title () ,project-name)
         (meta ((name . "viewport")
                (content . "width=device-width, initial-scale=1.0")))
         (meta ((name . "author")
                (content . ,author-name)))
         (meta ((name . "description")
                (content . ,description)))
         (style ()
                "
      body {
        padding-top: 20px;
        padding-bottom: 40px;
      }

      /* Custom container */
      .container-narrow {
        margin: 0 auto;
        max-width: 700px;
      }
      .container-narrow > hr {
        margin: 30px 0;
      }

      /* Main marketing message and sign up button */
      .jumbotron {
        margin: 60px 0;
        text-align: center;
      }
      .jumbotron h1 {
        font-size: 72px;
        line-height: 1;
      }
      .jumbotron .btn {
        font-size: 21px;
        padding: 14px 24px;
      }

      /* Supporting marketing content */
      .marketing {
        margin: 60px 0;
      }
      .marketing p + h4 {
        margin-top: 28px;
      }")
         (link ((href . "/static/css/bootstrap.css")
                (rel . "stylesheet")))
         (link ((href . "/static/css/bootstrap-responsive.min.css")
                (rel . "stylesheet")))
         "
    <!-- HTML5 shim, for IE6-8 support of HTML5 elements -->
    <!--[if lt IE 9]>
      <script src=\"http://html5shim.googlecode.com/svn/trunk/html5.js\"></script>
    <![endif]-->
"))

(defun orgtrello-webadmin/--main-body () "Build the main body where we will display informations (without all the html boilerplate)."
  `(div ((class . "row-fluid marketing"))
        (div ((class . "span6"))
             (div ((style . "font-size: 2em;margin-right: 10px;margin-bottom: 10px")) "Current action")
             (span ((id . "current-action"))))
        (div ((class . "span6"))
             (div ((style . "margin-bottom:10px"))
                  (span ((style . "font-size: 2em;margin-right: 10px")) "Next actions")
                  (span () ,(orgtrello-webadmin/--input-button-html "deleteEntities('/proxy/admin/entities/delete/');" "Delete all")))
             (span ((id . "next-actions"))))))

(defun orgtrello-webadmin/body (project-name) "Display the data inside the html body"
  `(body
    ()
    (div ((class . "navbar navbar-inverse navbar-fixed-top"))
         (div ((class . "navbar-inner"))
              (div ((class . "container"))
                   (button ((type . "button")
                            (class . "btn btn-navbar")
                            (data-toggle . "collapse")
                            (data-target . "nav-collapse"))
                           (span ((class . "icon-bar")))
                           (span ((class . "icon-bar")))
                           (span ((class . "icon-bar"))))
                   (a ((class . "brand")
                       (href . "#"))
                      ,project-name)
                   (div ((class . "nav-collapse collapse"))
                        (ul ((class . "nav"))
                            (li ((class . "active"))
                                (a ((href . "#"))
                                   "Home"))
                            (li ((class . "active"))
                                (a ((href . "#about"))
                                   "About"))
                            (li ((class . "active"))
                                (a ((href . "#contact"))
                                   "Contact")))))))
    (div ((class . "container"))
         (div ((class . "container-narrow"))
              ,(orgtrello-webadmin/--main-body)))
    (script ((src . "/static/js/bootstrap.min.js")) "")
    (script ((src . "/static/js/jquery.js")) "")
    (script ()
            "
function refresh (url, id) {
    $.ajax({
        url: url
    }).done(function (data) {
        $(id).html(data);
        setTimeout(function() { refresh(url, id); }, 500);
    });
}

function deleteEntities(url) {
    $.ajax({
        url:  url
    }).done(function (data) {

    });
}

refresh(\"/proxy/admin/entities/next/\", '#next-actions');
refresh(\"/proxy/admin/entities/current/\", '#current-action');
")))

(defun orgtrello-webadmin/--content-file (file) "Return the content of a file (absolute name)."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun orgtrello-webadmin/--header-table () "Generate headers."
  `(tr () (td ()) (td () "Action") (td () "Entity") (td () "Delete")))

(defun orgtrello-webadmin/--detail-entity (log-level entity-data) "Depending on the debug level, will display either the full entity data or simply its name."
  (if (= log-level *OT/INFO*) (orgtrello-data/entity-name entity-data) entity-data))

(defun orgtrello-webadmin/--input-button-html (action value) "Given a javascript action and a value, compute an html input button."
  `(input ((class . "btn btn-danger btn-mini")
           (type . "button")
           (onclick . ,action)
           (value . ,value))))

(defun orgtrello-webadmin/--delete-action (entity) "Generate the button to delete some action."
  (-if-let (entity-id (orgtrello-data/entity-id-or-marker entity))
      (orgtrello-webadmin/--input-button-html (format "deleteEntities('/proxy/admin/entities/delete/%s');" entity-id) "x")
    ""))

(defun orgtrello-webadmin/--compute-class (tr-class) "Compute the tr-class"
  `(class . ,(cond ((string= tr-class "icon-play")  "success")
                   ((string= tr-class "icon-pause") "warning")
                   (t                               ""))))

(defun orgtrello-webadmin/--entity (entity icon &optional tr-class) "Compute the entity file display rendering."
  `(tr
    (,(orgtrello-webadmin/--compute-class icon))
    (td () (i ((class . ,icon))))
    (td () ,(orgtrello-data/entity-action entity))
    (td () ,(format "%s" (orgtrello-webadmin/--detail-entity *orgtrello-log/level* entity)))
    (td () ,(orgtrello-webadmin/--delete-action entity))))

(defun orgtrello-webadmin/--list-entities-as-html (entities icon-array-nxt) "Given a list of entities, return as html data."
  (--map (orgtrello-webadmin/--entity it icon-array-nxt) entities))

(defun orgtrello-webadmin/--entities-as-html (entities &optional icon-array-running icon-array-next) "Return the list of files to send to trello"
  (let ((icon-array-run (if icon-array-running icon-array-running "icon-arrow-right"))
        (icon-array-nxt (if icon-array-next icon-array-next "icon-arrow-up")))
    (if entities
        `(table ((class . "table table-striped table-bordered table-hover")
                 (style . "font-size: 0.75em"))
                ;; header
                ,(orgtrello-webadmin/--header-table)
                ;; first next running action
                ,(orgtrello-webadmin/--entity (car entities) icon-array-run)
                ;; next running actions
                ,@(orgtrello-webadmin/--list-entities-as-html (cdr entities) icon-array-nxt))
        "None")))

(defun orgtrello-webadmin/--response-html (data http-con) "A response wrapper."
  (elnode-http-start http-con 201 '("Content-type" . "text/html"))
  (elnode-http-return http-con (orgtrello-webadmin/--render-html data)))

(defun orgtrello-webadmin/--elnode-admin (http-con) "A basic display of data"
  (-> (orgtrello-webadmin/html "org-trello/proxy-admin" "Commiters" "Administration the running queries to trello")
      (orgtrello-webadmin/--response-html  http-con)))

(defun compose-fn (funcs) "Composes several functions into one."
  (lexical-let ((intern-funcs funcs))
    (lambda (arg)
      (if intern-funcs
          (funcall (car intern-funcs)
                   (funcall (compose-fn (cdr intern-funcs)) arg))
          arg))))

(defun orgtrello-webadmin/--list-entities (levels &optional scan-flag) "Compute the actions into list."
  (let* ((list-fns '(orgtrello-elnode/compute-entity-level-dir))
         (scan-fns (if scan-flag (cons 'orgtrello-elnode/archived-scanning-dir list-fns) list-fns)) ;; build the list of functions to create the composed function
         (composed-fn (compose-fn scan-fns)))
    (--map
     (orgtrello-proxy/parse-query (read (orgtrello-webadmin/--content-file it)))
     (--mapcat (orgtrello-elnode/list-files (funcall composed-fn it)) levels))))

(defun orgtrello-webadmin/elnode-current-entity (http-con) "A basic display of the list of entities to scan."
  (-> *ORGTRELLO-LEVELS*
      (orgtrello-webadmin/--list-entities 'scan-folder)
      nreverse
      (orgtrello-webadmin/--entities-as-html "icon-play" "icon-pause")
      (orgtrello-webadmin/--response-html http-con)))

(defun orgtrello-webadmin/elnode-next-entities (http-con) "A basic display of the list of entities to scan."
  (-> *ORGTRELLO-LEVELS*
       orgtrello-webadmin/--list-entities
       orgtrello-webadmin/--entities-as-html
       (orgtrello-webadmin/--response-html http-con)))

(defun orgtrello-webadmin/elnode-static-file (http-con) "Serve static files if they exist. Throw 404 if it does not exists. Also, install bootstrap and jquery the first time round."
  ;; the first request will ask for installing bootstrap and jquery
  (orgtrello-webadmin/--install-css-js-files-once)
  (let ((full-file (format "%s/%s/%s" (orgtrello-webadmin/--compute-root-static-files) (elnode-http-mapping http-con 1) (elnode-http-mapping http-con 2))))
    (if (file-exists-p full-file)
        (elnode-send-file http-con full-file)
        (elnode-send-404 http-con (format "Resource file '%s' not found!" full-file)))))

(defun orgtrello-webadmin/--compute-filename-from-entity (entity) "Compute the filename of a file given an entity."
  (format "%s%s-%s.el" (orgtrello-elnode/compute-entity-level-dir (orgtrello-data/entity-level entity)) (orgtrello-data/entity-buffername entity) (orgtrello-data/entity-position entity)))

(defun orgtrello-webadmin/--delete-entity-file! (entity-file-name)
  "Given an entity, retrieve its full path name and delete it"
  (-> entity-file-name
    orgtrello-webadmin/--compute-filename-from-entity
    orgtrello-action/delete-file!))

(defun orgtrello-webadmin/--delete-entity-with-id (id) "Remove the entity/file which match the id id."
  (-if-let (entity-to-delete (->> *ORGTRELLO-LEVELS*
                                  orgtrello-webadmin/--list-entities
                                  (--filter (string= id (orgtrello-data/entity-id it)))
                                  first))
      (orgtrello-webadmin/--delete-entity-file! entity-to-delete)))

(defun orgtrello-webadmin/delete-entities! () "Remove the entities/files."
  (->> *ORGTRELLO-LEVELS*
       orgtrello-webadmin/--list-entities
       (--map (orgtrello-webadmin/--delete-entity-file! it))))

(defun orgtrello-webadmin/elnode-delete-entity (http-con) "Deal with actions to do on 'action' (entities)."
  (let ((id (elnode-http-mapping http-con 1)))
    (if (string= "" id) (orgtrello-webadmin/delete-entities!) (orgtrello-webadmin/--delete-entity-with-id id))))

(defvar *ORGTRELLO-QUERY-APP-ROUTES-WEBADMIN*
  '(("^localhost//proxy/admin/entities/current/\\(.*\\)" . orgtrello-webadmin/elnode-current-entity)
    ("^localhost//proxy/admin/entities/next/\\(.*\\)" . orgtrello-webadmin/elnode-next-entities)
    ("^localhost//proxy/admin/entities/delete/\\(.*\\)" . orgtrello-webadmin/elnode-delete-entity)
    ("^localhost//proxy/admin/\\(.*\\)" . orgtrello-webadmin/--elnode-admin)
    ;; static files
    ("^localhost//static/\\(.*\\)/\\(.*\\)" . orgtrello-webadmin/elnode-static-file))    ;; proxy to request trello
  "Webadmin routes")

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-webadmin loaded!")


(defvar *ORGTRELLO-SERVER-HOST* "localhost" "proxy host")
(defvar *ORGTRELLO-SERVER-PORT* nil         "proxy port")
(defvar *ORGTRELLO-SERVER-URL*  nil         "proxy url")

(defvar *ORGTRELLO-SERVER-DEFAULT-PORT* 9876 "Default proxy port") (setq *ORGTRELLO-SERVER-PORT* *ORGTRELLO-SERVER-DEFAULT-PORT*)

(defun orgtrello-server/--proxy-handler (http-con)
  "Proxy handler."
  (elnode-hostpath-dispatcher http-con (append *ORGTRELLO-QUERY-APP-ROUTES-WEBADMIN* *ORGTRELLO-QUERY-APP-ROUTES-PROXY*)))

(defun orgtrello-server/--start (port host)
  "Starting the proxy."
  (orgtrello-log/msg *OT/TRACE* "Proxy-server starting...")
  (elnode-start 'orgtrello-server/--proxy-handler :port port :host host)
  (setq elnode--do-error-logging nil)
  (orgtrello-log/msg *OT/TRACE* "Proxy-server started!"))

(defun orgtrello-server/start ()
  "Start the proxy."
  ;; update with the new port the user possibly changed
  (setq *ORGTRELLO-SERVER-URL* (format "http://%s:%d/proxy" *ORGTRELLO-SERVER-HOST* *ORGTRELLO-SERVER-PORT*))
  ;; start the proxy
  (orgtrello-server/--start *ORGTRELLO-SERVER-PORT* *ORGTRELLO-SERVER-HOST*)
  ;; and the timer
  (orgtrello-proxy/timer-start))

(defun orgtrello-server/stop ()
  "Stopping the proxy."
  (orgtrello-log/msg *OT/TRACE* "Proxy-server stopping...")
  ;; stop the timer
  (orgtrello-proxy/timer-stop)
  ;; then stop the proxy
  (elnode-stop *ORGTRELLO-SERVER-PORT*)
  (orgtrello-log/msg *OT/TRACE* "Proxy-server stopped!"))

(defun orgtrello-server/reload ()
  "Reload the proxy server."
  (orgtrello-proxy/stop)
  ;; stop the default port (only useful if the user changed from the default port)
  (elnode-stop *ORGTRELLO-SERVER-DEFAULT-PORT*)
  (orgtrello-proxy/start))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-server loaded!")


(defun orgtrello-buffer/org-entry-put! (point property value)
  (if (or (null value) (string= "" value))
      (orgtrello-buffer/delete-property-from-entry! property)
    (org-entry-put point property value)))

(defun orgtrello-buffer/back-to-card! ()
  "Given the current position, goes on the card's heading"
  (org-back-to-heading))

(defun orgtrello-buffer/--card-description-start-point! ()
  "Compute the first character of the card's description content."
  (save-excursion (orgtrello-buffer/back-to-card!) (1+ (point-at-eol))))

(defun orgtrello-buffer/--card-start-point! ()
  "Compute the first character of the card."
  (save-excursion (orgtrello-buffer/back-to-card!) (point-at-bol)))

(defun orgtrello-buffer/--card-metadata-end-point! ()
  "Compute the first position of the card's next checkbox."
  (save-excursion
    (orgtrello-buffer/back-to-card!)
    (orgtrello-cbx/--goto-next-checkbox)
    (1- (point))))

(defun orgtrello-buffer/extract-description-from-current-position! ()
  "Given the current position, extract the text content of current card."
  (let ((start (orgtrello-buffer/--card-description-start-point!))
        (end   (orgtrello-buffer/--card-metadata-end-point!)))
    (when (< start end)
          (orgtrello-buffer/filter-out-properties
           (buffer-substring-no-properties start end)))))

(defun orgtrello-buffer/get-card-comments! ()
  "Retrieve the card's comments. Can be nil if not on a card."
  (orgtrello-buffer/org-entry-get (point) *ORGTRELLO-CARD-COMMENTS*))

(defun orgtrello-buffer/put-card-comments! (comments)
  "Retrieve the card's comments. Can be nil if not on a card."
  (orgtrello-buffer/org-entry-put! (point) *ORGTRELLO-CARD-COMMENTS* comments))

(defun orgtrello-buffer/filter-out-properties (text-content)
  "Given a string, remove any org properties if any"
  (->> text-content
       (replace-regexp-in-string "^[ ]*:.*" "")
       (s-trim-left)))

(defun orgtrello-buffer/org-file-get-property! (property-key)
  (assoc-default property-key (orgtrello-buffer/org-file-properties!)))

(defun orgtrello-buffer/board-name! ()
  "Compute the board's name"
  (orgtrello-buffer/org-file-get-property! *BOARD-NAME*))

(defun orgtrello-buffer/board-id! ()
  "Compute the board's id"
  (orgtrello-buffer/org-file-get-property! *BOARD-ID*))

(defun orgtrello-buffer/me! ()
  "Compute the board's current user"
  (orgtrello-buffer/org-file-get-property! *ORGTRELLO-USER-ME*))

(defun orgtrello-buffer/labels! ()
  "Compute the board's current labels and return it as an association list."
  (mapcar (lambda (color) `(,color . ,(orgtrello-buffer/org-file-get-property! color))) '(":red" ":blue" ":orange" ":yellow" ":purple" ":green")))

(defun orgtrello-buffer/pop-up-with-content! (title body-content)
  "Compute a temporary buffer *ORGTRELLO-TITLE-BUFFER-INFORMATION* with the title and body-content."
  (with-temp-buffer-window
   *ORGTRELLO-TITLE-BUFFER-INFORMATION* nil nil
   (progn
     (temp-buffer-resize-mode 1)
     (insert (format "%s:\n\n%s" title body-content)))))

(defun orgtrello-buffer/set-property-comment! (comments)
  "Update comments property."
  (orgtrello-buffer/org-entry-put! nil *ORGTRELLO-CARD-COMMENTS* comments))

(defun orgtrello-buffer/compute-card-metadata-region! ()
  "Compute the card region zone (only the card headers + description) couple '(start end)."
  `(,(orgtrello-buffer/--card-start-point!) ,(orgtrello-buffer/--card-metadata-end-point!)))

(defun orgtrello-buffer/compute-checklist-header-region! ()
  "Compute the checklist's region (only the header, without computing the zone occupied by items) couple '(start end)."
  `(,(point-at-bol) ,(1+ (point-at-eol))))

(defun orgtrello-buffer/compute-entity-region! (entity)
  (cond ((orgtrello-data/entity-card-p entity)      (orgtrello-buffer/compute-card-region!))
        ((orgtrello-data/entity-checklist-p entity) (orgtrello-buffer/compute-checklist-region!))
        ((orgtrello-data/entity-item-p entity)      (orgtrello-buffer/compute-item-region!))))

(defun orgtrello-buffer/compute-checklist-region! ()
  "Compute the checklist's region (including the items) couple '(start end)."
  `(,(point-at-bol) ,(orgtrello-cbx/next-checklist-point!)))

(defun orgtrello-buffer/compute-item-region! ()
  "Compute the item region couple '(start end)."
  `(,(point-at-bol) ,(1+ (point-at-eol))))

(defun orgtrello-buffer/compute-card-region! ()
  "Compute the card region zone (only the card headers + description) couple '(start end)."
  `(,(orgtrello-buffer/--card-start-point!) ,(1- (orgtrello-cbx/compute-next-card-point!))))

(defun orgtrello-buffer/write-item! (item-id entities)
  "Write the item to the org buffer."
  (->> entities
       (gethash item-id)
       (orgtrello-buffer/write-entity! item-id)))

(defun orgtrello-buffer/write-checklist-header! (entity-id entity)
  "Write the checklist data and properties without its structure."
  (orgtrello-buffer/write-entity! entity-id entity))

(defun orgtrello-buffer/write-checklist! (checklist-id entities adjacency)
  "Write the checklist and its structure inside the org buffer."
  (orgtrello-buffer/write-checklist-header! checklist-id (gethash checklist-id entities))
  (--map (orgtrello-buffer/write-item! it entities) (gethash checklist-id adjacency)))

(defun orgtrello-buffer/update-member-ids-property! (entity)
  "Update the users assigned property card entry."
  (--> entity
    (orgtrello-data/entity-member-ids it)
    (orgtrello-buffer/--csv-user-ids-to-csv-user-names it *HMAP-USERS-ID-NAME*)
    (replace-regexp-in-string *ORGTRELLO-USER-PREFIX* "" it)
    (orgtrello-buffer/set-usernames-assigned-property! it)))

(defun orgtrello-buffer/update-property-card-comments! (entity)
  "Update last comments "
  (->> entity
    orgtrello-data/entity-comments
    orgtrello-data/comments-to-list
    orgtrello-buffer/set-property-comment!))

(defun orgtrello-buffer/write-card-header! (card-id card)
  "Given a card entity, write its data and properties without its structure."
  (orgtrello-buffer/write-entity! card-id card)
  (orgtrello-buffer/update-member-ids-property! card)
  (orgtrello-buffer/update-property-card-comments! card)
  (-when-let (card-desc (orgtrello-data/entity-description card))
    (insert (format "%s" card-desc))))

(defun orgtrello-buffer/write-card! (card-id card entities adjacency)
  "Write the card and its structure inside the org buffer."
  (orgtrello-buffer/write-card-header! card-id card)
  (-when-let (checklists (gethash card-id adjacency))
    (insert "\n")
    (--map (orgtrello-buffer/write-checklist! it entities adjacency) checklists)))

(defun orgtrello-buffer/write-entity! (entity-id entity)
  "Write the entity in the buffer to the current position. Move the cursor position."
  (orgtrello-log/msg *OT/INFO* "Synchronizing entity '%s' with id '%s'..." (orgtrello-data/entity-name entity) entity-id)
  (insert (orgtrello-buffer/--compute-entity-to-org-entry entity))
  (when entity-id (orgtrello-buffer/--update-property entity-id (not (orgtrello-data/entity-card-p entity)))))

(defun orgtrello-buffer/clean-region! (region)
  "Given a region, remove everything in this region, including text and overlays"
  (apply 'orgtrello-cbx/remove-overlays! region)
  (apply 'delete-region region))

(defun orgtrello-buffer/overwrite-card-header! (card)
  "Given an updated card 'card' and the current position, overwrite the current position with the updated card data."
  (let ((region (orgtrello-buffer/compute-card-metadata-region!)))
    (orgtrello-buffer/clean-region! region)
    (puthash :member-ids (-> card orgtrello-data/entity-member-ids orgtrello-data/--users-to) card)
    (orgtrello-buffer/write-card-header! (orgtrello-data/entity-id card) card)))

(defun orgtrello-buffer/overwrite-checklist-header! (checklist)
  "Given an updated checklist 'checklist' and the current position, overwrite the current position with the updated checklist data."
  (let ((region (orgtrello-buffer/compute-checklist-header-region!)))
    (orgtrello-buffer/clean-region! region)
    (orgtrello-buffer/write-checklist-header! (orgtrello-data/entity-id checklist) checklist)))

(defun orgtrello-buffer/overwrite-item! (item)
  "Given an updated item 'item' and the current position, overwrite the current position with the updated item data."
  (let ((region (orgtrello-buffer/compute-item-region!)))
    (orgtrello-buffer/clean-region! region)
    (orgtrello-buffer/write-entity! (orgtrello-data/entity-id item) (orgtrello-data/merge-item item item)))) ;; hack to merge item to itself to map to the org-trello world, otherwise we lose status for example

(defun orgtrello-buffer/--csv-user-ids-to-csv-user-names (csv-users-id users-id-name)
  "Given a comma separated list of user id and a map, return a comma separated list of username."
  (->> csv-users-id
    orgtrello-data/--users-from
    (--map (gethash it users-id-name))
    orgtrello-data/--users-to))

(defun orgtrello-buffer/--compute-entity-to-org-entry (entity)
  "Given an entity, compute its org representation."
  (funcall
   (cond ((orgtrello-data/entity-card-p entity)      'orgtrello-buffer/--compute-card-to-org-entry)
         ((orgtrello-data/entity-checklist-p entity) 'orgtrello-buffer/--compute-checklist-to-org-entry)
         ((orgtrello-data/entity-item-p entity)      'orgtrello-buffer/--compute-item-to-org-entry))
   entity))

(defun orgtrello-buffer/--compute-due-date (due-date)
  "Compute the format of the due date."
  (if due-date (format "DEADLINE: <%s>\n" due-date) ""))

(defun orgtrello-buffer/--private-compute-card-to-org-entry (name status due-date tags)
  "Compute the org format for card."
  (let ((prefix-string (format "* %s %s" (if status status *TODO*) name)))
    (format "%s%s\n%s" prefix-string (orgtrello-buffer/--serialize-tags prefix-string tags) (orgtrello-buffer/--compute-due-date due-date))))

(defun orgtrello-buffer/--serialize-tags (prefix-string tags)
  "Compute the tags serialization string. If tags is empty, return \"\", otherwise, if prefix-string's length is superior to 72, only  "
  (if (or (null tags) (string= "" tags))
      ""
    (let ((l (length prefix-string)))
      (format "%s%s" (if (< 72 l) " " (orgtrello-buffer/--symbol " " (- 72 l))) tags))))

(defun orgtrello-buffer/--compute-card-to-org-entry (card)
  "Given a card, compute its org-mode entry equivalence. orgcheckbox-p is nil"
  (orgtrello-buffer/--private-compute-card-to-org-entry
   (orgtrello-data/entity-name card)
   (orgtrello-data/entity-keyword card)
   (orgtrello-data/entity-due card)
   (orgtrello-data/entity-tags card)))

(defun orgtrello-buffer/--compute-checklist-to-orgtrello-entry (name &optional level status)
  "Compute the orgtrello format checklist"
  (format "** %s\n" name))

(defun orgtrello-buffer/--symbol (sym n)
  "Compute the repetition of a symbol as a string"
  (--> n
       (-repeat it sym)
       (s-join "" it)))

(defun orgtrello-buffer/--space (n)
  "Given a level, compute the number of space for an org checkbox entry."
  (orgtrello-buffer/--symbol " "  n))

(defun orgtrello-buffer/--star (n)
  "Given a level, compute the number of space for an org checkbox entry."
  (orgtrello-buffer/--symbol "*"  n))

(defun orgtrello-buffer/--compute-state-checkbox (state)
  "Compute the status of the checkbox"
  (orgtrello-data/--compute-state-generic state '("[X]" "[-]")))

(defun orgtrello-buffer/--compute-level-into-spaces (level)
  "level 2 is 0 space, otherwise 2 spaces."
  (if (equal level *CHECKLIST-LEVEL*) 0 2))

(defun orgtrello-buffer/--compute-checklist-to-org-checkbox (name &optional level status)
  "Compute checklist to the org checkbox format"
  (format "%s- %s %s\n"
          (-> level
              orgtrello-buffer/--compute-level-into-spaces
              orgtrello-buffer/--space)
          (orgtrello-buffer/--compute-state-checkbox status)
          name))

(defun orgtrello-buffer/--compute-item-to-org-checkbox (name &optional level status)
  "Compute item to the org checkbox format"
  (format "%s- %s %s\n"
          (-> level
              orgtrello-buffer/--compute-level-into-spaces
              orgtrello-buffer/--space)
          (orgtrello-data/--compute-state-item-checkbox status)
          name))

(defun orgtrello-buffer/--compute-checklist-to-org-entry (checklist &optional orgcheckbox-p)
  "Given a checklist, compute its org-mode entry equivalence."
  (orgtrello-buffer/--compute-checklist-to-org-checkbox (orgtrello-data/entity-name checklist) *CHECKLIST-LEVEL* "incomplete"))

(defun orgtrello-buffer/--compute-item-to-org-entry (item)
  "Given a checklist item, compute its org-mode entry equivalence."
  (orgtrello-buffer/--compute-item-to-org-checkbox (orgtrello-data/entity-name item) *ITEM-LEVEL* (orgtrello-data/entity-keyword item)))

(defun orgtrello-buffer/--put-card-with-adjacency (current-meta entities adjacency)
  "Deal with adding card to entities."
  (-> current-meta
      (orgtrello-buffer/--put-entities entities)
      (list adjacency)))

(defun orgtrello-buffer/--dispatch-create-entities-map-with-adjacency (entity)
  "Dispatch the function to update map depending on the entity level."
  (if (orgtrello-data/entity-card-p entity) 'orgtrello-buffer/--put-card-with-adjacency 'orgtrello-backend/--put-entities-with-adjacency))

(defun orgtrello-buffer/--compute-entities-from-org! (&optional region-end)
  "Compute the full entities present in the org buffer which already had been sync'ed previously. Return the list of entities map and adjacency map in this order. If region-end is specified, will work on the region (current-point, region-end), otherwise, work on all buffer."
  (let ((entities (orgtrello-hash/empty-hash))
        (adjacency (orgtrello-hash/empty-hash)))
    (orgtrello-buffer/org-map-entities-without-params!
     (lambda ()
       ;; either the region-end is null, so we work on all the buffer, or the region-end is specified and we need to filter out entities that are after the specified point.
       (when (or (null region-end) (< (point) region-end))
         ;; first will unfold every entries, otherwise https://github.com/org-trello/org-trello/issues/53
         (org-show-subtree)
         (let ((current-entity (-> (orgtrello-buffer/entry-get-full-metadata!) orgtrello-data/current)))
           (unless (-> current-entity orgtrello-data/entity-id orgtrello-data/id-p) ;; if no id, we set one
             (orgtrello-buffer/--set-marker (orgtrello-buffer/--compute-marker-from-entry current-entity)))
           (let ((current-meta (orgtrello-buffer/entry-get-full-metadata!)))
             (-> current-meta ;; we recompute the metadata because they may have changed
               orgtrello-data/current
               orgtrello-buffer/--dispatch-create-entities-map-with-adjacency
               (funcall current-meta entities adjacency)))))))
    (list entities adjacency)))

;; entities of the form: {entity-id '(entity-card {checklist-id (checklist (item))})}

(defun orgtrello-buffer/compute-entities-from-org-buffer! (&optional buffername region-start region-end)
  "Compute the current entities hash from the buffer in the same format as the sync-from-trello routine. Return the list of entities map and adjacency map in this order."
  (when buffername
    (set-buffer buffername))
  (save-excursion
    (goto-char (if region-start region-start (point-min))) ;; start from start-region if specified, otherwise, start from the start of the file
    (orgtrello-buffer/--compute-entities-from-org! region-end)))

(defun orgtrello-buffer/--put-entities (current-meta entities)
  "Deal with adding a new item to entities."
  (-> current-meta
      orgtrello-data/current
      (orgtrello-backend/--add-entity-to-entities entities)))

(defun orgtrello-buffer/--update-property (id orgcheckbox-p)
  "Update the property depending on the nature of thing to sync. Move the cursor position."
  (if orgcheckbox-p
      (save-excursion
        (forward-line -1) ;; need to get back one line backward for the checkboxes as their properties is at the same level (otherwise, for headings we do not care)
        (orgtrello-buffer/set-property *ORGTRELLO-ID* id))
      (orgtrello-buffer/set-property *ORGTRELLO-ID* id)))

(defun orgtrello-buffer/--set-marker (marker)
  "Set a marker to get back to later."
  (orgtrello-buffer/set-property *ORGTRELLO-ID* marker))

(defun orgtrello-buffer/set-marker-if-not-present (current-entity marker)
  "Set the marker to the entry if we never did."
  (unless (string= (orgtrello-data/entity-id current-entity) marker) ;; if never created before, we need a marker to add inside the file
    (orgtrello-buffer/--set-marker marker)))

(defun orgtrello-buffer/org-map-entities-without-params! (fn-to-execute)
  "Execute fn-to-execute function for all entities from buffer - fn-to-execute is a function without any parameters."
  (org-map-entries
   (lambda ()
     (funcall fn-to-execute) ;; execute on heading entry
     (orgtrello-cbx/map-checkboxes fn-to-execute)) t 'file))

(defun orgtrello-buffer/get-usernames-assigned-property! ()
  "Read the org users property from the current entry."
  (org-entry-get nil *ORGTRELLO-USERS-ENTRY*))

(defun orgtrello-buffer/set-usernames-assigned-property! (csv-users)
  "Update users org property."
  (orgtrello-buffer/org-entry-put! nil *ORGTRELLO-USERS-ENTRY* csv-users))

(defun orgtrello-buffer/delete-property-from-entry! (property)
  "Delete a property from the org buffer."
  (org-delete-property property))

(defun orgtrello-buffer/delete-property! (property)
  "Given a property name (checkbox), if found, delete it from the buffer."
  (orgtrello-buffer/delete-property-from-entry! property)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":PROPERTIES: {.*" nil t)
      (remove-overlays (point-at-bol) (point-at-eol)) ;; the current overlay on this line
      (replace-match "" nil t))))                     ;; then remove the property

(defun orgtrello-buffer/remove-overlays! ()
  "Remove every org-trello overlays from the current buffer."
  (orgtrello-cbx/remove-overlays! (point-min) (point-max)))

(defun orgtrello-buffer/install-overlays! ()
  "Install overlays throughout the all buffers."
  (orgtrello-buffer/remove-overlays!)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":PROPERTIES: {.*" nil t)
      (orgtrello-cbx/install-overlays! (match-beginning 0)))))


(defun orgtrello-buffer/--convert-orgmode-date-to-trello-date (orgmode-date)
  "Convert the org-mode deadline into a time adapted for trello."
  (if (and orgmode-date (not (string-match-p "T*Z" orgmode-date)))
      (cl-destructuring-bind (sec min hour day mon year dow dst tz)
                             (--map (if it (if (< it 10) (concat "0" (int-to-string it)) (int-to-string it)))
                                    (parse-time-string orgmode-date))
        (concat (concat year "-" mon "-" day "T") (if hour (concat hour ":" min ":" sec) "00:00:00") ".000Z"))
      orgmode-date))

(defun orgtrello-buffer/org-entity-metadata! ()
  "Compute the metadata the org-mode way."
  (org-heading-components))

(defun orgtrello-buffer/--extract-metadata! ()
  "Extract the current metadata depending on the org-trello's checklist policy."
  (funcall (if (orgtrello-cbx/checkbox-p) 'orgtrello-cbx/org-checkbox-metadata! 'orgtrello-buffer/org-entity-metadata!)))

(defun orgtrello-buffer/extract-identifier! (point)
  "Extract the identifier from the point."
  (orgtrello-buffer/org-entry-get point *ORGTRELLO-ID*))

(defun orgtrello-buffer/set-property (key value)
  "Either set the propery normally (as for entities) or specifically for checklist."
  (funcall (if (orgtrello-cbx/checkbox-p) 'orgtrello-cbx/org-set-property 'org-set-property) key value))

(defun orgtrello-buffer/org-entry-get (point key)
  "Extract the identifier from the point."
  (funcall (if (orgtrello-cbx/checkbox-p) 'orgtrello-cbx/org-get-property 'org-entry-get) point key))

(defun orgtrello-buffer/metadata! ()
  "Compute the metadata for a given org entry. Also add some metadata identifier/due-data/point/buffer-name/etc..."
  (let ((current-point (point)))
    (->> (orgtrello-buffer/--extract-metadata!)
         (cons (-> current-point (orgtrello-buffer/org-entry-get "DEADLINE") orgtrello-buffer/--convert-orgmode-date-to-trello-date))
         (cons (orgtrello-buffer/extract-identifier! current-point))
         (cons current-point)
         (cons (buffer-name))
         (cons (orgtrello-buffer/--user-ids-assigned-to-current-card))
         (cons (orgtrello-buffer/extract-description-from-current-position!))
         (cons (orgtrello-buffer/org-entry-get current-point *ORGTRELLO-CARD-COMMENTS*))
         orgtrello-buffer/--to-orgtrello-metadata)))

(defun orgtrello-buffer/org-up-parent! ()
  "A function to get back to the current entry's parent"
  (funcall (if (orgtrello-cbx/checkbox-p) 'orgtrello-cbx/org-up! 'org-up-heading-safe)))

(defun orgtrello-buffer/--parent-metadata! ()
  "Extract the metadata from the current heading's parent."
  (save-excursion
    (orgtrello-buffer/org-up-parent!)
    (orgtrello-buffer/metadata!)))

(defun orgtrello-buffer/--grandparent-metadata! ()
  "Extract the metadata from the current heading's grandparent."
  (save-excursion
    (orgtrello-buffer/org-up-parent!)
    (orgtrello-buffer/org-up-parent!)
    (orgtrello-buffer/metadata!)))

(defun orgtrello-buffer/entry-get-full-metadata! ()
  "Compute metadata needed for entry into a map with keys :current, :parent, :grandparent. Returns nil if the level is superior to 4."
  (let* ((current   (orgtrello-buffer/metadata!))
         (level     (orgtrello-data/entity-level current)))
    (when (< level *OUTOFBOUNDS-LEVEL*)
          (let ((ancestors (cond ((= level *CARD-LEVEL*)      '(nil nil))
                                 ((= level *CHECKLIST-LEVEL*) `(,(orgtrello-buffer/--parent-metadata!) nil))
                                 ((= level *ITEM-LEVEL*)      `(,(orgtrello-buffer/--parent-metadata!) ,(orgtrello-buffer/--grandparent-metadata!))))))
            (orgtrello-hash/make-hierarchy current (first ancestors) (second ancestors))))))

(defun orgtrello-buffer/--to-orgtrello-metadata (heading-metadata)
  "Given the heading-metadata returned by the function 'org-heading-components, make it a hashmap with key :level, :keyword, :name. and their respective value"
  (cl-destructuring-bind (comments description member-ids buffer-name point id due level _ keyword _ name tags) heading-metadata
                         (orgtrello-hash/make-hash-org member-ids level keyword name id due point buffer-name description comments tags)))

(defun orgtrello-buffer/current-level! ()
  "Compute the current level's position."
  (-> (orgtrello-buffer/metadata!) orgtrello-data/entity-level))

(defun orgtrello-buffer/filtered-kwds! ()
  "org keywords used (based on org-todo-keywords-1)."
  org-todo-keywords-1)

(defun orgtrello-buffer/org-file-properties! ()
  org-file-properties)

(defun orgtrello-buffer/org-map-entries (level fn-to-execute)
  "Map fn-to-execute to a given entities with level level. fn-to-execute is a function without any parameter."
  (org-map-entries (lambda () (when (= level (orgtrello-buffer/current-level!)) (funcall fn-to-execute)))))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-buffer loaded!")


(defun orgtrello-input/read-not-empty! (input-message)
  "Function dedicated to continue asking for input while the input typed is incorrect."
  (let ((value nil))
    (while (or (null value) (string= "" value))
      (setq value (read-string input-message)))
    value))

;; (orgtrello-input/read-not-empty! "some-value? ")

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-input loaded!")


;; Specific state - FIXME check if they do not already exist on org-mode to avoid potential collisions
(defconst *TODO* "TODO" "org-mode todo state")
(defconst *DONE* "DONE" "org-mode done state")

;; Properties key for the orgtrello headers #+PROPERTY board-id, etc...
(defconst *BOARD-ID*   "board-id" "orgtrello property board-id entry")
(defconst *BOARD-NAME* "board-name" "orgtrello property board-name entry")

(defvar *LIST-NAMES*         nil "orgtrello property names of the different lists. This use the standard 'org-todo-keywords property from org-mode.")
(defvar *HMAP-ID-NAME*       nil "orgtrello hash map containing for each id, the associated name (or org keyword).")
(defvar *HMAP-USERS-ID-NAME* nil "orgtrello hash map containing for each user name, the associated id.")
(defvar *HMAP-USERS-NAME-ID* nil "orgtrello hash map containing for each user id, the associated name.")

(defconst *CONFIG-DIR*  (concat (getenv "HOME") "/" ".trello"))
(defconst *CONFIG-FILE* (concat *CONFIG-DIR* "/config.el"))

(defun orgtrello-controller/compute-marker (buffer-name name position)
  "Compute the orgtrello marker which is composed of buffer-name, name and position"
  (->> (list *ORGTRELLO-MARKER* buffer-name name (if (stringp position) position (int-to-string position)))
       (-interpose "-")
       (apply 'concat)
       sha1
       (concat *ORGTRELLO-MARKER* "-")))

(defun orgtrello-controller/--list-user-entries (properties)
  "List the users entries."
  (--filter (string-match-p *ORGTRELLO-USER-PREFIX* (car it)) properties))

(defun orgtrello-controller/setup-properties (&optional args)
  "Setup the properties according to the org-mode setup. Return :ok."
  ;; read the setup
  (orgtrello-action/reload-setup)
  ;; now exploit some
  (let* ((list-keywords (reverse (orgtrello-buffer/filtered-kwds!)))
         (hmap-id-name (--reduce-from (progn
                                        (puthash (orgtrello-buffer/org-file-get-property! it) it acc)
                                        acc)
                                      (orgtrello-hash/empty-hash)
                                      list-keywords))
         (list-users (orgtrello-controller/--list-user-entries (orgtrello-buffer/org-file-properties!)))
         (hmap-user-id-name (orgtrello-hash/make-transpose-properties list-users))
         (hmap-user-name-id (orgtrello-hash/make-properties list-users)))
    (setq *LIST-NAMES*   list-keywords)
    (setq *HMAP-ID-NAME* hmap-id-name)
    (setq *HMAP-USERS-ID-NAME* hmap-user-id-name)
    (setq *HMAP-USERS-NAME-ID* hmap-user-name-id)
    (setq *ORGTRELLO-USER-LOGGED-IN* (orgtrello-buffer/me!))
    (add-to-list 'org-tag-alist '("red" . ?r))
    (add-to-list 'org-tag-alist '("green" . ?g))
    (add-to-list 'org-tag-alist '("yellow" . ?y))
    (add-to-list 'org-tag-alist '("blue" . ?b))
    (add-to-list 'org-tag-alist '("purple" . ?p))
    (add-to-list 'org-tag-alist '("orange" . ?o))
    :ok))

(defun orgtrello-controller/control-encoding (&optional args)
  "Use utf-8, otherwise, there will be trouble."
  (progn
    (orgtrello-log/msg *OT/ERROR* "Ensure you use utf-8 encoding for your org buffer.")
    :ok))

(defun orgtrello-controller/control-properties (&optional args)
  "org-trello needs the properties board-id and all list id from the trello board to be setuped on header property file. :ok if ok, or the error message if problems."
  (let ((hmap-count (hash-table-count *HMAP-ID-NAME*)))
    (if (and (orgtrello-buffer/org-file-properties!) (orgtrello-buffer/board-id!) (= (length *LIST-NAMES*) hmap-count))
        :ok
        "Setup problem.\nEither you did not connect your org-mode buffer with a trello board, to correct this:\n  * attach to a board through C-c o I or M-x org-trello/install-board-and-lists-ids\n  * or create a board from scratch with C-c o b or M-x org-trello/create-board).\nEither your org-mode's todo keyword list and your trello board lists are not named the same way (which they must).\nFor this, connect to trello and rename your board's list according to your org-mode's todo list.\nAlso, you can specify on your org-mode buffer the todo list you want to work with, for example: #+TODO: TODO DOING | DONE FAIL (hit C-c C-c to refresh the setup)")))

(defun orgtrello-controller/control-keys (&optional args)
  "org-trello needs the *consumer-key* and the *access-token* to access the trello resources. Returns :ok if everything is ok, or the error message if problems."
  (if (or (and *consumer-key* *access-token*)
          ;; the data are not set,
          (and (file-exists-p *CONFIG-FILE*)
               ;; trying to load them
               (load *CONFIG-FILE*)
               ;; still not loaded, something is not right!
               (and *consumer-key* *access-token*)))
      :ok
    "Setup problem - You need to install the consumer-key and the read/write access-token - C-c o i or M-x org-trello/install-board-and-lists-ids"))

(defun orgtrello-controller/--retrieve-state-of-card (card-meta)
  "Given a card, retrieve its state depending on its :keyword metadata. If empty or no keyword then, its equivalence is *TODO*, otherwise, return its current state."
  (-if-let (card-kwd (orgtrello-data/entity-keyword card-meta *TODO*)) card-kwd *TODO*))

(defun orgtrello-controller/--checks-before-sync-card (card-meta)
  "Checks done before synchronizing the cards."
  (if (orgtrello-data/entity-name card-meta) :ok *ERROR-SYNC-CARD-MISSING-NAME*))

(defun orgtrello-controller/--tags-to-labels (str)
  "Transform org tags string to csv labels."
  (when str
    (let* ((s (s-split ":" str))
           (ns (if (string= "" (car s)) (cdr s) s)))
      (s-join "," ns))))

(defun orgtrello-controller/--card (card-meta &optional parent-meta grandparent-meta)
  "Deal with create/update card query build. If the checks are ko, the error message is returned."
  (let ((checks-ok-or-error-message (orgtrello-controller/--checks-before-sync-card card-meta)))
    ;; name is mandatory
    (if (equal :ok checks-ok-or-error-message)
        ;; parent and grandparent are useless here
        (let* ((card-kwd                (orgtrello-controller/--retrieve-state-of-card card-meta))
               (list-id                 (orgtrello-buffer/org-file-get-property! card-kwd))
               (card-id                 (orgtrello-data/entity-id          card-meta))
               (card-name               (orgtrello-data/entity-name        card-meta))
               (card-due                (orgtrello-data/entity-due         card-meta))
               (card-desc               (orgtrello-data/entity-description card-meta))
               (card-user-ids-assigned  (orgtrello-data/entity-member-ids  card-meta))
               (card-labels             (orgtrello-controller/--tags-to-labels (orgtrello-data/entity-tags card-meta))))
          (if card-id
              ;; update
              (orgtrello-api/move-card card-id list-id card-name card-due card-user-ids-assigned card-desc card-labels)
            ;; create
            (orgtrello-api/add-card card-name list-id card-due card-user-ids-assigned card-desc card-labels)))
      checks-ok-or-error-message)))

(defun orgtrello-controller/--checks-before-sync-checklist (checklist-meta card-meta)
  "Checks done before synchronizing the checklist."
  (-if-let (checklist-name (orgtrello-data/entity-name checklist-meta))
      (-if-let (card-id (orgtrello-data/entity-id card-meta))
          :ok
        *ERROR-SYNC-CHECKLIST-SYNC-CARD-FIRST*)
    *ERROR-SYNC-CHECKLIST-MISSING-NAME*))

(defun orgtrello-controller/--checklist (checklist-meta &optional card-meta grandparent-meta)
  "Deal with create/update checklist query build. If the checks are ko, the error message is returned."
  (let ((checks-ok-or-error-message (orgtrello-controller/--checks-before-sync-checklist checklist-meta card-meta)))
    ;; name is mandatory
    (if (equal :ok checks-ok-or-error-message)
        ;; grandparent is useless here
        (let ((card-id        (orgtrello-data/entity-id card-meta))
              (checklist-name (orgtrello-data/entity-name checklist-meta)))
          (-if-let (checklist-id (orgtrello-data/entity-id checklist-meta))
              ;; update
              (orgtrello-api/update-checklist checklist-id checklist-name)
            ;; create
            (orgtrello-api/add-checklist card-id checklist-name)))
      checks-ok-or-error-message)))

(defun orgtrello-controller/--checks-before-sync-item (item-meta checklist-meta card-meta)
  "Checks done before synchronizing the checklist."
  (let ((orgtrello-controller/--item-name    (orgtrello-data/entity-name item-meta))
        (orgtrello-controller/--checklist-id (orgtrello-data/entity-id checklist-meta))
        (orgtrello-controller/--card-id      (orgtrello-data/entity-id card-meta)))
    (if orgtrello-controller/--item-name
        (if orgtrello-controller/--checklist-id
            (if orgtrello-controller/--card-id :ok *ERROR-SYNC-ITEM-SYNC-CARD-FIRST*)
          *ERROR-SYNC-ITEM-SYNC-CHECKLIST-FIRST*)
      *ERROR-SYNC-ITEM-MISSING-NAME*)))

(defun orgtrello-controller/--compute-state-from-keyword (state)
  "Given a state, compute the org equivalent (to use with org-todo function)"
  (if (string= *DONE* state) 'done 'none))

(defun orgtrello-controller/compute-state (state)
  "Given a state (TODO/DONE) compute the trello state equivalent."
  (orgtrello-data/--compute-state-generic state '("complete" "incomplete")))

(defun orgtrello-controller/compute-check (state)
  "Given a state (TODO/DONE) compute the trello check equivalent."
  (orgtrello-data/--compute-state-generic state '(t nil)))

(defun orgtrello-controller/--item (item-meta &optional checklist-meta card-meta)
  "Deal with create/update item query build. If the checks are ko, the error message is returned."
  (let ((checks-ok-or-error-message (orgtrello-controller/--checks-before-sync-item item-meta checklist-meta card-meta)))
    ;; name is mandatory
    (if (equal :ok checks-ok-or-error-message)
        ;; card-meta is only usefull for the update part
        (let* ((orgtrello-controller/--item-id      (orgtrello-data/entity-id item-meta))
               (orgtrello-controller/--checklist-id (orgtrello-data/entity-id checklist-meta))
               (orgtrello-controller/--card-id      (orgtrello-data/entity-id card-meta))
               (orgtrello-controller/--item-name    (orgtrello-data/entity-name item-meta))
               (orgtrello-controller/--item-state   (orgtrello-data/entity-keyword item-meta))
               (orgtrello-controller/--checklist-state    (orgtrello-data/entity-keyword checklist-meta)))

          ;; update/create items
          (if orgtrello-controller/--item-id
              ;; update - rename, check or uncheck the item
              (orgtrello-api/update-item orgtrello-controller/--card-id
                                         orgtrello-controller/--checklist-id
                                         orgtrello-controller/--item-id
                                         orgtrello-controller/--item-name
                                         (orgtrello-controller/compute-state orgtrello-controller/--item-state))
              ;; create
              (orgtrello-api/add-items orgtrello-controller/--checklist-id
                                       orgtrello-controller/--item-name
                                       (orgtrello-controller/compute-check orgtrello-controller/--item-state))))
      checks-ok-or-error-message)))

(defun orgtrello-controller/--too-deep-level (meta &optional parent-meta grandparent-meta)
  "Deal with too deep level."
  "Your arborescence depth is too deep. We only support up to depth 3.\nLevel 1 - card\nLevel 2 - checklist\nLevel 3 - items")

(defvar *MAP-DISPATCH-CREATE-UPDATE* (orgtrello-hash/make-properties `((,*CARD-LEVEL*      . orgtrello-controller/--card)
                                                                       (,*CHECKLIST-LEVEL* . orgtrello-controller/--checklist)
                                                                       (,*ITEM-LEVEL*      . orgtrello-controller/--item))) "Dispatch map for the creation/update of card/checklist/item.")

(defun orgtrello-controller/--dispatch-create (entry-metadata)
  "Dispatch the creation depending on the nature of the entry."
  (let ((current-meta        (orgtrello-data/current entry-metadata)))
    (-> current-meta
        orgtrello-data/entity-level
        (gethash *MAP-DISPATCH-CREATE-UPDATE* 'orgtrello-controller/--too-deep-level)
        (funcall current-meta (orgtrello-data/parent entry-metadata) (orgtrello-data/grandparent entry-metadata)))))

(defun orgtrello-controller/--update-query-with-org-metadata (query-map position buffer-name &optional name success-callback sync)
  "Given a trello query, add proxy metadata needed to work."
  (puthash :position     position                                                       query-map)
  (puthash :buffername   buffer-name                                                    query-map)
  (when success-callback (puthash :callback success-callback query-map))
  (when sync             (puthash :sync     sync             query-map))
  (when name             (puthash :name     name             query-map))
  query-map)

(defun orgtrello-buffer/--compute-marker-from-entry (entry)
  "Compute and set the marker (either a sha1 or the id of the entry-metadata)."
  (-if-let (current-entry-id (orgtrello-data/entity-id entry))
      current-entry-id
    (orgtrello-controller/compute-marker (orgtrello-data/entity-buffername entry) (orgtrello-data/entity-name entry) (orgtrello-data/entity-position entry))))

(defun orgtrello-controller/--right-level-p (entity)
  "Compute if the level is correct (not higher than level 4)."
  (if (< (-> entity orgtrello-data/current orgtrello-data/entity-level) *OUTOFBOUNDS-LEVEL*) :ok "Level too high. Do not deal with entity other than card/checklist/items!"))

(defun orgtrello-controller/--already-synced-p (entity)
  "Compute if the entity has already been synchronized."
  (if (-> entity orgtrello-data/current orgtrello-data/entity-id) :ok "Entity must been synchronized with trello first!"))

(defun orgtrello-controller/--mandatory-name-ok-p (entity)
  "Ensure entity can be synced regarding the mandatory data."
  (let* ((current (orgtrello-data/current entity))
         (level   (orgtrello-data/entity-level current))
         (name    (orgtrello-data/entity-name current)))
    (if (and name (< 0 (length name)))
        :ok
        (cond ((= level *CARD-LEVEL*)      *ERROR-SYNC-CARD-MISSING-NAME*)
              ((= level *CHECKLIST-LEVEL*) *ERROR-SYNC-CHECKLIST-MISSING-NAME*)
              ((= level *ITEM-LEVEL*)      *ERROR-SYNC-ITEM-MISSING-NAME*)))))

(defun orgtrello-controller/--delegate-to-the-proxy (full-meta action)
  "Execute the delegation to the consumer."
  (let* ((current (orgtrello-data/current full-meta))
         (marker  (orgtrello-buffer/--compute-marker-from-entry current)))
    (orgtrello-buffer/set-marker-if-not-present current marker)
    (puthash :id      marker current)
    (puthash :action  action current)
    (orgtrello-proxy/http-producer current)))

(defun orgtrello-controller/--checks-then-delegate-action-on-entity-to-proxy (functional-controls action)
  "Execute the functional controls then if all pass, delegate the action 'action' to the proxy."
  (orgtrello-action/functional-controls-then-do functional-controls (orgtrello-buffer/entry-get-full-metadata!) 'orgtrello-controller/--delegate-to-the-proxy action))

(defun orgtrello-controller/do-delete-simple (&optional sync)
  "Do the deletion of an entity."
  (orgtrello-controller/--checks-then-delegate-action-on-entity-to-proxy '(orgtrello-controller/--right-level-p orgtrello-controller/--already-synced-p) *ORGTRELLO-ACTION-DELETE*))

(defun orgtrello-controller/do-sync-entity-to-trello! ()
  "Do the entity synchronization (if never synchronized, will create it, update it otherwise)."
  (orgtrello-controller/--checks-then-delegate-action-on-entity-to-proxy '(orgtrello-controller/--right-level-p orgtrello-controller/--mandatory-name-ok-p) *ORGTRELLO-ACTION-SYNC*))

(defun orgtrello-controller/do-sync-full-entity-to-trello! ()
  "Do the actual full card creation - from card to item. Beware full side effects..."
  (orgtrello-log/msg *OT/INFO* "Synchronizing full entity with its structure on board '%s'..." (orgtrello-buffer/board-name!))
  ;; in any case, we need to show the subtree, otherwise https://github.com/org-trello/org-trello/issues/53
  (org-show-subtree)
  (if (org-at-heading-p)
      (org-map-tree (lambda () (orgtrello-controller/do-sync-entity-to-trello!) (orgtrello-controller/map-sync-checkboxes)))
      (orgtrello-controller/map-sync-checkboxes)))

(defun orgtrello-controller/map-sync-checkboxes ()
  "Map the sync to checkboxes."
  (orgtrello-cbx/map-checkboxes 'orgtrello-controller/do-sync-entity-to-trello!))

(defun orgtrello-controller/do-sync-full-file-to-trello! ()
  "Full org-mode file synchronisation."
  (orgtrello-log/msg *OT/WARN* "Synchronizing org-mode file to the board '%s'. This may take some time, some coffee may be a good idea..." (orgtrello-buffer/board-name!))
  (orgtrello-buffer/org-map-entries *CARD-LEVEL* 'orgtrello-controller/do-sync-full-entity-to-trello!))

(defun orgtrello-controller/--sync-buffer-with-trello-data (data buffer-name)
  "Given all the entities, update the current buffer with those."
  (let ((entities (first data))
        (adjacency (second data)))
    (with-current-buffer buffer-name
      (goto-char (point-max)) ;; go at the end of the file
      (maphash
       (lambda (new-id entity)
         (when (orgtrello-data/entity-card-p entity)
               (orgtrello-buffer/write-card! new-id entity entities adjacency)))
       entities)
      (goto-char (point-min)) ;; go back to the beginning of file
      (org-sort-entries t ?o) ;; sort the entries on their keywords
      (save-buffer))))

(defun orgtrello-controller/--cleanup-org-entries ()
  "Cleanup org-entries from the buffer (FIXME find a suiter way of merging data than removing them all and put them back)."
  (goto-char (point-min))
  (outline-next-heading)
  (orgtrello-cbx/remove-overlays! (point-at-bol) (point-max))
  (kill-region (point-at-bol) (point-max)))

(defun orgtrello-controller/--sync-buffer-with-trello-data-callback (buffername &optional position name)
  "Generate a callback which knows the buffer with which it must work. (this callback must take a buffer-name and a position)"
  (lexical-let ((buffer-name              buffername)
                (entities-from-org-buffer (orgtrello-buffer/compute-entities-from-org-buffer! buffername)))
    (function* (lambda (&key data &allow-other-keys) "Synchronize the buffer with the response data."
       (orgtrello-log/msg *OT/TRACE* "proxy - response data: %S" data)
       (-> data                                                                 ;; compute merge between already sync'ed entries and the trello data
           orgtrello-backend/compute-full-cards-from-trello!                        ;; slow computation with network access
           (orgtrello-data/merge-entities-trello-and-org entities-from-org-buffer) ;; slow merge computation
           ((lambda (entry) (orgtrello-controller/--cleanup-org-entries) entry))           ;; hack to clean the org entries just before synchronizing the buffer
           (orgtrello-controller/--sync-buffer-with-trello-data buffer-name)
           (orgtrello-action/safe-wrap (orgtrello-log/msg *OT/INFO* "Synchronizing the trello and org data merge - done!")))))))

(defun orgtrello-controller/do-sync-full-file-from-trello! (&optional sync)
  "Full org-mode file synchronisation. Beware, this will block emacs as the request is synchronous."
  (orgtrello-log/msg *OT/INFO* "Synchronizing the trello board '%s' to the org-mode file. This may take a moment, some coffee may be a good idea..." (orgtrello-buffer/board-name!))
  ;; then start the sync computations
  (--> (orgtrello-buffer/board-id!)
       (orgtrello-api/get-cards it)
       (orgtrello-controller/--update-query-with-org-metadata it nil (buffer-name) nil 'orgtrello-controller/--sync-buffer-with-trello-data-callback)
       (orgtrello-proxy/http it sync)))

(defun orgtrello-controller/--sync-entity-to-buffer-with-trello-data-callback (buffername &optional position name)
  "Generate a callback which knows the buffer with which it must work. (this callback must take a buffer-name and a position)"
  (lexical-let ((buffer-name buffername)
                (pos         position))
    (function* (lambda (&key data &allow-other-keys) "Synchronize the buffer with the response data."
       (orgtrello-log/msg *OT/TRACE* "proxy - response data: %S" data)
       (orgtrello-action/safe-wrap
        (save-excursion
          (goto-char pos)
          (point-at-bol)
          (org-show-subtree)
          (funcall
           (cond ((orgtrello-data/entity-card-p data)      'orgtrello-buffer/overwrite-card-header!)
                 ((orgtrello-data/entity-checklist-p data) 'orgtrello-buffer/overwrite-checklist-header!)
                 ((orgtrello-data/entity-item-p data)      'orgtrello-buffer/overwrite-item!))
           data)
          (save-buffer))
        (orgtrello-log/msg *OT/INFO* "Synchronizing the trello and org data merge - done!"))))))

(defun orgtrello-controller/fetch-and-overwrite-card! (card)
  "Given a card, retrieve latest information from trello and overwrite in current buffer."
  (let* ((card-id                  (orgtrello-data/entity-id card))
         (region                   (orgtrello-buffer/compute-entity-region! card))
         (entities-from-org-buffer (apply 'orgtrello-buffer/compute-entities-from-org-buffer! (cons nil region)))
         (entities-from-trello     (orgtrello-backend/compute-full-cards-from-trello! (list card)))
         (merged-entities          (orgtrello-data/merge-entities-trello-and-org entities-from-trello entities-from-org-buffer))
         (entities                 (first merged-entities))
         (entities-adj             (second merged-entities)))
    (orgtrello-buffer/clean-region! region)
    (orgtrello-buffer/write-card! card-id (gethash card-id entities) entities entities-adj)))

(defun orgtrello-controller/fetch-and-overwrite-checklist! (checklist)
  "Given a checklist, retrieve latest information from trello and overwrite in current buffer."
  (let* ((checklist-id             (orgtrello-data/entity-id checklist))
         (region                   (orgtrello-buffer/compute-entity-region! checklist))
         (entities-from-org-buffer (apply 'orgtrello-buffer/compute-entities-from-org-buffer! (cons nil region)))
         (entities-from-trello     (orgtrello-backend/compute-full-checklist-from-trello! checklist))
         (merged-entities          (orgtrello-data/merge-entities-trello-and-org entities-from-trello entities-from-org-buffer))
         (entities                 (first merged-entities))
         (entities-adj             (second merged-entities)))
    (orgtrello-buffer/clean-region! region)
    (orgtrello-buffer/write-checklist! checklist-id entities entities-adj)))

(defun orgtrello-controller/--sync-entity-and-structure-to-buffer-with-trello-data-callback (buffername &optional position name)
  "Generate a callback which knows the buffer with which it must work. (this callback must take a buffer-name and a position)"
  (lexical-let ((buffer-name buffername)
                (pos         position))
    (function* (lambda (&key data &allow-other-keys) "Synchronize the buffer with the response data."
       (orgtrello-log/msg *OT/TRACE* "proxy - response data: %S" data)
       (orgtrello-action/safe-wrap
        (save-excursion
          ;; buffer manipulation
          (goto-char pos)
          (point-at-bol)
          (org-show-subtree)
          ;; data manipulation + computations
          (funcall
           (cond ((orgtrello-data/entity-card-p data)      'orgtrello-controller/fetch-and-overwrite-card!)
                 ((orgtrello-data/entity-checklist-p data) 'orgtrello-controller/fetch-and-overwrite-checklist!)
                 ((orgtrello-data/entity-item-p data)      'orgtrello-buffer/overwrite-item!))
           data)
          ;; at last
          (save-buffer))
        (orgtrello-log/msg *OT/INFO* "Synchronizing the trello and org data merge - done!"))))))

(defun orgtrello-controller/--dispatch-sync-request (entity &optional with-filter)
  "Dispatch the sync request creation depending on the nature of the entry."
  (let* ((current-meta (orgtrello-data/current entity))
         (entity-id    (orgtrello-data/entity-id current-meta))
         (parent-id    (-> entity orgtrello-data/parent orgtrello-data/entity-id))
         (level        (orgtrello-data/entity-level current-meta)))
    (cond ((= level *CARD-LEVEL*)      (orgtrello-api/get-card entity-id))
          ((= level *CHECKLIST-LEVEL*) (orgtrello-api/get-checklist entity-id with-filter))
          ((= level *ITEM-LEVEL*)      (orgtrello-api/get-item parent-id entity-id)))))

(defun orgtrello-controller/do-sync-entity-from-trello! (&optional sync)
  "Entity (card/checklist/item) synchronization (without its structure) from trello."
  (orgtrello-log/msg *OT/INFO* "Synchronizing the trello entity to the org-mode file...")
  (-> (orgtrello-buffer/entry-get-full-metadata!)
    (orgtrello-controller/--dispatch-sync-request 'with-filter)
    (orgtrello-controller/--update-query-with-org-metadata (point) (buffer-name) nil 'orgtrello-controller/--sync-entity-to-buffer-with-trello-data-callback)
    (orgtrello-proxy/http sync)))

(defun orgtrello-controller/do-sync-entity-and-structure-from-trello! (&optional sync)
  "Entity (card/checklist/item) synchronization (with its structure) from trello."
  (orgtrello-log/msg *OT/INFO* "Synchronizing the trello entity and its structure to the org-mode file...")
  (-> (orgtrello-buffer/entry-get-full-metadata!)
    orgtrello-controller/--dispatch-sync-request
    (orgtrello-controller/--update-query-with-org-metadata (point) (buffer-name) nil 'orgtrello-controller/--sync-entity-and-structure-to-buffer-with-trello-data-callback)
    (orgtrello-proxy/http sync)))

(defun orgtrello-controller/--card-delete (card-meta &optional parent-meta)
  "Deal with the deletion query of a card" ;; parent is useless here
  (orgtrello-api/delete-card (orgtrello-data/entity-id card-meta)))

(defun orgtrello-controller/--checklist-delete (checklist-meta &optional parent-meta)
  "Deal with the deletion query of a checklist" ;; parent is useless here
  (orgtrello-api/delete-checklist (orgtrello-data/entity-id checklist-meta)))

(defun orgtrello-controller/--item-delete (item-meta &optional checklist-meta)
  "Deal with create/update item query build"
  (orgtrello-api/delete-item (orgtrello-data/entity-id checklist-meta) (orgtrello-data/entity-id item-meta)))

(defvar *MAP-DISPATCH-DELETE* (orgtrello-hash/make-properties `((,*CARD-LEVEL*      . orgtrello-controller/--card-delete)
                                                                (,*CHECKLIST-LEVEL* . orgtrello-controller/--checklist-delete)
                                                                (,*ITEM-LEVEL*      . orgtrello-controller/--item-delete))) "Dispatch map for the deletion query of card/checklist/item.")

(defun orgtrello-controller/--dispatch-delete (meta &optional parent-meta)
  "Dispatch the delete function to call depending on the level information."
  (-> meta
      orgtrello-data/entity-level
      (gethash *MAP-DISPATCH-DELETE* 'orgtrello-controller/--too-deep-level)
      (funcall meta parent-meta)))

(defun orgtrello-controller/--do-delete-card (&optional sync)
  "Delete the card."
  (when (= *CARD-LEVEL* (-> (orgtrello-buffer/entry-get-full-metadata!)
                            orgtrello-data/current
                            orgtrello-data/entity-level))
        (orgtrello-controller/do-delete-simple sync)))

(defun orgtrello-controller/do-delete-entities (&optional sync)
  "Launch a batch deletion of every single entities present on the buffer."
  (org-map-entries (lambda () (orgtrello-controller/--do-delete-card sync)) t 'file))

(defun orgtrello-controller/--do-install-config-file (*consumer-key* *access-token*)
  "Persist the file config-file with the input of the user."
  (make-directory *CONFIG-DIR* t)
  (with-temp-file *CONFIG-FILE*
    (erase-buffer)
    (goto-char (point-min))
    (insert (format "(setq *consumer-key* \"%s\")\n" *consumer-key*))
    (insert (format "(setq *access-token* \"%s\")" *access-token*))
    (write-file *CONFIG-FILE* 't)))

(defun orgtrello-controller/do-install-key-and-token ()
  "Procedure to install the *consumer-key* and the token for the user in the config-file."
  (interactive)
  (browse-url (org-trello/https-trello "/1/appKey/generate"))
  (let ((orgtrello-controller/--*consumer-key* (read-string "*consumer-key*: ")))
    (browse-url (org-trello/https-trello (format "/1/authorize?response_type=token&name=org-trello&scope=read,write&expiration=never&key=%s" orgtrello-controller/--*consumer-key*)))
    (let ((orgtrello-controller/--access-token (read-string "Access-token: ")))
      (orgtrello-controller/--do-install-config-file orgtrello-controller/--*consumer-key* orgtrello-controller/--access-token)
      "Install key and read/write access token done!")))

(defun orgtrello-controller/--id-name (entities)
  "Given a list of entities, return a map of (id, name)."
  (--reduce-from (progn (puthash (orgtrello-data/entity-id it) (orgtrello-data/entity-name it) acc) acc) (orgtrello-hash/empty-hash) entities))

(defun orgtrello-controller/--name-id (entities)
  "Given a list of entities, return a map of (id, name)."
  (--reduce-from (progn (puthash (orgtrello-data/entity-name it) (orgtrello-data/entity-id it) acc) acc) (orgtrello-hash/empty-hash) entities))

(defun orgtrello-controller/--list-boards! ()
  "Return the map of the existing boards associated to the current account. (Synchronous request)"
  (--remove (orgtrello-data/entity-closed it) (orgtrello-query/http-trello (orgtrello-api/get-boards) 'do-sync-query)))

(defun orgtrello-controller/--list-board-lists! (board-id)
  "Return the map of the existing list of the board with id board-id. (Synchronous request)"
  (orgtrello-query/http-trello (orgtrello-api/get-lists board-id) 'do-sync-query))

(defun orgtrello-controller/--board! (board-id)
  "Return the board with id board-id. (Synchronous request)"
  (orgtrello-query/http-trello (orgtrello-api/get-board board-id) 'do-sync-query))

(defun orgtrello-controller/--index-board-map (boards)
  "Given a map of board (id . name), return a map of (position . name)"
  (let ((i               0)
        (index-board-map (orgtrello-hash/empty-hash)))
    (maphash (lambda (id _)
               (puthash (format "%d" i) id index-board-map)
               (setq i (+ 1 i)))
             boards)
    index-board-map))

(defun orgtrello-controller/--display-boards-to-choose (boards)
  "Given a map of board (id . name), return a string to display in minibuffer."
  (let ((string-result  "")
        (i            0))
    (maphash (lambda (_ name)
               (setq string-result (format "%s%d: %s\n" string-result i name))
               (setq i (+ 1 i)))
             boards)
    string-result))

(defun orgtrello-controller/choose-board! (boards)
  "Given a map of boards, display the possible boards for the user to choose which one he wants to work with."  ;; ugliest ever
  (let* ((index-selected-board    nil)
         (display-board-to-choose (orgtrello-controller/--display-boards-to-choose boards))
         (index-board-map         (orgtrello-controller/--index-board-map boards)))
    ;; keep asking the selection until the choice is possible
    (while (not (gethash index-selected-board index-board-map))
      (setq index-selected-board (read-string (format "%s\nInput the number of the board desired: " display-board-to-choose))))
    ;; when we are good
    (let ((selected-id-board (gethash index-selected-board index-board-map)))
      (list selected-id-board (gethash selected-id-board boards)))))

;; (orgtrello-controller/choose-board! (orgtrello-hash/make-properties '((:id-board0 . "board0-name") (:id-board1 . "board1-name"))))

(defun orgtrello-controller/--convention-property-name (name)
  "Use the right convention for the property used in the headers of the org-mode file."
  (replace-regexp-in-string " " "-" name))

(defun orgtrello-controller/--delete-buffer-property! (property-name)
  "A simple routine to delete a #+property: entry from the org-mode buffer."
  (save-excursion
    (goto-char (point-min))
    (-when-let (current-point (search-forward property-name nil t))
               (goto-char current-point)
               (beginning-of-line)
               (kill-line)
               (kill-line))))

(defun orgtrello-controller/compute-property (property-name &optional property-value)
  "Compute a formatted entry in org buffer"
  (format "#+property: %s %s" property-name (if property-value property-value "")))

(defun orgtrello-controller/--compute-hash-name-id-to-list (users-hash-name-id)
  (let ((res-list nil))
    (maphash (lambda (name id) (--> name
                                    (replace-regexp-in-string *ORGTRELLO-USER-PREFIX* "" it)
                                    (format "%s%s" *ORGTRELLO-USER-PREFIX* it)
                                    (orgtrello-controller/compute-property it id)
                                    (push it res-list)))
             users-hash-name-id)
    res-list))

(defun orgtrello-controller/--remove-properties-file! (list-keywords users-hash-name-id user-me &optional update-todo-keywords)
  "Remove the current org-trello header metadata."
  (with-current-buffer (current-buffer)
    ;; compute the list of properties to purge
    (->> `(":PROPERTIES"
           ,(orgtrello-controller/compute-property *BOARD-NAME*)
           ,(orgtrello-controller/compute-property *BOARD-ID*)
           ,@(--map (orgtrello-controller/compute-property (orgtrello-controller/--convention-property-name it)) list-keywords)
           ,@(orgtrello-controller/--compute-hash-name-id-to-list users-hash-name-id)
           ,(orgtrello-controller/compute-property *ORGTRELLO-USER-ME* user-me)
           ,(when update-todo-keywords "#+TODO: ")
           ":red" ":blue" ":yellow" ":green" ":orange" ":purple"
           ":END:")
      (mapc 'orgtrello-controller/--delete-buffer-property!))))

(defun orgtrello-controller/--properties-labels (board-labels)
  "Compute properties labels."
  (let ((res-list))
    (maphash (lambda (name id)
               (push (format "#+PROPERTY: %s %s" name id) res-list))
             board-labels)
    res-list))

(defun orgtrello-controller/--compute-metadata! (board-name board-id board-lists-hash-name-id board-users-hash-name-id user-me board-labels &optional update-todo-keywords)
  "Compute the org-trello metadata to dump on header file."
  `(":PROPERTIES:"
    ,(orgtrello-controller/compute-property *BOARD-NAME* board-name)
    ,(orgtrello-controller/compute-property *BOARD-ID* board-id)
    ,@(orgtrello-controller/--compute-board-lists-hash-name-id board-lists-hash-name-id)
    ,(if update-todo-keywords (orgtrello-controller/--properties-compute-todo-keywords-as-string board-lists-hash-name-id) "")
    ,@(orgtrello-controller/--properties-compute-users-ids board-users-hash-name-id)
    ,@(orgtrello-controller/--properties-labels board-labels)
    ,(format "#+PROPERTY: %s %s" *ORGTRELLO-USER-ME* user-me)
    ":END:"))

(defun orgtrello-controller/--compute-keyword-separation (name)
  "Given a keyword done (case insensitive) return a string '| done' or directly the keyword"
  (if (string= "done" (downcase name)) (format "| %s" name) name))

(defun orgtrello-controller/--compute-board-lists-hash-name-id (board-lists-hash-name-id)
  "Compute board lists with hash name and id"
  (let ((res-list))
    (maphash (lambda (name id) (--> (orgtrello-controller/--convention-property-name name)
                                    (format "#+PROPERTY: %s %s" it id)
                                    (push it res-list)))
             board-lists-hash-name-id)
    res-list))

(defun orgtrello-controller/--properties-compute-todo-keywords-as-string (board-lists-hash-name-id)

  (mapconcat 'identity `("#+TODO: "
                         ,@(let ((res-list))
                           (maphash (lambda (name _) (--> name
                                                          (orgtrello-controller/--convention-property-name it)
                                                          (orgtrello-controller/--compute-keyword-separation it)
                                                          (format "%s " it)
                                                          (push it res-list)))
                                    board-lists-hash-name-id)
                           (nreverse res-list))) ""))

(defun orgtrello-controller/--properties-compute-users-ids (board-users-hash-name-id)
  (let ((res-list))
    (maphash (lambda (name id) (--> name
                                    (format "#+PROPERTY: %s%s %s" *ORGTRELLO-USER-PREFIX* it id)
                                    (push it res-list)))
             board-users-hash-name-id)
    res-list))

(defun orgtrello-controller/--update-orgmode-file-with-properties! (board-name board-id board-lists-hash-name-id board-users-hash-name-id user-me board-labels &optional update-todo-keywords)
  "Update the orgmode file with the needed headers for org-trello to work."
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (set-buffer-file-coding-system 'utf-8-auto) ;; force utf-8
    (->> (orgtrello-controller/--compute-metadata! board-name board-id board-lists-hash-name-id board-users-hash-name-id user-me board-labels update-todo-keywords)
      (--map (insert it "\n")))
    (goto-char (point-min))
    (org-cycle)
    (save-buffer)
    (orgtrello-action/reload-setup)))

(defun orgtrello-controller/--hash-table-keys (hash-table)
  "Extract the keys from the hash table."
  (let ((keys ()))
    (maphash (lambda (k v) (push k keys)) hash-table)
    keys))

(defun orgtrello-controller/--user-logged-in! ()
  "Compute the current user."
  (-> (orgtrello-api/get-me)
       (orgtrello-query/http-trello 'do-sync-query)
       orgtrello-data/entity-username))

(defun orgtrello-controller/do-install-board-and-lists ()
  "Command to install the list boards."
  (interactive)
  (let* ((board-info        (-> (orgtrello-controller/--list-boards!)
                              orgtrello-controller/--id-name
                              orgtrello-controller/choose-board!))
         (chosen-board-id   (first board-info))
         (chosen-board-name (second board-info))
         (board-lists       (orgtrello-controller/--list-board-lists! chosen-board-id))
         (board-labels      (->> chosen-board-id orgtrello-controller/--board! orgtrello-data/entity-labels))
         (user-logged-in    (orgtrello-controller/--user-logged-in!)))
    ;; Update metadata about the board
    (orgtrello-controller/do-write-board-metadata! chosen-board-id chosen-board-name user-logged-in board-lists board-labels))
  "Install board and list ids done!")

(defun orgtrello-controller/--compute-user-properties (memberships-map)
  "Given a map, extract the map of user informations."
  (mapcar 'orgtrello-data/entity-member memberships-map))

(defun orgtrello-controller/--compute-user-properties-hash (user-properties)
  (--reduce-from (progn (puthash (orgtrello-data/entity-username it) (orgtrello-data/entity-id it) acc) acc) (orgtrello-hash/empty-hash) user-properties))

(defun orgtrello-controller/--compute-user-properties-hash-from-board (board-info)
  "Compute user properties given board's informations."
  (->> board-info
       orgtrello-data/entity-memberships
       orgtrello-controller/--compute-user-properties
       orgtrello-controller/--compute-user-properties-hash))

(defun orgtrello-controller/--board-users-information-from-board-id! (board-id)
  "Compute board users' informations."
  (--> board-id
       (orgtrello-api/get-board it)
       (orgtrello-query/http-trello it 'do-sync-query)
       (orgtrello-controller/--compute-user-properties-hash-from-board it)))

(defun orgtrello-controller/--create-board (board-name &optional board-description)
  "Create a board with name and eventually a description."
  (orgtrello-log/msg *OT/INFO* "Creating board '%s'" board-name)
  (let ((board-data (orgtrello-query/http-trello (orgtrello-api/add-board board-name board-description) 'do-sync-query)))
    (list (orgtrello-data/entity-id board-data) (orgtrello-data/entity-name board-data))))

(defun orgtrello-controller/--close-lists (list-ids)
  "Given a list of ids, close those lists."
  (mapc (lambda (list-id)
          (orgtrello-log/msg *OT/INFO* "Closing default list with id %s" list-id)
          (orgtrello-query/http-trello (orgtrello-api/close-list list-id)))
        list-ids))

(defun orgtrello-controller/--create-lists-according-to-keywords (board-id list-keywords)
  "Given a list of names, build those lists on the trello boards. Return the hashmap (name, id) of the new lists created."
  (--reduce-from (progn
                   (orgtrello-log/msg *OT/INFO* "Board id %s - Creating list '%s'"
                                      board-id it)
                   (puthash it (orgtrello-data/entity-id (orgtrello-query/http-trello (orgtrello-api/add-list it board-id) 'do-sync-query)) acc)
                   acc)
                 (orgtrello-hash/empty-hash)
                 list-keywords))

(defun orgtrello-controller/do-create-board-and-lists ()
  "Command to create a board and the lists."
  (let ((input-board-name        (orgtrello-input/read-not-empty! "Please, input the desired board name: "))
        (input-board-description (read-string "Please, input the board description (empty for none): ")))

    ;; do create the board and more
    (cl-destructuring-bind (board-id board-name) (orgtrello-controller/--create-board input-board-name input-board-description)
      (let* (;; first retrieve the existing lists (created by default on trello)
             (board-list-ids       (mapcar 'orgtrello-data/entity-id (orgtrello-controller/--list-board-lists! board-id)))
             ;; close those lists (they may surely not match the name we want)
             (lists-to-close       (orgtrello-controller/--close-lists board-list-ids))
             ;; create the list, this returns the ids list
             (board-lists-hname-id (orgtrello-controller/--create-lists-according-to-keywords board-id *LIST-NAMES*))
             ;; retrieve user informations
             (board-users-name-id  (orgtrello-controller/--board-users-information-from-board-id! board-id))
             ;; compute the current user's information
             (user-logged-in       (orgtrello-controller/--user-logged-in!)))
        ;; clean the buffer's old metadata
        (orgtrello-controller/do-cleanup-from-buffer!)
        ;; update org buffer with new ones
        (orgtrello-controller/--update-orgmode-file-with-properties! board-name board-id board-lists-hname-id board-users-name-id user-logged-in))))
  "Create board and lists done!")

(defun orgtrello-controller/--add-user (user users) "Add the user to the users list"
  (if (member user users) users (cons user users)))

(defun orgtrello-controller/--remove-user (user users) "Add the user to the users list"
  (if (member user users) (remove user users) users users))

(defun orgtrello-buffer/--user-ids-assigned-to-current-card () "Compute the user ids assigned to the current card."
  (--> (orgtrello-buffer/get-usernames-assigned-property!)
       (orgtrello-data/--users-from it)
       (--map (gethash (format "%s%s" *ORGTRELLO-USER-PREFIX* it) *HMAP-USERS-NAME-ID*) it)
       (orgtrello-data/--users-to it)))

(defun orgtrello-controller/do-assign-me () "Command to assign oneself to the card."
  (--> (orgtrello-buffer/get-usernames-assigned-property!)
       (orgtrello-data/--users-from it)
       (orgtrello-controller/--add-user *ORGTRELLO-USER-LOGGED-IN* it)
       (orgtrello-data/--users-to it)
       (orgtrello-buffer/set-usernames-assigned-property! it)))

(defun orgtrello-controller/do-unassign-me () "Command to unassign oneself of the card."
  (--> (orgtrello-buffer/get-usernames-assigned-property!)
       (orgtrello-data/--users-from it)
       (orgtrello-controller/--remove-user *ORGTRELLO-USER-LOGGED-IN* it)
       (orgtrello-data/--users-to it)
       (orgtrello-buffer/set-usernames-assigned-property! it)))

(defun orgtrello-controller/do-show-card-comments! ()
  "Show the card comments in a temporary buffer."
  (save-excursion
    (orgtrello-buffer/back-to-card!)
    (let* ((current-card-name (-> (orgtrello-buffer/metadata!) orgtrello-data/entity-name))
           (comments-title (format "comments for card '%s'" current-card-name))
           (comments-formatted (-> (orgtrello-buffer/get-card-comments!)
                                 orgtrello-data/format-comments)))
      (orgtrello-buffer/pop-up-with-content! comments-title comments-formatted))))

(defun orgtrello-controller/--update-comments! (new-comment)
  "Given a current position on a card and a new comment, add a new comment to the current comments."
  (let ((comments (orgtrello-buffer/get-card-comments!)))
    (->> (if comments comments "")
      orgtrello-data/format-comments
      (concat (orgtrello-buffer/me!) ": " new-comment *ORGTRELLO-CARD-COMMENTS-DELIMITER-PRINT*)
      orgtrello-data/unformat-comments
      orgtrello-buffer/put-card-comments!)))

(defun orgtrello-controller/do-add-card-comment! ()
  "Wait for the input to add a comment to the current card."
  (save-excursion
    (orgtrello-buffer/back-to-card!)
    (let* ((card-id (-> (orgtrello-buffer/metadata!) orgtrello-data/entity-id))
           (comment (read-string "Add a comment: ")))
      (if (or (null card-id) (string= "" card-id) (string= "" comment))
          (message "Empty comment - skip.")
        (orgtrello-query/http-trello (orgtrello-api/add-card-comment card-id comment) t
                                     (function* (lambda (&key data &allow-other-keys) "Synchronize the buffer with the response data."
                                                  (orgtrello-log/msg *OT/TRACE* "proxy - response data: %S" data)
                                                  (orgtrello-controller/--update-comments! comment)
                                                  (when *ORGTRELLO-DO-SHOW-CARD-COMMENTS-AFTER-ADDING*
                                                    (orgtrello-controller/do-show-card-comments!)))))))))

(defun orgtrello-controller/do-cleanup-from-buffer! (&optional globally-flag)
  "Permit to clean the buffer from trello data."
  (orgtrello-controller/--remove-properties-file! *LIST-NAMES* *HMAP-USERS-NAME-ID* *ORGTRELLO-USER-LOGGED-IN* t) ;; remove any orgtrello relative entries
  (when globally-flag
    (mapc 'orgtrello-buffer/delete-property! `(,*ORGTRELLO-ID* ,*ORGTRELLO-USERS-ENTRY* ,*ORGTRELLO-CARD-COMMENTS*))))

(defun orgtrello-controller/do-write-board-metadata! (board-id board-name user-logged-in board-lists board-labels)
  "Given a board id, write in the current buffer the updated data."
  (let* ((board-lists-hname-id (orgtrello-controller/--name-id board-lists))
         (board-list-keywords  (orgtrello-controller/--hash-table-keys board-lists-hname-id))
         (board-users-name-id  (orgtrello-controller/--board-users-information-from-board-id! board-id)))
    ;; remove any eventual present entry
    (orgtrello-controller/do-cleanup-from-buffer!)
    ;; update with new ones
    (orgtrello-controller/--update-orgmode-file-with-properties!
     board-name
     board-id
     board-lists-hname-id
     board-users-name-id
     user-logged-in
     board-labels
     t)))

(defun orgtrello-controller/do-update-board-metadata! ()
  "Update metadata about the current board we are connected to."
  (let* ((board-id (orgtrello-buffer/board-id!))
         (board-lists (orgtrello-controller/--list-board-lists! board-id))
         (board-labels (->> board-id orgtrello-controller/--board! orgtrello-data/entity-labels)))
    (orgtrello-controller/do-write-board-metadata! board-id (orgtrello-buffer/board-name!) (orgtrello-buffer/me!) board-lists board-labels)))

(defun orgtrello-controller/do-show-board-labels! ()
  (->> (orgtrello-buffer/labels!)
    orgtrello-data/format-labels
    (orgtrello-buffer/pop-up-with-content! "Labels")))

(defun orgtrello-controller/jump-to-card! ()
  "Given a current entry, execute the extraction and the jump to card action."
  (let* ((full-meta       (orgtrello-buffer/entry-get-full-metadata!))
         (entity          (orgtrello-data/current full-meta))
         (right-entity-fn (cond ((orgtrello-data/entity-item-p entity)      'orgtrello-data/grandparent)
                                ((orgtrello-data/entity-checklist-p entity) 'orgtrello-data/parent)
                                ((orgtrello-data/entity-card-p entity)      'orgtrello-data/current))))
    (-if-let (card-id (->> full-meta (funcall right-entity-fn) orgtrello-data/entity-id))
        (browse-url (org-trello/https-trello (format "/c/%s" card-id))))))

(defun orgtrello-controller/jump-to-board! ()
  "Given the current position, execute the information extraction and jump to board action."
  (browse-url (org-trello/https-trello (format "/b/%s" (orgtrello-buffer/board-id!)))))

(defun orgtrello-controller/delete-setup! ()
  "Global org-trello metadata clean up."
  (orgtrello-controller/do-cleanup-from-buffer! t)
  (orgtrello-log/msg *OT/NOLOG* "Cleanup done!"))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-controller loaded!")


(defun org-trello/proxy-do (action-label action-fn &optional with-save-flag)
  "Execute sync action."
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
   action-label
   '(orgtrello-controller/setup-properties orgtrello-controller/control-keys orgtrello-controller/control-properties orgtrello-controller/control-encoding)
   action-fn
   (when with-save-flag 'do-save-buffer)
   (when with-save-flag 'do-reload-setup)))

(defun org-trello/proxy-do-and-save (action-label action-fn &optional no-check-flag)
  "Execute action and then save the buffer."
  (orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do
   action-label
   (if no-check-flag nil '(orgtrello-controller/setup-properties orgtrello-controller/control-keys))
   action-fn
   'do-save-buffer
   'do-reload-setup))

(defun org-trello/do (action-fn)
  "First checks, then if controls ok, execute"
  (orgtrello-action/controls-or-actions-then-do
   '(orgtrello-controller/setup-properties orgtrello-controller/control-keys orgtrello-controller/control-properties orgtrello-controller/control-encoding)
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

(defun org-trello/--replace-string-prefix-in-string (keybinding string-to-replace)
  (replace-regexp-in-string "#PREFIX#" keybinding string-to-replace t))

(defun org-trello/--startup-message (keybinding)
  (let ((template-string "org-trello/ot is on! To begin with, hit #PREFIX# h or M-x 'org-trello/help-describing-bindings"))
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

(defvar org-trello-mode-hook '() "org-trello-hook for user to extend org-trello with their own behavior.")

(defun org-trello-mode-on-hook-fn (&optional partial-mode)
  "Actions to do when org-trello starts."
  (unless partial-mode
          (org-trello/install-local-prefix-mode-keybinding! *ORGTRELLO-MODE-PREFIX-KEYBINDING*)
          (orgtrello-server/start)
          ;; buffer-invisibility-spec
          (add-to-invisibility-spec '(org-trello-cbx-property)) ;; for an ellipsis (...) change to '(org-trello-cbx-property . t)
          ;; installing hooks
          (add-hook 'before-save-hook 'orgtrello-buffer/install-overlays!) ;; before-change-functions
          ;; migrate all checkbox at org-trello mode activation
          (orgtrello-buffer/install-overlays!)
          ;; a little message in the minibuffer to notify the user
          (orgtrello-log/msg *OT/NOLOG* (org-trello/--startup-message *ORGTRELLO-MODE-PREFIX-KEYBINDING*))
          ;; Overwrite the org-mode-map
          (define-key org-trello-mode-map [remap org-end-of-line] 'org-trello/end-of-line!)
          ;; run hook at startup
          (run-hooks 'org-trello-mode-hook)))

(defun org-trello-mode-off-hook-fn (&optional partial-mode)
  "Actions to do when org-trello stops."
  (unless partial-mode
          (org-trello/remove-local-prefix-mode-keybinding! *ORGTRELLO-MODE-PREFIX-KEYBINDING*)
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


;;; org-trello.el ends here
