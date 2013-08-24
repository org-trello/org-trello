;;; org-trello.el --- Org minor mode to synchronize with trello

;; Copyright (C) 2013 Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.1.4
;; Package-Requires: ((org "8.0.7") (dash "1.5.0") (request "0.2.0") (cl-lib "0.3.0") (json "1.2") (elnode "0.9.9.7.6") (esxml "0.3.0"))
;; Keywords: org-mode trello sync org-trello
;; URL: https://github.com/ardumont/org-trello

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

;;; Code:


(require 'org)
(require 'json)
(require 'dash)
(require 'request)
(eval-when-compile (require 'cl-lib))
(require 'parse-time)
(require 'elnode)
(require 'timer)



;; #################### overriding setup

(defvar *ORGTRELLO-CHECKLIST-UPDATE-ITEMS* t
  "A variable to permit the checklist's status to be pass along to its items. t, if checklist's status is DONE, the items are updated to DONE (org-mode buffer and trello board), nil only the items's status is used.
  To deactivate such behavior, update in your init.el:
  (require 'org-trello)
  (setq *ORGTRELLO-CHECKLIST-UPDATE-ITEMS* nil)")



;; #################### static setup

(defvar *ORGTRELLO-VERSION*           "0.1.4"            "Version")
(defvar *consumer-key*                nil                "Id representing the user.")
(defvar *access-token*                nil                "Read/write access token to use trello on behalf of the user.")
(defvar *ORGTRELLO-MARKER*            "orgtrello-marker" "A marker used inside the org buffer to synchronize entries.")
(defvar *do-sync-query*               t                  "An alias to t to make the boolean more significant in the given context.")
(defvar *do-save-buffer*              t                  "Another alias to t to make the boolean more significant in the given context.")
(defvar *do-reload-setup*             t                  "Another alias to t to make the boolean more significant in the given context.")
(defvar *do-not-display-log*          t                  "Another alias to t to make the boolean more significant in the given context.")
(defvar *ORGTRELLO-LEVELS*            '(1 2 3)           "Current levels 1 is card, 2 is checklist, 3 is item.")



;; #################### orgtrello-version

(defun org-trello/version () (interactive) "Version of org-trello"
  (message "org-trello version: %s" *ORGTRELLO-VERSION*))



;; #################### orgtrello-log

(defvar orgtrello/loglevel 3
  "Set log level.
Levels:
0 - no logging
1 - log errors
2 - log warnings
3 - log info
4 - log debug
5 - log trace")
;; (setq orgtrello/loglevel 5)

(defun orgtrello-log/msg (level &rest args)
  "Log message."
  (when (<= level orgtrello/loglevel)
    (apply 'message args)))

(orgtrello-log/msg 4 "org-trello - orgtrello-log loaded!")



;; #################### orgtrello-hash

(defun orgtrello-hash/make-hash-org (level keyword name id due position buffer-name)
  "Utility function to ease the creation of the orgtrello-metadata"
  (let ((h (make-hash-table :test 'equal)))
    (puthash :buffername buffer-name h)
    (puthash :position   position    h)
    (puthash :level      level       h)
    (puthash :keyword    keyword     h)
    (puthash :name       name        h)
    (puthash :id         id          h)
    (puthash :due        due         h)
    h))

(defun orgtrello-hash/make-hash (method uri &optional params)
  "Utility function to ease the creation of the map - wait, where are my clojure data again!?"
  (let ((h (make-hash-table :test 'equal)))
    (puthash :method method h)
    (puthash :uri    uri    h)
    (if params (puthash :params params h))
    h))

(orgtrello-log/msg 4 "org-trello - orgtrello-hash loaded!")



;; #################### orgtrello-data

(defvar *ORGTRELLO-ID* "orgtrello-id" "Marker used for the trello identifier")

(defun orgtrello-data/--convert-orgmode-date-to-trello-date (orgmode-date)
  "Convert the org-mode deadline into a time adapted for trello."
  (if (and orgmode-date (not (string-match-p "T*Z" orgmode-date)))
      (cl-destructuring-bind (sec min hour day mon year dow dst tz)
                             (--map (if it
                                        (if (< it 10)
                                            (concat "0" (int-to-string it))
                                          (int-to-string it)))
                                    (parse-time-string orgmode-date))
                             (let* ((year-mon-day (concat year "-" mon "-" day "T"))
                                    (hour-min-sec (if hour (concat hour ":" min ":" sec) "00:00:00")))
                               (concat year-mon-day hour-min-sec ".000Z")))
    orgmode-date))

(defun orgtrello-data/extract-identifier (point)
  "Extract the identifier from the point."
  (org-entry-get point *ORGTRELLO-ID*))

(defun orgtrello-data/metadata ()
  "Compute the metadata from the org-heading-components entry, add the identifier and extract the metadata needed."
  (let* ((orgtrello-data/metadata--point       (point))
         (orgtrello-data/metadata--id          (orgtrello-data/extract-identifier orgtrello-data/metadata--point))
         (orgtrello-data/metadata--due         (orgtrello-data/--convert-orgmode-date-to-trello-date (org-entry-get orgtrello-data/metadata--point "DEADLINE")))
         (orgtrello-data/metadata--buffer-name (buffer-name))
         (orgtrello-data/metadata--metadata    (org-heading-components)))
    (->> orgtrello-data/metadata--metadata
         (cons orgtrello-data/metadata--due)
         (cons orgtrello-data/metadata--id)
         (cons orgtrello-data/metadata--point)
         (cons orgtrello-data/metadata--buffer-name)
         orgtrello-data/--get-metadata)))

(defun orgtrello-data/--parent-metadata ()
  "Extract the metadata from the current heading's parent."
  (save-excursion
    (org-up-heading-safe)
    (orgtrello-data/metadata)))

(defun orgtrello-data/--grandparent-metadata ()
  "Extract the metadata from the current heading's grandparent."
  (save-excursion
    (org-up-heading-safe)
    (org-up-heading-safe)
    (orgtrello-data/metadata)))

(defun orgtrello-data/entry-get-full-metadata ()
  "Compute the metadata needed for one entry into a map with keys :current, :parent, :grandparent.
   Returns nil if the level is superior to 4."
  (let* ((heading (orgtrello-data/metadata))
         (level   (gethash :level heading)))
    (if (< level 4)
        (let* ((parent-heading      (orgtrello-data/--parent-metadata))
               (grandparent-heading (orgtrello-data/--grandparent-metadata))
               (mapdata (make-hash-table :test 'equal)))
          ;; build the metadata with every important pieces
          (puthash :current     heading             mapdata)
          (puthash :parent      parent-heading      mapdata)
          (puthash :grandparent grandparent-heading mapdata)
          mapdata))))

(defun orgtrello-data/current (entry-meta)
  "Given an entry-meta, return the current entry"
  (gethash :current entry-meta))

(defun orgtrello-data/parent (entry-meta)
  "Given an entry-meta, return the current entry"
  (gethash :parent entry-meta))

(defun orgtrello-data/grandparent (entry-meta)
  "Given an entry-meta, return the grandparent entry"
  (gethash :grandparent entry-meta))

(defun orgtrello-data/--get-metadata (heading-metadata)
  "Given the heading-metadata returned by the function 'org-heading-components, make it a hashmap with key :level, :keyword, :name. and their respective value"
  (cl-destructuring-bind (buffer-name point id due level _ keyword _ name &rest) heading-metadata
                         (orgtrello-hash/make-hash-org level keyword name id due point buffer-name)))

(orgtrello-log/msg 4 "org-trello - orgtrello-data loaded!")



;; #################### orgtrello-api

(defun orgtrello-api/add-board (name &optional description)
  "Create a board"
  (let* ((payload (if description
                      `(("name" . ,name)
                        ("desc" . ,description))
                    `(("name" . ,name)))))
    (orgtrello-hash/make-hash "POST" "/boards" payload)))

(defun orgtrello-api/get-boards ()
  "Retrieve the boards of the current user."
  (orgtrello-hash/make-hash "GET" "/members/me/boards"))

(defun orgtrello-api/get-board (id)
  "Retrieve the boards of the current user."
  (orgtrello-hash/make-hash "GET" (format "/boards/%s" id)))

(defun orgtrello-api/get-cards (board-id)
  "cards of a board"
  (orgtrello-hash/make-hash "GET" (format "/boards/%s/cards" board-id)))

(defun orgtrello-api/get-card (card-id)
  "Detail of a card with id card-id."
  (orgtrello-hash/make-hash "GET" (format "/cards/%s" card-id)))

(defun orgtrello-api/delete-card (card-id)
  "Delete a card with id card-id."
  (orgtrello-hash/make-hash "DELETE" (format "/cards/%s" card-id)))

(defun orgtrello-api/get-lists (board-id)
  "Display the lists of the board"
  (orgtrello-hash/make-hash "GET" (format "/boards/%s/lists" board-id)))

(defun orgtrello-api/close-list (list-id)
  "'Close' the list with id list-id."
  (orgtrello-hash/make-hash "PUT" (format "/lists/%s/closed" list-id) '((value . t))))

(defun orgtrello-api/get-list (list-id)
  "Get a list by id"
  (orgtrello-hash/make-hash "GET" (format "/lists/%s" list-id)))

(defun orgtrello-api/add-list (name idBoard)
  "Add a list - the name and the board id are mandatory (so i say!)."
  (orgtrello-hash/make-hash "POST" "/lists/" `(("name" . ,name) ("idBoard" . ,idBoard))))

(defun orgtrello-api/add-card (name idList &optional due)
  "Add a card to a board"
  (let* ((orgtrello-api/add-card--default-params `(("name" . ,name) ("idList" . ,idList)))
         (orgtrello-api/add-card--params (if due (cons `("due" . ,due) orgtrello-api/add-card--default-params) orgtrello-api/add-card--default-params)))
    (orgtrello-hash/make-hash "POST" "/cards/" orgtrello-api/add-card--params)))

(defun orgtrello-api/get-cards-from-list (list-id)
  "List all the cards"
  (orgtrello-hash/make-hash "GET" (format "/lists/%s/cards" list-id)))

(defun orgtrello-api/move-card (card-id idList &optional name due)
  "Move a card to another list"
  (let* ((orgtrello-api/move-card--default-params `(("idList" . ,idList)))
         (orgtrello-api/move-card--params-name (if name (cons `("name" . ,name) orgtrello-api/move-card--default-params) orgtrello-api/move-card--default-params))
         (orgtrello-api/move-card--params-due  (if due (cons `("due" . ,due) orgtrello-api/move-card--params-name) orgtrello-api/move-card--params-name)))
    (orgtrello-hash/make-hash "PUT" (format "/cards/%s" card-id) orgtrello-api/move-card--params-due)))

(defun orgtrello-api/add-checklist (card-id name)
  "Add a checklist to a card"
  (orgtrello-hash/make-hash "POST"
             (format "/cards/%s/checklists" card-id)
             `(("name" . ,name))))

(defun orgtrello-api/update-checklist (checklist-id name)
  "Update the checklist's name"
  (orgtrello-hash/make-hash "PUT"
             (format "/checklists/%s" checklist-id)
             `(("name" . ,name))))

(defun orgtrello-api/get-checklists (card-id)
  "List the checklists of a card"
  (orgtrello-hash/make-hash "GET" (format "/cards/%s/checklists" card-id)))

(defun orgtrello-api/get-checklist (checklist-id)
  "Retrieve all the information from a checklist"
  (orgtrello-hash/make-hash "GET" (format "/checklists/%s" checklist-id)))

(defun orgtrello-api/delete-checklist (checklist-id)
  "Delete a checklist with checklist-id"
  (orgtrello-hash/make-hash "DELETE" (format "/checklists/%s" checklist-id)))

(defun orgtrello-api/add-items (checklist-id name &optional checked)
  "Add todo items (trello items) to a checklist with id 'id'"
  (let* ((payload (if checked
                      `(("name"  . ,name) ("checked" . ,checked))
                    `(("name" . ,name)))))
    (orgtrello-hash/make-hash "POST" (format "/checklists/%s/checkItems" checklist-id) payload)))

(defun orgtrello-api/update-item (card-id checklist-id item-id name &optional state)
  "Update a item"
  (let* ((payload (if state
                      `(("name"  . ,name) ("state" . ,state))
                    `(("name" . ,name)))))
    (orgtrello-hash/make-hash
     "PUT"
     (format "/cards/%s/checklist/%s/checkItem/%s" card-id checklist-id item-id)
     payload)))

(defun orgtrello-api/get-items (checklist-id)
  "List the checklist items."
    (orgtrello-hash/make-hash "GET" (format "/checklists/%s/checkItems/" checklist-id)))

(defun orgtrello-api/delete-item (checklist-id item-id)
  "Delete a item with id item-id"
  (orgtrello-hash/make-hash "DELETE" (format "/checklists/%s/checkItems/%s" checklist-id item-id)))

(orgtrello-log/msg 4 "org-trello - orgtrello-api loaded!")



;; #################### orgtrello-query

(defvar *TRELLO-URL* "https://api.trello.com/1" "The needed prefix url for trello")

;; macro? defmethod?
(defun orgtrello-query/--method (query-map) "Retrieve the http method"    (gethash :method query-map))
(defun orgtrello-query/--uri    (query-map) "Retrieve the http uri"       (gethash :uri query-map))
(defun orgtrello-query/--sync   (query-map) "Retrieve the http sync flag" (gethash :sync query-map))
(defun orgtrello-query/--params (query-map) "Retrieve the http params"    (gethash :params query-map))

(defun orgtrello-query/--marker         (entity-data) "Extract the marker of the entity from the entity-data"          (assoc-default 'marker entity-data))
(defun orgtrello-query/--buffername     (entity-data) "Extract the buffername of the entity from the entity-data"      (assoc-default 'buffername entity-data))
(defun orgtrello-query/--position       (entity-data) "Extract the position of the entity from the entity-data"        (assoc-default 'position entity-data))
(defun orgtrello-query/--id             (entity-data) "Extract the id of the entity from the entity"                   (assoc-default 'id entity-data))
(defun orgtrello-query/--name           (entity-data) "Extract the name of the entity from the entity"                 (assoc-default 'name entity-data))
(defun orgtrello-query/--list-id        (entity-data) "Extract the list identitier of the entity from the entity"      (assoc-default 'idList entity-data))
(defun orgtrello-query/--checklist-ids  (entity-data) "Extract the checklist identifier of the entity from the entity" (assoc-default 'idChecklists entity-data))
(defun orgtrello-query/--check-items    (entity-data) "Extract the checklist identifier of the entity from the entity" (assoc-default 'checkItems entity-data))
(defun orgtrello-query/--card-id        (entity-data) "Extract the card identifier of the entity from the entity"      (assoc-default 'idCard entity-data))
(defun orgtrello-query/--due            (entity-data) "Extract the due date of the entity from the query response"     (assoc-default 'due entity-data))
(defun orgtrello-query/--state          (entity-data) "Extract the state of the entity"                                (assoc-default 'state entity-data))
(defun orgtrello-query/--close-property (entity-data) "Extract the closed property of the entity"                      (assoc-default 'closed entity-data))
(defun orgtrello-query/--callback       (entity-data) "Extract the callback property of the entity"                    (assoc-default 'callback entity-data))
(defun orgtrello-query/--sync-          (entity-data) "Extract the sync property of the entity"                        (assoc-default 'sync entity-data))
(defun orgtrello-query/--level          (entity-data) "Extract the callback property of the entity"                    (assoc-default 'level entity-data))
(defun orgtrello-query/--method-        (entity-data) "Extract the method property of the entity"                      (assoc-default 'method entity-data))
(defun orgtrello-query/--uri-           (entity-data) "Extract the uri property of the entity"                         (assoc-default 'uri entity-data))
(defun orgtrello-query/--params-        (entity-data) "Extract the params property of the entity"                      (assoc-default 'params entity-data))
(defun orgtrello-query/--start          (entity-data) "Extract the start property of the entity"                       (assoc-default 'start entity-data))

(defun orgtrello-query/--compute-url (server uri)
  "Compute the trello url from the given uri."
  (format "%s%s" server uri))

(cl-defun orgtrello-query/--standard-error-callback (&key error-thrown symbol-status response &allow-other-keys)
  "Standard error callback. Simply displays a message in the minibuffer with the error code."
  (orgtrello-log/msg 4 "client - Problem during the request to the proxy- error-thrown: %s" error-thrown))

(cl-defun orgtrello-query/--standard-success-callback (&key data &allow-other-keys)
  "Standard success callback. Simply displays a \"Success\" message in the minibuffer."
  (orgtrello-log/msg 4 "client - Proxy received and acknowledged the request%s" (if data (format " - response data: %S." data) ".")))

(defun orgtrello-query/--authentication-params ()
  "Generates the list of http authentication parameters"
  `((key . ,*consumer-key*) (token . ,*access-token*)))

(defun orgtrello-query/--get (server query-map &optional success-callback error-callback authentication-p)
  "GET"
  (let* ((method (orgtrello-query/--method query-map))
         (uri    (orgtrello-query/--uri    query-map))
         (sync   (orgtrello-query/--sync   query-map)))
    (request (orgtrello-query/--compute-url server uri)
             :sync    sync
             :type    method
             :params  (if authentication-p (orgtrello-query/--authentication-params))
             :parser  'json-read
             :success (if success-callback success-callback 'orgtrello-query/--standard-success-callback)
             :error   (if error-callback error-callback 'orgtrello-query/--standard-error-callback))))

(defun orgtrello-query/--post-or-put (server query-map &optional success-callback error-callback authentication-p)
  "POST or PUT"
  (let* ((method  (orgtrello-query/--method query-map))
         (uri     (orgtrello-query/--uri    query-map))
         (payload (orgtrello-query/--params query-map))
         (sync    (orgtrello-query/--sync   query-map)))
    (request (orgtrello-query/--compute-url server uri)
             :sync    sync
             :type    method
             :params  (if authentication-p (orgtrello-query/--authentication-params))
             :headers '(("Content-type" . "application/json"))
             :data    (json-encode payload)
             :parser  'json-read
             :success (if success-callback success-callback 'orgtrello-query/--standard-success-callback)
             :error   (if error-callback error-callback 'orgtrello-query/--standard-error-callback))))

(defun orgtrello-query/--delete (server query-map &optional success-callback error-callback authentication-p)
  "DELETE"
  (let* ((method (orgtrello-query/--method query-map))
         (uri    (orgtrello-query/--uri    query-map))
         (sync   (orgtrello-query/--sync   query-map)))
    (request (orgtrello-query/--compute-url server uri)
             :sync    sync
             :type    method
             :params  (if authentication-p (orgtrello-query/--authentication-params))
             :success (if success-callback success-callback 'orgtrello-query/--standard-success-callback)
             :error   (if error-callback error-callback 'orgtrello-query/--standard-error-callback))))

(defun orgtrello-query/--dispatch-http-query (method)
  (cond ((string= "GET" method)                              'orgtrello-query/--get)
        ((or (string= "POST" method) (string= "PUT" method)) 'orgtrello-query/--post-or-put)
        ((string= "DELETE" method)                           'orgtrello-query/--delete)))

(defun orgtrello-query/--http (server query-map &optional sync success-callback error-callback authentication-p)
  "HTTP query the server with the query-map."
  (let ((fn-dispatch (orgtrello-query/--dispatch-http-query (orgtrello-query/--method query-map))))
    (if sync
        (progn ;; synchronous request
          (puthash :sync t query-map)
          (let ((request-response (funcall fn-dispatch server query-map success-callback error-callback authentication-p)))
            (request-response-data request-response)))
      (funcall fn-dispatch server query-map success-callback error-callback authentication-p))))

(defun orgtrello-query/http-trello (query-map &optional sync success-callback error-callback)
  "Query the trello api."
  ;; request to trello with authentication
  (orgtrello-query/--http *TRELLO-URL* query-map sync success-callback error-callback t))

(orgtrello-log/msg 4 "org-trello - orgtrello-query loaded!")



;; #################### orgtrello-action

(defun orgtrello-action/reload-setup ()
  "Reload orgtrello setup."
  (org-set-regexps-and-options))

(defmacro orgtrello-action/safe-wrap (fn &rest clean-up)
  "A macro to deal with intercept uncaught error when executing the fn call and cleaning up using the clean-up body."
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "### org-trello ### Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))

(defun org-action/--controls-or-actions-then-do (control-or-action-fns fn-to-execute &optional nolog-p)
  "Execute the function fn if control-fns is nil or if the result of apply every function to fn is ok."
  (if control-or-action-fns
      (let* ((org-trello/--controls-or-actions-done (--map (funcall it) control-or-action-fns))
             (org-trello/--error-messages (--filter (not (equal :ok it)) org-trello/--controls-or-actions-done)))
        (if org-trello/--error-messages
            (unless nolog-p
                    ;; there are some trouble, we display all the error messages to help the user understand the problem
                    (orgtrello-log/msg 1 "List of errors:\n %s" (--mapcat (concat "- " it "\n") org-trello/--error-messages)))
          ;; ok execute the function as the controls are ok
          (funcall fn-to-execute)))
    ;; no control, we simply execute the function
    (funcall fn-to-execute)))

(defun org-action/--msg-controls-or-actions-then-do (msg control-or-action-fns fn-to-execute &optional save-buffer-p reload-setup-p nolog-p)
  "A simple decorator function to display message in mini-buffer before and after the execution of the control"
  (unless nolog-p (orgtrello-log/msg 3 (concat msg "...")))
  ;; now execute the controls and the main action
  (orgtrello-action/safe-wrap
   (org-action/--controls-or-actions-then-do control-or-action-fns fn-to-execute nolog-p)
   (progn
     ;; do we have to save the buffer
     (when save-buffer-p  (save-buffer))
     (when reload-setup-p (orgtrello-action/reload-setup))
     (unless nolog-p (orgtrello-log/msg 3 (concat msg " - done!"))))))

(defun org-action/--deal-with-consumer-msg-controls-or-actions-then-do
  (msg control-or-action-fns fn-to-execute &optional save-buffer-p reload-setup-p nolog-p)
  "A simple decorator function to display message in mini-buffer before and after the execution of the control"
  ;; stop the timer
  (orgtrello-timer/stop)
  ;; Execute as usual
  (org-action/--msg-controls-or-actions-then-do msg control-or-action-fns fn-to-execute save-buffer-p reload-setup-p nolog-p)
  ;; start the timer
  (orgtrello-timer/start))



;; #################### orgtrello-proxy

(defvar *ORGTRELLO-PROXY-HOST* "localhost" "proxy host")
(defvar *ORGTRELLO-PROXY-PORT* nil         "proxy port")
(defvar *ORGTRELLO-PROXY-URL*  nil         "proxy url")

(defvar *ORGTRELLO-PROXY-DEFAULT-PORT* 9876 "Default proxy port")
(setq *ORGTRELLO-PROXY-PORT* *ORGTRELLO-PROXY-DEFAULT-PORT*)

(defun orgtrello-proxy/http (query-map &optional sync success-callback error-callback)
  "Query the trello api asynchronously."
  (let ((query-map-proxy (orgtrello-hash/make-hash "POST" "/trello/" query-map)))
    (orgtrello-log/msg 5 "Request to proxy wrapped: %S" query-map-proxy)
    (orgtrello-query/--http *ORGTRELLO-PROXY-URL* query-map-proxy sync success-callback error-callback)))

(defun orgtrello-proxy/http-producer (query-map &optional sync)
  "Query the proxy producer"
  (let ((query-map-proxy (orgtrello-hash/make-hash "POST" "/producer/" query-map)))
    (orgtrello-log/msg 5 "Request to proxy wrapped: %S" query-map-proxy)
    (orgtrello-query/--http *ORGTRELLO-PROXY-URL* query-map-proxy sync)))

(defun orgtrello-proxy/http-consumer (start)
  "Query the http-consumer process once to make it trigger a timer"
  (let ((query-map (orgtrello-hash/make-hash "POST" "/timer/" `((start . ,start)))))
    (orgtrello-query/--http *ORGTRELLO-PROXY-URL* query-map t)))

(defun orgtrello-proxy/--dispatch-http-query (method)
  "Dispach query function depending on the http method input parameter."
  (cond ((string= "GET" method)      'orgtrello-proxy/--get)
        ((or (string= "POST" method)
             (string= "PUT" method)) 'orgtrello-proxy/--post-or-put)
        ((string= "DELETE" method)   'orgtrello-proxy/--delete)))

(defun orgtrello-proxy/--extract-trello-query (http-con)
  "Given an httpcon object, extract the params entry which corresponds to the real trello query."
  (let ((params-proxy-json (caar (elnode-http-params http-con))))
    (json-read-from-string params-proxy-json)))

(defun orgtrello-proxy/--compute-trello-query (query-map-wrapped)
  (let ((method  (orgtrello-query/--method- query-map-wrapped))
        (uri     (orgtrello-query/--uri-    query-map-wrapped))
        (payload (orgtrello-query/--params- query-map-wrapped)))
    (orgtrello-hash/make-hash method uri payload)))

(defun orgtrello-proxy/--response (http-con data)
  "A response wrapper"
  (let ((response-data (json-encode data)))
    (orgtrello-log/msg 5 "Proxy - Responding to client with data '%s'." response-data)
    (elnode-http-start http-con 201 '("Content-type" . "application/json"))
    (elnode-http-return http-con response-data)))

(defun orgtrello-proxy/--compute-entity-level-dir (level)
  "Given a level, compute the folder onto which the file will be serialized."
  (format "%s%s/%s/" elnode-webserver-docroot "org-trello" level))

(defun orgtrello-proxy/response-ok (http-con)
  "OK response from the proxy to the client."
  ;; all is good
  (orgtrello-proxy/--response http-con '((status . "ok"))))

(defun orgtrello-proxy/--elnode-proxy (http-con)
  "Deal with request to trello (for creation/sync request, use orgtrello-proxy/--elnode-proxy-producer)."
  (orgtrello-log/msg 5 "Proxy - Request received. Transmitting...")
  (let* ((query-map-wrapped    (orgtrello-proxy/--extract-trello-query http-con))                     ;; wrapped query is mandatory
         (position             (orgtrello-query/--position query-map-wrapped))                        ;; position is mandatory
         (buffer-name          (orgtrello-query/--buffername query-map-wrapped))                      ;; buffer-name is mandatory
         (standard-callback    (orgtrello-query/--callback query-map-wrapped))                        ;; there is the possibility to transmit the callback from the client to the proxy
         (standard-callback-fn (when standard-callback (symbol-function (intern standard-callback)))) ;; the callback is passed as a string, we want it as a function when defined
         (sync                 (orgtrello-query/--sync- query-map-wrapped))                           ;; there is a possibility to enforce the sync between proxy and client
         (query-map            (orgtrello-proxy/--compute-trello-query query-map-wrapped))            ;; extracting the query
         (name                 (orgtrello-query/--name query-map-wrapped)))                       ;; extracting the name of the entity (optional)
    ;; check out the sync behaviour here
    ;; Execute the request to trello (at the moment, synchronous)
    (orgtrello-query/http-trello query-map sync (when standard-callback-fn (funcall standard-callback-fn buffer-name position name)))
    ;; Answer about the update
    (orgtrello-proxy/response-ok http-con)))

(defun orgtrello-proxy/--compute-metadata-filename (root-dir buffer-name position)
  "Compute the metadata entity filename"
  (format "%s%s-%s.el" root-dir buffer-name position))

(defun orgtrello-proxy/--elnode-proxy-producer (http-con)
  "A handler which is an entity informations producer on files under the docroot/level-entities/"
  (orgtrello-log/msg 5 "Proxy-producer - Request received. Generating entity file...")
  (let* ((query-map-wrapped    (orgtrello-proxy/--extract-trello-query http-con)) ;; wrapped query is mandatory
         (position             (orgtrello-query/--position query-map-wrapped))    ;; position is mandatory
         (buffer-name          (orgtrello-query/--buffername query-map-wrapped))  ;; buffer-name is mandatory
         (level                (orgtrello-query/--level query-map-wrapped))
         (root-dir             (orgtrello-proxy/--compute-entity-level-dir level)))
    ;; generate a file with the entity information
    (with-temp-file (orgtrello-proxy/--compute-metadata-filename root-dir buffer-name position)
      (insert (format "%S\n" query-map-wrapped)))
    ;; all is good
    (orgtrello-proxy/response-ok http-con)))

(defun orgtrello-proxy/--read-lines (fPath)
  "Return a list of lines of a file at FPATH."
  (with-temp-buffer
    (insert-file-contents fPath)
    (split-string (buffer-string) "\n" t)))

(defun orgtrello/compute-marker (buffer-name name position)
  "Compute the orgtrello marker which is composed of buffer-name, name and position"
  (->> (list *ORGTRELLO-MARKER* buffer-name name (if (stringp position) position (int-to-string position)))
       (-interpose "-")
       (apply 'concat)
       sha1
       (concat *ORGTRELLO-MARKER* "-")))

(defun orgtrello-proxy/--remove-file (file-to-remove)
  "Remove metadata file."
  (when (file-exists-p file-to-remove)
        (delete-file file-to-remove)))

(defun orgtrello-proxy/--cleanup-and-save-buffer-metadata (file marker)
  "To cleanup metadata after the all actions are done!"
  (orgtrello-proxy/--remove-file file)
  (org-delete-property-globally marker)
  (save-buffer))

(defmacro orgtrello-proxy/--safe-wrap-or-throw-error (fn)
  "A macro to deal with intercept uncaught error when executing the fn call and cleaning up using the clean-up body. If error is thrown, send the 'org-trello-timer-go-to-sleep flag."
  `(condition-case ex
       (progn ,fn)
     ('error
      (orgtrello-log/msg 1 (concat "### org-trello - consumer ### Caught exception: [" ex "]"))
      (throw 'org-trello-timer-go-to-sleep t))))

(defun orgtrello-proxy/--getting-back-to-headline (data)
  (orgtrello-proxy/--getting-back-to-marker
   (orgtrello/--compute-entity-to-org-entry data)))

(defun orgtrello-proxy/--getting-back-to-marker (marker)
  "Given a marker, getting back to marker function."
  (goto-char (point-min))
  (search-forward (format ":%s:" marker) nil t))

(defun orgtrello-proxy/--standard-post-or-put-success-callback (entity-to-sync file-to-cleanup)
  "Return a callback function able to deal with the update of the buffer at a given position."
  (lexical-let ((orgtrello-proxy/--entry-position    (orgtrello-query/--position entity-to-sync))
                (orgtrello-proxy/--entry-buffer-name (orgtrello-query/--buffername entity-to-sync))
                (orgtrello-proxy/--entry-file        file-to-cleanup)
                (orgtrello-proxy/--marker            (orgtrello-query/--marker entity-to-sync)))
    (cl-defun put-some-insignificant-name (&key data &allow-other-keys)
      (orgtrello-proxy/--safe-wrap-or-throw-error
       (let* ((orgtrello-proxy/--entry-new-id (orgtrello-query/--id data)))
         ;; switch to the right buffer
         (set-buffer orgtrello-proxy/--entry-buffer-name)
         ;; will update via tag the trello id of the new persisted data (if needed)
         (save-excursion
           ;; get back to the buffer and update the id if need be
           (let* ((goto-ok  (orgtrello-proxy/--getting-back-to-marker orgtrello-proxy/--marker))
                  (goto-ok2 (if goto-ok goto-ok (orgtrello-proxy/--getting-back-to-headline data))) ;; I don't get yet why some small % of time, i must do this
                  (str-msg  (when goto-ok2
                                  ;; now we extract the data
                                  (let* ((orgtrello-proxy/--entry-metadata (orgtrello-data/metadata))
                                         (orgtrello-proxy/--entry-id       (orgtrello/--id orgtrello-proxy/--entry-metadata)))
                                    (if orgtrello-proxy/--entry-id ;; id already present in the org-mode file
                                        ;; no need to add another
                                        (concat "Entity '" (orgtrello/--name orgtrello-proxy/--entry-metadata) "' with id '" orgtrello-proxy/--entry-id "' synced!")
                                        (let ((orgtrello-proxy/--entry-name (orgtrello-query/--name data)))
                                          ;; not present, this was just created, we add a simple property
                                          (org-set-property *ORGTRELLO-ID* orgtrello-proxy/--entry-new-id)
                                          (concat "Newly entity '" orgtrello-proxy/--entry-name "' with id '" orgtrello-proxy/--entry-new-id "' synced!")))))))
             (orgtrello-proxy/--cleanup-and-save-buffer-metadata orgtrello-proxy/--entry-file orgtrello-proxy/--marker)
             (when str-msg (orgtrello-log/msg 3 str-msg)))))))))

(defun orgtrello-proxy/--archived-scanning-dir (dir-name)
  "Given a filename, return the archived scanning directory"
  (format "%s/.scanning" dir-name))

(defun orgtrello-proxy/--archived-scanning-file (file)
  "Given a filename, return its archived filename if we were to move such file."
  (let ((dir-name (orgtrello-proxy/--archived-scanning-dir (file-name-directory file))))
    ;; return the name for the new file
    (format "%s/%s" dir-name (file-name-nondirectory file))))

(defun orgtrello-proxy/--archive-entity-file-when-scanning (file-to-archive file-archive-name)
  "Move the file to the running folder to specify a sync is running."
  (rename-file file file-archive-name t))

(defun orgtrello-proxy/--deal-with-entity-sync (entity-data file-to-archive)
  "Compute the synchronization of an entity (retrieving latest information from buffer)"
  (let* ((position                 (orgtrello-query/--position entity-data))  ;; position is mandatory
         (buffer-name              (orgtrello-query/--buffername entity-data));; buffer-name too
         (level                    (orgtrello-query/--level entity-data))     ;; level too
         (orgtrello-query/--marker (orgtrello/compute-marker buffer-name (orgtrello-query/--name entity-data) position)))
    (orgtrello-log/msg 5 "Proxy-consumer - Searching entity metadata from buffer '%s' at point '%s' to sync..." buffer-name position)
    ;; switch to the right buffer
    (set-buffer buffer-name)
    ;; will update via tag the trello id of the new persisted data (if needed)
    (orgtrello-proxy/--safe-wrap-or-throw-error
     (save-excursion
       ;; Get back to the buffer's position to update
       (when (orgtrello-proxy/--getting-back-to-marker orgtrello-query/--marker)
             ;; sync the entity
             (let* ((orgtrello-query/--entry-metadata      (orgtrello-data/entry-get-full-metadata))
                    (orgtrello-query/--entry-file-archived (orgtrello-proxy/--archived-scanning-file file))
                    (orgtrello-query/--query-map           (orgtrello/--dispatch-create orgtrello-query/--entry-metadata)))
               ;; archive the scanned file
               (orgtrello-proxy/--archive-entity-file-when-scanning file-to-archive orgtrello-query/--entry-file-archived)
               ;; Execute the request
               (if (hash-table-p orgtrello-query/--query-map)
                   ;; execute the request
                   (orgtrello-query/http-trello
                    orgtrello-query/--query-map
                    *do-sync-query*
                    (orgtrello-proxy/--standard-post-or-put-success-callback entity-data orgtrello-query/--entry-file-archived)
                    (cl-defun orgtrello-proxy/--standard-post-or-put-error-callback (&allow-other-keys) (throw 'org-trello-timer-go-to-sleep t)))
                   (progn
                     (orgtrello-log/msg 3 orgtrello-query/--query-map)
                     (throw 'org-trello-timer-go-to-sleep t)))))))))

(defun orgtrello-proxy/--deal-with-entity-file-sync (file)
  "Given an entity file, load it and run the query through trello"
  (when (file-exists-p file)
        ;; extract the entity data
        (orgtrello-proxy/--deal-with-entity-sync (-> file orgtrello-proxy/--read-lines read) file)))

(defun dictionary-lessp (str1 str2)
  "return t if STR1 is < STR2 when doing a dictionary compare (splitting the string at numbers and doing numeric compare with them)"
  (let ((str1-components (dict-split str1))
        (str2-components (dict-split str2)))
    (dict-lessp str1-components str2-components)))

(defun dict-lessp (slist1 slist2)
  "compare the two lists of strings & numbers"
  (cond ((null slist1)
         (not (null slist2)))
        ((null slist2)
         nil)
        ((and (numberp (car slist1))
              (stringp (car slist2)))
         t)
        ((and (numberp (car slist2))
              (stringp (car slist1)))
         nil)
        ((and (numberp (car slist1))
              (numberp (car slist2)))
         (or (< (car slist1) (car slist2))
             (and (= (car slist1) (car slist2))
                  (dict-lessp (cdr slist1) (cdr slist2)))))
        (t
         (or (string-lessp (car slist1) (car slist2))
             (and (string-equal (car slist1) (car slist2))
                  (dict-lessp (cdr slist1) (cdr slist2)))))))

(defun dict-split (str)
  "split a string into a list of number and non-number components"
  (save-match-data
    (let ((res nil))
      (while (and str (not (string-equal "" str)))
        (let ((p (string-match "[0-9]*\\.?[0-9]+" str)))
          (cond ((null p)
                 (setq res (cons str res))
                 (setq str nil))
                ((= p 0)
                 (setq res (cons (string-to-number (match-string 0 str)) res))
                 (setq str (substring str (match-end 0))))
                (t
                 (setq res (cons (substring str 0 (match-beginning 0)) res))
                 (setq str (substring str (match-beginning 0)))))))
      (reverse res))))

(defun orgtrello-proxy/--list-files (directory &optional sort-lexicographically)
  "Compute list of regular files (no directory . and ..). List is sorted lexicographically if sort-flag-lexicographically is set, naturally otherwise."
  (let ((orgtrello-proxy/--list-files-result (--filter (file-regular-p it) (directory-files directory t))))
    (unless sort-lexicographically
        orgtrello-proxy/--list-files-result
        (sort orgtrello-proxy/--list-files-result 'dictionary-lessp))))

(defun orgtrello-proxy/--deal-with-directory-sync (level directory)
  "Given a directory, list the files and take the first one (entity) and sync it with trello. Call again if it remains other entities."
  (let ((orgtrello-proxy/--files (orgtrello-proxy/--list-files directory)))
    (when orgtrello-proxy/--files
          ;; try and sync the file
          (orgtrello-proxy/--deal-with-entity-file-sync (car orgtrello-proxy/--files))
          ;; if it potentially remains files to sync, recall recursively this function
          (when (< 1 (length orgtrello-proxy/--files)) (orgtrello-proxy/--deal-with-level level directory)))))

(defun orgtrello-proxy/--level-done-p (level)
  "Is the level done"
  (-> level
      orgtrello-proxy/--compute-entity-level-dir
      orgtrello-proxy/--list-files
      null))

(defun orgtrello-proxy/--level-inf-done-p (level)
  "Ensure the synchronization of the lower level is done (except for level 1 which has no deps)!"
  (cond ((= 1 level) t)
        ((= 2 level) (orgtrello-proxy/--level-done-p 1))
        ((= 3 level) (and (orgtrello-proxy/--level-done-p 2) (orgtrello-proxy/--level-done-p 1)))))

(defun orgtrello-proxy/--deal-with-level (level directory)
 "Given a level, retrieve one file (which represents an entity) for this level and sync it, then remove such file. Then recall the function recursively."
 (if (orgtrello-proxy/--level-inf-done-p level)
     (orgtrello-proxy/--deal-with-directory-sync level directory)
     (throw 'org-trello-timer-go-to-sleep t)))

(defun orgtrello-proxy/--deal-with-archived-files (level)
 "Given a level, retrieve one file (which represents an entity) for this level and sync it, then remove such file. Then recall the function recursively."
 (mapc (lambda (file) (rename-file file (format "../%s" (file-name-nondirectory file)) t)) (-> level
                                                                                               orgtrello-proxy/--compute-entity-level-dir
                                                                                               orgtrello-proxy/--archived-scanning-dir
                                                                                               orgtrello-proxy/--list-files)))

(defun orgtrello-proxy/--consumer-entity-files-hierarchically-and-sync ()
  "A handler to extract the entity informations from files (in order card, checklist, items)."
  ;; now let's deal with the entities sync in order with level
  (with-local-quit
    ;; if archived file exists, get them back in the queue before anything else
    (dolist (l *ORGTRELLO-LEVELS*) (orgtrello-proxy/--deal-with-archived-files l))
    ;; if some check regarding order fails, we catch and let the timer sleep for it the next time to get back normally to the upper level in order
    (catch 'org-trello-timer-go-to-sleep
      (dolist (l *ORGTRELLO-LEVELS*) (orgtrello-proxy/--deal-with-level l (orgtrello-proxy/--compute-entity-level-dir l))))))

(defun orgtrello-proxy/--compute-lock-filename ()
  "Compute the name of a lock file"
  (format "%s%s/%s" elnode-webserver-docroot "org-trello" "org-trello-already-scanning.lock"))

(defvar *ORGTRELLO-LOCK* (orgtrello-proxy/--compute-lock-filename) "Lock file to ensure one timer is running at a time.")

(defun orgtrello-proxy/--timer-put-lock (lock-file)
  "Start triggering the timer."
  (with-temp-file lock-file
    (insert "Timer - Scanning entities...")))

(defun orgtrello-proxy/--timer-delete-lock (lock-file)
  "Cleanup after the timer has been triggered."
  (orgtrello-proxy/--remove-file lock-file))

(defun orgtrello-proxy/--consumer-lock-and-scan-entity-files-hierarchically-and-sync ()
  "A handler to extract the entity informations from files (in order card, checklist, items)."
  (undo-boundary)
  ;; only one timer at a time
  (orgtrello-action/safe-wrap
   (progn
     (orgtrello-proxy/--timer-put-lock *ORGTRELLO-LOCK*)
     (orgtrello-proxy/--consumer-entity-files-hierarchically-and-sync))
   (orgtrello-proxy/--timer-delete-lock *ORGTRELLO-LOCK*))
  ;; undo boundary, to make a unit of undo
  (undo-boundary))

(defun orgtrello-proxy/--check-network-ok ()
  "Ensure there is some network running (simply check that there is more than the lo interface)."
  (if (< 1 (length (network-interface-list))) :ok "No network!"))

(defun orgtrello-proxy/--check-no-running-timer ()
  "Ensure there is not another running timer already."
  (if (file-exists-p (orgtrello-proxy/--compute-lock-filename)) "Timer already running!" :ok))

(defun orgtrello-proxy/--controls-and-scan-if-ok ()
  "Execution of the timer which consumes the entities and execute the sync to trello."
  (org-action/--msg-controls-or-actions-then-do
   "Scanning entities to sync"
   '(orgtrello-proxy/--check-network-ok orgtrello-proxy/--check-no-running-timer)
   'orgtrello-proxy/--consumer-lock-and-scan-entity-files-hierarchically-and-sync
   nil ;; cannot save the buffer
   nil ;; do not need to reload the org-trello setup
   *do-not-display-log*));; do no want to log

(defun orgtrello-proxy/--prepare-filesystem ()
  "Prepare the filesystem for every level."
  (dolist (l *ORGTRELLO-LEVELS*)
    (-> l
        orgtrello-proxy/--compute-entity-level-dir
        orgtrello-proxy/--archived-scanning-dir
        (mkdir t))))

(defvar *ORGTRELLO-TIMER* nil "A timer run by elnode")

(defun orgtrello-proxy/--elnode-timer (http-con)
  "A process on elnode to trigger even regularly."
  (let* ((query-map     (orgtrello-proxy/--extract-trello-query http-con))
         (start-or-stop (orgtrello-query/--start query-map)))
    (if start-or-stop
        ;; cleanup before starting anew
        (progn
          (orgtrello-log/msg 4 "Proxy-timer - Request received. Start timer.")
          ;; cleanup anything that the timer possibly left behind
          (orgtrello-proxy/--timer-delete-lock *ORGTRELLO-LOCK*)
          ;; Prepare the filesystem with the right folders
          (orgtrello-proxy/--prepare-filesystem)
          ;; start the timer
          (setq *ORGTRELLO-TIMER* (run-with-timer 0 5 'orgtrello-proxy/--controls-and-scan-if-ok)))
        ;; otherwise, stop it
        (when *ORGTRELLO-TIMER*
              (orgtrello-log/msg 4 "Proxy-timer - Request received. Stop timer.")
              ;; stop the timer
              (cancel-timer *ORGTRELLO-TIMER*)
              ;; nil the orgtrello reference
              (setq *ORGTRELLO-TIMER* nil)))
    ;; ok in any case
    (orgtrello-proxy/response-ok http-con)))

(defun orgtrello-timer/start ()
  ;; cleanup anything that the timer possibly left behind
  (orgtrello-proxy/http-consumer t))

(defun orgtrello-timer/stop ()
  "Stop the orgtrello-timer."
  (orgtrello-proxy/http-consumer nil))



;; #################### orgtrello-admin

(require 'esxml)

(defun orgtrello-admin/--compute-root-static-files ()
  "Root files under which css and js files are installed."
  (format "%s%s" elnode-webserver-docroot "org-trello/bootstrap"))

(defun orgtrello-admin/--installation-needed-p ()
  "Determine if the installation is needed."
  (let ((dir (orgtrello-admin/--compute-root-static-files)))
    (not (and (file-exists-p dir)
              (< 3 (-> dir
                       directory-files
                       length)))))) ;; . and .. are returned by default

(defvar *ORGTRELLO-FILES* (let ((tmp (make-hash-table :test 'equal)))
                            ;;                    url                                                  temp file            install destination
                            (puthash :bootstrap `("http://getbootstrap.com/2.3.2/assets/bootstrap.zip" "/tmp/bootstrap.zip" ,(orgtrello-admin/--compute-root-static-files)) tmp)
                            (puthash :jquery    `("http://code.jquery.com/jquery-2.0.3.min.js"         "/tmp/jquery.js"     ,(format "%s/js" (orgtrello-admin/--compute-root-static-files))) tmp)
                            tmp))

(defun orgtrello-admin/--unzip-and-install (file dest)
  "Execute the unarchive command. Dependency on unzip on the system."
  (let ((ext (file-name-extension file)))
    (shell-command (format "unzip -o %s -d %s" file dest))))

(defun orgtrello-admin/--install-file (file file-dest)
  "Install the file from temporary location to the final destination."
  (when (file-exists-p file)
        (rename-file file file-dest t)))

(defun orgtrello-admin/--download-and-install-file (key-file)
  "Download the file represented by the parameter. Also, if the archive downloaded is a zip, unzip it."
  (let* ((url-tmp-dest (gethash key-file *ORGTRELLO-FILES*))
         (url          (first  url-tmp-dest))
         (tmp-dest     (second url-tmp-dest))
         (final-dest   (third  url-tmp-dest))
         (extension    (file-name-extension url)))
    ;; download the file
    (url-copy-file url tmp-dest t)
    (if (equal "zip" extension)
        (orgtrello-admin/--unzip-and-install tmp-dest (file-name-directory final-dest))
        (orgtrello-admin/--install-file tmp-dest final-dest))))

(defun orgtrello-admin/--install-css-js-files-once ()
  "Install bootstrap and jquery if need be."
  (when (orgtrello-admin/--installation-needed-p)
        (mapc (lambda (key-file) (orgtrello-admin/--download-and-install-file key-file)) '(:bootstrap :jquery))))

(defun orgtrello-admin/html ()
  "Main html page"
  (let ((project-name "org-trello/proxy-admin")
        (author-name  "Commiters")
        (description  "Administration the running queries to trello"))
    (esxml-to-xml
     `(html
       ()
       ,(orgtrello-admin/head project-name author-name description)
       ,(orgtrello-admin/body project-name)))))

(defun orgtrello-admin/head (project-name author-name description)
  "Generate html <head>"
  (esxml-to-xml
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
                 "body { padding-top: 60px; /* 60px to make the container go all the way to the bottom of the topbar */ }")
          (link ((href . "/static/css/bootstrap.css")
                 (rel . "stylesheet")))
          (link ((href . "/static/css/bootstrap-responsive.min.css")
                 (rel . "stylesheet")))
          "
    <!-- HTML5 shim, for IE6-8 support of HTML5 elements -->
    <!--[if lt IE 9]>
      <script src=\"http://html5shim.googlecode.com/svn/trunk/html5.js\"></script>
    <![endif]-->
")))

(defun orgtrello-admin/body (project-name)
  "Display the data inside the html body"
  (esxml-to-xml
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
          (h1 () "List of entities to sync")
          (div ((id . "server-name"))))
     (script ((src . "/static/js/bootstrap.min.js")) "")
     (script ((src . "/static/js/jquery.js")) "")
     (script ()
             "
function refresh () {
    $.ajax({
        url: \"/proxy/admin/entities/\"
    }).done(function (data) {
        $('#server-name').html(data);
        setTimeout(function() { refresh(); }, 500);
    });
}

refresh();
"))))

(defun orgtrello-admin/--content-file (file)
  "Return the content of a file (absolute name)."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun orgtrello-admin/list-files (list-of-files)
  "Return the list of files to send to trello"
  (let ((fst-file   (car list-of-files))
        (rest-files (cdr list-of-files)))
    (if list-of-files
        (esxml-to-xml
         `(table ((class . "table table-striped table-bordered table-hover")
                  (style . "font-size: 0.75em"))
                 (tr
                  ()
                  (td () (i ((class . "icon-arrow-right"))))
                  (td () ,(orgtrello-admin/--content-file fst-file)))
                 ,(loop for entry in rest-files
                        concat
                        (esxml-to-xml
                         `(tr
                           ()
                           (td () (i ((class . "icon-arrow-up"))))
                           (td () ,(orgtrello-admin/--content-file entry)))))))
        "Empty!")))

(defun orgtrello-proxy/--response-html (http-con data)
  "A response wrapper"
  (elnode-http-start http-con 201 '("Content-type" . "text/html"))
  (elnode-http-return http-con data))

(defun orgtrello-proxy/--elnode-admin (http-con)
  "A basic display of data"
  (orgtrello-proxy/--response-html
   http-con
   (orgtrello-admin/html)))

(defun orgtrello-proxy/--elnode-admin-scan (http-con)
  "A basic display of the list of entities to scan"
  (orgtrello-proxy/--response-html
   http-con
   (orgtrello-admin/list-files
    (--mapcat (orgtrello-proxy/--list-files (orgtrello-proxy/--compute-entity-level-dir it)) *ORGTRELLO-LEVELS*))))

(defun orgtrello-proxy/--elnode-static-file (http-con)
  "Service static files if they exist"
  ;; the first request will ask for installing bootstrap and jquery
  (orgtrello-admin/--install-css-js-files-once)
  (let ((full-file (format "%s/%s/%s" (orgtrello-admin/--compute-root-static-files) (elnode-http-mapping http-con 1) (elnode-http-mapping http-con 2))))
    (if (file-exists-p full-file)
        (elnode-send-file http-con full-file)
        (elnode-send-404 http-con (format "Resource file '%s' not found!" full-file)))))



;; #################### orgtrello-proxy installation

(defvar *ORGTRELLO-QUERY-APP-ROUTES*
  '(;; proxy to request trello
    ("^localhost//proxy/admin/entities/\\(.*\\)" . orgtrello-proxy/--elnode-admin-scan)
    ("^localhost//proxy/admin/\\(.*\\)" . orgtrello-proxy/--elnode-admin)
    ;; proxy to request trello
    ("^localhost//proxy/trello/\\(.*\\)" . orgtrello-proxy/--elnode-proxy)
    ;; proxy producer to receive async creation request
    ("^localhost//proxy/producer/\\(.*\\)" . orgtrello-proxy/--elnode-proxy-producer)
    ;; proxy to request trello
    ("^localhost//proxy/timer/\\(.*\\)" . orgtrello-proxy/--elnode-timer)
    ;; static files
    ("^localhost//static/\\(.*\\)/\\(.*\\)" . orgtrello-proxy/--elnode-static-file))
  "Org-trello dispatch routes for the webserver")

(defun orgtrello-proxy/--proxy-handler (http-con)
  "Proxy handler."
  (elnode-hostpath-dispatcher http-con *ORGTRELLO-QUERY-APP-ROUTES*))

(defun orgtrello-proxy/--start (port host)
  "Starting the proxy."
  (orgtrello-log/msg 5 "Proxy-server starting...")
  (elnode-start 'orgtrello-proxy/--proxy-handler :port port :host host)
  (setq elnode--do-error-logging nil)
  (orgtrello-log/msg 5 "Proxy-server started!"))

(defun orgtrello-proxy/start ()
  "Start the proxy."
  ;; update with the new port the user possibly changed
  (setq *ORGTRELLO-PROXY-URL* (format "http://%s:%d/proxy" *ORGTRELLO-PROXY-HOST* *ORGTRELLO-PROXY-PORT*))
  ;; start the proxy
  (orgtrello-proxy/--start *ORGTRELLO-PROXY-PORT* *ORGTRELLO-PROXY-HOST*)
  ;; and the timer
  (orgtrello-timer/start))

(defun orgtrello-proxy/stop ()
  "Stopping the proxy."
  (orgtrello-log/msg 5 "Proxy-server stopping...")
  ;; stop the timer
  (orgtrello-timer/stop)
  ;; then stop the proxy
  (elnode-stop *ORGTRELLO-PROXY-PORT*)
  (orgtrello-log/msg 5 "Proxy-server stopped!"))

(defun orgtrello-proxy/reload ()
  "Reload the proxy server."
  (interactive)
  (orgtrello-proxy/stop)
  ;; stop the default port (only useful if the user changed from the default port)
  (elnode-stop *ORGTRELLO-PROXY-DEFAULT-PORT*)
  (orgtrello-proxy/start))

(orgtrello-log/msg 4 "org-trello - orgtrello-proxy loaded!")



;; #################### orgtrello

;; Specific state - FIXME check if they do not already exist on org-mode to avoid potential collisions
(defvar *TODO* "TODO" "org-mode todo state")
(defvar *DONE* "DONE" "org-mode done state")

;; Properties key for the orgtrello headers #+PROPERTY board-id, etc...
(defvar *BOARD-ID*   "board-id" "orgtrello property board-id entry")
(defvar *BOARD-NAME* "board-name" "orgtrello property board-name entry")

(defvar *LIST-NAMES*   nil "orgtrello property names of the different lists. This use the standard 'org-todo-keywords property from org-mode.")
(defvar *HMAP-ID-NAME* nil "orgtrello hash map containing for each id, the associated name (or org keyword).")

(defvar *CONFIG-DIR*  (concat (getenv "HOME") "/" ".trello"))
(defvar *CONFIG-FILE* (concat *CONFIG-DIR* "/config.el"))

(defun orgtrello/filtered-kwds ()
  "org keywords used (based on org-todo-keywords-1)."
  org-todo-keywords-1)

(defun orgtrello/--setup-properties ()
  "Setup the properties according to the org-mode setup. Return :ok."
  ;; read the setup
  (orgtrello-action/reload-setup)
  ;; now exploit some
  (let* ((orgtrello/--list-keywords (nreverse (orgtrello/filtered-kwds)))
         (orgtrello/--hmap-id-name (cl-reduce
                                    (lambda (hmap name)
                                      (progn
                                        (puthash (assoc-default name org-file-properties) name hmap)
                                        hmap))
                                    orgtrello/--list-keywords
                                    :initial-value (make-hash-table :test 'equal))))
    (setq *LIST-NAMES*   orgtrello/--list-keywords)
    (setq *HMAP-ID-NAME* orgtrello/--hmap-id-name)
    :ok))

(defun orgtrello/--control-encoding ()
  "Use utf-8, otherwise, there will be trouble."
  (progn
    (orgtrello-log/msg 1 "Ensure you use utf-8 encoding for your org buffer.")
    :ok))

(defun orgtrello/--board-name ()
  "Compute the board's name"
  (assoc-default *BOARD-NAME* org-file-properties))

(defun orgtrello/--board-id ()
  "Compute the board's id"
  (assoc-default *BOARD-ID* org-file-properties))

(defun orgtrello/--control-properties ()
  "org-trello needs the properties board-id and all list id from the trello board to be setuped on header property file. Returns :ok if everything is ok, or the error message if problems."
  (let ((orgtrello/--hmap-count (hash-table-count *HMAP-ID-NAME*)))
    (if (and org-file-properties (orgtrello/--board-id) (= (length *LIST-NAMES*) orgtrello/--hmap-count))
        :ok
        "Setup problem.\nEither you did not connect your org-mode buffer with a trello board, to correct this:\n  * attach to a board through C-c o I or M-x org-trello/install-board-and-lists-ids\n  * or create a board from scratch with C-c o b or M-x org-trello/create-board).\nEither your org-mode's todo keyword list and your trello board lists are not named the same way (which they must).\nFor this, connect to trello and rename your board's list according to your org-mode's todo list.\nAlso, you can specify on your org-mode buffer the todo list you want to work with, for example: #+TODO: TODO DOING | DONE FAIL (hit C-c C-c to refresh the setup)")))

(defun orgtrello/--control-keys ()
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

(defun orgtrello/--keyword (entity-meta &optional default-value)
  "Retrieve the keyword from the entity. If default-value is specified, this is the default value if no keyword is present"
  (gethash :keyword entity-meta default-value))

(defun orgtrello/--name (entity-meta)
  "Retrieve the name from the entity."
  (gethash :name entity-meta))

(defun orgtrello/--id (entity-meta)
  "Retrieve the id from the entity."
  (gethash :id entity-meta))

(defun orgtrello/--level (entity-meta)
  "Retrieve the level from the entity."
  (gethash :level entity-meta))

(defun orgtrello/--due (entity-meta)
  "Retrieve the due date from the entity."
  (gethash :due entity-meta))

(defun orgtrello/--buffername (entity-meta)
  "Retrieve the point from the entity."
  (gethash :buffername entity-meta))

(defun orgtrello/--position (entity-meta)
  "Retrieve the point from the entity."
  (gethash :position entity-meta))

(defun orgtrello/--retrieve-state-of-card (card-meta)
  "Given a card, retrieve its state depending on its :keyword metadata. If empty or no keyword then, its equivalence is *TODO*, otherwise, return its current state."
  (let* ((orgtrello/--card-kwd (orgtrello/--keyword card-meta *TODO*)))
    (if orgtrello/--card-kwd orgtrello/--card-kwd *TODO*)))

(defun orgtrello/--checks-before-sync-card (card-meta)
  "Checks done before synchronizing the cards."
  (let ((orgtrello/--card-name (orgtrello/--name card-meta)))
    (if orgtrello/--card-name
        :ok
      "Cannot synchronize the card - missing mandatory name. Skip it...")))

(defun orgtrello/--card (card-meta &optional parent-meta grandparent-meta)
  "Deal with create/update card query build. If the checks are ko, the error message is returned."
  (let ((checks-ok-or-error-message (orgtrello/--checks-before-sync-card card-meta)))
    ;; name is mandatory
    (if (equal :ok checks-ok-or-error-message)
        ;; parent and grandparent are useless here
        (let* ((orgtrello/--card-kwd  (orgtrello/--retrieve-state-of-card card-meta))
               (orgtrello/--list-id   (assoc-default orgtrello/--card-kwd org-file-properties))
               (orgtrello/--card-id   (orgtrello/--id    card-meta))
               (orgtrello/--card-name (orgtrello/--name card-meta))
               (orgtrello/--card-due  (orgtrello/--due   card-meta)))
          (if orgtrello/--card-id
              ;; update
              (orgtrello-api/move-card orgtrello/--card-id orgtrello/--list-id orgtrello/--card-name orgtrello/--card-due)
            ;; create
            (orgtrello-api/add-card orgtrello/--card-name orgtrello/--list-id orgtrello/--card-due)))
      checks-ok-or-error-message)))

(defun orgtrello/--checks-before-sync-checklist (checklist-meta card-meta)
  "Checks done before synchronizing the checklist."
  (let ((orgtrello/--checklist-name (orgtrello/--name checklist-meta))
        (orgtrello/--card-id        (orgtrello/--id card-meta)))
    (if orgtrello/--checklist-name
        (if orgtrello/--card-id
            :ok
          "Cannot synchronize the checklist - the card must be synchronized first. Skip it...")
      "Cannot synchronize the checklist - missing mandatory name. Skip it...")))

(defun orgtrello/--checklist (checklist-meta &optional card-meta grandparent-meta)
  "Deal with create/update checklist query build. If the checks are ko, the error message is returned."
  (let ((checks-ok-or-error-message (orgtrello/--checks-before-sync-checklist checklist-meta card-meta)))
    ;; name is mandatory
    (if (equal :ok checks-ok-or-error-message)
        ;; grandparent is useless here
        (let* ((orgtrello/--checklist-id   (orgtrello/--id checklist-meta))
               (orgtrello/--card-id        (orgtrello/--id card-meta))
               (orgtrello/--checklist-name (orgtrello/--name checklist-meta)))
          (if orgtrello/--checklist-id
              ;; update
              (orgtrello-api/update-checklist orgtrello/--checklist-id orgtrello/--checklist-name)
            ;; create
            (orgtrello-api/add-checklist orgtrello/--card-id orgtrello/--checklist-name)))
      checks-ok-or-error-message)))

(defun orgtrello/--checks-before-sync-item (item-meta checklist-meta card-meta)
  "Checks done before synchronizing the checklist."
  (let ((orgtrello/--item-name    (orgtrello/--name item-meta))
        (orgtrello/--checklist-id (orgtrello/--id checklist-meta))
        (orgtrello/--card-id      (orgtrello/--id card-meta)))
    (if orgtrello/--item-name
        (if orgtrello/--checklist-id
            (if orgtrello/--card-id
                :ok
              "Cannot synchronize the item - the card must be synchronized first. Skip it...")
          "Cannot synchronize the item - the checklist must be synchronized first. Skip it...")
      "Cannot synchronize the item - missing mandatory name. Skip it...")))

(defun orgtrello/--item-compute-state-or-check (checklist-update-items-p item-state checklist-state possible-states)
  "Compute the item's state/check (for creation/update). The 2 possible states are in the list possible states, first position is the 'checked' one, and second the unchecked one."
  (let* ((orgtrello/--item-checked   (first possible-states))
         (orgtrello/--item-unchecked (second possible-states)))
    (cond ((and checklist-update-items-p (string= *DONE* checklist-state))                      orgtrello/--item-checked)
          ((and checklist-update-items-p (or checklist-state (string= *TODO* checklist-state))) orgtrello/--item-unchecked)
          ((string= *DONE* item-state)                                                          orgtrello/--item-checked)
          (t                                                                                    orgtrello/--item-unchecked))))

(defun orgtrello/--item-compute-state (checklist-update-items-p item-state checklist-state)
  "Compute the item's state (for creation)."
  (orgtrello/--item-compute-state-or-check checklist-update-items-p item-state checklist-state '("complete" "incomplete")))

(defun orgtrello/--item-compute-check (checklist-update-items-p item-state checklist-state)
  "Compute the item's check status (for update)."
    (orgtrello/--item-compute-state-or-check checklist-update-items-p item-state checklist-state '(t nil)))

(defun orgtrello/--compute-state-from-keyword (state)
  "Given a state, compute the org equivalent (to use with org-todo function)"
  (if (string= *DONE* state) 'done 'none))

(defun orgtrello/--update-item-according-to-checklist-status (checklist-update-items-p checklist-meta)
  "Update the item of the checklist according to the status of the checklist."
  (if checklist-update-items-p
      (let ((orgtrello/--checklist-status (orgtrello/--compute-state-from-keyword (orgtrello/--keyword checklist-meta))))
        (org-todo orgtrello/--checklist-status))))

(defun orgtrello/--item (item-meta &optional checklist-meta card-meta)
  "Deal with create/update item query build. If the checks are ko, the error message is returned."
  (let ((checks-ok-or-error-message (orgtrello/--checks-before-sync-item item-meta checklist-meta card-meta)))
    ;; name is mandatory
    (if (equal :ok checks-ok-or-error-message)
        ;; card-meta is only usefull for the update part
        (let* ((orgtrello/--item-id      (orgtrello/--id item-meta))
               (orgtrello/--checklist-id (orgtrello/--id checklist-meta))
               (orgtrello/--card-id      (orgtrello/--id card-meta))
               (orgtrello/--item-name    (orgtrello/--name item-meta))
               (orgtrello/--item-state   (orgtrello/--keyword item-meta))
               (orgtrello/--checklist-state    (orgtrello/--keyword checklist-meta)))

          (orgtrello/--update-item-according-to-checklist-status *ORGTRELLO-CHECKLIST-UPDATE-ITEMS* checklist-meta)
          ;; update/create items
          (if orgtrello/--item-id
              ;; update - rename, check or uncheck the item
              (orgtrello-api/update-item orgtrello/--card-id orgtrello/--checklist-id orgtrello/--item-id orgtrello/--item-name (orgtrello/--item-compute-state *ORGTRELLO-CHECKLIST-UPDATE-ITEMS* orgtrello/--item-state orgtrello/--checklist-state))
            ;; create
            (orgtrello-api/add-items orgtrello/--checklist-id orgtrello/--item-name (orgtrello/--item-compute-check *ORGTRELLO-CHECKLIST-UPDATE-ITEMS* orgtrello/--item-state orgtrello/--checklist-state))))
      checks-ok-or-error-message)))

(defun orgtrello/--too-deep-level (meta &optional parent-meta grandparent-meta)
  "Deal with too deep level."
  "Your arborescence depth is too deep. We only support up to depth 3.\nLevel 1 - card\nLevel 2 - checklist\nLevel 3 - items")

(defun orgtrello/--dispatch-map-creation ()
  "Dispatch map for the creation of card/checklist/item. Key is the level of the entity, value is the create/update query map to sync such entity."
  (let* ((dispatch-map (make-hash-table :test 'equal)))
    (puthash 1 'orgtrello/--card      dispatch-map)
    (puthash 2 'orgtrello/--checklist dispatch-map)
    (puthash 3 'orgtrello/--item      dispatch-map)
    dispatch-map))

(defvar *MAP-DISPATCH-CREATE-UPDATE* (orgtrello/--dispatch-map-creation) "Dispatch map for the creation/update of card/checklist/item.")

(defun orgtrello/--dispatch-create (entry-metadata)
  "Dispatch the creation depending on the nature of the entry."
  (let* ((current-meta        (orgtrello-data/current entry-metadata))
         (current-entry-level (orgtrello/--level current-meta))
         (parent-meta         (orgtrello-data/parent entry-metadata))
         (grandparent-meta    (orgtrello-data/grandparent entry-metadata))
         (dispatch-fn         (gethash current-entry-level *MAP-DISPATCH-CREATE-UPDATE* 'orgtrello/--too-deep-level)))
    ;; then execute the call
    (funcall dispatch-fn current-meta parent-meta grandparent-meta)))

(defun orgtrello/--update-query-with-org-metadata (query-map position buffer-name &optional name success-callback sync)
  "Given a trello api query, add some metadata needed for org-trello to work (those metadata will be exploited by the proxy)."
  (puthash :position       position         query-map)
  (puthash :buffername     buffer-name      query-map)
  (when success-callback (puthash :callback success-callback query-map))
  (when sync             (puthash :sync     sync             query-map))
  (when name             (puthash :name     name             query-map))
  query-map)

(defun orgtrello/--set-marker (marker)
  "Set a marker to get back to later."
  (org-set-property marker marker))

(defun orgtrello/do-create-simple-entity ()
  "Do the actual simple creation of a card."
  (let* ((orgtrello/--current-entry (orgtrello-data/metadata))
         (orgtrello/--marker        (orgtrello/compute-marker
                                     (orgtrello/--buffername orgtrello/--current-entry)
                                     (orgtrello/--name orgtrello/--current-entry)
                                     (orgtrello/--position orgtrello/--current-entry))))
    (when (< (orgtrello/--level orgtrello/--current-entry) 4)
           ;; set a marker for later getting back to information
           (orgtrello/--set-marker orgtrello/--marker)
           ;; adding the marker to the entry
           (puthash :marker orgtrello/--marker orgtrello/--current-entry)
           ;; and send the data to the proxy
           (orgtrello-proxy/http-producer orgtrello/--current-entry))))

(defun orgtrello/do-create-complex-entity ()
  "Do the actual full card creation - from card to item. Beware full side effects..."
  (orgtrello-log/msg 3 "Synchronizing full entity with its structure on board '%s'..." (orgtrello/--board-name))
  ;; iterate over the map of entries and sync them, breadth first
  (org-map-tree 'orgtrello/do-create-simple-entity))

(defun orgtrello/do-sync-full-file ()
  "Full org-mode file synchronisation."
  (orgtrello-log/msg 2 "Synchronizing org-mode file to the board '%s'. This may take some time, some coffee may be a good idea..." (orgtrello/--board-name))
  (org-map-entries 'orgtrello/do-create-simple-entity t 'file))

(defun trace (e &optional label)
  "Decorator for some inaccessible code to easily 'message'."
  (progn
    (if label
        (message "TRACE: %s: %S" label e)
        (message "TRACE: %S" e))
    e))

(defun orgtrello/--compute-card-status (card-id-list)
  "Given a card's id, compute its status."
  (gethash card-id-list *HMAP-ID-NAME*))

(defun orgtrello/--compute-card-to-org-entry (card)
  "Given a card, compute its org-mode entry equivalence."
  (let* ((orgtrello/--card-name     (orgtrello-query/--name card))
         (orgtrello/--card-status   (orgtrello/--compute-card-status (orgtrello-query/--list-id card)))
         (orgtrello/--card-due-date (orgtrello-query/--due card)))
    (format "* %s %s\n%s" orgtrello/--card-status orgtrello/--card-name
            (if orgtrello/--card-due-date (format "DEADLINE: <%s>\n" orgtrello/--card-due-date) ""))))

(defun orgtrello/--compute-checklist-to-org-entry (checklist)
  "Given a checklist, compute its org-mode entry equivalence."
  (let ((orgtrello/--checklist-name  (orgtrello-query/--name checklist)))
    (format "** %s\n" orgtrello/--checklist-name)))

(defun orgtrello/--compute-item-to-org-entry (item)
  "Given a checklist item, compute its org-mode entry equivalence."
  (let* ((orgtrello/--item-name  (orgtrello-query/--name  item))
         (orgtrello/--item-state (orgtrello-query/--state item)))
    (format "*** %s %s\n"
            (if (string= "complete" orgtrello/--item-state) *DONE* *TODO*)
            orgtrello/--item-name)))

(defun orgtrello/--compute-entity-to-org-entry (entity)
  "Given an entity, compute its org representation."
  (cond ((orgtrello-query/--list-id entity) (orgtrello/--compute-card-to-org-entry entity))           ;; card      (level 1)
        ((orgtrello-query/--card-id entity) (orgtrello/--compute-checklist-to-org-entry entity))      ;; checklist (level 2)
        ((orgtrello-query/--state entity)   (orgtrello/--compute-item-to-org-entry entity))))          ;; items     (level 3)

(defun orgtrello/--do-retrieve-checklists-from-card (card)
  "Given a card, return the list containing the card, the checklists from this card, and the items from the checklists. The order is guaranted."
  (cl-reduce
   (lambda (acc-list checklist-id)
     (let ((orgtrello/--checklist (orgtrello-query/http-trello (orgtrello-api/get-checklist checklist-id) *do-sync-query*)))
       (append (cons orgtrello/--checklist (orgtrello/--do-retrieve-checklists-and-items orgtrello/--checklist)) acc-list)))
   (orgtrello-query/--checklist-ids card)
   :initial-value nil))

(defun orgtrello/--do-retrieve-checklists-and-items (checklist)
  "Given a checklist id, retrieve all the items from the checklist and return a list containing first the checklist, then the items."
  (--map it (orgtrello-query/--check-items checklist)))

(defun orgtrello/--compute-full-entities-from-trello (cards)
  "Given a list of cards, compute the full cards data from the trello boards. The order from the trello board is now kept."
  ;; will compute the hash-table of entities (id, entity)
  (cl-reduce
   (lambda (orgtrello/--acc-hash orgtrello/--entity-card)
     (orgtrello-log/msg 3 "Computing card '%s' data..." (orgtrello-query/--name orgtrello/--entity-card))
     ;; adding the entity card
     (puthash (orgtrello-query/--id orgtrello/--entity-card) orgtrello/--entity-card orgtrello/--acc-hash)
     ;; fill in the other remaining entities (checklist/items)
     (mapc
      (lambda (it)
        (puthash (orgtrello-query/--id it) it orgtrello/--acc-hash))
      (orgtrello/--do-retrieve-checklists-from-card orgtrello/--entity-card))
     orgtrello/--acc-hash)
   cards
   :initial-value (make-hash-table :test 'equal)))

(defun orgtrello/--update-buffer-with-remaining-trello-data (entities buffer-name)
  "Given a map of entities, dump those entities in the current buffer."
  (if entities ;; could be empty
      (with-current-buffer buffer-name
        ;; go at the end of the file
        (goto-char (point-max))
        ;; dump the remaining entities
        (maphash
         (lambda (orgtrello/--entry-new-id orgtrello/--entity)
           (let ((orgtrello/--entry-new-name  (orgtrello-query/--name orgtrello/--entity)))
             (orgtrello-log/msg 3 "Synchronizing new entity '%s' with id '%s'..." orgtrello/--entry-new-name orgtrello/--entry-new-id)
             (insert (orgtrello/--compute-entity-to-org-entry orgtrello/--entity))
             (org-set-property *ORGTRELLO-ID* orgtrello/--entry-new-id)))
         entities)
        (goto-char (point-min))
        (org-sort-entries t ?o)
        (save-buffer))))

(defun orgtrello/--sync-buffer-with-trello-data (entities buffer-name)
  "Given all the entities, update the current buffer with those."
  (with-current-buffer buffer-name
    (org-map-entries
     (lambda ()
       (let ((entry-metadata (orgtrello-data/entry-get-full-metadata)))
         (if entry-metadata ;; if level > 4, entry-metadata is not considered as this is not represented in trello board
             ;; will search 'entities' hash table for updates (do not compute diffs, take them as is)
             (let* ((orgtrello/--entity         (gethash :current entry-metadata))
                    (orgtrello/--entity-id      (orgtrello/--id orgtrello/--entity))
                    (orgtrello/--entity-updated (gethash orgtrello/--entity-id entities)))
               (if orgtrello/--entity-updated
                   ;; found something, we update by squashing the current contents
                   (let* ((orgtrello/--entry-new-id    (orgtrello-query/--id   orgtrello/--entity-updated))
                          (orgtrello/--entity-due-date (orgtrello-query/--due  orgtrello/--entity-updated))
                          (orgtrello/--entry-new-name  (orgtrello-query/--name orgtrello/--entity-updated)))
                     ;; update the buffer with the new updates (there may be none but naively we will overwrite at the moment)
                     (orgtrello-log/msg 3 "Synchronizing entity '%s' with id '%s'..." orgtrello/--entry-new-name orgtrello/--entry-new-id)
                     (org-show-entry)
                     (kill-whole-line)
                     (if orgtrello/--entity-due-date (kill-whole-line))
                     (insert (orgtrello/--compute-entity-to-org-entry orgtrello/--entity-updated))
                     ;; remove the entry from the hash-table
                     (remhash orgtrello/--entity-id entities)))))))
     t
     'file))
  ;; return the entities which has been dryed
  entities)

(defun orgtrello/--sync-buffer-with-trello-data-callback (buffername &optional position name)
  "Generate a callback which knows the buffer with which it must work. (this callback must take a buffer-name and a position)"
  (lexical-let ((buffer-name buffername))
    (cl-defun sync-from-trello-insignificant-callback-name (&key data &allow-other-keys)
      "Synchronize the buffer with the response data."
      (orgtrello-log/msg 5 "proxy - response data: %S" data)
      (let* ((orgtrello/--entities-hash-map  (orgtrello/--compute-full-entities-from-trello data));; data is the cards
             (orgtrello/--remaining-entities (orgtrello/--sync-buffer-with-trello-data orgtrello/--entities-hash-map buffer-name)))
        (orgtrello/--update-buffer-with-remaining-trello-data orgtrello/--remaining-entities buffer-name)
        (orgtrello-log/msg 3 "Synchronizing the trello board from trello - done!")))))

(defun orgtrello/do-sync-full-from-trello (&optional sync)
  "Full org-mode file synchronisation. Beware, this will block emacs as the request is synchronous."
  (orgtrello-log/msg 3 "Synchronizing the trello board '%s' to the org-mode file. This may take a moment, some coffee may be a good idea..." (orgtrello/--board-name))
  (orgtrello-proxy/http (orgtrello/--update-query-with-org-metadata
                         (orgtrello-api/get-cards (orgtrello/--board-id))
                         nil
                         (buffer-name)
                         nil
                         'orgtrello/--sync-buffer-with-trello-data-callback)
                        sync))

(defun orgtrello/--card-delete (card-meta &optional parent-meta)
  "Deal with the deletion query of a card"
  ;; parent is useless here
  (orgtrello-api/delete-card (orgtrello/--id card-meta)))

(defun orgtrello/--checklist-delete (checklist-meta &optional parent-meta)
  "Deal with the deletion query of a checklist"
  ;; parent is useless here
  (orgtrello-api/delete-checklist (orgtrello/--id checklist-meta)))

(defun orgtrello/--item-delete (item-meta &optional checklist-meta)
  "Deal with create/update item query build"
  (let* ((orgtrello/--item-id      (orgtrello/--id item-meta))
         (orgtrello/--checklist-id (orgtrello/--id checklist-meta)))
    (orgtrello-api/delete-item orgtrello/--checklist-id orgtrello/--item-id)))

(defun orgtrello/--dispatch-map-delete ()
  "Dispatch map for the deletion of card/checklist/item."
  (let* ((dispatch-map (make-hash-table :test 'equal)))
    (puthash 1 'orgtrello/--card-delete      dispatch-map)
    (puthash 2 'orgtrello/--checklist-delete dispatch-map)
    (puthash 3 'orgtrello/--item-delete      dispatch-map)
    dispatch-map))

(defvar *MAP-DISPATCH-DELETE* (orgtrello/--dispatch-map-delete) "Dispatch map for the deletion query of card/checklist/item.")

(defun orgtrello/--dispatch-delete (meta &optional parent-meta)
  (let* ((level       (orgtrello/--level meta))
         (dispatch-fn (gethash level *MAP-DISPATCH-DELETE* 'orgtrello/--too-deep-level)))
    (funcall dispatch-fn meta parent-meta)))

(defun orgtrello-proxy/--standard-delete-success-callback (buffer-name &optional position name)
  "Return a callback function able to deal with the position."
  (lexical-let* ((orgtrello-query/--entry-position    position)
                 (orgtrello-query/--entry-buffer-name buffer-name)
                 (orgtrello-query/--marker (orgtrello/compute-marker buffer-name name position)))
    (lambda (&rest response)
      (orgtrello-action/safe-wrap
       (progn
         (set-buffer orgtrello-query/--entry-buffer-name)
         (save-excursion
           (when (orgtrello-proxy/--getting-back-to-marker orgtrello-query/--marker)
                 (org-back-to-heading t)
                 (org-delete-property *ORGTRELLO-ID*)
                 (hide-subtree)
                 (beginning-of-line)
                 (kill-line)
                 (kill-line))))
       (progn
         (save-buffer)
         (orgtrello-log/msg 3 "Deleting entity in the buffer '%s' at point '%s' done!" orgtrello-query/--entry-buffer-name orgtrello-query/--entry-position))))))

(defun orgtrello/do-delete-simple (&optional sync)
  "Do the simple deletion of a card, checklist or item."
  (let* ((entry-metadata         (orgtrello-data/entry-get-full-metadata))
         (current-metadata       (orgtrello-data/current entry-metadata))
         (id                     (orgtrello/--id current-metadata))
         (orgtrello/--position   (orgtrello/--position current-metadata))
         (orgtrello/--buffername (orgtrello/--buffername current-metadata))
         (orgtrello/--name       (orgtrello/--name current-metadata)))
    (if (and current-metadata id)
        (progn
          (orgtrello/--set-marker (orgtrello/compute-marker orgtrello/--buffername orgtrello/--name orgtrello/--position))
          (let ((query-http-or-error-msg (orgtrello/--dispatch-delete current-metadata (orgtrello-data/parent entry-metadata))))
            (if (hash-table-p query-http-or-error-msg)
                (progn
                  (orgtrello-proxy/http
                   (orgtrello/--update-query-with-org-metadata query-http-or-error-msg
                                                               orgtrello/--position
                                                               (orgtrello/--buffername current-metadata)
                                                               (orgtrello/--name current-metadata)
                                                               'orgtrello-proxy/--standard-delete-success-callback
                                                               sync)
                   sync)
                  "Delete entity done!")
                query-http-or-error-msg)))
        "Entity not synchronized on trello yet!")))

(defun orgtrello/--do-delete-card (&optional sync)
  "Delete the card."
  (when (= 1 (-> (orgtrello-data/entry-get-full-metadata) orgtrello-data/current orgtrello/--level))
        (orgtrello/do-delete-simple sync)))

(defun orgtrello/do-delete-entities (&optional sync)
  "Launch a batch deletion of every single entities present on the buffer."
  (org-map-entries (lambda () (orgtrello/--do-delete-card sync)) t 'file))

(defun orgtrello/--do-install-config-file (*consumer-key* *access-token*)
  "Persist the file config-file with the input of the user."
  (make-directory *CONFIG-DIR* t)
  (with-temp-file *CONFIG-FILE*
    (erase-buffer)
    (goto-char (point-min))
    (insert (format "(setq *consumer-key* \"%s\")\n" *consumer-key*))
    (insert (format "(setq *access-token* \"%s\")" *access-token*))
    (write-file *CONFIG-FILE* 't)))

(defun orgtrello/do-install-key-and-token ()
  "Procedure to install the *consumer-key* and the token for the user in the config-file."
  (interactive)
  (browse-url "https://trello.com/1/appKey/generate")
  (let ((orgtrello/--*consumer-key* (read-string "*consumer-key*: ")))
    (browse-url (format "https://trello.com/1/authorize?response_type=token&name=org-trello&scope=read,write&expiration=never&key=%s" orgtrello/--*consumer-key*))
    (let ((orgtrello/--access-token (read-string "Access-token: ")))
      (orgtrello/--do-install-config-file orgtrello/--*consumer-key* orgtrello/--access-token)
      "Install key and read/write access token done!")))

(defun orgtrello/--id-name (entities)
  "Given a list of entities, return a map of (id, name)."
  (let* ((id-name (make-hash-table :test 'equal)))
    (mapc (lambda (it) (puthash (orgtrello-query/--id it) (orgtrello-query/--name it) id-name)) entities)
    id-name))

(defun orgtrello/--name-id (entities)
  "Given a list of entities, return a map of (id, name)."
  (let* ((name-id (make-hash-table :test 'equal)))
    (mapc (lambda (it) (puthash (orgtrello-query/--name it) (orgtrello-query/--id it) name-id)) entities)
    name-id))

(defun orgtrello/--list-boards ()
  "Return the map of the existing boards associated to the current account. (Synchronous request)"
  (cl-remove-if-not
   (lambda (board) (equal :json-false (orgtrello-query/--close-property board)))
   (orgtrello-query/http-trello (orgtrello-api/get-boards) *do-sync-query*)))

(defun orgtrello/--list-board-lists (board-id)
  "Return the map of the existing list of the board with id board-id. (Synchronous request)"
  (orgtrello-query/http-trello (orgtrello-api/get-lists board-id) *do-sync-query*))

(defun orgtrello/--choose-board (boards)
  "Given a map of boards, display the possible boards for the user to choose which one he wants to work with."
  ;; ugliest ever
  (defvar orgtrello/--board-chosen nil)
  (setq orgtrello/--board-chosen nil)
  (let* ((str-key-val  "")
         (i            0)
         (i-id (make-hash-table :test 'equal)))
    (maphash (lambda (id name)
               (setq str-key-val (format "%s%d: %s\n" str-key-val i name))
               (puthash (format "%d" i) id i-id)
               (setq i (+ 1 i)))
             boards)
    (while (not (gethash orgtrello/--board-chosen i-id))
      (setq orgtrello/--board-chosen
            (read-string (format "%s\nInput the number of the board desired: " str-key-val))))
    (let* ((orgtrello/--chosen-board-id   (gethash orgtrello/--board-chosen i-id))
           (orgtrello/--chosen-board-name (gethash orgtrello/--chosen-board-id boards)))
      `(,orgtrello/--chosen-board-id ,orgtrello/--chosen-board-name))))

(defun orgtrello/--convention-property-name (name)
  "Use the right convention for the property used in the headers of the org-mode file."
  (replace-regexp-in-string " " "-" name))

(defun orgtrello/--delete-buffer-property (property-name)
  "A simple routine to delete a #+property: entry from the org-mode buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((current-point (search-forward property-name nil t)))
      (if current-point
          (progn
            (goto-char current-point)
            (beginning-of-line)
            (kill-line)
            (kill-line))))))

(defun orgtrello/--remove-properties-file (list-keywords &optional update-todo-keywords)
  "Remove the current org-trello properties"
  (with-current-buffer (current-buffer)
    (orgtrello/--delete-buffer-property (format "#+property: %s" *BOARD-ID*))
    (orgtrello/--delete-buffer-property (format "#+property: %s" *BOARD-NAME*))
    (mapc (lambda (name) (orgtrello/--delete-buffer-property (format "#+property: %s" (orgtrello/--convention-property-name name)))) list-keywords)
    (if update-todo-keywords
        (orgtrello/--delete-buffer-property "#+TODO: "))))

(defun orgtrello/--compute-keyword-separation (name)
  "Given a keyword done (case insensitive) return a string '| done' or directly the keyword"
  (if (string= "done" (downcase name)) (format "| %s" name) name))

(defun orgtrello/--update-orgmode-file-with-properties (board-name board-id board-lists-hash-name-id &optional update-todo-keywords)
  "Update the orgmode file with the needed headers for org-trello to work."
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    ;; force utf-8
    (set-buffer-file-coding-system 'utf-8-auto)
    ;; install board-name and board-id
    (insert (format "#+property: %s    %s\n" *BOARD-NAME* board-name))
    (insert (format "#+property: %s      %s\n" *BOARD-ID* board-id))
    ;; install the other properties regarding the org keywords
    (maphash
     (lambda (name id)
       (insert (format "#+property: %s %s\n" (orgtrello/--convention-property-name name) id)))
     board-lists-hash-name-id)
    (if update-todo-keywords
        (progn
          ;; install the todo list
          (insert "#+TODO: ")
          (maphash (lambda (name _) (insert (concat (orgtrello/--compute-keyword-separation (orgtrello/--convention-property-name name)) " "))) board-lists-hash-name-id)
          (insert "\n")))
    ;; save the buffer
    (save-buffer)
    ;; restart org to make org-trello aware of the new setup
    (orgtrello-action/reload-setup)))

(defun orgtrello/--hash-table-keys (hash-table)
  "Extract the keys from the hash table"
  (let ((keys ()))
    (maphash (lambda (k v) (push k keys)) hash-table)
    keys))

(defun orgtrello/do-install-board-and-lists ()
  "Interactive command to install the list boards"
  (interactive)
  (cl-destructuring-bind
      (orgtrello/--chosen-board-id orgtrello/--chosen-board-name) (-> (orgtrello/--list-boards)
                                                                      orgtrello/--id-name
                                                                      orgtrello/--choose-board)
    (let* ((orgtrello/--board-lists-hname-id (-> orgtrello/--chosen-board-id
                                                 orgtrello/--list-board-lists
                                                 orgtrello/--name-id))
           (orgtrello/--board-list-keywords (orgtrello/--hash-table-keys orgtrello/--board-lists-hname-id)))
      ;; remove any eventual present entry
      (orgtrello/--remove-properties-file orgtrello/--board-list-keywords t)
      ;; update with new ones
      (orgtrello/--update-orgmode-file-with-properties
       orgtrello/--chosen-board-name
       orgtrello/--chosen-board-id
       orgtrello/--board-lists-hname-id
       t))
    "Install board and list ids done!"))

(defun orgtrello/--create-board (board-name &optional board-description)
  "Create a board with name and eventually a description."
  (progn
    (orgtrello-log/msg 3 "Creating board '%s'" board-name)
    (let* ((board-data (orgtrello-query/http-trello (orgtrello-api/add-board board-name board-description) *do-sync-query*)))
      (list (orgtrello-query/--id board-data) (orgtrello-query/--name board-data)))))

(defun orgtrello/--close-lists (list-ids)
  "Given a list of ids, close those lists."
  (mapc (lambda (list-id)
          (progn
            (orgtrello-log/msg 3 "Closing default list with id %s" list-id)
            (orgtrello-query/http-trello (orgtrello-api/close-list list-id))))
        list-ids))

(defun orgtrello/--create-lists-according-to-keywords (board-id list-keywords)
  "Given a list of names, build those lists on the trello boards. Return the hashmap (name, id) of the new lists created."
  (cl-reduce
   (lambda (acc-hash-name-id list-name)
     (progn
       (orgtrello-log/msg 3 "Board id %s - Creating list '%s'" board-id list-name)
       (puthash list-name (orgtrello-query/--id (orgtrello-query/http-trello (orgtrello-api/add-list list-name board-id) *do-sync-query*)) acc-hash-name-id)
       acc-hash-name-id))
   list-keywords
   :initial-value (make-hash-table :test 'equal)))

(defun orgtrello/do-create-board-and-lists ()
  "Interactive command to create a board and the lists"
  (interactive)
  (defvar orgtrello/--board-name nil)        (setq orgtrello/--board-name nil)
  (defvar orgtrello/--board-description nil) (setq orgtrello/--board-description nil)
  (while (not orgtrello/--board-name) (setq orgtrello/--board-name (read-string "Please, input the desired board name: ")))
  (setq orgtrello/--board-description (read-string "Please, input the board description (empty for none): "))
  (cl-destructuring-bind (orgtrello/--board-id orgtrello/--board-name) (orgtrello/--create-board orgtrello/--board-name orgtrello/--board-description)
                         (let* ((orgtrello/--board-list-ids       (--map (orgtrello-query/--id it) (orgtrello/--list-board-lists orgtrello/--board-id)))  ;; first retrieve the existing lists (created by default on trello)
                                (orgtrello/--lists-to-close       (orgtrello/--close-lists orgtrello/--board-list-ids))                                ;; close those lists (they may surely not match the name we want)
                                (orgtrello/--board-lists-hname-id (orgtrello/--create-lists-according-to-keywords orgtrello/--board-id *LIST-NAMES*))) ;; create the list, this returns the ids list
                           ;; remove eventual already present entry
                           (orgtrello/--remove-properties-file *LIST-NAMES*)
                           ;; update org buffer with new ones
                           (orgtrello/--update-orgmode-file-with-properties orgtrello/--board-name orgtrello/--board-id orgtrello/--board-lists-hname-id)))
  "Create board and lists done!")

(orgtrello-log/msg 4 "org-trello - orgtrello loaded!")



;; #################### org-trello

(defun org-trello/create-simple-entity ()
  "Control first, then if ok, create a simple entity."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting entity sync"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     'orgtrello/do-create-simple-entity))

(defun org-trello/create-complex-entity ()
  "Control first, then if ok, create an entity and all its arborescence if need be."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting entity and structure sync"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     'orgtrello/do-create-complex-entity))

(defun org-trello/sync-to-trello ()
  "Control first, then if ok, sync the org-mode file completely to trello."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting sync org buffer to trello board"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     'orgtrello/do-sync-full-file))

(defun org-trello/sync-from-trello ()
  "Control first, then if ok, sync the org-mode file from the trello board."
  (interactive)
  ;; execute the action
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting sync org buffer from trello board"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     'orgtrello/do-sync-full-from-trello
     *do-save-buffer*))

(defun org-trello/kill-entity ()
  "Control first, then if ok, delete the entity and all its arborescence."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting deleting entity"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     'orgtrello/do-delete-simple))

(defun org-trello/kill-all-entities ()
  "Control first, then if ok, delete the entity and all its arborescence."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting deleting entities"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     'orgtrello/do-delete-entities))

(defun org-trello/install-key-and-token ()
  "No control, trigger the setup installation of the key and the read/write token."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
   "Setup key and token"
   nil
   'orgtrello/do-install-key-and-token
   *do-save-buffer*
   *do-reload-setup*))

(defun org-trello/install-board-and-lists-ids ()
  "Control first, then if ok, trigger the setup installation of the trello board to sync with."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Install boards and lists"
     '(orgtrello/--setup-properties orgtrello/--control-keys)
     'orgtrello/do-install-board-and-lists
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/create-board ()
  "Control first, then if ok, trigger the board creation."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Create board and lists"
     '(orgtrello/--setup-properties orgtrello/--control-keys)
     'orgtrello/do-create-board-and-lists
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/check-setup ()
  "Check the current setup."
  (interactive)
  (org-action/--controls-or-actions-then-do
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     (lambda () (orgtrello-log/msg 0 "Setup ok!"))))

(defun org-trello/delete-setup ()
  "Delete the current setup."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
   "Deleting current org-trello setup"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     (lambda ()
       ;; remove any orgtrello relative entries
       (orgtrello/--remove-properties-file *LIST-NAMES* t)
       ;; remove any identifier from the buffer
       (org-delete-property-globally *ORGTRELLO-ID*)
       ;; a simple message to tell the client that the work is done!
       (orgtrello-log/msg 0 "Cleanup done!"))
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/help-describing-bindings ()
  "A simple message to describe the standard bindings used."
  (interactive)
  (orgtrello-log/msg 0
"# SETUP RELATED
C-c o i - M-x org-trello/install-key-and-token       - Install the keys and the access-token.
C-c o I - M-x org-trello/install-board-and-lists-ids - Select the board and attach the todo, doing and done list.
C-c o d - M-x org-trello/check-setup                 - Check that the setup is ok. If everything is ok, will simply display 'Setup ok!'
C-c o D - M-x org-trello/delete-setup                - Clean up the org buffer from all org-trello informations
# TRELLO RELATED
C-c o b - M-x org-trello/create-board                - Create interactively a board and attach the org-mode file to this trello board.
C-c o c - M-x org-trello/create-simple-entity        - Create/Update an entity (card/checklist/item) depending on its level and status. Do not deal with level superior to 4.
C-c o C - M-x org-trello/create-complex-entity       - Create/Update a complete entity card/checklist/item and its subtree (depending on its level).
C-c o s - M-x org-trello/sync-to-trello              - Synchronize the org-mode file to the trello board (org-mode -> trello).
C-c o S - M-x org-trello/sync-from-trello            - Synchronize the org-mode file from the trello board (trello -> org-mode).
C-c o k - M-x org-trello/kill-entity                 - Kill the entity (and its arborescence tree) from the trello board and the org buffer.
C-c o K - M-x org-trello/kill-all-entities           - Kill all the entities (and their arborescence tree) from the trello board and the org buffer.
# HELP
C-c o h - M-x org-trello/help-describing-bindings    - This help message."))

;;;###autoload
(define-minor-mode org-trello-mode "Sync your org-mode and your trello together."
  :lighter " ot" ;; the name on the modeline
  :keymap  (let ((map (make-sparse-keymap)))
             ;; setup relative
             (define-key map (kbd "C-c o i") 'org-trello/install-key-and-token)
             (define-key map (kbd "C-c o I") 'org-trello/install-board-and-lists-ids)
             (define-key map (kbd "C-c o d") 'org-trello/check-setup)
             (define-key map (kbd "C-c o x") 'org-trello/delete-setup)
             ;; synchronous request (direct to trello)
             (define-key map (kbd "C-c o b") 'org-trello/create-board)
             (define-key map (kbd "C-c o S") 'org-trello/sync-from-trello)
             ;; asynchronous requests (requests through proxy)
             (define-key map (kbd "C-c o c") 'org-trello/create-simple-entity)
             (define-key map (kbd "C-c o C") 'org-trello/create-complex-entity)
             (define-key map (kbd "C-c o k") 'org-trello/kill-entity)
             (define-key map (kbd "C-c o K") 'org-trello/kill-all-entities)
             (define-key map (kbd "C-c o s") 'org-trello/sync-to-trello)
             ;; Help
             (define-key map (kbd "C-c o h") 'org-trello/help-describing-bindings)
             map))

(add-hook 'org-trello-mode-on-hook
          (lambda ()
            ;; start the proxy
            (orgtrello-proxy/start)
            ;; a little message in the minibuffer to notify the user
            (orgtrello-log/msg 0 "org-trello/ot is on! To begin with, hit C-c o h or M-x 'org-trello/help-describing-bindings")))

(add-hook 'org-trello-mode-off-hook
          (lambda ()
            ;; stop the proxy
            (orgtrello-proxy/stop)
            ;; a little message in the minibuffer to notify the user
            (orgtrello-log/msg 0 "org-trello/ot is off!")))

(orgtrello-log/msg 4 "org-trello loaded!")

(provide 'org-trello)

;;; org-trello.el ends here
