;;; org-trello.el --- Org minor mode to synchronize with trello

;; Copyright (C) 2013 Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.1.2
;; Package-Requires: ((org "8.0.7") (dash "1.5.0") (request "0.2.0") (cl-lib "0.3.0") (json "1.2") (elnode "0.9.9.7.6"))
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



;; #################### overriding setup

(defvar *ORGTRELLO-CHECKLIST-UPDATE-ITEMS* t
  "A variable to permit the checklist's status to be pass along to its items. t, if checklist's status is DONE, the items are updated to DONE (org-mode buffer and trello board), nil only the items's status is used.
  To deactivate such behavior, update in your init.el:
  (require 'org-trello)
  (setq *ORGTRELLO-CHECKLIST-UPDATE-ITEMS* nil)")

(defvar *consumer-key*     nil "Id representing the user")
(defvar *access-token*     nil "Read/write Access token to use trello in the user's name ")



;; #################### orgtrello-log

(defvar orgtrello/loglevel 5
  "Set log level.
Levels:
0 - no logging
1 - log errors
2 - log warnings
3 - log info
4 - log debug
5 - log trace")

(defun orgtrello-log/msg (level &rest args)
  "Log message."
  (when (<= level orgtrello/loglevel)
    (apply 'message args)))

(orgtrello-log/msg 4 "org-trello - orgtrello-log loaded!")



;; #################### orgtrello-hash

(defun orgtrello-hash/make-hash-org (level keyword title id due position buffer-name)
  "Utility function to ease the creation of the orgtrello-metadata"
  (let ((h (make-hash-table :test 'equal)))
    (puthash :buffername buffer-name h)
    (puthash :position   position    h)
    (puthash :level      level       h)
    (puthash :keyword    keyword     h)
    (puthash :title      title       h)
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
         (orgtrello-data/metadata--metadata (org-heading-components)))
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

(defun orgtrello-data/--get-metadata (heading-metadata)
  "Given the heading-metadata returned by the function 'org-heading-components, make it a hashmap with key :level, :keyword, :title. and their respective value"
  (cl-destructuring-bind (buffer-name point id due level _ keyword _ title &rest) heading-metadata
                         (orgtrello-hash/make-hash-org level keyword title id due point buffer-name)))

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

(defun orgtrello-api/add-tasks (checklist-id name &optional checked)
  "Add todo tasks (trello items) to a checklist with id 'id'"
  (let* ((payload (if checked
                      `(("name"  . ,name) ("checked" . ,checked))
                    `(("name" . ,name)))))
    (orgtrello-hash/make-hash "POST" (format "/checklists/%s/checkItems" checklist-id) payload)))

(defun orgtrello-api/update-task (card-id checklist-id task-id name &optional state)
  "Update a task"
  (let* ((payload (if state
                      `(("name"  . ,name) ("state" . ,state))
                    `(("name" . ,name)))))
    (orgtrello-hash/make-hash
     "PUT"
     (format "/cards/%s/checklist/%s/checkItem/%s" card-id checklist-id task-id)
     payload)))

(defun orgtrello-api/get-tasks (checklist-id)
  "List the checklist items."
    (orgtrello-hash/make-hash "GET" (format "/checklists/%s/checkItems/" checklist-id)))

(defun orgtrello-api/delete-task (checklist-id task-id)
  "Delete a task with id task-id"
  (orgtrello-hash/make-hash "DELETE" (format "/checklists/%s/checkItems/%s" checklist-id task-id)))

(orgtrello-log/msg 4 "org-trello - orgtrello-api loaded!")



;; #################### orgtrello-query

(defvar *TRELLO-URL* "https://api.trello.com/1" "The needed prefix url for trello")

(defun orgtrello-query/--method (query-map)
  "Retrieve the http method"
  (gethash :method query-map))

(defun orgtrello-query/--uri (query-map)
  "Retrieve the http uri"
  (gethash :uri query-map))

(defun orgtrello-query/--sync (query-map)
  "Retrieve the http sync flag"
  (gethash :sync query-map))

(defun orgtrello-query/--params (query-map)
  "Retrieve the http params"
  (gethash :params query-map))

(defun orgtrello-query/--buffername (entity-data)
  "Extract the position of the entity from the entity-data"
  (assoc-default 'buffername entity-data))

(defun orgtrello-query/--position (entity-data)
  "Extract the position of the entity from the entity-data"
  (assoc-default 'position entity-data))

(defun orgtrello-query/--id (entity-data)
  "Extract the id of the entity from the entity"
  (assoc-default 'id entity-data))

(defun orgtrello-query/--name (entity-data)
  "Extract the name of the entity from the entity"
  (assoc-default 'name entity-data))

(defun orgtrello-query/--list-id (entity-data)
  "Extract the list identitier of the entity from the entity"
  (assoc-default 'idList entity-data))

(defun orgtrello-query/--checklist-ids (entity-data)
  "Extract the checklist identifier of the entity from the entity"
  (assoc-default 'idChecklists entity-data))

(defun orgtrello-query/--check-items (entity-data)
  "Extract the checklist identifier of the entity from the entity"
  (assoc-default 'checkItems entity-data))

(defun orgtrello-query/--card-id (entity-data)
  "Extract the card identifier of the entity from the entity"
  (assoc-default 'idCard entity-data))

(defun orgtrello-query/--due (entity-data)
  "Extract the due date of the entity from the query response"
  (assoc-default 'due entity-data))

(defun orgtrello-query/--state (entity-data)
  "Extract the state of the entity"
  (assoc-default 'state entity-data))

(defun orgtrello-query/--close-property (entity-data)
  "Extract the closed property of the entity"
  (assoc-default 'closed entity-data))

(defun orgtrello-query/--compute-url (server uri)
  "Compute the trello url from the given uri."
  (format "%s%s" server uri))

(cl-defun orgtrello-query/--standard-error-callback (&key error-thrown symbol-status response &allow-other-keys)
  "Standard error callback. Simply displays a message in the minibuffer with the error code."
  (orgtrello-log/msg 3 "client - There was some problem during the request:\n
- error-thrown: %s\nresponse: %s" error-thrown symbol-status response))

(cl-defun orgtrello-query/--standard-success-callback (&key data &allow-other-keys)
  "Standard success callback. Simply displays a \"Success\" message in the minibuffer."
  (when data (orgtrello-log/msg 3 "client - response data: %S" data))
  (orgtrello-log/msg 3 "Success."))

(cl-defun orgtrello-query/--update-entity-id-to-buffer-callback (&key data &allow-other-keys)
  "POST/PUT callback to create/update the trello id in the org-mode file."
  (let ((orgtrello-query/--entry-new-id      (orgtrello-query/--id data))
        (orgtrello-query/--entry-position    (orgtrello-query/--position data))
        (orgtrello-query/--entry-buffer-name (orgtrello-query/--buffername data)))
    (orgtrello-log/msg 5 "client - Updating entity '%s' in the buffer '%s' at point '%s'..." orgtrello-query/--entry-new-id orgtrello-query/--entry-buffer-name orgtrello-query/--entry-position)
    ;; switch to the right buffer
    (set-buffer orgtrello-query/--entry-buffer-name)
    ;; will update via tag the trello id of the new persisted data (if needed)
    (save-excursion
      ;; Get back to the buffer
      (goto-char orgtrello-query/--entry-position)
      ;; now we extract the data
      (let* ((orgtrello-query/--entry-metadata (orgtrello-data/metadata))
             (orgtrello-query/--entry-id       (orgtrello/--id orgtrello-query/--entry-metadata))
             (orgtrello-query/--entry-name     (orgtrello/--label orgtrello-query/--entry-metadata) ))
        (if orgtrello-query/--entry-id ;; id already present in the org-mode file
            ;; no need to add another
            (orgtrello-log/msg 3 "Entity '%s' synced with id '%s'" orgtrello-query/--entry-name orgtrello-query/--entry-id)
          (progn
            ;; not present, this was just created, we add a simple property
            (org-set-property *ORGTRELLO-ID* orgtrello-query/--entry-new-id)
            (orgtrello-log/msg 3 "Newly entity '%s' synced with id '%s'" orgtrello-query/--entry-name orgtrello-query/--entry-new-id)))))))

(cl-defun orgtrello-query/--delete-success-callback (&key data &allow-other-keys)
  "Callback function called at the end of a successful delete request."
  (let ((orgtrello-query/--entry-position    (orgtrello-query/--position data))
        (orgtrello-query/--entry-buffer-name (orgtrello-query/--buffername data)))
    (orgtrello-log/msg 5 "client - Deleting entity in the buffer '%s' at point '%s'" orgtrello-query/--entry-buffer-name orgtrello-query/--entry-position)
    (set-buffer orgtrello-query/--entry-buffer-name)
    (save-excursion
      (goto-char orgtrello-query/--entry-position)
      (org-back-to-heading t)
      (org-delete-property *ORGTRELLO-ID*)
      (hide-subtree)
      (beginning-of-line)
      (kill-line)
      (kill-line)))
  (orgtrello-log/msg 3 "Entity deleted!"))

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



;; #################### orgtrello-proxy

(defvar *ORGTRELLO-PROXY-HOST* "localhost" "proxy host")
(defvar *ORGTRELLO-PROXY-PORT* nil         "proxy port")
(defvar *ORGTRELLO-PROXY-URL* nil          "proxy url")

(defvar *ORGTRELLO-PROXY-DEFAULT-PORT* 9876 "Default proxy port")
(setq *ORGTRELLO-PROXY-PORT* *ORGTRELLO-PROXY-DEFAULT-PORT*)

(defun orgtrello-proxy/http (query-map &optional sync success-callback error-callback)
  "Query the trello api asynchronously."
  (orgtrello-log/msg 5 "Request to trello server to wrap: %S" query-map)
  (let ((query-map-proxy (orgtrello-hash/make-hash "POST" "/" query-map)))
    (orgtrello-log/msg 5 "Request to proxy wrapped: %S" query-map-proxy)
    (orgtrello-query/--http *ORGTRELLO-PROXY-URL* query-map-proxy sync success-callback error-callback)))

(defvar orgtrello-query/--app-routes '(;; proxy routine
                                       ("^localhost//proxy/\\(.*\\)" . orgtrello-proxy/--elnode-proxy)))

(defun orgtrello-proxy/--response (http-con data)
  "A response wrapper"
  (let ((response-data (json-encode data)))
    (orgtrello-log/msg 5 "proxy - Responding to client with data '%s'." response-data)
    (elnode-http-start http-con 201 '("Content-type" . "application/json"))
    (elnode-http-return http-con response-data)))

(defun orgtrello-proxy/--standard-get-success-callback (http-connection)
  (lexical-let ((http-con http-connection))
    "Return a callback function able to deal with the position."
    (cl-defun get-some-insignificant-name (&key data &allow-other-keys)
      "Standard get response will simply relay the information to the client."
      (orgtrello-log/msg 5 "proxy - get callback: %S" data)
      (orgtrello-proxy/--response http-con data))))

(defun orgtrello-proxy/--standard-post-or-put-or-delete-success-callback (http-connection buffer-metadata)
  "Return a callback function able to deal with the position."
  (lexical-let ((http-con http-connection)
                (position (first buffer-metadata))
                (buffername (second buffer-metadata)))
    (cl-defun put-some-insignificant-name (&key data &allow-other-keys)
      "Will read the information from the response and simply return id and position."
      (orgtrello-log/msg 5 "proxy - post/put/delete callback: %S - position: %s" data position)
      (let ((orgtrello-query/--identifier (orgtrello-query/--id data)))
        (orgtrello-proxy/--response http-con `((id . ,orgtrello-query/--identifier)
                                               (position . ,position)
                                               (buffername . ,buffername)))))))

(defun orgtrello-proxy/--get (http-con query-map &optional buffer-metadata sync)
  "GET on trello with the callback"
  (orgtrello-query/http-trello query-map sync (orgtrello-proxy/--standard-get-success-callback http-con)))

(defun orgtrello-proxy/--post-or-put-or-delete (http-con query-map &optional buffer-metadata sync)
  "POST/PUT/DELETE"
  (orgtrello-query/http-trello query-map sync (orgtrello-proxy/--standard-post-or-put-or-delete-success-callback http-con buffer-metadata)))

(defun orgtrello-proxy/--dispatch-http-query (method)
  "Dispach query function depending on the http method input parameter."
  (cond ((string= "GET" method)         'orgtrello-proxy/--get)
        ((or (string= "POST" method)
             (string= "PUT" method)
             (string= "DELETE" method)) 'orgtrello-proxy/--post-or-put-or-delete)))

(defun orgtrello-proxy/--extract-trello-query (http-con)
  "Given an httpcon object, extract the params entry which corresponds to the real trello query."
  (let ((params-proxy-json (caar (elnode-http-params http-con))))
    (json-read-from-string params-proxy-json)))

(defun orgtrello-proxy/--compute-trello-query (query-map-wrapped)
  (let ((method  (assoc-default 'method query-map-wrapped))
        (uri     (assoc-default 'uri query-map-wrapped))
        (payload (assoc-default 'params query-map-wrapped)))
    (orgtrello-hash/make-hash method uri payload)))

(defun orgtrello-proxy/--elnode-proxy (http-con)
  "A simple handler to extract the params information and make the request to trello."
  (orgtrello-log/msg 5 "Proxy: Request received. Transmitting...")
  (let* ((query-map-wrapped (orgtrello-proxy/--extract-trello-query http-con))
         (position          (assoc-default 'position query-map-wrapped))
         (buffer-name       (assoc-default 'buffername query-map-wrapped))
         (query-map         (orgtrello-proxy/--compute-trello-query query-map-wrapped))
         (method            (orgtrello-query/--method query-map))
         (fn-dispatch       (orgtrello-proxy/--dispatch-http-query method)))
    ;; Execute the request to trello (at the moment, synchronous)
    (funcall fn-dispatch http-con query-map (list position buffer-name) t)))

(defun orgtrello-proxy/--proxy-handler (http-con)
  "Proxy handler."
  (elnode-hostpath-dispatcher http-con orgtrello-query/--app-routes))

(defun orgtrello-proxy/--start (port host)
  "Starting the proxy."
  (elnode-start 'orgtrello-proxy/--proxy-handler :port port :host host)
  (setq elnode--do-error-logging nil))

(defun orgtrello-proxy/reload ()
  "Reload the proxy server."
  (interactive)
  ;; stop the default port
  (elnode-stop *ORGTRELLO-PROXY-DEFAULT-PORT*)
  ;; update with the new port the user possibly changed
  (setq *ORGTRELLO-PROXY-URL* (format "http://%s:%d/proxy" *ORGTRELLO-PROXY-HOST* *ORGTRELLO-PROXY-PORT*))
  ;; start the proxy
  (orgtrello-proxy/--start *ORGTRELLO-PROXY-PORT* *ORGTRELLO-PROXY-HOST*))

(orgtrello-proxy/reload)

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

(defun orgtrello/reload-setup ()
  "reload orgtrello setup"
  (org-set-regexps-and-options))

(defun orgtrello/--setup-properties ()
  "Setup the properties according to the org-mode setup. Return :ok."
  ;; read the setup
  (orgtrello/reload-setup)
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

(defun orgtrello/--control-properties ()
  "org-trello needs the properties board-id and all list id from the trello board to be setuped on header property file. Returns :ok if everything is ok, or the error message if problems."
  (let ((orgtrello/--hmap-count   (hash-table-count *HMAP-ID-NAME*)))
    (if (and (assoc-default *BOARD-ID* org-file-properties)
             (= (length *LIST-NAMES*) orgtrello/--hmap-count))
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

(defun orgtrello/--label (entity-meta)
  "Retrieve the label from the entity."
  (gethash :title entity-meta))

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
  (let ((orgtrello/--card-name (orgtrello/--label card-meta)))
    (if orgtrello/--card-name
        :ok
      "Cannot synchronize the card - missing mandatory label. Skip it...")))

(defun orgtrello/--card (card-meta &optional parent-meta grandparent-meta)
  "Deal with create/update card query build. If the checks are ko, the error message is returned."
  (let ((checks-ok-or-error-message (orgtrello/--checks-before-sync-card card-meta)))
    ;; title is mandatory
    (if (equal :ok checks-ok-or-error-message)
        ;; parent and grandparent are useless here
        (let* ((orgtrello/--card-kwd  (orgtrello/--retrieve-state-of-card card-meta))
               (orgtrello/--list-id   (assoc-default orgtrello/--card-kwd org-file-properties))
               (orgtrello/--card-id   (orgtrello/--id    card-meta))
               (orgtrello/--card-name (orgtrello/--label card-meta))
               (orgtrello/--card-due  (orgtrello/--due   card-meta)))
          (if orgtrello/--card-id
              ;; update
              (orgtrello-api/move-card orgtrello/--card-id orgtrello/--list-id orgtrello/--card-name orgtrello/--card-due)
            ;; create
            (orgtrello-api/add-card orgtrello/--card-name orgtrello/--list-id orgtrello/--card-due)))
      checks-ok-or-error-message)))

(defun orgtrello/--checks-before-sync-checklist (checklist-meta card-meta)
  "Checks done before synchronizing the checklist."
  (let ((orgtrello/--checklist-name (orgtrello/--label checklist-meta))
        (orgtrello/--card-id        (orgtrello/--id card-meta)))
    (if orgtrello/--checklist-name
        (if orgtrello/--card-id
            :ok
          "Cannot synchronize the checklist - the card must be synchronized first. Skip it...")
      "Cannot synchronize the checklist - missing mandatory label. Skip it...")))

(defun orgtrello/--checklist (checklist-meta &optional card-meta grandparent-meta)
  "Deal with create/update checklist query build. If the checks are ko, the error message is returned."
  (let ((checks-ok-or-error-message (orgtrello/--checks-before-sync-checklist checklist-meta card-meta)))
    ;; title is mandatory
    (if (equal :ok checks-ok-or-error-message)
        ;; grandparent is useless here
        (let* ((orgtrello/--checklist-id   (orgtrello/--id checklist-meta))
               (orgtrello/--card-id        (orgtrello/--id card-meta))
               (orgtrello/--checklist-name (orgtrello/--label checklist-meta)))
          (if orgtrello/--checklist-id
              ;; update
              (orgtrello-api/update-checklist orgtrello/--checklist-id orgtrello/--checklist-name)
            ;; create
            (orgtrello-api/add-checklist orgtrello/--card-id orgtrello/--checklist-name)))
      checks-ok-or-error-message)))

(defun orgtrello/--checks-before-sync-item (task-meta checklist-meta card-meta)
  "Checks done before synchronizing the checklist."
  (let ((orgtrello/--task-name    (orgtrello/--label task-meta))
        (orgtrello/--checklist-id (orgtrello/--id checklist-meta))
        (orgtrello/--card-id      (orgtrello/--id card-meta)))
    (if orgtrello/--task-name
        (if orgtrello/--checklist-id
            (if orgtrello/--card-id
                :ok
              "Cannot synchronize the item - the card must be synchronized first. Skip it...")
          "Cannot synchronize the item - the checklist must be synchronized first. Skip it...")
      "Cannot synchronize the item - missing mandatory label. Skip it...")))

(defun orgtrello/--task-compute-state-or-check (checklist-update-items-p task-state checklist-state possible-states)
  "Compute the task's state/check (for creation/update). The 2 possible states are in the list possible states, first position is the 'checked' one, and second the unchecked one."
  (let* ((orgtrello/--task-checked   (first possible-states))
         (orgtrello/--task-unchecked (second possible-states)))
    (cond ((and checklist-update-items-p (string= *DONE* checklist-state))                      orgtrello/--task-checked)
          ((and checklist-update-items-p (or checklist-state (string= *TODO* checklist-state))) orgtrello/--task-unchecked)
          ((string= *DONE* task-state)                                                          orgtrello/--task-checked)
          (t                                                                                    orgtrello/--task-unchecked))))

(defun orgtrello/--task-compute-state (checklist-update-items-p task-state checklist-state)
  "Compute the task's state (for creation)."
  (orgtrello/--task-compute-state-or-check checklist-update-items-p task-state checklist-state '("complete" "incomplete")))

(defun orgtrello/--task-compute-check (checklist-update-items-p task-state checklist-state)
  "Compute the task's check status (for update)."
    (orgtrello/--task-compute-state-or-check checklist-update-items-p task-state checklist-state '(t nil)))

(defun orgtrello/--compute-state-from-keyword (state)
  "Given a state, compute the org equivalent (to use with org-todo function)"
  (cond ((or (not state) (string= "" state)) *TODO*)
        ((string= *DONE* state)              'done)
        ((string= *TODO* state)              *TODO*)
        (t                                   *TODO*)))

(defun orgtrello/--update-item-according-to-checklist-status (checklist-update-items-p checklist-meta)
  "Update the item of the checklist according to the status of the checklist."
  (if checklist-update-items-p
      (let ((orgtrello/--checklist-status (orgtrello/--compute-state-from-keyword (orgtrello/--keyword checklist-meta))))
        (org-todo orgtrello/--checklist-status))))

(defun orgtrello/--task (task-meta &optional checklist-meta card-meta)
  "Deal with create/update task query build. If the checks are ko, the error message is returned."
  (let ((checks-ok-or-error-message (orgtrello/--checks-before-sync-item task-meta checklist-meta card-meta)))
    ;; title is mandatory
    (if (equal :ok checks-ok-or-error-message)
        ;; card-meta is only usefull for the update part
        (let* ((orgtrello/--task-id      (orgtrello/--id task-meta))
               (orgtrello/--checklist-id (orgtrello/--id checklist-meta))
               (orgtrello/--card-id      (orgtrello/--id card-meta))
               (orgtrello/--task-name    (orgtrello/--label task-meta))
               (orgtrello/--task-state   (orgtrello/--keyword task-meta))
               (orgtrello/--checklist-state    (orgtrello/--keyword checklist-meta)))

          (orgtrello/--update-item-according-to-checklist-status *ORGTRELLO-CHECKLIST-UPDATE-ITEMS* checklist-meta)
          ;; update/create items
          (if orgtrello/--task-id
              ;; update - rename, check or uncheck the task
              (orgtrello-api/update-task orgtrello/--card-id orgtrello/--checklist-id orgtrello/--task-id orgtrello/--task-name (orgtrello/--task-compute-state *ORGTRELLO-CHECKLIST-UPDATE-ITEMS* orgtrello/--task-state orgtrello/--checklist-state))
            ;; create
            (orgtrello-api/add-tasks orgtrello/--checklist-id orgtrello/--task-name (orgtrello/--task-compute-check *ORGTRELLO-CHECKLIST-UPDATE-ITEMS* orgtrello/--task-state orgtrello/--checklist-state))))
      checks-ok-or-error-message)))

(defun orgtrello/--too-deep-level (meta &optional parent-meta grandparent-meta)
  "Deal with too deep level."
  "Your arborescence depth is too deep. We only support up to depth 3.\nLevel 1 - card\nLevel 2 - checklist\nLevel 3 - items/tasks")

(defun orgtrello/--dispatch-map-creation ()
  "Dispatch map for the creation of card/checklist/item."
  (let* ((dispatch-map (make-hash-table :test 'equal)))
    (puthash 1 'orgtrello/--card      dispatch-map)
    (puthash 2 'orgtrello/--checklist dispatch-map)
    (puthash 3 'orgtrello/--task      dispatch-map)
    dispatch-map))

(defvar *MAP-DISPATCH-CREATE-UPDATE* (orgtrello/--dispatch-map-creation) "Dispatch map for the creation/update of card/checklist/task")

(defun orgtrello/--dispatch-create (meta &optional parent-meta grandparent-meta)
  (let* ((level       (orgtrello/--level meta))
         (dispatch-fn (gethash level *MAP-DISPATCH-CREATE-UPDATE* 'orgtrello/--too-deep-level)))
    ;; then execute the call
    (funcall dispatch-fn meta parent-meta grandparent-meta)))

(defun orgtrello/--update-query-with-org-metadata (query-map org-metadata)
  "Given a trello api query, add some metadata needed for org-trello to work (at the moment, only the position)."
  (puthash :position   (orgtrello/--position org-metadata)   query-map)
  (puthash :buffername (orgtrello/--buffername org-metadata) query-map)
  query-map)

(defun orgtrello/do-create-simple-entity (&optional sync)
  "Do the actual simple creation of a card, checklist or task. Optionally, we can render the creation synchronous."
  (let* ((entry-metadata (orgtrello-data/entry-get-full-metadata))
         (current-entry  (gethash :current entry-metadata)))
    (if entry-metadata
        (let ((query-http-or-error-msg (orgtrello/--dispatch-create current-entry (gethash :parent entry-metadata) (gethash :grandparent entry-metadata))))
          (if (hash-table-p query-http-or-error-msg)
              ;; if it's a hash-table we can do the sync
              (progn
                ;; we enrich the trello query with some org/org-trello metadata (the proxy will deal with them later)
                ;; and execute the request
                (orgtrello-proxy/http (orgtrello/--update-query-with-org-metadata query-http-or-error-msg current-entry) sync 'orgtrello-query/--update-entity-id-to-buffer-callback)
                "Synchronizing simple entity done!")
            ;; else it's a string to display
            query-http-or-error-msg)))))

(defun orgtrello/--board-name ()
  "Compute the board's name"
  (assoc-default *BOARD-NAME* org-file-properties))

(defun orgtrello/do-create-complex-entity ()
  "Do the actual full card creation - from card to task. Beware full side effects..."
  (let ((orgtrello/--board-name-to-sync (orgtrello/--board-name)))
    (orgtrello-log/msg 3 "Synchronizing full entity with its structure on board '%s'..." orgtrello/--board-name-to-sync)
    (save-excursion
      ;; iterate over the map of entries and sync them
      (org-map-tree 'orgtrello/do-create-simple-entity))
    (format "Synchronizing full entity with its structure on board '%s' - done" orgtrello/--board-name-to-sync)))

(defun orgtrello/do-sync-full-file ()
  "Full org-mode file synchronisation. Beware, this will block emacs as the request is synchronous."
  (let ((orgtrello/--board-name-to-sync (orgtrello/--board-name)))
    (orgtrello-log/msg 2 "Synchronizing org-mode file to the board '%s'. This may take some time, some coffee may be a good idea..." orgtrello/--board-name-to-sync)
    (org-map-entries 'orgtrello/do-create-simple-entity t 'file)
    (format "Synchronizing org-mode file to the board '%s' - done!" orgtrello/--board-name-to-sync)))

(defun trace (e &optional label)
  "Decorator for some inaccessible code to easily 'message'."
  (progn
    (if label
        (orgtrello-log/msg 5 "TRACE: %s: %S" label e)
        (orgtrello-log/msg 5 "TRACE: %S" e))
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
     (let ((orgtrello/--checklist (orgtrello-query/http-trello (orgtrello-api/get-checklist checklist-id) t)))
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
        (org-sort-entries t ?o))))

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

(cl-defun orgtrello/--sync-buffer-with-trello-data-callback (&key data &allow-other-keys)
  "Synchronize the buffer with the response data."
  (orgtrello-log/msg 5 "Response data: %S" data)
  (let* ((buffer-name                    (buffer-name))
         (orgtrello/--cards              data)
         (orgtrello/--entities-hash-map  (orgtrello/--compute-full-entities-from-trello orgtrello/--cards))
         (orgtrello/--remaining-entities (orgtrello/--sync-buffer-with-trello-data orgtrello/--entities-hash-map buffer-name)))
    (orgtrello/--update-buffer-with-remaining-trello-data orgtrello/--remaining-entities buffer-name)))

(defun orgtrello/do-sync-full-from-trello (&optional sync)
  "Full org-mode file synchronisation. Beware, this will block emacs as the request is synchronous."
  (let ((orgtrello/--board-name-to-sync (orgtrello/--board-name)))
    (orgtrello-log/msg 2 "Synchronizing the trello board '%s' to the org-mode file. This may take a moment, some coffee may be a good idea..." orgtrello/--board-name-to-sync)
    (let* ((orgtrello/--board-id (assoc-default *BOARD-ID* org-file-properties)))
      (orgtrello-query/http-trello (orgtrello-api/get-cards orgtrello/--board-id) sync 'orgtrello/--sync-buffer-with-trello-data-callback))
    (format "Synchronizing the trello board '%s' to the org-mode file - done!" orgtrello/--board-name-to-sync)))

(defun orgtrello/--card-delete (card-meta &optional parent-meta)
  "Deal with the deletion query of a card"
  ;; parent is useless here
  (orgtrello-api/delete-card (orgtrello/--id card-meta)))

(defun orgtrello/--checklist-delete (checklist-meta &optional parent-meta)
  "Deal with the deletion query of a checklist"
  ;; parent is useless here
  (orgtrello-api/delete-checklist (orgtrello/--id checklist-meta)))

(defun orgtrello/--task-delete (task-meta &optional checklist-meta)
  "Deal with create/update task query build"
  (let* ((orgtrello/--task-id      (orgtrello/--id task-meta))
         (orgtrello/--checklist-id (orgtrello/--id checklist-meta)))
    (orgtrello-api/delete-task orgtrello/--checklist-id orgtrello/--task-id)))

(defun orgtrello/--dispatch-map-delete ()
  "Dispatch map for the deletion of card/checklist/item."
  (let* ((dispatch-map (make-hash-table :test 'equal)))
    (puthash 1 'orgtrello/--card-delete      dispatch-map)
    (puthash 2 'orgtrello/--checklist-delete dispatch-map)
    (puthash 3 'orgtrello/--task-delete      dispatch-map)
    dispatch-map))

(defvar *MAP-DISPATCH-DELETE* (orgtrello/--dispatch-map-delete) "Dispatch map for the deletion query of card/checklist/task.")

(defun orgtrello/--dispatch-delete (meta &optional parent-meta)
  (let* ((level       (orgtrello/--level meta))
         (dispatch-fn (gethash level *MAP-DISPATCH-DELETE* 'orgtrello/--too-deep-level)))
    (funcall dispatch-fn meta parent-meta)))

(defun orgtrello/do-delete-simple (&optional sync)
  "Do the simple deletion of a card, checklist or task."
  (let* ((entry-metadata   (orgtrello-data/entry-get-full-metadata))
         (current-metadata (gethash :current entry-metadata))
         (id               (orgtrello/--id current-metadata)))
    (if (and current-metadata id)
        (let ((query-http-or-error-msg (orgtrello/--dispatch-delete current-metadata (gethash :parent entry-metadata))))
          (if (hash-table-p query-http-or-error-msg)
              (progn
                (orgtrello-proxy/http
                 (orgtrello/--update-query-with-org-metadata query-http-or-error-msg current-metadata)
                 sync
                 'orgtrello-query/--delete-success-callback)
                "Delete entity done!")
            query-http-or-error-msg))
      "Entity not synchronized on trello yet!")))

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
   (orgtrello-query/http-trello (orgtrello-api/get-boards) t)))

(defun orgtrello/--list-board-lists (board-id)
  "Return the map of the existing list of the board with id board-id. (Synchronous request)"
  (orgtrello-query/http-trello (orgtrello-api/get-lists board-id) t))

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

;; (defun orgtrello/remove-properties-file ()
;;   "to debug"
;;   (interactive)
;;   (orgtrello/--remove-properties-file *LIST-NAMES* t))

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
    (orgtrello/reload-setup)))

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
    (let* ((board-data (orgtrello-query/http-trello (orgtrello-api/add-board board-name board-description) t)))
      (list (orgtrello-query/--id board-data) (orgtrello-query/--name board-data)))))

(defun orgtrello/--close-lists (list-ids)
  "Given a list of ids, close those lists."
  (mapc (lambda (list-id)
          (progn
            (orgtrello-log/msg 3 "Closing default list with id %s" list-id)
            (orgtrello-query/http-trello (orgtrello-api/close-list list-id) nil nil 'simple-error-callback)))
        list-ids))

(defun orgtrello/--create-lists-according-to-keywords (board-id list-keywords)
  "Given a list of names, build those lists on the trello boards. Return the hashmap (name, id) of the new lists created."
  (cl-reduce
   (lambda (acc-hash-name-id list-name)
     (progn
       (orgtrello-log/msg 3 "Board id %s - Creating list '%s'" board-id list-name)
       (puthash list-name (orgtrello-query/--id (orgtrello-query/http-trello (orgtrello-api/add-list list-name board-id) t)) acc-hash-name-id)
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

(defun org-trello/--msg-deco-control-and-do (msg control-fns fn-to-control-and-execute &optional save-buffer-p)
  "A simple decorator function to display message in mini-buffer before and after the execution of the control"
  (orgtrello-log/msg 3 (concat msg "..."))
  (let ((org-trello/--result-action (org-trello/--control-and-do control-fns fn-to-control-and-execute)))
    ;; do we have to save the buffer
    (when save-buffer-p
        (progn
          ;; save the buffer
          (save-buffer)
          ;; reload setup
          (orgtrello/reload-setup)))
    (if (string-or-null-p org-trello/--result-action)
      (orgtrello-log/msg 3 org-trello/--result-action)
      (orgtrello-log/msg 3 (concat msg " - done!")))))

(defun org-trello/--control-and-do (control-fns fn-to-control-and-execute)
  "Execute the function fn if control-fns is nil or if the result of apply every function to fn is ok."
  (if control-fns
      (let* ((org-trello/--error-messages (--filter (not (equal :ok (funcall it))) control-fns)))
        (if org-trello/--error-messages
            ;; there are some trouble, we display all the error messages to help the user understand the problem
            (orgtrello-log/msg 1 "List of errors:\n %s" (--mapcat (concat "- " it "\n") org-trello/--error-messages))
          ;; ok execute the function as the controls are ok
          (funcall fn-to-control-and-execute)))
    ;; no control, we simply execute the function
    (funcall fn-to-control-and-execute)))

(defun org-trello/create-simple-entity ()
  "Control first, then if ok, create a simple entity."
  (interactive)
  (org-trello/--msg-deco-control-and-do
     "Synchronizing entity"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     'orgtrello/do-create-simple-entity
     t))

(defun org-trello/create-complex-entity ()
  "Control first, then if ok, create an entity and all its arborescence if need be."
  (interactive)
  (org-trello/--msg-deco-control-and-do
     "Synchronizing complex entity"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     'orgtrello/do-create-complex-entity
     t))

(defun org-trello/sync-to-trello ()
  "Control first, then if ok, sync the org-mode file completely to trello."
  (interactive)
  (org-trello/--msg-deco-control-and-do
     "Synchronizing org-mode file to trello"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     'orgtrello/do-sync-full-file
     t))

(defun org-trello/sync-from-trello ()
  "Control first, then if ok, sync the org-mode file from the trello board."
  (interactive)
  ;; as this action will write on the buffer, we need to remove the hook
  (remove-hook 'after-change-functions 'org-trello/--create-entity-when-writing)
  ;; execute the action
  (org-trello/--msg-deco-control-and-do
     "Synchronizing trello board to org-mode file"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     (lambda () (orgtrello/do-sync-full-from-trello t))
     t)
  ;; add the hook again
  (add-hook 'after-change-functions 'org-trello/--create-entity-when-writing))

(defun org-trello/kill-entity ()
  "Control first, then if ok, delete the entity and all its arborescence."
  (interactive)
  (org-trello/--msg-deco-control-and-do
     "Delete entity"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     'orgtrello/do-delete-simple
     t))

(defun org-trello/install-key-and-token ()
  "No control, trigger the setup installation of the key and the read/write token."
  (interactive)
  (org-trello/--msg-deco-control-and-do "Setup key and token" nil 'orgtrello/do-install-key-and-token t))

(defun org-trello/install-board-and-lists-ids ()
  "Control first, then if ok, trigger the setup installation of the trello board to sync with."
  (interactive)
  (org-trello/--msg-deco-control-and-do
     "Install boards and lists"
     '(orgtrello/--setup-properties orgtrello/--control-keys)
     'orgtrello/do-install-board-and-lists
     t))

(defun org-trello/create-board ()
  "Control first, then if ok, trigger the board creation."
  (interactive)
  (org-trello/--msg-deco-control-and-do
     "Create board and lists"
     '(orgtrello/--setup-properties orgtrello/--control-keys)
     'orgtrello/do-create-board-and-lists
     t))

(defun org-trello/check-setup ()
  "Check the current setup."
  (interactive)
  (org-trello/--control-and-do
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     (lambda () (orgtrello-log/msg 0 "Setup ok!"))))

(defun org-trello/delete-setup ()
  "Delete the current setup."
  (interactive)
  (org-trello/--control-and-do
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     (lambda ()
       ;; remove any orgtrello relative entries
       (orgtrello/--remove-properties-file *LIST-NAMES* t)
       ;; remove any identifier from the buffer
       (org-delete-property-globally *ORGTRELLO-ID*)
       ;; a simple message to tell the client that the work is done!
       (orgtrello-log/msg 0 "Cleanup done!"))))

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
C-c o k - M-x org-trello/kill-entity                 - Kill the entity (and its arborescence tree).
# HELP
C-c o h - M-x org-trello/help-describing-bindings    - This help message."))

(defun org-trello/--trigger-create-p (s)
  "A predicate function to determine if we trigger the creation/update of an entity"
  (when (stringp s) (string-match-p "^[\*]+ .+\n$" s)))

(defun org-trello/--create-entity-when-writing (beg end len)
  (when org-trello-mode
      (let ((org-trello/--potential-entity (thing-at-point 'line)))
        (unless (orgtrello-data/extract-identifier (point))
                (orgtrello-log/msg 5 "line: '%s'" org-trello/--potential-entity)
                (when (org-trello/--trigger-create-p org-trello/--potential-entity)
                      (save-excursion
                        (beginning-of-line)
                        (org-trello/create-simple-entity)))))))

(defun org-trello/--prepare-org-trello-mode ()
  "Preparing org-trello mode"
  (if org-trello-mode
      (progn
;;        (if auto-complete-mode (auto-complete 0))
        (add-hook 'after-change-functions 'org-trello/--create-entity-when-writing)
        (orgtrello-log/msg 0 "org-trello/ot is on! To begin with, hit C-c o h or M-x 'org-trello/help-describing-bindings"))
      (progn
        (remove-hook 'after-change-functions 'org-trello/--create-entity-when-writing)
        (orgtrello-log/msg 0 "org-trello/ot is off!"))))

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
             (define-key map (kbd "C-c o k") 'org-trello/kill-entity)
             (define-key map (kbd "C-c o s") 'org-trello/sync-to-trello)
             (define-key map (kbd "C-c o C") 'org-trello/create-complex-entity)
             ;; Help
             (define-key map (kbd "C-c o h") 'org-trello/help-describing-bindings)
             map)
  :after-hook (org-trello/--prepare-org-trello-mode))

(orgtrello-log/msg 4 "org-trello loaded!")

(provide 'org-trello)

;;; org-trello.el ends here
