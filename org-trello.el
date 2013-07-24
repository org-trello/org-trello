;;; org-trello.el --- Minor mode for org-mode to sync org-mode and trello

;; Copyright (C) 2013 Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.0.6
;; Package-Requires: ((org "7.9.2") (dash "1.4.0") (request "0.1.0") (cl-lib "0.3.0") (json "1.3"))
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
;;
;; Automatically
;; 2) Once - Install the consumer-key and the read-write token for org-trello to be able to work in your name with your trello boards (C-c o i)
;; M-x org-trello/install-key-and-token
;;
;; 3) Once per org-mode file/board you want to connect to (C-c o I)
;; M-x org-trello/install-board-and-lists-ids
;;
;; *Beware* you must setup your trello board with the name you use as keywords (TODO, DONE e.g) on your org-mode file.
;;
;; 4) You can also create a board directly from a org-mode buffer (C-c o b)
;; M-x org-trello/create-board
;;
;;; Code:

(require 'org)
(require 'json)
(require 'dash)
(require 'request)
(eval-when-compile (require 'cl-lib))

;; #################### orgtrello-hash

(defun orgtrello-hash/make-hash-org (level keyword title id)
  "Utility function to ease the creation of the orgtrello-metadata"
  (let ((h (make-hash-table :test 'equal)))
    (puthash :level   level   h)
    (puthash :keyword keyword h)
    (puthash :title   title   h)
    (puthash :id      id      h)
    h))

(defun orgtrello-hash/make-hash (method uri &optional params)
  "Utility function to ease the creation of the map - wait, where are my clojure data again!?"
  (let ((h (make-hash-table :test 'equal)))
    (puthash :method method h)
    (puthash :uri    uri    h)
    (if params (puthash :params params h))
    h))

(message "orgtrello-hash loaded!")

;; #################### orgtrello-data

(defvar *ORGTRELLO-ID* "orgtrello-id" "Marker used for the trello identifier")

(defun orgtrello-data/metadata ()
  "Compute the metadata from the org-heading-components entry, add the identifier and extract the metadata needed."
  (let* ((id           (org-entry-get (point) *ORGTRELLO-ID*))
         (org-metadata (org-heading-components)))
    (->> org-metadata
         (cons id)
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
  (cl-destructuring-bind (id level _ keyword _ title &rest) heading-metadata
                         (orgtrello-hash/make-hash-org level keyword title id)))

(message "orgtrello-data loaded!")

;; #################### orgtrello-api

(defun orgtrello-api/add-board (name &optional description)
  "Create a board"
  (let* ((payload (if description
                      `(("name" . ,name)
                        ("desc" . ,description))
                    `(("name" . ,name)))))
    (orgtrello-hash/make-hash :post "/boards" payload)))

(defun orgtrello-api/get-boards ()
  "Retrieve the boards of the current user."
  (orgtrello-hash/make-hash :get "/members/me/boards"))

(defun orgtrello-api/get-board (id)
  "Retrieve the boards of the current user."
  (orgtrello-hash/make-hash :get (format "/boards/%s" id)))

(defun orgtrello-api/get-cards (board-id)
  "cards of a board"
  (orgtrello-hash/make-hash :get (format "/boards/%s/cards" board-id)))

(defun orgtrello-api/get-card (card-id)
  "Detail of a card with id card-id."
  (orgtrello-hash/make-hash :get (format "/cards/%s" card-id)))

(defun orgtrello-api/delete-card (card-id)
  "Delete a card with id card-id."
  (orgtrello-hash/make-hash :delete (format "/cards/%s" card-id)))

(defun orgtrello-api/get-lists (board-id)
  "Display the lists of the board"
  (orgtrello-hash/make-hash :get (format "/boards/%s/lists" board-id)))

(defun orgtrello-api/close-list (list-id)
  "'Close' the list with id list-id."
  (orgtrello-hash/make-hash :put (format "/lists/%s/closed" list-id) '((value . t))))

(defun orgtrello-api/get-list (list-id)
  "Get a list by id"
  (orgtrello-hash/make-hash :get (format "/lists/%s" list-id)))

(defun orgtrello-api/add-list (name idBoard)
  "Add a list - the name and the board id are mandatory (so i say!)."
  (orgtrello-hash/make-hash :post "/lists/" `(("name" . ,name) ("idBoard" . ,idBoard))))

(defun orgtrello-api/add-card (name idList)
  "Add a card to a board"
  (orgtrello-hash/make-hash :post "/cards/" `(("name" . ,name) ("idList" . ,idList))))

(defun orgtrello-api/get-cards-from-list (list-id)
  "List all the cards"
  (orgtrello-hash/make-hash :get (format "/lists/%s/cards" list-id)))

(defun orgtrello-api/move-card (card-id idList &optional name)
  "Move a card to another list"
  (let ((orgtrello-api/move-card-data (if name
                                           `(("name"   . ,name)
                                             ("idList" . ,idList))
                                         `(("idList" . ,idList)))))
    (orgtrello-hash/make-hash :put (format "/cards/%s" card-id) orgtrello-api/move-card-data)))

(defun orgtrello-api/add-checklist (card-id name)
  "Add a checklist to a card"
  (orgtrello-hash/make-hash :post
             (format "/cards/%s/checklists" card-id)
             `(("name" . ,name))))

(defun orgtrello-api/update-checklist (checklist-id name)
  "Update the checklist's name"
  (orgtrello-hash/make-hash :put
             (format "/checklists/%s" checklist-id)
             `(("name" . ,name))))

(defun orgtrello-api/get-checklists (card-id)
  "List the checklists of a card"
  (orgtrello-hash/make-hash :get (format "/cards/%s/checklists" card-id)))

(defun orgtrello-api/get-checklist (checklist-id)
  "Retrieve all the information from a checklist"
  (orgtrello-hash/make-hash :get (format "/checklists/%s" checklist-id)))

(defun orgtrello-api/delete-checklist (checklist-id)
  "Delete a checklist with checklist-id"
  (orgtrello-hash/make-hash :delete (format "/checklists/%s" checklist-id)))

(defun orgtrello-api/add-tasks (checklist-id name &optional checked)
  "Add todo tasks (trello items) to a checklist with id 'id'"
  (let* ((payload (if checked
                      `(("name"  . ,name) ("checked" . ,checked))
                    `(("name" . ,name)))))
    (orgtrello-hash/make-hash :post (format "/checklists/%s/checkItems" checklist-id) payload)))

(defun orgtrello-api/update-task (card-id checklist-id task-id name &optional state)
  "Update a task"
  (let* ((payload (if state
                      `(("name"  . ,name) ("state" . ,state))
                    `(("name" . ,name)))))
    (orgtrello-hash/make-hash
     :put
     (format "/cards/%s/checklist/%s/checkItem/%s" card-id checklist-id task-id)
     payload)))

(defun orgtrello-api/get-tasks (checklist-id)
  "List the checklist items."
    (orgtrello-hash/make-hash :get (format "/checklists/%s/checkItems/" checklist-id)))

(defun orgtrello-api/delete-task (checklist-id task-id)
  "Delete a task with id task-id"
  (orgtrello-hash/make-hash :delete (format "/checklists/%s/checkItems/%s" checklist-id task-id)))

(message "orgtrello-api loaded!")

;; #################### orgtrello-query/

(defvar *TRELLO-URL* "https://api.trello.com/1" "The needed prefix url for trello")

(defun orgtrello-query/--make-dispatch-http-query ()
  "Make a map that will dispatch the function to call depending on the http verb :get, :put, :post, etc..."
  (let* ((map-dispatch (make-hash-table :test 'equal)))
    (puthash :get    'orgtrello-query/--get         map-dispatch)
    (puthash :put    'orgtrello-query/--post-or-put map-dispatch)
    (puthash :post   'orgtrello-query/--post-or-put map-dispatch)
    (puthash :delete 'orgtrello-query/--delete      map-dispatch)
    map-dispatch))

(defvar *MAP-DISPATCH-HTTP-QUERY* (orgtrello-query/--make-dispatch-http-query))

(defun orgtrello-query/http (query-map &optional success-callback error-callback)
  "Query the trello api asynchronously."
  (let* ((method      (gethash :method query-map))
         (fn-dispatch (gethash method *MAP-DISPATCH-HTTP-QUERY*)))
    (funcall fn-dispatch query-map success-callback error-callback)))

(defun orgtrello-query/http-sync (query-map &optional success-callback error-callback)
  "Query the trello api synchronously and return the data of the request."
  (let* ((method      (gethash :method query-map))
         (fn-dispatch (gethash method *MAP-DISPATCH-HTTP-QUERY*)))
    (puthash :sync t query-map)
    (let ((request-response (funcall fn-dispatch query-map success-callback error-callback)))
      (request-response-data request-response))))

(defun orgtrello-query/--map-dispatch-http-verb ()
  (let* ((map-dispatch (make-hash-table :test 'equal)))
    (puthash :get    "GET"    map-dispatch)
    (puthash :put    "PUT"    map-dispatch)
    (puthash :post   "POST"   map-dispatch)
    (puthash :delete "DELETE" map-dispatch)
    map-dispatch))

(defvar *MAP-DISPATCH-HTTP-VERB* (orgtrello-query/--map-dispatch-http-verb))

(defun orgtrello-query/--compute-method (method)
  "Given the keywords :get, :post, :put, :delete, map them into standard uppercase string."
  (gethash method *MAP-DISPATCH-HTTP-VERB*))

(defun orgtrello-query/--compute-url (uri)
  "Compute the trello url from the given uri."
  (format "%s%s" *TRELLO-URL* uri))

(cl-defun error-callback-and-action (&key error-thrown &allow-other-keys)
  "Standard error callback"
  (save-excursion
      ;; find the current entry through the pointer
      (org-goto-local-search-headings *ORGTRELLO-MARKER* nil t)
      ;; remove the marker now that we're done
      (org-delete-property *ORGTRELLO-MARKER*))
  (message "There was some problem during the request to trello: %s" error-thrown))

(cl-defun standard-success-callback ()
  "Standard success callback"
  (message "Success."))

(cl-defun standard-error-callback (&key error-thrown &allow-other-keys)
  "Standard success callback"
  (message "There was some problem during the request to trello: %s" error-thrown))

(cl-defun standard-success-callback-display (&key data &allow-other-keys)
  "Standard success callback"
  (message "data: %S" data))

(defun orgtrello-query/--get (query-map &optional success-callback error-callback)
  "GET"
  (let* ((method (gethash :method query-map))
         (uri    (gethash :uri    query-map))
         (sync   (gethash :sync   query-map)))
    (request (orgtrello-query/--compute-url uri)
             :sync    sync
             :type    (orgtrello-query/--compute-method method)
             :params  `((key . ,*consumer-key*)
                        (token . ,*access-token*))
             :parser  'json-read
             :success (if success-callback success-callback 'standard-success-callback)
             :error   (if error-callback error-callback 'error-callback-and-action))))

(cl-defun orgtrello-query/--post-put-success-callback-update-id (&key data &allow-other-keys)
  "Called back function at the end of the post/put request to update the trello id in the org-mode file."
  (let* ((orgtrello-query/--entry-new-id (assoc-default 'id data))
         (orgtrello-query/--entry-name   (assoc-default 'name data)))
    ;; will update via tag the trello id of the new persisted data (if needed)
    (save-excursion
      ;;(while (org-up-heading-safe))
      ;; find the current entry through the pointer
      (org-goto-local-search-headings *ORGTRELLO-MARKER* nil t)
      ;; remove the marker now that we're done
      (org-delete-property *ORGTRELLO-MARKER*)
      ;; now we extract the data
      (let* ((orgtrello-query/--entry-metadata (orgtrello-data/metadata))
             (orgtrello-query/--entry-id       (gethash :id orgtrello-query/--entry-metadata)))
        (if orgtrello-query/--entry-id ;; id already present in the org-mode file
            ;; no need to add another
            (message "Entity '%s' synced with id '%s'" orgtrello-query/--entry-name orgtrello-query/--entry-id)
          (progn
            ;; not present, this was just created, we add a simple property
            (org-set-property *ORGTRELLO-ID* orgtrello-query/--entry-new-id)
            (message "Newly entity '%s' synced with id '%s'" orgtrello-query/--entry-name orgtrello-query/--entry-new-id)))))))

(defun orgtrello-query/--post-or-put (query-map &optional success-callback error-callback)
  "POST or PUT"
  (let* ((method  (gethash :method query-map))
         (uri     (gethash :uri    query-map))
         (payload (gethash :params query-map))
         (sync    (gethash :sync   query-map)))
    (request (orgtrello-query/--compute-url uri)
             :sync    sync
             :type    (orgtrello-query/--compute-method method)
             :params  `((key . ,*consumer-key*)
                        (token . ,*access-token*))
             :headers '(("Content-type" . "application/json"))
             :data    (json-encode payload)
             :parser  'json-read
             :success (if success-callback success-callback 'standard-success-callback)
             :error   (if error-callback error-callback 'error-callback-and-action))))

(cl-defun orgtrello-query/--delete-success-callback (&key data response &allow-other-keys)
  "Callback function called at the end of a successful delete request."
  (progn (org-delete-property *ORGTRELLO-ID*)
         (hide-subtree)
         (kill-line)
         (kill-line)
         (message "Entity deleted!")))

(defun orgtrello-query/--delete (query-map &optional success-callback error-callback)
  "DELETE"
  (let* ((method (gethash :method query-map))
         (uri    (gethash :uri    query-map))
         (sync   (gethash :sync   query-map)))
    (request (orgtrello-query/--compute-url uri)
             :sync    sync
             :type    (orgtrello-query/--compute-method method)
             :params  `((key . ,*consumer-key*)
                        (token . ,*access-token*))
             :success (if success-callback success-callback 'standard-success-callback)
             :error   (if error-callback error-callback 'error-callback-and-action))))

(message "orgtrello-query/ loaded!")

;; #################### orgtrello

;; Specific state - FIXME check if they do not already exist on org-mode to avoid potential collisions
(defvar *TODO* "TODO" "org-mode todo state")
(defvar *DONE* "DONE" "org-mode done state")

;; Properties key for the orgtrello headers #+PROPERTY board-id, etc...
(defvar *BOARD-ID* "board-id" "orgtrello property board-id entry")
(defvar *BOARD-NAME* "board-name" "orgtrello property board-name entry")

(defun orgtrello/filtered-kwds ()
  "org keywords used (based on org-todo-keywords-1)."
  org-todo-keywords-1)

(defvar *LIST-NAMES*   nil "orgtrello property names of the different lists. This use the standard 'org-todo-keywords property from org-mode.")
(defvar *HMAP-ID-NAME* nil "orgtrello hash map containing for each id, the associated name (or org keyword).")

(defvar *CONFIG-DIR*  (concat (getenv "HOME") "/" ".trello"))
(defvar *CONFIG-FILE* (concat *CONFIG-DIR* "/config.el"))

(defvar *consumer-key*     nil "Id representing the user")
(defvar *access-token*     nil "Read/write Access token to use trello in the user's name ")
(defvar *ORGTRELLO-MARKER* nil "Marker used for syncing the data in trello")

(defun orgtrello/--setup-properties ()
  "Setup the properties according to the org-mode setup"
  (let* ((orgtrello/--list-keywords (orgtrello/filtered-kwds))
         (orgtrello/--hmap-id-name (cl-reduce
                                    (lambda (hmap name)
                                      (progn
                                        (puthash (assoc-default name org-file-properties) name hmap)
                                        hmap))
                                    orgtrello/--list-keywords
                                    :initial-value (make-hash-table :test 'equal))))
    (setq *LIST-NAMES*   orgtrello/--list-keywords)
    (setq *HMAP-ID-NAME* orgtrello/--hmap-id-name)
    t))

(defun orgtrello/--control-properties ()
  "org-trello needs the properties board-id and all list id from the trello board to be setuped on header property file."
  (let ((orgtrello/--hmap-count   (cl-hash-table-count *HMAP-ID-NAME*)))
    (and (assoc-default *BOARD-ID* org-file-properties)
         (= (length *LIST-NAMES*) orgtrello/--hmap-count))))

(defun orgtrello/--control-keys ()
  "org-trello needs the *consumer-key* and the *access-token* to access the trello resources. Return t if everything is ok."
  (or (and *consumer-key* *access-token*)
      ;; the data are not set,
      (and (file-exists-p *CONFIG-FILE*)
           ;; trying to load them
           (load *CONFIG-FILE*)
           ;; still not loaded, something is not right!
           (and *consumer-key* *access-token*)
           ;; setting the marker once
           (setq *ORGTRELLO-MARKER* (format "orgtrello-marker-%s" *consumer-key*)))))

(defun orgtrello/--retrieve-state-of-card (card-meta)
  "Given a card, retrieve its state depending on its :keyword metadata. If empty or no keyword then, its equivalence is *TODO*, otherwise, return its current state."
  (let* ((orgtrello/--card-kwd (gethash :keyword card-meta *TODO*)))
    (if orgtrello/--card-kwd orgtrello/--card-kwd *TODO*)))

(defun orgtrello/--checks-before-sync-card (card-meta)
  "Checks done before synchronizing the cards."
  (let ((orgtrello/--card-name (gethash :title card-meta)))
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
               (orgtrello/--card-id   (gethash :id    card-meta))
               (orgtrello/--card-name (gethash :title card-meta)))
          (if orgtrello/--card-id
              ;; update
              (orgtrello-api/move-card orgtrello/--card-id orgtrello/--list-id orgtrello/--card-name)
            ;; create
            (orgtrello-api/add-card orgtrello/--card-name orgtrello/--list-id)))
      checks-ok-or-error-message)))

(defun orgtrello/--checks-before-sync-checklist (checklist-meta card-meta)
  "Checks done before synchronizing the checklist."
  (let ((orgtrello/--checklist-name (gethash :title checklist-meta))
        (orgtrello/--card-id        (gethash :id card-meta)))
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
        (let* ((orgtrello/--checklist-id   (gethash :id checklist-meta))
               (orgtrello/--card-id        (gethash :id card-meta))
               (orgtrello/--checklist-name (gethash :title checklist-meta)))
          (if orgtrello/--checklist-id
              ;; update
              (orgtrello-api/update-checklist orgtrello/--checklist-id orgtrello/--checklist-name)
            ;; create
            (orgtrello-api/add-checklist orgtrello/--card-id orgtrello/--checklist-name)))
      checks-ok-or-error-message)))

(defun orgtrello/--checks-before-sync-item (task-meta checklist-meta card-meta)
  "Checks done before synchronizing the checklist."
  (let ((orgtrello/--task-name    (gethash :title task-meta))
        (orgtrello/--checklist-id (gethash :id checklist-meta))
        (orgtrello/--card-id      (gethash :id card-meta)))
    (if orgtrello/--task-name
        (if orgtrello/--checklist-id
            (if orgtrello/--card-id
                :ok
              "Cannot synchronize the item - the card must be synchronized first. Skip it...")
          "Cannot synchronize the item - the checklist must be synchronized first. Skip it...")
      "Cannot synchronize the item - missing mandatory label. Skip it...")))

(defun orgtrello/--task (task-meta &optional checklist-meta card-meta)
  "Deal with create/update task query build. If the checks are ko, the error message is returned."
  (let ((checks-ok-or-error-message (orgtrello/--checks-before-sync-item task-meta checklist-meta card-meta)))
    ;; title is mandatory
    (if (equal :ok checks-ok-or-error-message)
        ;; card-meta is only usefull for the update part
        (let* ((orgtrello/--task-id      (gethash :id task-meta))
               (orgtrello/--checklist-id (gethash :id checklist-meta))
               (orgtrello/--card-id      (gethash :id card-meta))
               (orgtrello/--task-name    (gethash :title task-meta))
               ;; FIXME - the trello api is strange - extract those calls into function
               (orgtrello/--task-state   (if (string= *DONE* (gethash :keyword task-meta)) "complete" "incomplete")) ;; update api call
               (orgtrello/--task-check   (if (string= *DONE* (gethash :keyword task-meta)) 't nil))) ;; create api call
          (if orgtrello/--task-id
              ;; update - rename, check or uncheck the task
              (orgtrello-api/update-task orgtrello/--card-id orgtrello/--checklist-id orgtrello/--task-id orgtrello/--task-name orgtrello/--task-state)
            ;; create
            (orgtrello-api/add-tasks orgtrello/--checklist-id orgtrello/--task-name orgtrello/--task-check)))
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

(defun orgtrello/--set-marker ()
  "Set the consumer-key to make a pointer to get back to when the request is finished"
  (org-set-property *ORGTRELLO-MARKER* *ORGTRELLO-MARKER*))

(defun orgtrello/--dispatch-create (meta &optional parent-meta grandparent-meta)
  (let* ((level       (gethash :level meta))
         (dispatch-fn (gethash level *MAP-DISPATCH-CREATE-UPDATE* 'orgtrello/--too-deep-level)))
    ;; then execute the call
    (funcall dispatch-fn meta parent-meta grandparent-meta)))

(defun orgtrello/do-create-simple-entity (&optional sync)
  "Do the actual simple creation of a card, checklist or task. Optionally, we can render the creation synchronous."
  (let ((entry-metadata (orgtrello-data/entry-get-full-metadata)))
    (if entry-metadata
        (let ((query-http (orgtrello/--dispatch-create (gethash :current entry-metadata) (gethash :parent entry-metadata) (gethash :grandparent entry-metadata))))
          (if (hash-table-p query-http)
              ;; if it's a hash-table we can do the sync
              (progn
                ;; set the consumer-key to make a pointer to get back to when the request is finished
                (orgtrello/--set-marker)
                ;; is the request synchroneous or not?
                (if sync
                    ;; synchroneous request
                    (orgtrello-query/http-sync query-http 'orgtrello-query/--post-put-success-callback-update-id)
                  ;; asynchroneous one
                  (orgtrello-query/http query-http 'orgtrello-query/--post-put-success-callback-update-id)))
            ;; else it's a string to display
            (message query-http))
          ;; query-http returns nil so we cannot do any sync, the only reason possible is that some mandatory field (for example, the entity label is missing)
          ))))

(defun orgtrello/--merge-map (entry map-ids-by-name)
  "Given a map of (id . name) and an entry, return the entry updated with the id if not already present."
  (let* ((orgtrello/--merge-map-id   (gethash :id entry))
         (orgtrello/--merge-map-name (gethash :title entry)))
    (if orgtrello/--merge-map-id
        ;; already identified, return the entry without any modification
        entry
      ;; not present, we add the entry :id with its value and return such value
      (progn
        (puthash :id (gethash orgtrello/--merge-map-name map-ids-by-name) entry)
        entry))))

(defun orgtrello/--board-name ()
  "Compute the board's name"
  (assoc-default *BOARD-NAME* org-file-properties))

(defun orgtrello/do-create-complex-entity ()
  "Do the actual full card creation - from card to task. Beware full side effects..."
  (message "Synchronizing full card structure on board '%s'..." (orgtrello/--board-name))
  (save-excursion
    ;; up to the highest level to begin the sync in order
    (while (org-up-heading-safe))
    ;; iterate over the map of
    (org-map-tree (lambda () (orgtrello/do-create-simple-entity t)))))

(defun orgtrello/do-sync-full-file ()
  "Full org-mode file synchronisation. Beware, this will block emacs as the request is synchronous."
  (message "Synchronizing org-mode file to the board '%s'. This may take some time, some coffee may be a good idea..." (orgtrello/--board-name))
  (org-map-entries (lambda () (orgtrello/do-create-simple-entity t)) t 'file))

(defun trace (e &optional label)
  "Decorator for some inaccessible code to easily 'message'."
  (progn
    (if label
        (message "TRACE: %s: %S" label e)
        (message "TRACE: %S" e))
    e))

(defun orgtrello/--compute-card-status (card-id-list )
  "Given a card's id, compute its status."
  (gethash card-id-list *HMAP-ID-NAME*))

(defun orgtrello/--compute-card-to-org-entry (card)
  "Given a card, compute its org-mode entry equivalence."
  (let* ((orgtrello/--card-name                                    (assoc-default 'name card))
         (orgtrello/--card-status (orgtrello/--compute-card-status (assoc-default 'idList card))))
    (format "* %s %s\n" orgtrello/--card-status orgtrello/--card-name)))

(defun orgtrello/--compute-checklist-to-org-entry (checklist)
  "Given a checklist, compute its org-mode entry equivalence."
  (let ((orgtrello/--checklist-name  (assoc-default 'name checklist)))
    (format "** %s\n" orgtrello/--checklist-name)))

(defun orgtrello/--compute-item-to-org-entry (item)
  "Given a checklist item, compute its org-mode entry equivalence."
  (let* ((orgtrello/--item-name  (assoc-default 'name  item))
         (orgtrello/--item-state (assoc-default 'state item)))
    (format "*** %s %s\n"
            (if (string= "complete" orgtrello/--item-state) "DONE" "TODO")
            orgtrello/--item-name)))

(defun orgtrello/--compute-entity-to-org-entry (entity)
  "Given an entity, compute its org representation."
  (cond ((assoc-default 'idList entity) (orgtrello/--compute-card-to-org-entry entity))           ;; card      (level 1)
        ((assoc-default 'idCard entity) (orgtrello/--compute-checklist-to-org-entry entity))      ;; checklist (level 2)
        ((assoc-default 'state entity)  (orgtrello/--compute-item-to-org-entry entity))))         ;; items     (level 3)

(defun orgtrello/--do-retrieve-checklists-from-card (card)
  "Given a card, return the list containing the card, the checklists from this card, and the items from the checklists. The order is guaranted."
  (cl-reduce
   (lambda (acc-list checklist-id)
     (let ((orgtrello/--checklist (orgtrello-query/http-sync (orgtrello-api/get-checklist checklist-id))))
       (append (cons orgtrello/--checklist (orgtrello/--do-retrieve-checklists-and-items orgtrello/--checklist)) acc-list)))
   (assoc-default 'idChecklists card)
   :initial-value nil))

(defun orgtrello/--do-retrieve-checklists-and-items (checklist)
  "Given a checklist id, retrieve all the items from the checklist and return a list containing first the checklist, then the items."
  (--map it (assoc-default 'checkItems checklist)))

(defun orgtrello/--compute-full-entities-from-trello (cards)
  "Given a list of cards, compute the full cards data from the trello boards"
  ;; will compute the hash-table from such list
  (cl-reduce
   (lambda (orgtrello/--acc-hash orgtrello/--entity)
     (puthash (assoc-default 'id orgtrello/--entity) orgtrello/--entity orgtrello/--acc-hash)
     orgtrello/--acc-hash)
   ;; will compute the full list of entities
   (cl-reduce
    (lambda (orgtrello/--acc-list orgtrello/--entity-card)
      (message "Computing card '%s' data..." (assoc-default 'name orgtrello/--entity-card))
      (append (cons orgtrello/--entity-card (orgtrello/--do-retrieve-checklists-from-card orgtrello/--entity-card)) orgtrello/--acc-list))
    cards
    :initial-value nil)
   :initial-value (make-hash-table :test 'equal)))

(defun orgtrello/--update-buffer-with-remaining-trello-data (entities)
  "Given a map of entities, dump those entities in the current buffer."
  (if entities ;; could be empty
      (with-current-buffer (current-buffer)
        ;; go at the end of the file
        (goto-char (point-max))
        ;; dump the remaining entities
        (maphash
         (lambda (orgtrello/--entry-new-id orgtrello/--entity)
           (let ((orgtrello/--entry-new-name (assoc-default 'name orgtrello/--entity)))
             (message "Synchronizing new entity '%s' with id '%s'..." orgtrello/--entry-new-name orgtrello/--entry-new-id)
             (insert (orgtrello/--compute-entity-to-org-entry orgtrello/--entity))
             (org-set-property *ORGTRELLO-ID* orgtrello/--entry-new-id)))
         entities)
        (save-buffer))))

(defun orgtrello/--sync-buffer-with-trello-data (entities)
  "Given all the entites, update the current buffer with those."
  (with-current-buffer (current-buffer)
    (org-map-entries
     (lambda ()
       (let ((entry-metadata (orgtrello-data/entry-get-full-metadata)))
         (if entry-metadata ;; if level > 4, entry-metadata is not considered as this is not represented in trello board
             ;; will search 'entities' hash table for updates (do not compute diffs, take them as is)
             (let* ((orgtrello/--entity         (gethash :current entry-metadata))
                    (orgtrello/--entity-id      (gethash :id orgtrello/--entity))
                    (orgtrello/--entity-updated (gethash orgtrello/--entity-id entities)))
               (if orgtrello/--entity-updated
                   ;; found something, we update by squashing the current contents
                   (let* ((orgtrello/--entry-new-id   (assoc-default 'id   orgtrello/--entity-updated))
                          (orgtrello/--entry-new-name (assoc-default 'name orgtrello/--entity-updated)))
                     ;; update the buffer with the new updates (there may be none but naively we will overwrite at the moment)
                     (message "Synchronizing entity '%s' with id '%s'..." orgtrello/--entry-new-name orgtrello/--entry-new-id)
                     (org-show-entry)
                     (kill-whole-line)
                     (insert (orgtrello/--compute-entity-to-org-entry orgtrello/--entity-updated))
                     ;; remove the entry from the hash-table
                     (remhash orgtrello/--entity-id entities)))))))
     t
     'file)
    (save-buffer))
  ;; return the entities which has been dryed
  entities)

(defun orgtrello/do-sync-full-from-trello ()
  "Full org-mode file synchronisation. Beware, this will block emacs as the request is synchronous."
  (message "Synchronizing the trello board '%s' to the org-mode file. This may take a moment, some coffee may be a good idea..." (orgtrello/--board-name))
  (let* ((orgtrello/--board-id           (assoc-default *BOARD-ID* org-file-properties))
         (orgtrello/--cards              (orgtrello-query/http-sync (orgtrello-api/get-cards orgtrello/--board-id) 'standard-success-callback))
         (orgtrello/--entities-hash-map  (orgtrello/--compute-full-entities-from-trello orgtrello/--cards))
         (orgtrello/--remaining-entities (orgtrello/--sync-buffer-with-trello-data orgtrello/--entities-hash-map)))
    (orgtrello/--update-buffer-with-remaining-trello-data orgtrello/--remaining-entities)))

(defun orgtrello/describe-heading ()
  (interactive)
  "Describe the current heading's metadata"
  (let* ((entry-metadata (orgtrello-data/entry-get-full-metadata)))
    (message "entry metadata: %S" entry-metadata)))

(defun orgtrello/describe-headings ()
  (interactive)
  "Describe the heading and its sublist."
  (let* ((orgtrello/--describe-headings-meta       (orgtrello-data/compute-full-metadata))
         (orgtrello/--describe-headings-count-meta (length orgtrello/--describe-headings-meta)))
    (message "meta: %S\ncount: %s" orgtrello/--describe-headings-meta orgtrello/--describe-headings-count-meta)))

(defun orgtrello/find-block ()
  (interactive)
  (message "found: %s" (org-entry-get (point) *ORGTRELLO-ID*)))

(defun orgtrello/--card-delete (card-meta &optional parent-meta)
  "Deal with the deletion query of a card"
  ;; parent is useless here
  (orgtrello-api/delete-card (gethash :id card-meta)))

(defun orgtrello/--checklist-delete (checklist-meta &optional parent-meta)
  "Deal with the deletion query of a checklist"
  ;; parent is useless here
  (orgtrello-api/delete-checklist (gethash :id checklist-meta)))

(defun orgtrello/--task-delete (task-meta &optional checklist-meta)
  "Deal with create/update task query build"
  (let* ((orgtrello/--task-id      (gethash :id task-meta))
         (orgtrello/--checklist-id (gethash :id checklist-meta)))
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
  (let* ((level       (gethash :level meta))
         (dispatch-fn (gethash level *MAP-DISPATCH-DELETE* 'orgtrello/--too-deep-level)))
    (funcall dispatch-fn meta parent-meta)))

(defun orgtrello/do-delete-simple ()
  "Do the simple deletion of a card, checklist or task."
  (let* ((entry-metadata   (orgtrello-data/entry-get-full-metadata))
         (current-metadata (gethash :current entry-metadata))
         (id               (gethash :id current-metadata)))
    (if (and current-metadata id)
        (let ((query-http (orgtrello/--dispatch-delete (gethash :current entry-metadata) (gethash :parent entry-metadata))))
          (if (hash-table-p query-http)
              (orgtrello-query/http query-http 'orgtrello-query/--delete-success-callback)
            (message query-http)))
      (message "Entity not synchronized on trello yet!"))))

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
  (defvar orgtrello/--*consumer-key* nil)
  (defvar orgtrello/--access-token nil)
  (browse-url "https://trello.com/1/appKey/generate")
  (setq orgtrello/--*consumer-key* (read-string "*consumer-key*: "))
  (browse-url (format "https://trello.com/1/authorize?response_type=token&name=org-trello&scope=read,write&expiration=never&key=%s" orgtrello/--*consumer-key*))
  (setq orgtrello/--access-token (read-string "Access-token: "))
  (orgtrello/--do-install-config-file orgtrello/--*consumer-key* orgtrello/--access-token))

(defun orgtrello/--id-name (entities)
  "Given a list of association list (representing entities), return a map (id, name)."
  (let* ((id-name (make-hash-table :test 'equal)))
    (--map (puthash (assoc-default 'id it) (assoc-default 'name it) id-name) entities)
    id-name))

(defun orgtrello/--name-id (entities)
  "Given a list of association list (representing entities), return a map (id, name)."
  (let* ((name-id (make-hash-table :test 'equal)))
    (--map (puthash (assoc-default'name it) (assoc-default 'id it) name-id) entities)
    name-id))

(defun orgtrello/--list-boards ()
  "Return the map of the existing boards associated to the current account. (Synchronous request)"
  (remove-if-not
   (lambda (board) (equal :json-false (assoc-default 'closed board)))
   (orgtrello-query/http-sync (orgtrello-api/get-boards) 'standard-success-callback)))

(defun orgtrello/--list-board-lists (board-id)
  "Return the map of the existing list of the board with id board-id. (Synchronous request)"
  (orgtrello-query/http-sync (orgtrello-api/get-lists board-id) 'standard-success-callback))

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

(defun orgtrello/convention-property-name (name)
  "Use the right convention for the property used in the headers of the org-mode file."
  name)

(defun orgtrello/update-orgmode-file-with-properties (board-name board-id board-lists-hash-name-id)
  "Update the orgmode file with the needed headers for org-trello to work."
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (insert (format "#+property: board-name    %s\n" board-name))
    (insert (format "#+property: board-id      %s\n" board-id))
    (maphash
     (lambda (name id)
       (insert (format "#+property: %s %s\n" (orgtrello/convention-property-name name) id)))
     board-lists-hash-name-id)
    (save-buffer)
    (org-mode-restart)))

(defun orgtrello/do-install-board-and-lists ()
  "Interactive command to install the list boards"
  (interactive)
  (cl-destructuring-bind (orgtrello/--chosen-board-id orgtrello/--chosen-board-name) (orgtrello/--choose-board (orgtrello/--id-name (orgtrello/--list-boards)))
   (orgtrello/update-orgmode-file-with-properties
    orgtrello/--chosen-board-name
    orgtrello/--chosen-board-id
    (orgtrello/--name-id (orgtrello/--list-board-lists orgtrello/--chosen-board-id)))))

(defun orgtrello/--create-board (board-name &optional board-description)
  "Create a board with name and eventually a description."
  (progn
    (message "Creating board '%s'" board-name)
    (let* ((board-data (orgtrello-query/http-sync (orgtrello-api/add-board board-name board-description))))
      (--map (assoc-default it board-data) '(id name)))))

(defun orgtrello/--close-lists (list-ids)
  "Given a list of ids, close those lists."
  (mapc (lambda (list-id)
          (progn
            (message "Closing default list with id %s" list-id)
            (orgtrello-query/http (orgtrello-api/close-list list-id) 'standard-success-callback 'standard-error-callback)))
        list-ids))

(defun orgtrello/--create-lists-according-to-keywords (board-id list-keywords)
  "Given a list of names, build those lists on the trello boards. Return the hashmap (name, id) of the new lists created."
  (cl-reduce
   (lambda (acc-hash-name-id list-name)
     (progn
       (message "Board id %s - Creating list '%s'" board-id list-name)
       (puthash list-name (assoc-default 'id (orgtrello-query/http-sync (orgtrello-api/add-list list-name board-id))) acc-hash-name-id)
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
                         (let* ((orgtrello/--board-list-ids       (--map (assoc-default 'id it) (orgtrello/--list-board-lists orgtrello/--board-id)))  ;; first retrieve the existing lists (created by default on trello)
                                (orgtrello/--lists-to-close       (orgtrello/--close-lists orgtrello/--board-list-ids))                                ;; close those lists (they may surely not match the name we want)
                                (orgtrello/--board-lists-hname-id (orgtrello/--create-lists-according-to-keywords orgtrello/--board-id *LIST-NAMES*))) ;; create the list, this returns the ids list
                           (orgtrello/update-orgmode-file-with-properties orgtrello/--board-name orgtrello/--board-id orgtrello/--board-lists-hname-id))))

(message "orgtrello loaded!")

;; #################### org-trello

(defun org-trello/--control-and-do (control-fns fn-to-control-and-execute)
  "Execute the function fn if control-fns is nil or if the result of apply every function to fn is ok."
  (if control-fns
      (progn
        (if (--all? (identity it) (--map (funcall it) control-fns));; beware, i'm calling control functions which have
            ;; side effects, not a good idea with mapcar but i need their respective result
            ;; ok, we call the function
            (funcall fn-to-control-and-execute)
          ;; there is some trouble, trying to help the user
          (message "You need to:\n- C-c o i or M-x org-trello/install-key-and-token      - Setup your consumer-key and r/w access-token.\n- C-c o I or M-x org-trello/install-board-and-list-ids - Setup org-mode file and connect it to trello.\n                                                       - Beware, for this, you need to prepare your trello board lists with the same name as your\n                                                       - org-mode keywords (TODO, DONE for example).\n- C-c o b or M-x org-trello/create-board               - You can replace the previous step by creating directly a new board from your org-mode buffer.")))
    (funcall fn-to-control-and-execute)))

(defun org-trello/create-simple-entity ()
  "Control first, then if ok, create a simple entity."
  (interactive)
  (message "Synchronizing entity...")
  (org-trello/--control-and-do '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties) 'orgtrello/do-create-simple-entity))

(defun org-trello/create-complex-entity ()
  "Control first, then if ok, create an entity and all its arborescence if need be."
  (interactive)
  (message "Synchronizing complex entity...")
  (org-trello/--control-and-do '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties) 'orgtrello/do-create-complex-entity))

(defun org-trello/sync-to-trello ()
  "Control first, then if ok, sync the org-mode file completely to trello."
  (interactive)
  (message "Synchronizing org-mode file to trello...")
  (org-trello/--control-and-do '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties) 'orgtrello/do-sync-full-file))

(defun org-trello/sync-from-trello ()
  "Control first, then if ok, sync the org-mode file from the trello board."
  (interactive)
  (message "Synchronizing trello board to org-mode file...")
  (org-trello/--control-and-do '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties) 'orgtrello/do-sync-full-from-trello))

(defun org-trello/kill-entity ()
  "Control first, then if ok, delete the entity and all its arborescence."
  (interactive)
  (message "Delete entity...")
  (org-trello/--control-and-do '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties) 'orgtrello/do-delete-simple))

(defun org-trello/install-key-and-token ()
  "No control, trigger the setup installation of the key and the read/write token."
  (interactive)
  (message "Setup key and token...")
  (org-trello/--control-and-do nil 'orgtrello/do-install-key-and-token))

(defun org-trello/install-board-and-lists-ids ()
  "Control first, then if ok, trigger the setup installation of the trello board to sync with."
  (interactive)
  (message "Install boards and lists...")
  (org-trello/--control-and-do '(orgtrello/--setup-properties orgtrello/--control-keys) 'orgtrello/do-install-board-and-lists))

(defun org-trello/create-board ()
  "Control first, then if ok, trigger the board creation."
  (interactive)
  (message "Install boards and lists...")
  (org-trello/--control-and-do '(orgtrello/--setup-properties orgtrello/--control-keys) 'orgtrello/do-create-board-and-lists))

(defun org-trello/check-setup ()
  "Check the current setup."
  (interactive)
  (message "Describe the current setup...")
  (org-trello/--control-and-do '(orgtrello/--setup-properties orgtrello/--control-keys) (lambda () (message "Setup ok!"))))

(defun org-trello/help-describing-bindings ()
  "A simple message to describe the standard bindings used."
  (interactive)
  (message "C-c o i - Interactive command to install the keys and the access-token.
C-c o I - Interactive command to select the board and attach the todo, doing and done list.
C-c o b - Create interactively a board and attach the org-mode file to this trello board.
C-c o c - Create/Update asynchronously an entity (card/checklist/item) depending on its level and status. Do not deal with level superior to 4.
C-c o C - Create/Update a complete entity card/checklist/item and its subtree (depending on its level).
C-c o s - Synchronize the org-mode file to the trello board (org-mode -> trello).
C-c o S - Synchronize the org-mode file from the trello board (trello -> org-mode).
C-c o k - Kill the entity (and its arborescence tree).
C-c o h - This help message."))

;;;###autoload
(define-minor-mode org-trello-mode "Sync your org-mode and your trello together."
  :lighter " ot" ;; the name on the modeline
  :keymap  (let ((map (make-sparse-keymap)))
             ;; binding will change
             (define-key map (kbd "C-c o i") 'org-trello/install-key-and-token)
             (define-key map (kbd "C-c o I") 'org-trello/install-board-and-lists-ids)
             (define-key map (kbd "C-c o b") 'org-trello/create-board)
             (define-key map (kbd "C-c o c") 'org-trello/create-simple-entity)
             (define-key map (kbd "C-c o C") 'org-trello/create-complex-entity)
             (define-key map (kbd "C-c o s") 'org-trello/sync-to-trello)
             (define-key map (kbd "C-c o S") 'org-trello/sync-from-trello)
             (define-key map (kbd "C-c o k") 'org-trello/kill-entity)
             (define-key map (kbd "C-c o h") 'org-trello/help-describing-bindings)
             ;; for debugging purposes (I do not know any better yet)
             (define-key map (kbd "C-c o d") 'org-trello/check-setup)
             ;; (define-key map (kbd "C-c z") 'orgtrello/describe-heading)
             ;; (define-key map (kbd "C-c x") 'orgtrello/describe-headings)
             ;; (define-key map (kbd "C-c F") 'orgtrello/find-block)
             ;; define other bindings...
             map))

(add-hook 'org-mode-hook 'org-trello-mode)

(message "org-trello loaded!")

(provide 'org-trello)

;;; org-trello.el ends here
