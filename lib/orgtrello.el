;;; orgtrello.el

(require 'orgtrello-api)
(require 'orgtrello-query)
(require 'orgtrello-data)

;; Specific state - FIXME check if they do not already exist on org-mode to avoid potential collisions
(defvar *TODO* "TODO" "org-mode todo state")
(defvar *DONE* "DONE" "org-mode done state")

;; Properties key for the orgtrello headers #+PROPERTY board-id, etc...
(defvar *BOARD-ID*      "board-id"      "orgtrello property board-id entry")
(defvar *TODO-LIST-ID*  "todo-list-id"  "orgtrello property todo list id")
(defvar *DOING-LIST-ID* "doing-list-id" "orgtrello property doing list id")
(defvar *DONE-LIST-ID*  "done-list-id"  "orgtrello property done list id")

(defvar *CONFIG-DIR*  (concat (getenv "HOME") "/" ".trello"))
(defvar *CONFIG-FILE* (concat *CONFIG-DIR* "/config.el"))

(defvar *consumer-key*     nil "Id representing the user")
(defvar *access-token*     nil "Read/write Access token to use trello in the user's name ")
(defvar *ORGTRELLO-MARKER* nil "Marker used for syncing the data in trello")

(defun orgtrello/--control-properties ()
  "org-trello needs the properties board-id, todo-list-id, doing-list-id, done-list-id to be able to work ok."
  (and (assoc-default *BOARD-ID*      org-file-properties)
       (assoc-default *TODO-LIST-ID*  org-file-properties)
       (assoc-default *DOING-LIST-ID* org-file-properties)
       (assoc-default *DONE-LIST-ID*  org-file-properties)))

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

(defun orgtrello--compute-list-key (state)
  "Given a state, compute the list id for the creation of a card"
  (cond ((string= state *TODO*) *TODO-LIST-ID*)
        ((string= state *DONE*) *DONE-LIST-ID*)
        (t                      *DOING-LIST-ID*)))

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
    ;; set the consumer-key to make a pointer to get back to when the request is finished
    (org-set-property *ORGTRELLO-MARKER* *ORGTRELLO-MARKER*)
    ;; then execute the call
    (funcall dispatch-fn meta parent-meta grandparent-meta)))

(defun orgtrello-do-create-simple ()
  "Do the actual simple creation of a card, checklist or task."
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

(defun orgtrello-do-create-full-card ()
  "Do the actual full card creation - from card to task. Beware full side effects..."
  ;; beware, the list-entries-metadata is stored once and not updated after each http call, thus do not possess the
  ;; newly created id
  (let* ((list-entries-metadata (orgtrello-data-compute-full-metadata))
         (map-ids               (make-hash-table :test 'equal)))
    (mapcar (lambda (mapdata)
              (let* ((current                                           (gethash :current     mapdata))
                     (parent                                            (gethash :parent      mapdata))
                     (grandparent                                       (gethash :grandparent mapdata))
                     (query-http                                        (orgtrello--dispatch-create
                                                                         (orgtrello--merge-map current map-ids)
                                                                         (orgtrello--merge-map parent map-ids)
                                                                         (orgtrello--merge-map grandparent map-ids)))
                     ;; the query is synchronous as there is order in the current list - FIXME any better way? queues?
                     ;; execute and retrieve the result of the request
                     (orgtrello--do-create-full-card-response-http-data (orgtrello-query-http-sync query-http)))
                ;; keep the last id
                (puthash (assoc-default 'name orgtrello--do-create-full-card-response-http-data)
                         (assoc-default 'id   orgtrello--do-create-full-card-response-http-data)
                         map-ids)))
            list-entries-metadata)))

(defun orgtrello-describe-heading ()
  (interactive)
  "Describe the current heading's metadata"
  (let* ((entry-metadata (orgtrello-data-entry-get-full-metadata)))
    (message "entry metadata: %S" entry-metadata)))

(defun orgtrello-describe-headings ()
  (interactive)
  "Describe the heading and its sublist."
  (let* ((orgtrello--describe-headings-meta       (orgtrello-data-compute-full-metadata))
         (orgtrello--describe-headings-count-meta (length orgtrello--describe-headings-meta)))
    (message "meta: %S\ncount: %s" orgtrello--describe-headings-meta orgtrello--describe-headings-count-meta)))

(defun orgtrello-find-block ()
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

(defun orgtrello-do-delete-simple ()
  "Do the simple deletion of a card, checklist or task."
  (let* ((entry-metadata   (orgtrello-data-entry-get-full-metadata))
         (current-metadata (gethash :current entry-metadata))
         (id               (gethash :id current-metadata)))
    (if (and current-metadata id)
        (let ((query-http (orgtrello--dispatch-delete (gethash :current entry-metadata) (gethash :parent entry-metadata))))
          (if (hash-table-p query-http)
              (orgtrello-query-http query-http)
            (message query-http)))
      (message "Entity not synchronized on trello yet!"))))

(defun orgtrello--do-install-config-file (*consumer-key* *access-token*)
  "Persist the file config-file with the input of the user."
  (make-directory *CONFIG-DIR* t)
  (with-temp-file *CONFIG-FILE*
    (erase-buffer)
    (goto-char (point-min))
    (insert (format "(setq *consumer-key* \"%s\")\n" *consumer-key*))
    (insert (format "(setq *access-token* \"%s\")" *access-token*))
    (write-file *CONFIG-FILE* 't)))

(defun orgtrello-do-install-keys-and-token ()
  "Procedure to install the *consumer-key* and the token for the user in the config-file."
  (interactive)
  (defvar orgtrello--*consumer-key* nil)
  (defvar orgtrello--access-token nil)
  (browse-url "https://trello.com/1/appKey/generate")
  (setq orgtrello--*consumer-key* (read-string "*consumer-key*: "))
  (browse-url (format "https://trello.com/1/authorize?response_type=token&name=org-trello&scope=read,write&expiration=never&key=%s" orgtrello--*consumer-key*))
  (setq orgtrello--access-token (read-string "Access-token: "))
  (orgtrello--do-install-config-file orgtrello--*consumer-key* orgtrello--access-token))

(defun orgtrello--id-name (entities)
  "Given a list of association list (representing entities), return a map (id, name)."
  (let* ((id-name (make-hash-table :test 'equal)))
    (->> entities
      (--map (puthash (assoc-default 'id it) (assoc-default 'name it) id-name)))
    id-name))

(defun orgtrello--name-id (entities)
  "Given a list of association list (representing entities), return a map (id, name)."
  (let* ((name-id (make-hash-table :test 'equal)))
    (->> entities
      (--map (puthash (downcase (assoc-default'name it)) (downcase (assoc-default 'id it)) name-id)))
    name-id))

(defun orgtrello--list-boards ()
  "Return the map of the existing boards associated to the current account. (Synchronous request)"
  (orgtrello-query-http-sync (orgtrello-api--get-boards)))

(defun orgtrello--list-board-lists (board-id)
  "Return the map of the existing list of the board with id board-id. (Synchronous request)"
  (orgtrello-query-http-sync (orgtrello-api--get-lists board-id)))

(defun orgtrello--choose-board (boards)
  "Given a map of boards, display the possible boards for the user to choose which one he wants to work with."
  ;; ugliest ever
  (defvar board-chosen nil)
  (let* ((str-key-val  "")
         (i            0)
         (i-id (make-hash-table :test 'equal)))
    (maphash (lambda (id name)
               (setq str-key-val (format "%s%d: %s\n" str-key-val i name))
               (puthash (format "%d" i) id i-id)
               (setq i (+ 1 i)))
             boards)
    (setq board-chosen
          (read-string (format "%s\nInput the number of the board desired: " str-key-val)))
    (while (not (gethash board-chosen i-id))
      (setq board-chosen
            (read-string (format "%s\nInput the number of the board desired: " str-key-val))))
    (gethash board-chosen i-id)))

(defun orgtrello-update-orgmode-file-with-properties (board-id board-lists-hash-name-id)
  "Update the orgmode file with the needed headers for org-trello to work."
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (insert (format "#+property: board-id      %s\n" board-id))
    (insert (format "#+property: todo-list-id  %s\n" (gethash "todo"  board-lists-hash-name-id)))
    (insert (format "#+property: doing-list-id %s\n" (gethash "doing" board-lists-hash-name-id)))
    (insert (format "#+property: done-list-id  %s\n" (gethash "done"  board-lists-hash-name-id)))
    (save-buffer)
    (org-mode-restart)))

(defun orgtrello-do-install-board-and-lists ()
  "Interactive command to install the list boards"
  (interactive)
  (load *CONFIG-FILE*)
  (if (not (and *consumer-key* *access-token*))
      (message "You need to setup your account to be able to connect to trello.\nInstall manually (report to the doc) or M-x orgtrello-do-install-keys-and-token")
    (let* ((chosen-id-board (orgtrello--choose-board (orgtrello--id-name (orgtrello--list-boards))))
           (board-lists     (orgtrello--name-id (orgtrello--list-board-lists chosen-id-board))))
      (orgtrello-update-orgmode-file-with-properties chosen-id-board board-lists))))

(message "orgtrello loaded!")

(provide 'orgtrello)

;;; orgtrello.el ends here
