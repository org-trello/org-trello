;;; org-trello-controller.el --- Controller of org-trello mode
;;; Commentary:
;;; Code:

(require 'org-trello-setup)
(require 'org-trello-log)
(require 'org-trello-buffer)
(require 'org-trello-data)
(require 'org-trello-hash)
(require 'org-trello-api)
(require 'org-trello-cbx)
(require 'org-trello-action)
(require 'org-trello-backend)
(require 'org-trello-buffer)
(require 'org-trello-input)
(require 'org-trello-proxy)

(org-trello/require-cl)

(defun orgtrello-controller/--list-user-entries (properties)
  "List the users entries from PROPERTIES."
  (--filter (string-match-p *ORGTRELLO/USER-PREFIX* (car it)) properties))

(defun orgtrello-controller/hmap-id-name (org-keywords properties)
  "Given an ORG-KEYWORDS and a PROPERTIES, return a map.
This map is a key/value of (trello-id, trello-list-name-and-org-keyword-name).
If either org-keywords or properties is nil, return an empty hash-map."
  (if (or (null org-keywords) (null properties))
      (orgtrello-hash/empty-hash)
    (--reduce-from (orgtrello-hash/puthash-data (orgtrello-buffer/org-get-property it properties) it acc)
                   (orgtrello-hash/empty-hash)
                   org-keywords)))

(defun orgtrello-controller/setup-properties! (&optional args)
  "Setup the org-trello properties according to the 'org-mode' setup in the current buffer.
Return :ok.
ARGS is not used."
  ;; read the setup
  (orgtrello-action/reload-setup!)
  ;; now exploit some
  (let* ((org-keywords        (orgtrello-buffer/filtered-kwds!))
         (org-file-properties (orgtrello-buffer/org-file-properties!))
         (org-trello-users    (orgtrello-controller/--list-user-entries org-file-properties)))

    (setq *ORGTRELLO/LIST-NAMES*                   (reverse org-keywords))
    (setq *ORGTRELLO/HMAP-LIST-ORGKEYWORD-ID-NAME* (orgtrello-controller/hmap-id-name org-keywords org-file-properties))
    (setq *ORGTRELLO/HMAP-USERS-ID-NAME*           (orgtrello-hash/make-transpose-properties org-trello-users))
    (setq *ORGTRELLO/HMAP-USERS-NAME-ID*           (orgtrello-hash/make-properties org-trello-users))
    (setq *ORGTRELLO/USER-LOGGED-IN*               (orgtrello-buffer/me!))

    (mapc (lambda (color) (add-to-list 'org-tag-alist color))
          '(("red" . ?r) ("green" . ?g) ("yellow" . ?y) ("blue" . ?b) ("purple" . ?p) ("orange" . ?o)))
    :ok))

(defun orgtrello-controller/control-encoding! (&optional args)
  "Use utf-8, otherwise, there will be trouble.
ARGS is not used."
  (progn
    (orgtrello-log/msg *OT/ERROR* "Ensure you use utf-8 encoding for your org buffer.")
    :ok))

(defun orgtrello-controller/control-properties! (&optional args)
  "Org-trello needs some header buffer properties set (board id, list ids, ...).
Return :ok if ok, or the error message if problems.
ARGS is not used."
  (let ((hmap-count (hash-table-count *ORGTRELLO/HMAP-LIST-ORGKEYWORD-ID-NAME*)))
    (if (and (orgtrello-buffer/org-file-properties!) (orgtrello-buffer/board-id!) (= (length *ORGTRELLO/LIST-NAMES*) hmap-count))
        :ok
      "Setup problem.\nEither you did not connect your org-mode buffer with a trello board, to correct this:\n  * attach to a board through C-c o I or M-x org-trello/install-board-and-lists-ids\n  * or create a board from scratch with C-c o b or M-x org-trello/create-board).\nEither your org-mode's todo keyword list and your trello board lists are not named the same way (which they must).\nFor this, connect to trello and rename your board's list according to your org-mode's todo list.\nAlso, you can specify on your org-mode buffer the todo list you want to work with, for example: #+TODO: TODO DOING | DONE FAIL (hit C-c C-c to refresh the setup)")))

(defun orgtrello-controller/load-keys! (&optional args)
  "Load the credentials keys from the configuration file.
ARGS is not used."
  (if (and (file-exists-p *ORGTRELLO/CONFIG-FILE*) (load *ORGTRELLO/CONFIG-FILE*))
      :ok
    "Setup problem - Problem during credentials (consumer-key and the read/write access-token) loading - C-c o i or M-x org-trello/install-key-and-token"))

(defun orgtrello-controller/control-keys! (&optional args)
  "Org-trello needs the *consumer-key* and *access-token* for trello resources.
Returns :ok if everything is ok, or the error message if problems.
ARGS is not used."
  (if (and *consumer-key* *access-token*)
      :ok
    "Setup problem - You need to install the consumer-key and the read/write access-token - C-c o i or M-x org-trello/install-key-and-token"))

(defun orgtrello-controller/--right-level-p (entity)
  "Compute if the ENTITY level is correct (not higher than level 4)."
  (if (< (-> entity orgtrello-data/current orgtrello-data/entity-level) *ORGTRELLO/OUTOFBOUNDS-LEVEL*) :ok "Level too high. Do not deal with entity other than card/checklist/items!"))

(defun orgtrello-controller/--already-synced-p (entity)
  "Compute if the ENTITY has already been synchronized."
  (if (-> entity orgtrello-data/current orgtrello-data/entity-id) :ok "Entity must been synchronized with trello first!"))

(defun orgtrello-controller/--mandatory-name-ok-p (entity)
  "Ensure ENTITY can be synced regarding the mandatory data."
  (let* ((current (orgtrello-data/current entity))
         (level   (orgtrello-data/entity-level current))
         (name    (orgtrello-data/entity-name current)))
    (if (and name (< 0 (length name)))
        :ok
      (cond ((= level *ORGTRELLO/CARD-LEVEL*)      *ORGTRELLO/ERROR-SYNC-CARD-MISSING-NAME*)
            ((= level *ORGTRELLO/CHECKLIST-LEVEL*) *ORGTRELLO/ERROR-SYNC-CHECKLIST-MISSING-NAME*)
            ((= level *ORGTRELLO/ITEM-LEVEL*)      *ORGTRELLO/ERROR-SYNC-ITEM-MISSING-NAME*)))))

(defun orgtrello-controller/--do-delete! (full-meta action &optional entities-adjacencies)
  "Execute on FULL-META the ACTION.
Provide entities-adjacencies for more information."
  (let* ((current (orgtrello-data/current full-meta))
         (marker  (orgtrello-buffer/--compute-marker-from-entry current)))
    (orgtrello-buffer/set-marker-if-not-present! current marker)
    (orgtrello-data/put-entity-id     marker current)
    (orgtrello-data/put-entity-action action current)
    (eval (orgtrello-proxy/do-action-on-entity current entities-adjacencies))))

(defun orgtrello-controller/do-delete-simple (&optional sync)
  "Do the deletion of an entity.
SYNC is not used."
  (orgtrello-action/functional-controls-then-do '(orgtrello-controller/--right-level-p orgtrello-controller/--already-synced-p)
                                                (orgtrello-buffer/entry-get-full-metadata!)
                                                'orgtrello-controller/--do-delete!
                                                *ORGTRELLO/ACTION-DELETE*))

(defun orgtrello-controller/--do-action-on-entity! (entity action &optional entities-adjacencies)
  "Execute on ENTITY the ACTION.
Use ENTITIES-ADJACENCIES to provide more information."
  (let ((marker (orgtrello-buffer/--compute-marker-from-entry entity)))
    (orgtrello-data/put-entity-id     marker entity)
    (orgtrello-data/put-entity-action action entity)
    (orgtrello-proxy/do-action-on-entity entity entities-adjacencies)))

(defun orgtrello-controller/do-sync-card-to-trello! ()
  "Do the actual card creation/update - from card to item."
  (orgtrello-log/msg *OT/INFO* "Synchronizing card on board '%s'..." (orgtrello-buffer/board-name!))
  ;; in any case, we need to show the subtree, otherwise https://github.com/org-trello/org-trello/issues/53
  (org-show-subtree)
  (-> (current-buffer)
    orgtrello-controller/build-card-structure!
    orgtrello-controller/execute-sync-entity-structure!))

(defun orgtrello-controller/do-sync-full-file-to-trello! ()
  "Full org-mode file synchronisation."
  (orgtrello-log/msg *OT/WARN* "Synchronizing org-mode file to the board '%s'. This may take some time, some coffee may be a good idea..." (orgtrello-buffer/board-name!))
  (orgtrello-buffer/org-map-entries 'orgtrello-controller/do-sync-card-to-trello!))

(defun orgtrello-controller/--sync-buffer-with-trello-data (data)
  "Update the current buffer with DATA (entities and adjacency)."
  (let ((entities (car data))
        (adjacency (cadr data)))
    (goto-char (point-max)) ;; go at the end of the file
    (maphash
     (lambda (new-id entity)
       (when (orgtrello-data/entity-card-p entity)
         (orgtrello-buffer/write-card! new-id entity entities adjacency)))
     entities)
    (goto-char (point-min)) ;; go back to the beginning of file
    (org-sort-entries t ?o) ;; sort the entries on their keywords
    ;;(org-global-cycle '(4)) ;; fold all entries
    ))

(defun orgtrello-controller/--cleanup-org-entries ()
  "Cleanup org-entries from the buffer.
Does not preserve position."
  (goto-char (point-min))
  (outline-next-heading)
  (orgtrello-cbx/remove-overlays! (point-at-bol) (point-max))
  (kill-region (point-at-bol) (point-max)))

(defun orgtrello-controller/--sync-buffer-with-trello-cards (buffer-name trello-cards)
  "Synchronize the buffer BUFFER-NAME with the TRELLO-CARDS."
  (with-local-quit
    (with-current-buffer buffer-name
      (let ((entities-from-org-buffer (orgtrello-buffer/compute-entities-from-org-buffer! buffer-name)))
        (-> trello-cards
          orgtrello-backend/compute-org-trello-card-from
          (orgtrello-data/merge-entities-trello-and-org entities-from-org-buffer)
          ((lambda (entry) (orgtrello-controller/--cleanup-org-entries) entry))   ;; hack to clean the org entries just before synchronizing the buffer
          orgtrello-controller/--sync-buffer-with-trello-data)))))

(defun orgtrello-controller/do-sync-full-file-from-trello! ()
  "Full org-mode file synchronisation. Beware, this will block emacs as the request is synchronous."
  (orgtrello-log/msg *OT/INFO* "Synchronizing the trello board '%s' to the org-mode file. This may take a moment, some coffee may be a good idea..." (orgtrello-buffer/board-name!))
  (lexical-let ((buffer-name (current-buffer)))
    (save-excursion
      (lexical-let* ((board-id (orgtrello-buffer/board-id!)))
        (deferred:$
          (deferred:next
            (lambda ()
              (-> board-id
                orgtrello-api/get-full-cards
                (orgtrello-query/http-trello 'sync))))
          (deferred:nextc it
            (lambda (trello-cards) ;; We have the full result in one query, now we can compute the translation in org-trello model
              (orgtrello-log/msg *OT/DEBUG* "trello-card: %S" trello-cards)
              (orgtrello-controller/--sync-buffer-with-trello-cards buffer-name trello-cards)))
          (deferred:nextc it
            (lambda ()
              (orgtrello-buffer/save-buffer buffer-name)
              (orgtrello-log/msg *OT/INFO* "Sync buffer '%s' from trello done!" buffer-name)))
          (deferred:error it
            (lambda (err) (orgtrello-log/msg *OT/ERROR* "Sync buffer from trello - Catch error: %S" err))))))))

(defun orgtrello-controller/build-card-structure! (buffer-name)
  "Build the card structure on the current BUFFER-NAME at current point.
No synchronization is done."
  (->> (orgtrello-buffer/compute-card-region!)
    (cons buffer-name)
    (apply 'orgtrello-buffer/compute-entities-from-org-buffer!)))

(defun orgtrello-controller/execute-sync-entity-structure! (entity-structure)
  "Execute synchronization of ENTITY-STRUCTURE (entities at first position, adjacency list in second position).
The entity-structure is self contained.
Synchronization is done here.
Along the way, the buffer BUFFER-NAME is written with new informations."
  (lexical-let ((entities             (car entity-structure))
                (entities-adjacencies entity-structure)
                (card-computations))
    ;; compute the card to sync computations
    (maphash (lambda (id entity)
               (when (orgtrello-data/entity-card-p entity)
                 (-> entity
                   (orgtrello-controller/--do-action-on-entity! *ORGTRELLO/ACTION-SYNC* entities-adjacencies)
                   (push card-computations))))
             entities)

    (-> card-computations
      nreverse
      (orgtrello-proxy/execute-sync-computations "card(s) sync ok!" "FAILURE! cards(s) sync KO!"))))

(defun orgtrello-controller/compute-and-overwrite-card! (buffer-name trello-card)
  (with-local-quit
    (with-current-buffer buffer-name
      (let* ((card-id                  (orgtrello-data/entity-id trello-card))
             (region                   (orgtrello-buffer/compute-card-region!))
             (entities-from-org-buffer (apply 'orgtrello-buffer/compute-entities-from-org-buffer! (cons buffer-name region)))
             (entities-from-trello     (orgtrello-backend/compute-org-trello-card-from (list trello-card)))
             (merged-entities          (orgtrello-data/merge-entities-trello-and-org entities-from-trello entities-from-org-buffer))
             (entities                 (car merged-entities))
             (entities-adj             (cadr merged-entities)))
        (goto-char (car region))
        (orgtrello-buffer/clean-region! region)
        (orgtrello-buffer/write-card! card-id (gethash card-id entities) entities entities-adj)))))

(defun orgtrello-controller/do-sync-card-from-trello! ()
  "Entity (card/checklist/item) synchronization (with its structure) from trello.
Optionally, SYNC permits to synchronize the query."
  (orgtrello-log/msg *OT/INFO* "Synchronizing the trello card to the org-mode file...")
  (lexical-let ((buffer-name (current-buffer)))
    (save-excursion
      (lexical-let* ((card-meta (orgtrello-data/current (orgtrello-buffer/entry-get-full-metadata!)))
                     (card-name (orgtrello-data/entity-name card-meta)))
        (deferred:$
          (deferred:next
            (lambda ()
              (-> card-meta
                orgtrello-data/entity-id
                orgtrello-api/get-full-card
                (orgtrello-query/http-trello 'sync))))
          (deferred:nextc it
            (lambda (trello-card) ;; We have the full result in one query, now we can compute the translation in org-trello model
              (orgtrello-log/msg *OT/DEBUG* "trello-card: %S" trello-card)
              (orgtrello-controller/compute-and-overwrite-card! buffer-name trello-card)))
          (deferred:nextc it
            (lambda ()
              (orgtrello-buffer/save-buffer buffer-name)
              (orgtrello-log/msg *OT/INFO* "Sync card '%s' from trello done!" card-name)))
          (deferred:error it
            (lambda (err) (orgtrello-log/msg *OT/ERROR* "Catch error: %S" err))))))))

(defun orgtrello-controller/--do-delete-card ()
  "Delete the card.
SYNC flag permit to synchronize the http query."
  (when (= *ORGTRELLO/CARD-LEVEL* (-> (orgtrello-buffer/entry-get-full-metadata!)
                          orgtrello-data/current
                          orgtrello-data/entity-level))
    (orgtrello-controller/do-delete-simple)))

(defun orgtrello-controller/do-delete-entities ()
  "Launch a batch deletion of every single entities present on the buffer.
SYNC flag permit to synchronize the http query."
  (org-map-entries 'orgtrello-controller/--do-delete-card  t 'file))

(defun orgtrello-controller/--do-install-config-file (consumer-key access-token)
  "Persist the file config-file with the CONSUMER-KEY and ACCESS-TOKEN."
  (make-directory *ORGTRELLO/CONFIG-DIR* t)
  (with-temp-file *ORGTRELLO/CONFIG-FILE*
    (erase-buffer)
    (goto-char (point-min))
    (insert (format "(setq *consumer-key* \"%s\")\n" consumer-key))
    (insert (format "(setq *access-token* \"%s\")" access-token))
    (write-file *ORGTRELLO/CONFIG-FILE* 'do-ask-for-overwrite)))

(defun orgtrello-controller/do-install-key-and-token ()
  "Procedure to install the *consumer-key* and the token for the user in the config-file."
  (browse-url (org-trello/compute-url "/1/appKey/generate"))
  (let ((consumer-key (read-string "Consumer key: ")))
    (browse-url (org-trello/compute-url (format "/1/authorize?response_type=token&name=org-trello&scope=read,write&expiration=never&key=%s" consumer-key)))
    (let ((access-token (read-string "Access token: ")))
      (orgtrello-controller/--do-install-config-file consumer-key access-token)
      "Install key and read/write access token done!")))

(defun orgtrello-controller/--id-name (entities)
  "Given a list of ENTITIES, return a map of (id, name)."
  (--reduce-from (orgtrello-hash/puthash-data (orgtrello-data/entity-id it) (orgtrello-data/entity-name it) acc) (orgtrello-hash/empty-hash) entities))

(defun orgtrello-controller/--name-id (entities)
  "Given a list of ENTITIES, return a map of (id, name)."
  (--reduce-from (orgtrello-hash/puthash-data (orgtrello-data/entity-name it) (orgtrello-data/entity-id it) acc) (orgtrello-hash/empty-hash) entities))

(defun orgtrello-controller/--list-boards! ()
  "Return the map of the existing boards associated to the current account. (Synchronous request)"
  (--remove (orgtrello-data/entity-closed it) (orgtrello-query/http-trello (orgtrello-api/get-boards) 'synchronous-query)))

(defun orgtrello-controller/--list-board-lists! (board-id)
  "Return the map of the existing list of the board with id board-id. (Synchronous request)"
  (orgtrello-query/http-trello (orgtrello-api/get-lists board-id) 'synchronous-query))

(defun orgtrello-controller/--board! (board-id)
  "Return the board with id board-id. (Synchronous request)"
  (orgtrello-query/http-trello (orgtrello-api/get-board board-id) 'synchronous-query))

(defun orgtrello-controller/--index-board-map (boards)
  "Given BOARDS, a map of board (id . name), return a map of (position . name)."
  (let ((i               0)
        (index-board-map (orgtrello-hash/empty-hash)))
    (maphash (lambda (id _)
               (orgtrello-hash/puthash-data (format "%d" i) id index-board-map)
               (setq i (+ 1 i)))
             boards)
    index-board-map))

(defun orgtrello-controller/--display-boards-to-choose (boards)
  "Given BOARDS, a map of board (id . name), return a string to display in minibuffer."
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

(defun orgtrello-controller/--convention-property-name (name)
  "Given a NAME, use the right convention for the property used in the headers of the 'org-mode' file."
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
  "Compute a formatted property in org buffer from PROPERTY-NAME and optional PROPERTY-VALUE."
  (format "#+property: %s %s" property-name (if property-value property-value "")))

(defun orgtrello-controller/--compute-hash-name-id-to-list (users-hash-name-id)
  "Compute the hash of name id to list from USERS-HASH-NAME-ID."
  (let ((res-list nil))
    (maphash (lambda (name id) (--> name
                                 (replace-regexp-in-string *ORGTRELLO/USER-PREFIX* "" it)
                                 (format "%s%s" *ORGTRELLO/USER-PREFIX* it)
                                 (orgtrello-controller/compute-property it id)
                                 (push it res-list)))
             users-hash-name-id)
    res-list))

(defun orgtrello-controller/--remove-properties-file! (org-keywords users-hash-name-id user-me &optional update-todo-keywords)
  "Remove the current org-trello header metadata."
  (with-current-buffer (current-buffer)
    ;; compute the list of properties to purge
    (->> `(":PROPERTIES"
           ,(orgtrello-controller/compute-property *ORGTRELLO/BOARD-NAME*)
           ,(orgtrello-controller/compute-property *ORGTRELLO/BOARD-ID*)
           ,@(--map (orgtrello-controller/compute-property (orgtrello-controller/--convention-property-name it)) org-keywords)
           ,@(orgtrello-controller/--compute-hash-name-id-to-list users-hash-name-id)
           ,(orgtrello-controller/compute-property *ORGTRELLO/USER-ME* user-me)
           ,(when update-todo-keywords "#+TODO: ")
           ":red" ":blue" ":yellow" ":green" ":orange" ":purple"
           ":END:")
      (mapc 'orgtrello-controller/--delete-buffer-property!))))

(defun orgtrello-controller/--properties-labels (board-labels)
  "Compute properties labels from BOARD-LABELS."
  (let ((res-list))
    (maphash (lambda (name id)
               (push (format "#+PROPERTY: %s %s" name id) res-list))
             board-labels)
    res-list))

(defun orgtrello-controller/--compute-metadata! (board-name board-id board-lists-hash-name-id board-users-hash-name-id user-me board-labels &optional update-todo-keywords)
  "Compute the org-trello metadata to dump on header file."
  `(":PROPERTIES:"
    ,(orgtrello-controller/compute-property *ORGTRELLO/BOARD-NAME* board-name)
    ,(orgtrello-controller/compute-property *ORGTRELLO/BOARD-ID* board-id)
    ,@(orgtrello-controller/--compute-board-lists-hash-name-id board-lists-hash-name-id)
    ,(if update-todo-keywords (orgtrello-controller/--properties-compute-todo-keywords-as-string board-lists-hash-name-id) "")
    ,@(orgtrello-controller/--properties-compute-users-ids board-users-hash-name-id)
    ,@(orgtrello-controller/--properties-labels board-labels)
    ,(format "#+PROPERTY: %s %s" *ORGTRELLO/USER-ME* user-me)
    ":END:"))

(defun orgtrello-controller/--compute-keyword-separation (name)
  "Given a keyword NAME (case insensitive) return a string '| done' or directly the keyword."
  (if (string= "done" (downcase name)) (format "| %s" name) name))

(defun orgtrello-controller/--compute-board-lists-hash-name-id (board-lists-hash-name-id)
  "Compute board lists of key/name from BOARD-LISTS-HASH-NAME-ID."
  (let ((res-list))
    (maphash (lambda (name id) (--> (orgtrello-controller/--convention-property-name name)
                                 (format "#+PROPERTY: %s %s" it id)
                                 (push it res-list)))
             board-lists-hash-name-id)
    res-list))

(defun orgtrello-controller/--properties-compute-todo-keywords-as-string (board-lists-hash-name-id)
  "Compute org keywords from the BOARD-LISTS-HASH-NAME-ID."
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
                                 (format "#+PROPERTY: %s%s %s" *ORGTRELLO/USER-PREFIX* it id)
                                 (push it res-list)))
             board-users-hash-name-id)
    res-list))

(defun orgtrello-controller/--update-orgmode-file-with-properties!
    (board-name board-id board-lists-hash-name-id board-users-hash-name-id user-me board-labels &optional update-todo-keywords)
  "Update the orgmode file with the needed headers for org-trello to work."
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (set-buffer-file-coding-system 'utf-8-auto) ;; force utf-8
    (->> (orgtrello-controller/--compute-metadata! board-name board-id board-lists-hash-name-id board-users-hash-name-id user-me board-labels update-todo-keywords)
      (mapc (lambda (it) (insert it "\n"))))
    (goto-char (point-min))
    (org-cycle)))

(defun orgtrello-controller/--hash-table-keys (hash-table)
  "Extract the keys from the HASH-TABLE."
  (let ((keys ()))
    (maphash (lambda (k v) (push k keys)) hash-table)
    keys))

(defun orgtrello-controller/--user-logged-in! ()
  "Compute the current user."
  (-> (orgtrello-api/get-me)
    (orgtrello-query/http-trello 'synchronous-query)
    orgtrello-data/entity-username))

(defun orgtrello-controller/do-install-board-and-lists ()
  "Command to install the list boards."
  (let* ((board-info        (-> (orgtrello-controller/--list-boards!)
                              orgtrello-controller/--id-name
                              orgtrello-controller/choose-board!))
         (chosen-board-id   (car board-info))
         (chosen-board-name (cadr board-info))
         (board-lists       (orgtrello-controller/--list-board-lists! chosen-board-id))
         (board-labels      (->> chosen-board-id orgtrello-controller/--board! orgtrello-data/entity-labels))
         (user-logged-in    (orgtrello-controller/--user-logged-in!)))
    ;; Update metadata about the board
    (orgtrello-controller/do-write-board-metadata! chosen-board-id chosen-board-name user-logged-in board-lists board-labels))
  "Install board and list ids done!")

(defun orgtrello-controller/--compute-user-properties (memberships-map)
  "Given a map MEMBERSHIPS-MAP, extract the map of user information."
  (mapcar 'orgtrello-data/entity-member memberships-map))

(defun orgtrello-controller/--compute-user-properties-hash (user-properties)
  "Compute user's properties from USER-PROPERTIES."
  (--reduce-from (orgtrello-hash/puthash-data (orgtrello-data/entity-username it) (orgtrello-data/entity-id it) acc) (orgtrello-hash/empty-hash) user-properties))

(defun orgtrello-controller/--compute-user-properties-hash-from-board (board-info)
  "Compute user properties given board's informations BOARD-INFO."
  (->> board-info
    orgtrello-data/entity-memberships
    orgtrello-controller/--compute-user-properties
    orgtrello-controller/--compute-user-properties-hash))

(defun orgtrello-controller/--board-users-information-from-board-id! (board-id)
  "Compute board users' informations."
  (--> board-id
    (orgtrello-api/get-board it)
    (orgtrello-query/http-trello it 'synchronous-query)
    (orgtrello-controller/--compute-user-properties-hash-from-board it)))

(defun orgtrello-controller/--create-board (board-name &optional board-description)
  "Create a board with name BOARD-NAME and optionally a BOARD-DESCRIPTION."
  (orgtrello-log/msg *OT/INFO* "Creating board '%s'" board-name)
  (let ((board-data (orgtrello-query/http-trello (orgtrello-api/add-board board-name board-description) 'synchronous-query)))
    (list (orgtrello-data/entity-id board-data) (orgtrello-data/entity-name board-data))))

(defun orgtrello-controller/--close-lists (list-ids)
  "Given a list of ids LIST-IDS, close those lists."
  (mapc (lambda (list-id)
          (orgtrello-log/msg *OT/INFO* "Closing default list with id %s" list-id)
          (orgtrello-query/http-trello (orgtrello-api/close-list list-id)))
        list-ids))

(defun orgtrello-controller/--create-lists-according-to-keywords (board-id org-keywords)
  "For the BOARD-ID, create the list names from LIST-KEYWORDS.
Return the hashmap (name, id) of the new lists created."
  (--reduce-from (progn
                   (orgtrello-log/msg *OT/INFO* "Board id %s - Creating list '%s'" board-id it)
                   (orgtrello-hash/puthash-data it (orgtrello-data/entity-id (orgtrello-query/http-trello (orgtrello-api/add-list it board-id) 'synchronous-query)) acc))
                 (orgtrello-hash/empty-hash)
                 org-keywords))

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
             (board-lists-hname-id (orgtrello-controller/--create-lists-according-to-keywords board-id *ORGTRELLO/LIST-NAMES*))
             ;; retrieve user informations
             (board-users-name-id  (orgtrello-controller/--board-users-information-from-board-id! board-id))
             ;; compute the current user's information
             (user-logged-in       (orgtrello-controller/--user-logged-in!)))
        ;; clean the buffer's old metadata
        (orgtrello-controller/do-cleanup-from-buffer!)
        ;; update org buffer with new ones
        (orgtrello-controller/--update-orgmode-file-with-properties!
         board-name
         board-id
         board-lists-hname-id
         board-users-name-id
         user-logged-in
         (orgtrello-hash/make-properties '((:red) (:green) (:yellow) (:purple) (:blue) (:orange)))))))
  "Create board and lists done!")

(defun orgtrello-controller/--add-user (user users)
  "Add the USER to the USERS list."
  (if (member user users) users (cons user users)))

(defun orgtrello-controller/--remove-user (user users)
  "Remove the USER from the USERS list."
  (if (member user users) (remove user users) users users))

(defun orgtrello-controller/do-assign-me () "Command to assign oneself to the card."
       (--> (orgtrello-buffer/get-usernames-assigned-property!)
         (orgtrello-data/--users-from it)
         (orgtrello-controller/--add-user *ORGTRELLO/USER-LOGGED-IN* it)
         (orgtrello-data/--users-to it)
         (orgtrello-buffer/set-usernames-assigned-property! it)))

(defun orgtrello-controller/do-unassign-me () "Command to unassign oneself of the card."
       (--> (orgtrello-buffer/get-usernames-assigned-property!)
         (orgtrello-data/--users-from it)
         (orgtrello-controller/--remove-user *ORGTRELLO/USER-LOGGED-IN* it)
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
      (concat (orgtrello-buffer/me!) ": " new-comment *ORGTRELLO/CARD-COMMENTS-DELIMITER-PRINT*)
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
                                                  (when *ORGTRELLO/DO-SHOW-CARD-COMMENTS-AFTER-ADDING*
                                                    (orgtrello-controller/do-show-card-comments!)))))))))

(defun orgtrello-controller/do-cleanup-from-buffer! (&optional globally-flag)
  "Permit to clean the buffer from trello data."
  (orgtrello-controller/--remove-properties-file! *ORGTRELLO/LIST-NAMES* *ORGTRELLO/HMAP-USERS-NAME-ID* *ORGTRELLO/USER-LOGGED-IN* t) ;; remove any orgtrello relative entries
  (when globally-flag
    (mapc 'orgtrello-buffer/delete-property! `(,*ORGTRELLO/ID* ,*ORGTRELLO/USERS-ENTRY* ,*ORGTRELLO/CARD-COMMENTS*))))

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
        (browse-url (org-trello/compute-url (format "/c/%s" card-id))))))

(defun orgtrello-controller/jump-to-board! ()
  "Given the current position, execute the information extraction and jump to board action."
  (browse-url (org-trello/compute-url (format "/b/%s" (orgtrello-buffer/board-id!)))))

(defun orgtrello-controller/delete-setup! ()
  "Global org-trello metadata clean up."
  (orgtrello-controller/do-cleanup-from-buffer! t)
  (orgtrello-log/msg *OT/NOLOG* "Cleanup done!"))

(defun orgtrello-controller/mode-on-hook-fn ()
  "Start org-trello hook function to install some org-trello setup."
  ;; buffer-invisibility-spec
  (add-to-invisibility-spec '(org-trello-cbx-property)) ;; for an ellipsis (...) change to '(org-trello-cbx-property . t)
  ;; installing hooks
  (mapc (lambda (h)
          (add-hook 'before-save-hook h)) '(orgtrello-buffer/install-overlays!
                                            orgtrello-buffer/indent-card-descriptions!))
  ;; migrate all checkbox at org-trello mode activation
  (orgtrello-buffer/install-overlays!)
  (orgtrello-buffer/indent-card-descriptions!)
  ;; run hook at startup
  (run-hooks 'org-trello-mode-hook))

(defun orgtrello-controller/mode-off-hook-fn ()
  "Stop org-trello hook function to deinstall some org-trello setup."
  ;; remove the invisible property names
  (remove-from-invisibility-spec '(org-trello-cbx-property)) ;; for an ellipsis (...) change to '(org-trello-cbx-property . t)
  ;; removing hooks
  (mapc (lambda (h)
          (remove-hook 'before-save-hook h)) '(orgtrello-buffer/install-overlays!
                                               orgtrello-buffer/indent-card-descriptions!))
  ;; remove org-trello overlays
  (orgtrello-buffer/remove-overlays!))

(orgtrello-log/msg *OT/DEBUG* "orgtrello-controller loaded!")

(provide 'org-trello-controller)
;;; org-trello-controller.el ends here
