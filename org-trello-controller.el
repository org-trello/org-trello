;;; org-trello-controller.el --- Controller of org-trello mode
;;; Commentary:
;;; Code:

(require 'org-trello-setup)
(require 'org-trello-log)
(require 'org-trello-buffer)
(require 'org-trello-data)
(require 'org-trello-hash)
(require 'org-trello-api)
(require 'org-trello-entity)
(require 'org-trello-cbx)
(require 'org-trello-action)
(require 'org-trello-backend)
(require 'org-trello-buffer)
(require 'org-trello-input)
(require 'org-trello-proxy)
(require 's)

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

(defun orgtrello-setup/display-current-buffer-setup!()
  "Display current buffer's setup."
  (list :users-id-name *ORGTRELLO/HMAP-USERS-ID-NAME*
        :users-name-id *ORGTRELLO/HMAP-USERS-NAME-ID*
        :user-logged-in *ORGTRELLO/USER-LOGGED-IN*
        :org-keyword-trello-list-names *ORGTRELLO/ORG-KEYWORD-TRELLO-LIST-NAMES*
        :org-keyword-id-name *ORGTRELLO/HMAP-LIST-ORGKEYWORD-ID-NAME*))

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

    (setq *ORGTRELLO/ORG-KEYWORD-TRELLO-LIST-NAMES* org-keywords)
    (setq *ORGTRELLO/HMAP-LIST-ORGKEYWORD-ID-NAME*  (orgtrello-controller/hmap-id-name org-keywords org-file-properties))
    (setq *ORGTRELLO/HMAP-USERS-ID-NAME*            (orgtrello-hash/make-transpose-properties org-trello-users))
    (setq *ORGTRELLO/HMAP-USERS-NAME-ID*            (orgtrello-hash/make-properties org-trello-users))
    (setq *ORGTRELLO/USER-LOGGED-IN*                (orgtrello-buffer/me!))

    (mapc (lambda (color) (add-to-list 'org-tag-alist color))
          '(("red" . ?r) ("green" . ?g) ("yellow" . ?y) ("blue" . ?b) ("purple" . ?p) ("orange" . ?o)))
    :ok))

(defun orgtrello-controller/control-properties! (&optional args)
  "Org-trello needs some header buffer properties set (board id, list ids, ...).
Return :ok if ok, or the error message if problems.
ARGS is not used."
  (let ((hmap-count (hash-table-count *ORGTRELLO/HMAP-LIST-ORGKEYWORD-ID-NAME*)))
    (if (and (orgtrello-buffer/org-file-properties!) (orgtrello-buffer/board-id!) (= (length *ORGTRELLO/ORG-KEYWORD-TRELLO-LIST-NAMES*) hmap-count))
        :ok
      "Setup problem.\nEither you did not connect your org-mode buffer with a trello board, to correct this:\n  * attach to a board through C-c o I or M-x org-trello/install-board-metadata\n  * or create a board from scratch with C-c o b or M-x org-trello/create-board-and-install-metadata).\nEither your org-mode's todo keyword list and your trello board lists are not named the same way (which they must).\nFor this, connect to trello and rename your board's list according to your org-mode's todo list.\nAlso, you can specify on your org-mode buffer the todo list you want to work with, for example: #+TODO: TODO DOING | DONE FAIL (hit C-c C-c to refresh the setup)")))

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

(defun orgtrello-controller/--on-entity-p (entity)
  "Compute if the org-trello ENTITY exists.
If it does not not, error."
  (if entity :ok "You need to be on an org-trello entity (card/checklist/item) for this action to occur!"))

(defun orgtrello-controller/--right-level-p (entity)
  "Compute if the ENTITY level is correct (not higher than level 4)."
  (if (and entity (< (-> entity orgtrello-data/current orgtrello-data/entity-level) *ORGTRELLO/OUTOFBOUNDS-LEVEL*)) :ok "Wrong level. Do not deal with entity other than card/checklist/item!"))

(defun orgtrello-controller/--already-synced-p (entity)
  "Compute if the ENTITY has already been synchronized."
  (if (-> entity orgtrello-data/current orgtrello-data/entity-id) :ok "Entity must been synchronized with trello first!"))

(defun orgtrello-controller/--entity-mandatory-name-ok-p (simple-entity)
  "Ensure SIMPLE-ENTITY can be synced regarding the mandatory data."
  (if simple-entity
    (let* ((level   (orgtrello-data/entity-level simple-entity))
           (name    (orgtrello-data/entity-name simple-entity)))
      (if (and name (< 0 (length name)))
          :ok
        (cond ((= level *ORGTRELLO/CARD-LEVEL*)      *ORGTRELLO/ERROR-SYNC-CARD-MISSING-NAME*)
              ((= level *ORGTRELLO/CHECKLIST-LEVEL*) *ORGTRELLO/ERROR-SYNC-CHECKLIST-MISSING-NAME*)
              ((= level *ORGTRELLO/ITEM-LEVEL*)      *ORGTRELLO/ERROR-SYNC-ITEM-MISSING-NAME*))))
    :ok))

(defun orgtrello-controller/--mandatory-name-ok-p (entity)
  "Ensure ENTITY can be synced regarding the mandatory data."
  (-> entity
    orgtrello-data/current
    orgtrello-controller/--entity-mandatory-name-ok-p))

(defun orgtrello-controller/checks-then-delete-simple ()
  "Do the deletion of an entity."
  (orgtrello-action/functional-controls-then-do '(orgtrello-controller/--on-entity-p orgtrello-controller/--right-level-p orgtrello-controller/--already-synced-p)
                                                (orgtrello-buffer/safe-entry-full-metadata!)
                                                'orgtrello-controller/delete-card!
                                                (current-buffer)))

(defun orgtrello-controller/delete-card! (full-meta &optional buffer-name)
  "Execute on FULL-META the ACTION.
BUFFER-NAME to specify the buffer with which we currently work."
  (with-current-buffer buffer-name
    (let* ((current (orgtrello-data/current full-meta))
           (marker  (orgtrello-buffer/--compute-marker-from-entry current)))
      (orgtrello-buffer/set-marker-if-not-present! current marker)
      (orgtrello-data/put-entity-id marker current)
      (eval (orgtrello-proxy/--delete current)))))

(defun orgtrello-controller/checks-then-sync-card-to-trello! ()
  "Execute checks then do the actual sync if everything is ok."
  (orgtrello-action/functional-controls-then-do '(orgtrello-controller/--on-entity-p orgtrello-controller/--right-level-p orgtrello-controller/--mandatory-name-ok-p)
                                                (orgtrello-buffer/safe-entry-full-metadata!)
                                                'orgtrello-controller/sync-card-to-trello!
                                                (current-buffer)))

(defun orgtrello-controller/sync-card-to-trello! (full-meta &optional buffer-name)
  "Do the actual card creation/update - from card to item."
  (let ((current-checksum (orgtrello-buffer/card-checksum!))
        (previous-checksum (orgtrello-buffer/get-card-local-checksum!)))
    (if (string= current-checksum previous-checksum)
        (orgtrello-log/msg *OT/INFO* "Card already synchronized, nothing to do!")
      (progn
        (orgtrello-log/msg *OT/INFO* "Synchronizing card on board '%s'..." (orgtrello-buffer/board-name!))
        (org-show-subtree) ;; we need to show the subtree, otherwise https://github.com/org-trello/org-trello/issues/53
        (-> buffer-name
          orgtrello-buffer/build-org-card-structure!
          orgtrello-controller/execute-sync-entity-structure!)))))

(defun orgtrello-controller/do-sync-buffer-to-trello! ()
  "Full org-mode file synchronisation."
  (orgtrello-log/msg *OT/WARN* "Synchronizing org-mode file to the board '%s'. This may take some time, some coffee may be a good idea..." (orgtrello-buffer/board-name!))
  (-> (current-buffer)
    orgtrello-buffer/build-org-entities!
    orgtrello-controller/execute-sync-entity-structure!))

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
    (goto-char (point-min))                 ;; go back to the beginning of file
    (ignore-errors (org-sort-entries t ?o)) ;; sort the entries on their keywords and ignore if there are errors (if nothing to sort for example)
    (org-global-cycle '(4))))               ;; fold all entries

(defun orgtrello-controller/--cleanup-org-entries ()
  "Cleanup org-entries from the buffer.
Does not preserve position."
  (goto-char (point-min))
  (outline-next-heading)
  (orgtrello-buffer/remove-overlays! (point-at-bol) (point-max))
  (kill-region (point-at-bol) (point-max)))

(defun orgtrello-controller/sync-buffer-with-trello-cards! (buffer-name org-trello-cards)
  "Synchronize the buffer BUFFER-NAME with the TRELLO-CARDS."
  (with-local-quit
    (with-current-buffer buffer-name
      (save-excursion
        (let ((entities-from-org-buffer (orgtrello-buffer/build-org-entities! buffer-name)))
          (-> org-trello-cards
            orgtrello-backend/compute-org-trello-card-from
            (orgtrello-data/merge-entities-trello-and-org entities-from-org-buffer)
            ((lambda (entry) (orgtrello-controller/--cleanup-org-entries) entry))   ;; hack to clean the org entries just before synchronizing the buffer
            orgtrello-controller/--sync-buffer-with-trello-data))))))

(defun orgtrello-controller/do-sync-buffer-from-trello! ()
  "Full org-mode file synchronisation. Beware, this will block emacs as the request is synchronous."
  (lexical-let ((buffer-name (current-buffer))
                (board-name  (orgtrello-buffer/board-name!))
                (point-start (point))
                (board-id (orgtrello-buffer/board-id!)))
    (orgtrello-log/msg *OT/INFO* "Synchronizing the trello board '%s' to the org-mode file..." board-name)
    (deferred:$
      (deferred:parallel ;; concurrently retrieve:
        (deferred:next ;; - `'trello archived`' card
          (lambda ()
            (-> board-id
              orgtrello-api/get-archived-cards
              (orgtrello-query/http-trello 'sync))))
        (deferred:next ;; - `'trello opened`' card
          (lambda ()
            (-> board-id
              orgtrello-api/get-full-cards
              (orgtrello-query/http-trello 'sync)))))
      (deferred:nextc it
        (lambda (trello-archived-and-trello-opened-cards)
          (let ((trello-archived-cards (elt trello-archived-and-trello-opened-cards 0))
                (trello-cards (elt trello-archived-and-trello-opened-cards 1)))
            ;; first archive the cards that needs to be
            (orgtrello-log/msg *OT/DEBUG* "Archived trello-cards: %S" trello-archived-cards)
            (orgtrello-buffer/archive-cards! trello-archived-cards)
            ;; Then update the buffer with the other opened trello cards
            (orgtrello-log/msg *OT/DEBUG* "Opened trello-cards: %S" trello-cards)
            (->> trello-cards
              (mapcar 'orgtrello-data/to-org-trello-card)
              (orgtrello-controller/sync-buffer-with-trello-cards! buffer-name)))))
      (deferred:nextc it
        (lambda ()
          (orgtrello-buffer/save-buffer buffer-name)
          (goto-char point-start)
          (orgtrello-log/msg *OT/INFO* "Synchronizing the trello board '%s' to the org-mode file '%s' done!" board-name buffer-name)))
      (deferred:error it
        (lambda (err) (orgtrello-log/msg *OT/ERROR* "Sync buffer from trello - Catch error: %S" err))))))

(defun orgtrello-controller/check-trello-connection! ()
  "Full org-mode file synchronisation. Beware, this will block emacs as the request is synchronous."
  (orgtrello-log/msg *OT/INFO* "Checking trello connection...")
  (deferred:$
    (deferred:next (lambda () (orgtrello-query/http-trello (orgtrello-api/get-me) 'sync)))
    (deferred:nextc it
      (lambda (user-me)
        (orgtrello-log/msg *OT/INFO*
                           (if user-me
                               (format "Account '%s' configured! Everything is ok!" (orgtrello-data/entity-username user-me))
                             "There is a problem with your credentials.\nMake sure you used M-x org-trello/install-key-and-token and installed correctly the consumer-key and access-token.\nSee http://org-trello.github.io/trello-setup.html#credentials for more information."))))
    (deferred:error it
      (lambda (err) (orgtrello-log/msg *OT/ERROR* "Setup ko - '%s'" err)))))

(defun orgtrello-controller/execute-sync-entity-structure! (entity-structure)
  "Execute synchronization of ENTITY-STRUCTURE (entities at first position, adjacency list in second position).
The entity-structure is self contained.
Synchronization is done here.
Along the way, the buffer BUFFER-NAME is written with new informations."
  (lexical-let ((entities             (car entity-structure))
                (entities-adjacencies entity-structure)
                (card-computations))
    (maphash (lambda (id entity)
               (when (and (orgtrello-data/entity-card-p entity) (eq :ok (orgtrello-controller/--entity-mandatory-name-ok-p entity)))
                 (-> entity
                   (orgtrello-proxy/--sync-entity entities-adjacencies)
                   (push card-computations))))
             entities)

    (if card-computations
        (-> card-computations
          nreverse
          (orgtrello-proxy/execute-async-computations "card(s) sync ok!" "FAILURE! cards(s) sync KO!"))
      (orgtrello-log/msg *OT/INFO* "No card(s) to sync."))))

(defun orgtrello-controller/compute-and-overwrite-card! (buffer-name org-trello-card)
  "Given BUFFER-NAME and TRELLO-CARD, compute, merge and update the buffer-name."
  (when org-trello-card
    (with-local-quit
      (with-current-buffer buffer-name
        (save-excursion
          (let* ((card-id                  (orgtrello-data/entity-id org-trello-card))
                 (region                   (orgtrello-entity/compute-card-region!))
                 (entities-from-org-buffer (apply 'orgtrello-buffer/build-org-entities! (cons buffer-name region)))
                 (entities-from-trello     (orgtrello-backend/compute-org-trello-card-from (list org-trello-card)))
                 (merged-entities          (orgtrello-data/merge-entities-trello-and-org entities-from-trello entities-from-org-buffer))
                 (entities                 (car merged-entities))
                 (entities-adj             (cadr merged-entities)))
            (orgtrello-buffer/overwrite-card! region (gethash card-id entities) entities entities-adj)))))))

(defun orgtrello-controller/checks-then-sync-card-from-trello! ()
  "Execute checks then do the actual sync if everything is ok."
  (orgtrello-action/functional-controls-then-do '(orgtrello-controller/--on-entity-p orgtrello-controller/--right-level-p orgtrello-controller/--already-synced-p)
                                                (orgtrello-buffer/safe-entry-full-metadata!)
                                                'orgtrello-controller/sync-card-from-trello!
                                                (current-buffer)))

(defun orgtrello-controller/sync-card-from-trello! (full-meta &optional buffer-name)
  "Entity (card/checklist/item) synchronization (with its structure) from trello.
Optionally, SYNC permits to synchronize the query."
  (lexical-let* ((buffer-name buffer-name)
                 (point-start (point))
                 (card-meta (progn (when (not (orgtrello-entity/card-at-pt!)) (orgtrello-entity/back-to-card!))
                                   (orgtrello-data/current (orgtrello-buffer/entry-get-full-metadata!))))
                 (card-name (orgtrello-data/entity-name card-meta)))
    (orgtrello-log/msg *OT/INFO* "Synchronizing the trello card to the org-mode file...")
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
          (->> trello-card
            orgtrello-data/to-org-trello-card
            (orgtrello-controller/compute-and-overwrite-card! buffer-name))))
      (deferred:nextc it
        (lambda ()
          (orgtrello-buffer/save-buffer buffer-name)
          (goto-char point-start)
          (orgtrello-log/msg *OT/INFO* "Synchronizing the trello card '%s' to the org-mode file done!" card-name)))
      (deferred:error it
        (lambda (err) (orgtrello-log/msg *OT/ERROR* "Catch error: %S" err))))))

(defun orgtrello-controller/--do-delete-card ()
  "Delete the card."
  (when (orgtrello-entity/card-at-pt!)
    (orgtrello-controller/checks-then-delete-simple)))

(defun orgtrello-controller/do-delete-entities ()
  "Launch a batch deletion of every single entities present on the buffer.
SYNC flag permit to synchronize the http query."
  (org-map-entries 'orgtrello-controller/--do-delete-card t 'file))

(defun orgtrello-controller/checks-and-do-archive-card ()
  "Check the functional requirements, then if everything is ok, archive the card."
  (let ((buffer-name (current-buffer)))
    (with-current-buffer buffer-name
      (save-excursion
        (let ((card-meta (progn (when (orgtrello-entity/org-checkbox-p!) (orgtrello-entity/back-to-card!))
                                (orgtrello-buffer/entry-get-full-metadata!))))
          (orgtrello-action/functional-controls-then-do '(orgtrello-controller/--right-level-p orgtrello-controller/--already-synced-p)
                                                        card-meta
                                                        'orgtrello-controller/do-archive-card
                                                        buffer-name))))))

(defun orgtrello-controller/do-archive-card (card-meta &optional buffer-name)
  "Archive current CARD-META at point.
BUFFER-NAME specifies the buffer onto which we work."
  (save-excursion
    (lexical-let* ((buffer-name buffer-name)
                   (point-start (point))
                   (card-meta   (orgtrello-data/current card-meta))
                   (card-name   (orgtrello-data/entity-name card-meta)))
      (deferred:$
        (deferred:next
          (lambda () ;; trello archive
            (orgtrello-log/msg *OT/INFO* "Archive card '%s'..." card-name)
            (orgtrello-log/msg *OT/DEBUG* "Archive card '%s' in trello...\n" card-name)
            (-> card-meta
              orgtrello-data/entity-id
              orgtrello-api/archive-card
              (orgtrello-query/http-trello 'sync))))
        (deferred:nextc it
          (lambda (card-result) ;; org archive
            (orgtrello-log/msg *OT/DEBUG* "Archive card '%s' in org..." card-name)
            (with-current-buffer buffer-name
              (goto-char point-start)
              (org-archive-subtree))))
        (deferred:nextc it
          (lambda () ;; save buffer
            (orgtrello-buffer/save-buffer buffer-name)
            (orgtrello-log/msg *OT/INFO* "Archive card '%s' done!" card-name)))))))

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
  (deferred:$
    (deferred:next
      (lambda () (browse-url (org-trello/compute-url "/1/appKey/generate"))))
    (deferred:nextc it
      (lambda ()
        (let ((consumer-key (read-string "Consumer key: ")))
          (browse-url (org-trello/compute-url (format "/1/authorize?response_type=token&name=org-trello&scope=read,write&expiration=never&key=%s" consumer-key)))
          consumer-key)))
    (deferred:nextc it
      (lambda (consumer-key)
        (orgtrello-log/msg *OT/DEBUG* "consumer-key: %S" consumer-key)
        (let ((access-token (read-string "Access token: ")))
          (mapcar 's-trim `(,consumer-key ,access-token)))))
    (deferred:nextc it
      (lambda (consumer-key-and-access-token)
        (orgtrello-log/msg *OT/DEBUG* "consumer-key-and-access-token: %S" consumer-key-and-access-token)
        (apply 'orgtrello-controller/--do-install-config-file consumer-key-and-access-token)))
    (deferred:nextc it
      (lambda () (orgtrello-log/msg *OT/INFO* "Setup key and token done!")))))

(defun orgtrello-controller/--id-name (entities)
  "Given a list of ENTITIES, return a map of (id, name)."
  (--reduce-from (orgtrello-hash/puthash-data (orgtrello-data/entity-id it) (orgtrello-data/entity-name it) acc) (orgtrello-hash/empty-hash) entities))

(defun orgtrello-controller/--name-id (entities)
  "Given a list of ENTITIES, return a map of (id, name)."
  (--reduce-from (orgtrello-hash/puthash-data (orgtrello-data/entity-name it) (orgtrello-data/entity-id it) acc) (orgtrello-hash/empty-hash) entities))

(defun orgtrello-controller/--list-boards! ()
  "Return the map of the existing boards associated to the current account. (Synchronous request)"
  (orgtrello-query/http-trello (orgtrello-api/get-boards "open") 'sync))

(defun orgtrello-controller/--list-board-lists! (board-id)
  "Return the map of the existing list of the board with id board-id. (Synchronous request)"
  (orgtrello-query/http-trello (orgtrello-api/get-lists board-id) 'sync))

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
  "Given a map of boards, ask the user to choose the boards.
This returns the identifier of such board."
  (let* ((index-selected-board    nil)
         (display-board-to-choose (orgtrello-controller/--display-boards-to-choose boards))
         (index-board-map         (orgtrello-controller/--index-board-map boards)))
    ;; keep asking the selection until the choice is possible
    (while (not (gethash index-selected-board index-board-map))
      (setq index-selected-board (read-string (format "%s\nInput the number of the board desired: " display-board-to-choose))))
    (gethash index-selected-board index-board-map)))

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

(defun orgtrello-controller/--update-orgmode-file-with-properties! (board-name board-id board-lists-hash-name-id board-users-hash-name-id user-me board-labels &optional update-todo-keywords)
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
    (orgtrello-query/http-trello 'sync)))

(defun orgtrello-controller/do-install-board-and-lists ()
  "Command to install the list boards."
  (lexical-let ((buffer-name (current-buffer)))
    (deferred:$
      (deferred:parallel ;; retrieve in parallel the open boards and the currently logged in user
        (deferred:next
          'orgtrello-controller/--list-boards!)
        (deferred:next
          'orgtrello-controller/--user-logged-in!))
      (deferred:nextc it
        (lambda (boards-and-user-logged-in)
          (let* ((boards         (elt boards-and-user-logged-in 0))
                 (user-logged-in (orgtrello-data/entity-username (elt boards-and-user-logged-in 1)))
                 (selected-id-board (->> boards
                                      orgtrello-controller/--id-name
                                      orgtrello-controller/choose-board!)))
            (list (car (--filter (string= selected-id-board (orgtrello-data/entity-id it)) boards)) user-logged-in))))
      (deferred:nextc it ;; hack everything has been retrieved with the first requests except for the members
        (lambda (board-and-user-logged-in)
          (-> board-and-user-logged-in
            car
            orgtrello-data/entity-id
            orgtrello-api/get-members
            (orgtrello-query/http-trello 'sync)
            (cons board-and-user-logged-in))))
      (deferred:nextc it
        (lambda (members-board-and-user)
          (cl-destructuring-bind (members chosen-board user-logged-in) members-board-and-user
            (orgtrello-controller/do-write-board-metadata! (orgtrello-data/entity-id chosen-board)
                                                           (orgtrello-data/entity-name chosen-board)
                                                           user-logged-in
                                                           (orgtrello-data/entity-lists chosen-board)
                                                           (orgtrello-data/entity-labels chosen-board)
                                                           (orgtrello-controller/--compute-user-properties-hash members)))))
      (deferred:nextc it
        (lambda ()
          (orgtrello-buffer/save-buffer buffer-name)
          (orgtrello-action/reload-setup!)
          (orgtrello-log/msg *OT/INFO* "Install board and list ids done!"))))))

(defun orgtrello-controller/--compute-user-properties (memberships-map)
  "Given a map MEMBERSHIPS-MAP, extract the map of user information."
  (mapcar 'orgtrello-data/entity-member memberships-map))

(defun orgtrello-controller/--compute-user-properties-hash (user-properties)
  "Compute user's properties from USER-PROPERTIES."
  (--reduce-from (orgtrello-hash/puthash-data (orgtrello-data/entity-username it) (orgtrello-data/entity-id it) acc) (orgtrello-hash/empty-hash) user-properties))

(defun orgtrello-controller/--create-board (board-name &optional board-description)
  "Create a board with name BOARD-NAME and optionally a BOARD-DESCRIPTION."
  (orgtrello-log/msg *OT/INFO* "Creating board '%s' with description '%s'" board-name board-description)
  (orgtrello-query/http-trello (orgtrello-api/add-board board-name board-description) 'sync))

(defun orgtrello-controller/--close-lists (list-ids)
  "Given a list of ids LIST-IDS, close those lists."
  (orgtrello-proxy/execute-async-computations
   (--map (lexical-let ((list-id it))
            (orgtrello-query/http-trello (orgtrello-api/close-list it) nil (lambda (response) (orgtrello-log/msg *OT/INFO* "Closed list with id %s" list-id)) (lambda ())))
         list-ids)
   "List(s) closed."
   "FAILURE - Problem during closing list."))

(defun orgtrello-controller/--create-lists-according-to-keywords (board-id org-keywords)
  "For the BOARD-ID, create the list names from ORG-KEYWORDS.
The list order in the trello board is the same as the ORG-KEYWORDS.
Return the hashmap (name, id) of the new lists created."
  (car
   (--reduce-from (cl-destructuring-bind (hash pos) acc
                    (orgtrello-log/msg *OT/INFO* "Board id %s - Creating list '%s'" board-id it)
                    (list (orgtrello-hash/puthash-data it (orgtrello-data/entity-id (orgtrello-query/http-trello (orgtrello-api/add-list it board-id pos) 'sync)) hash) (+ pos 1)))
                  (list (orgtrello-hash/empty-hash) 1)
                  org-keywords)))

(defun orgtrello-controller/do-create-board-and-install-metadata ()
  "Command to create a board and the lists."
  (lexical-let ((org-keywords (orgtrello-buffer/filtered-kwds!))
                (buffer-name  (current-buffer)))
    (deferred:$
      (deferred:next
        (lambda ()
          (orgtrello-log/msg *OT/DEBUG* "Input from the user.")
          (let ((input-board-name        (orgtrello-input/read-not-empty! "Please, input the desired board name: "))
                (input-board-description (read-string "Please, input the board description (empty for none): ")))
            (list input-board-name input-board-description))))
      (deferred:parallel
        (deferred:nextc it
          (lambda (input-board-name-and-description) ;; compute the current board's information
            (orgtrello-log/msg *OT/DEBUG* "Create the board. - %S" input-board-name-and-description)
            (apply 'orgtrello-controller/--create-board input-board-name-and-description)))
        (deferred:next
          (lambda () ;; compute the current user's information
            (orgtrello-log/msg *OT/DEBUG* "Computer user information.")
            (orgtrello-controller/--user-logged-in!))))
      (deferred:nextc it
        (lambda (board-and-user-logged-in)
          (orgtrello-log/msg *OT/DEBUG* "Computer default board lists - %S" board-and-user-logged-in)
          (let ((board (elt board-and-user-logged-in 0))
                (user  (elt board-and-user-logged-in 1)))
            (->> board
              orgtrello-data/entity-id
              orgtrello-controller/--list-board-lists!
              (mapcar 'orgtrello-data/entity-id)
              (list board user)))))
      (deferred:nextc it
        (lambda (board-user-list-ids)
          (orgtrello-log/msg *OT/DEBUG* "Close default lists - %S" board-user-list-ids)
          (cl-destructuring-bind (_ _ list-ids) board-user-list-ids
            (orgtrello-controller/--close-lists list-ids))
          board-user-list-ids))
      (deferred:nextc it
        (lambda (board-user-list-ids)
          (orgtrello-log/msg *OT/DEBUG* "Create user's list in board - %S" board-user-list-ids)
          (cl-destructuring-bind (board user list-ids) board-user-list-ids
            (--> board
              (orgtrello-data/entity-id it)
              (orgtrello-controller/--create-lists-according-to-keywords it org-keywords)
              (list board user it)))))
      (deferred:nextc it
        (lambda (board-user-list-ids)
          (orgtrello-log/msg *OT/DEBUG* "Update buffer with metadata - %S" board-user-list-ids)
          (cl-destructuring-bind (board user board-lists-hname-id) board-user-list-ids
            (orgtrello-controller/do-cleanup-from-buffer!)
            (orgtrello-controller/--update-orgmode-file-with-properties! (orgtrello-data/entity-name board)
                                                                         (orgtrello-data/entity-id board)
                                                                         board-lists-hname-id
                                                                         (orgtrello-hash/make-properties `((,(orgtrello-data/entity-username user) . ,(orgtrello-data/entity-id user))))
                                                                         (orgtrello-data/entity-username user)
                                                                         (orgtrello-hash/make-properties '((:red . "") (:green . "") (:yellow . "") (:purple . "") (:blue . "") (:orange . "")))
                                                                         org-keywords))))
      (deferred:nextc it
        (lambda ()
          (orgtrello-buffer/save-buffer buffer-name)
          (orgtrello-action/reload-setup!)
          (orgtrello-log/msg *OT/INFO* "Create board and lists done!"))))))

(defun orgtrello-controller/--add-user (user users)
  "Add the USER to the USERS list."
  (if (member user users) users (cons user users)))

(defun orgtrello-controller/--remove-user (user users)
  "Remove the USER from the USERS list."
  (if (member user users) (remove user users) users users))

(defun orgtrello-controller/do-assign-me ()
  "Command to assign oneself to the card."
  (--> (orgtrello-buffer/get-usernames-assigned-property!)
    (orgtrello-data/--users-from it)
    (orgtrello-controller/--add-user *ORGTRELLO/USER-LOGGED-IN* it)
    (orgtrello-data/--users-to it)
    (orgtrello-buffer/set-usernames-assigned-property! it)))

(defun orgtrello-controller/do-unassign-me ()
  "Command to unassign oneself of the card."
  (--> (orgtrello-buffer/get-usernames-assigned-property!)
    (orgtrello-data/--users-from it)
    (orgtrello-controller/--remove-user *ORGTRELLO/USER-LOGGED-IN* it)
    (orgtrello-data/--users-to it)
    (orgtrello-buffer/set-usernames-assigned-property! it)))

(defun orgtrello-controller/do-show-card-comments! ()
  "Show the card comments in a temporary buffer."
  (save-excursion
    (orgtrello-entity/back-to-card!)
    (let* ((current-card-name (-> (orgtrello-buffer/entity-metadata!) orgtrello-data/entity-name))
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
    (orgtrello-entity/back-to-card!)
    (let* ((card-id (-> (orgtrello-buffer/entity-metadata!) orgtrello-data/entity-id))
           (comment (read-string "Add a comment: ")))
      (if (or (null card-id) (string= "" card-id) (string= "" comment))
          (message "Empty comment - skip.")
        (orgtrello-query/http-trello (orgtrello-api/add-card-comment card-id comment) 'sync
                                     (function* (lambda (&key data &allow-other-keys) "Synchronize the buffer with the response data."
                                                  (orgtrello-log/msg *OT/TRACE* "proxy - response data: %S" data)
                                                  (orgtrello-controller/--update-comments! comment)
                                                  (when *ORGTRELLO/DO-SHOW-CARD-COMMENTS-AFTER-ADDING*
                                                    (orgtrello-controller/do-show-card-comments!)))))))))

(defun orgtrello-controller/do-cleanup-from-buffer! (&optional globally-flag)
  "Clean org-trello data in current buffer.
When GLOBALLY-FLAG is not nil, remove also local entities properties."
  (orgtrello-controller/--remove-properties-file! *ORGTRELLO/ORG-KEYWORD-TRELLO-LIST-NAMES* *ORGTRELLO/HMAP-USERS-NAME-ID* *ORGTRELLO/USER-LOGGED-IN* t) ;; remove any orgtrello relative entries
  (when globally-flag
    (mapc 'orgtrello-buffer/delete-property! `(,*ORGTRELLO/ID* ,*ORGTRELLO/USERS-ENTRY* ,*ORGTRELLO/CARD-COMMENTS*))))

(defun orgtrello-controller/do-write-board-metadata! (board-id board-name user-logged-in board-lists board-labels board-users-name-id)
  "Given a board id, write in the current buffer the updated data."
  (let* ((board-lists-hname-id (orgtrello-controller/--name-id board-lists))
         (board-list-keywords  (orgtrello-controller/--hash-table-keys board-lists-hname-id)))
    (orgtrello-controller/do-cleanup-from-buffer!)
    (orgtrello-controller/--update-orgmode-file-with-properties! board-name
                                                                 board-id
                                                                 board-lists-hname-id
                                                                 board-users-name-id
                                                                 user-logged-in
                                                                 board-labels
                                                                 board-list-keywords)))

(defun orgtrello-controller/do-update-board-metadata! ()
  "Update metadata about the current board we are connected to."
  (lexical-let ((buffer-name (current-buffer)))
    (deferred:$
      (deferred:next
        (lambda ()
          (-> (orgtrello-buffer/board-id!)
            orgtrello-api/get-board
            (orgtrello-query/http-trello 'sync))))
      (deferred:nextc it
        (lambda (board)
          (let ((members (->> board
                           orgtrello-data/entity-memberships
                           orgtrello-controller/--compute-user-properties
                           orgtrello-controller/--compute-user-properties-hash)))
            (orgtrello-controller/do-write-board-metadata! (orgtrello-data/entity-id board)
                                                           (orgtrello-data/entity-name board)
                                                           (orgtrello-buffer/me!)
                                                           (orgtrello-data/entity-lists board)
                                                           (orgtrello-data/entity-labels board)
                                                           members))))
      (deferred:nextc it
        (lambda ()
          (orgtrello-buffer/save-buffer buffer-name)
          (orgtrello-action/reload-setup!)
          (orgtrello-log/msg *OT/INFO* "Update board information done!"))))))

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
    (-when-let (card-id (->> full-meta (funcall right-entity-fn) orgtrello-data/entity-id))
        (browse-url (org-trello/compute-url (format "/c/%s" card-id))))))

(defun orgtrello-controller/jump-to-board! ()
  "Given the current position, execute the information extraction and jump to board action."
  (->> (orgtrello-buffer/board-id!)
    (format "/b/%s")
    org-trello/compute-url
    browse-url))

(defun orgtrello-controller/delete-setup! ()
  "Global org-trello metadata clean up."
  (orgtrello-controller/do-cleanup-from-buffer! t)
  (orgtrello-log/msg *OT/NOLOG* "Cleanup done!"))

(defun orgtrello-buffer/prepare-buffer! ()
  "Prepare the buffer to receive org-trello data."
  (when (and (eq major-mode 'org-mode) org-trello/mode)
    (orgtrello-buffer/install-overlays!)
    (orgtrello-buffer/indent-card-descriptions!)
    (orgtrello-buffer/indent-card-data!)))

(defun orgtrello-controller/mode-on-hook-fn ()
  "Start org-trello hook function to install some org-trello setup."
  ;; Activate org-trello/mode
  (setq org-trello/mode 'activated)
  ;; buffer-invisibility-spec
  (add-to-invisibility-spec '(org-trello-cbx-property)) ;; for an ellipsis (-> ...) change to '(org-trello-cbx-property . t)
  ;; installing hooks
  (add-hook 'before-save-hook 'orgtrello-buffer/prepare-buffer!)
  ;; prepare the buffer at activation time
  (orgtrello-buffer/prepare-buffer!)
  ;; run hook at startup
  (run-hooks 'org-trello-mode-hook))

(defun orgtrello-controller/mode-off-hook-fn ()
  "Stop org-trello hook function to deinstall some org-trello setup."
  ;; remove the invisible property names
  (remove-from-invisibility-spec '(org-trello-cbx-property)) ;; for an ellipsis (...) change to '(org-trello-cbx-property . t)
  ;; removing hooks
  (remove-hook 'before-save-hook 'orgtrello-buffer/prepare-buffer!)
  ;; remove org-trello overlays
  (orgtrello-buffer/remove-overlays!)
  ;; deactivate org-trello/mode
  (setq org-trello/mode))

(orgtrello-log/msg *OT/DEBUG* "orgtrello-controller loaded!")

(provide 'org-trello-controller)
;;; org-trello-controller.el ends here
