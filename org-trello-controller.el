;;; org-trello-controller.el --- Controller of org-trello mode
;;; Commentary:
;;; Code:

(defun orgtrello-controller/compute-marker (buffer-name name position)
  "Compute the orgtrello marker which is composed of buffer-name, name and position"
  (->> (list *ORGTRELLO/MARKER* buffer-name name (if (stringp position) position (int-to-string position)))
    (-interpose "-")
    (apply 'concat)
    sha1
    (concat *ORGTRELLO/MARKER* "-")))

(defun orgtrello-controller/--list-user-entries (properties)
  "List the users entries."
  (--filter (string-match-p *ORGTRELLO/USER-PREFIX* (car it)) properties))

(defun orgtrello-controller/setup-properties (&optional args)
  "Setup the properties according to the org-mode setup. Return :ok."
  ;; read the setup
  (orgtrello-action/reload-setup)
  ;; now exploit some
  (let* ((list-keywords (reverse (orgtrello-buffer/filtered-kwds!)))
         (hmap-id-name (--reduce-from (orgtrello-data/puthash-data (orgtrello-buffer/org-file-get-property! it) it acc)
                                      (orgtrello-hash/empty-hash)
                                      list-keywords))
         (list-users (orgtrello-controller/--list-user-entries (orgtrello-buffer/org-file-properties!)))
         (hmap-user-id-name (orgtrello-hash/make-transpose-properties list-users))
         (hmap-user-name-id (orgtrello-hash/make-properties list-users)))
    (setq *ORGTRELLO/LIST-NAMES*   list-keywords)
    (setq *ORGTRELLO/HMAP-LIST-ORGKEYWORD-ID-NAME* hmap-id-name)
    (setq *ORGTRELLO/HMAP-USERS-ID-NAME* hmap-user-id-name)
    (setq *ORGTRELLO/HMAP-USERS-NAME-ID* hmap-user-name-id)
    (setq *ORGTRELLO/USER-LOGGED-IN* (orgtrello-buffer/me!))
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
  (let ((hmap-count (hash-table-count *ORGTRELLO/HMAP-LIST-ORGKEYWORD-ID-NAME*)))
    (if (and (orgtrello-buffer/org-file-properties!) (orgtrello-buffer/board-id!) (= (length *ORGTRELLO/LIST-NAMES*) hmap-count))
        :ok
      "Setup problem.\nEither you did not connect your org-mode buffer with a trello board, to correct this:\n  * attach to a board through C-c o I or M-x org-trello/install-board-and-lists-ids\n  * or create a board from scratch with C-c o b or M-x org-trello/create-board).\nEither your org-mode's todo keyword list and your trello board lists are not named the same way (which they must).\nFor this, connect to trello and rename your board's list according to your org-mode's todo list.\nAlso, you can specify on your org-mode buffer the todo list you want to work with, for example: #+TODO: TODO DOING | DONE FAIL (hit C-c C-c to refresh the setup)")))

(defun orgtrello-controller/load-keys (&optional args)
  "Load the credentials keys from the configuration file."
  (if (and (file-exists-p *ORGTRELLO/CONFIG-FILE*) (load *ORGTRELLO/CONFIG-FILE*))
      :ok
    "Setup problem - Problem during credentials (consumer-key and the read/write access-token) loading - C-c o i or M-x org-trello/install-key-and-token"))

(defun orgtrello-controller/control-keys (&optional args)
  "org-trello needs the *consumer-key* and the *access-token* to access the trello resources. Returns :ok if everything is ok, or the error message if problems."
  (if (and *consumer-key* *access-token*)
      :ok
    "Setup problem - You need to install the consumer-key and the read/write access-token - C-c o i or M-x org-trello/install-key-and-token"))

(defun orgtrello-controller/--retrieve-state-of-card (card-meta)
  "Given a card, retrieve its state depending on its :keyword metadata. If empty or no keyword then, its equivalence is *ORGTRELLO/TODO*, otherwise, return its current state."
  (-if-let (card-kwd (orgtrello-data/entity-keyword card-meta *ORGTRELLO/TODO*)) card-kwd *ORGTRELLO/TODO*))

(defun orgtrello-controller/--checks-before-sync-card (card-meta)
  "Checks done before synchronizing the cards."
  (if (orgtrello-data/entity-name card-meta) :ok *ORGTRELLO/ERROR-SYNC-CARD-MISSING-NAME*))

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
        *ORGTRELLO/ERROR-SYNC-CHECKLIST-SYNC-CARD-FIRST*)
    *ORGTRELLO/ERROR-SYNC-CHECKLIST-MISSING-NAME*))

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
            (if orgtrello-controller/--card-id :ok *ORGTRELLO/ERROR-SYNC-ITEM-SYNC-CARD-FIRST*)
          *ORGTRELLO/ERROR-SYNC-ITEM-SYNC-CHECKLIST-FIRST*)
      *ORGTRELLO/ERROR-SYNC-ITEM-MISSING-NAME*)))

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

(defvar *MAP-DISPATCH-CREATE-UPDATE* (orgtrello-hash/make-properties `((,*ORGTRELLO/CARD-LEVEL*      . orgtrello-controller/--card)
                                                                       (,*ORGTRELLO/CHECKLIST-LEVEL* . orgtrello-controller/--checklist)
                                                                       (,*ORGTRELLO/ITEM-LEVEL*      . orgtrello-controller/--item))) "Dispatch map for the creation/update of card/checklist/item.")

(defun orgtrello-controller/--dispatch-create (entry-metadata)
  "Dispatch the creation depending on the nature of the entry."
  (let ((current-meta        (orgtrello-data/current entry-metadata)))
    (-> current-meta
      orgtrello-data/entity-level
      (gethash *MAP-DISPATCH-CREATE-UPDATE* 'orgtrello-controller/--too-deep-level)
      (funcall current-meta (orgtrello-data/parent entry-metadata) (orgtrello-data/grandparent entry-metadata)))))

(defun orgtrello-controller/--update-query-with-org-metadata (query-map position buffer-name &optional name success-callback sync)
  "Given a trello query, add proxy metadata needed to work."
  (when success-callback (orgtrello-data/put-entity-callback success-callback query-map))
  (when sync             (orgtrello-data/put-entity-sync     sync             query-map))
  (when name             (orgtrello-data/put-entity-name     name             query-map))
  (orgtrello-data/put-entity-position     position                            query-map)
  (orgtrello-data/put-entity-buffername   buffer-name                         query-map))

(defun orgtrello-buffer/--compute-marker-from-entry (entry)
  "Compute and set the marker (either a sha1 or the id of the entry-metadata)."
  (-if-let (current-entry-id (orgtrello-data/entity-id entry))
      current-entry-id
    (orgtrello-controller/compute-marker (orgtrello-data/entity-buffername entry) (orgtrello-data/entity-name entry) (orgtrello-data/entity-position entry))))

(defun orgtrello-controller/--right-level-p (entity)
  "Compute if the level is correct (not higher than level 4)."
  (if (< (-> entity orgtrello-data/current orgtrello-data/entity-level) *ORGTRELLO/OUTOFBOUNDS-LEVEL*) :ok "Level too high. Do not deal with entity other than card/checklist/items!"))

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
      (cond ((= level *ORGTRELLO/CARD-LEVEL*)      *ORGTRELLO/ERROR-SYNC-CARD-MISSING-NAME*)
            ((= level *ORGTRELLO/CHECKLIST-LEVEL*) *ORGTRELLO/ERROR-SYNC-CHECKLIST-MISSING-NAME*)
            ((= level *ORGTRELLO/ITEM-LEVEL*)      *ORGTRELLO/ERROR-SYNC-ITEM-MISSING-NAME*)))))

(defun orgtrello-controller/--delegate-to-the-proxy (full-meta action)
  "Execute the delegation to the consumer."
  (let* ((current (orgtrello-data/current full-meta))
         (marker  (orgtrello-buffer/--compute-marker-from-entry current)))
    (orgtrello-buffer/set-marker-if-not-present current marker)
    (orgtrello-data/put-entity-id     marker current)
    (orgtrello-data/put-entity-action action current)
    (orgtrello-proxy/http-producer current)))

(defun orgtrello-controller/--checks-then-delegate-action-on-entity-to-proxy (functional-controls action)
  "Execute the functional controls then if all pass, delegate the action 'action' to the proxy."
  (orgtrello-action/functional-controls-then-do functional-controls (orgtrello-buffer/entry-get-full-metadata!) 'orgtrello-controller/--delegate-to-the-proxy action))

(defun orgtrello-controller/do-delete-simple (&optional sync)
  "Do the deletion of an entity."
  (orgtrello-controller/--checks-then-delegate-action-on-entity-to-proxy '(orgtrello-controller/--right-level-p orgtrello-controller/--already-synced-p) *ORGTRELLO/ACTION-DELETE*))

(defun orgtrello-controller/do-sync-entity-to-trello! ()
  "Do the entity synchronization (if never synchronized, will create it, update it otherwise)."
  (orgtrello-controller/--checks-then-delegate-action-on-entity-to-proxy '(orgtrello-controller/--right-level-p orgtrello-controller/--mandatory-name-ok-p) *ORGTRELLO/ACTION-SYNC*))

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
  (orgtrello-buffer/org-map-entries *ORGTRELLO/CARD-LEVEL* 'orgtrello-controller/do-sync-full-entity-to-trello!))

(defun orgtrello-controller/--sync-buffer-with-trello-data (data buffer-name)
  "Given all the entities, update the current buffer with those."
  (let ((entities (car data))
        (adjacency (cadr data)))
    (with-current-buffer buffer-name
      (goto-char (point-max)) ;; go at the end of the file
      (maphash
       (lambda (new-id entity)
         (when (orgtrello-data/entity-card-p entity)
           (orgtrello-buffer/write-card! new-id entity entities adjacency)))
       entities)
      (goto-char (point-min)) ;; go back to the beginning of file
      (org-sort-entries t ?o) ;; sort the entries on their keywords
      (org-global-cycle '(4)) ;; fold all entries
      (save-buffer))))

(defun orgtrello-controller/--cleanup-org-entries ()
  "Cleanup org-entries from the buffer."
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
                 (-> data                                                                  ;; compute merge between already sync'ed entries and the trello data
                   orgtrello-backend/compute-full-cards-from-trello!                       ;; slow computation with network access
                   (orgtrello-data/merge-entities-trello-and-org entities-from-org-buffer) ;; slow merge computation
                   ((lambda (entry) (orgtrello-controller/--cleanup-org-entries) entry))   ;; hack to clean the org entries just before synchronizing the buffer
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
                     (cond ((orgtrello-data/entity-card-p data)      'orgtrello-buffer/overwrite-and-merge-card-header!)
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
         (entities                 (car merged-entities))
         (entities-adj             (cadr merged-entities)))
    (orgtrello-buffer/clean-region! region)
    (orgtrello-buffer/write-card! card-id (gethash card-id entities) entities entities-adj)))

(defun orgtrello-controller/fetch-and-overwrite-checklist! (checklist)
  "Given a checklist, retrieve latest information from trello and overwrite in current buffer."
  (let* ((checklist-id             (orgtrello-data/entity-id checklist))
         (region                   (orgtrello-buffer/compute-entity-region! checklist))
         (entities-from-org-buffer (apply 'orgtrello-buffer/compute-entities-from-org-buffer! (cons nil region)))
         (entities-from-trello     (orgtrello-backend/compute-full-checklist-from-trello! checklist))
         (merged-entities          (orgtrello-data/merge-entities-trello-and-org entities-from-trello entities-from-org-buffer))
         (entities                 (car merged-entities))
         (entities-adj             (cadr merged-entities)))
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
    (cond ((= level *ORGTRELLO/CARD-LEVEL*)      (orgtrello-api/get-card entity-id))
          ((= level *ORGTRELLO/CHECKLIST-LEVEL*) (orgtrello-api/get-checklist entity-id with-filter))
          ((= level *ORGTRELLO/ITEM-LEVEL*)      (orgtrello-api/get-item parent-id entity-id)))))

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

(defvar *MAP-DISPATCH-DELETE* (orgtrello-hash/make-properties `((,*ORGTRELLO/CARD-LEVEL*      . orgtrello-controller/--card-delete)
                                                                (,*ORGTRELLO/CHECKLIST-LEVEL* . orgtrello-controller/--checklist-delete)
                                                                (,*ORGTRELLO/ITEM-LEVEL*      . orgtrello-controller/--item-delete))) "Dispatch map for the deletion query of card/checklist/item.")

(defun orgtrello-controller/--dispatch-delete (meta &optional parent-meta)
  "Dispatch the delete function to call depending on the level information."
  (-> meta
    orgtrello-data/entity-level
    (gethash *MAP-DISPATCH-DELETE* 'orgtrello-controller/--too-deep-level)
    (funcall meta parent-meta)))

(defun orgtrello-controller/--do-delete-card (&optional sync)
  "Delete the card."
  (when (= *ORGTRELLO/CARD-LEVEL* (-> (orgtrello-buffer/entry-get-full-metadata!)
                          orgtrello-data/current
                          orgtrello-data/entity-level))
    (orgtrello-controller/do-delete-simple sync)))

(defun orgtrello-controller/do-delete-entities (&optional sync)
  "Launch a batch deletion of every single entities present on the buffer."
  (org-map-entries (lambda () (orgtrello-controller/--do-delete-card sync)) t 'file))

(defun orgtrello-controller/--do-install-config-file (consumer-key access-token)
  "Persist the file config-file with the input of the user."
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
  "Given a list of entities, return a map of (id, name)."
  (--reduce-from (orgtrello-data/puthash-data (orgtrello-data/entity-id it) (orgtrello-data/entity-name it) acc) (orgtrello-hash/empty-hash) entities))

(defun orgtrello-controller/--name-id (entities)
  "Given a list of entities, return a map of (id, name)."
  (--reduce-from (orgtrello-data/puthash-data (orgtrello-data/entity-name it) (orgtrello-data/entity-id it) acc) (orgtrello-hash/empty-hash) entities))

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
  "Given a map of board (id . name), return a map of (position . name)"
  (let ((i               0)
        (index-board-map (orgtrello-hash/empty-hash)))
    (maphash (lambda (id _)
               (orgtrello-data/puthash-data (format "%d" i) id index-board-map)
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
                                 (replace-regexp-in-string *ORGTRELLO/USER-PREFIX* "" it)
                                 (format "%s%s" *ORGTRELLO/USER-PREFIX* it)
                                 (orgtrello-controller/compute-property it id)
                                 (push it res-list)))
             users-hash-name-id)
    res-list))

(defun orgtrello-controller/--remove-properties-file! (list-keywords users-hash-name-id user-me &optional update-todo-keywords)
  "Remove the current org-trello header metadata."
  (with-current-buffer (current-buffer)
    ;; compute the list of properties to purge
    (->> `(":PROPERTIES"
           ,(orgtrello-controller/compute-property *ORGTRELLO/BOARD-NAME*)
           ,(orgtrello-controller/compute-property *ORGTRELLO/BOARD-ID*)
           ,@(--map (orgtrello-controller/compute-property (orgtrello-controller/--convention-property-name it)) list-keywords)
           ,@(orgtrello-controller/--compute-hash-name-id-to-list users-hash-name-id)
           ,(orgtrello-controller/compute-property *ORGTRELLO/USER-ME* user-me)
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
    ,(orgtrello-controller/compute-property *ORGTRELLO/BOARD-NAME* board-name)
    ,(orgtrello-controller/compute-property *ORGTRELLO/BOARD-ID* board-id)
    ,@(orgtrello-controller/--compute-board-lists-hash-name-id board-lists-hash-name-id)
    ,(if update-todo-keywords (orgtrello-controller/--properties-compute-todo-keywords-as-string board-lists-hash-name-id) "")
    ,@(orgtrello-controller/--properties-compute-users-ids board-users-hash-name-id)
    ,@(orgtrello-controller/--properties-labels board-labels)
    ,(format "#+PROPERTY: %s %s" *ORGTRELLO/USER-ME* user-me)
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
  "Given a map, extract the map of user informations."
  (mapcar 'orgtrello-data/entity-member memberships-map))

(defun orgtrello-controller/--compute-user-properties-hash (user-properties)
  (--reduce-from (orgtrello-data/puthash-data (orgtrello-data/entity-username it) (orgtrello-data/entity-id it) acc) (orgtrello-hash/empty-hash) user-properties))

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
    (orgtrello-query/http-trello it 'synchronous-query)
    (orgtrello-controller/--compute-user-properties-hash-from-board it)))

(defun orgtrello-controller/--create-board (board-name &optional board-description)
  "Create a board with name and eventually a description."
  (orgtrello-log/msg *OT/INFO* "Creating board '%s'" board-name)
  (let ((board-data (orgtrello-query/http-trello (orgtrello-api/add-board board-name board-description) 'synchronous-query)))
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
                   (orgtrello-log/msg *OT/INFO* "Board id %s - Creating list '%s'" board-id it)
                   (orgtrello-data/puthash-data it (orgtrello-data/entity-id (orgtrello-query/http-trello (orgtrello-api/add-list it board-id) 'synchronous-query)) acc))
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

(defun orgtrello-controller/--add-user (user users) "Add the user to the users list"
       (if (member user users) users (cons user users)))

(defun orgtrello-controller/--remove-user (user users) "Add the user to the users list"
       (if (member user users) (remove user users) users users))

(defun orgtrello-buffer/--user-ids-assigned-to-current-card () "Compute the user ids assigned to the current card."
       (--> (orgtrello-buffer/get-usernames-assigned-property!)
         (orgtrello-data/--users-from it)
         (--map (gethash (format "%s%s" *ORGTRELLO/USER-PREFIX* it) *ORGTRELLO/HMAP-USERS-NAME-ID*) it)
         (orgtrello-data/--users-to it)))

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

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-controller loaded!")

(provide 'org-trello-controller)
;;; org-trello-controller.el ends here
