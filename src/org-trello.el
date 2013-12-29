

(require 'org-trello-log)
(require 'org-trello-setup)

;; #################### org-trello

;; Specific state - FIXME check if they do not already exist on org-mode to avoid potential collisions
(defvar *TODO* "TODO" "org-mode todo state")
(defvar *DONE* "DONE" "org-mode done state")

;; Properties key for the orgtrello headers #+PROPERTY board-id, etc...
(defvar *BOARD-ID*   "board-id" "orgtrello property board-id entry")
(defvar *BOARD-NAME* "board-name" "orgtrello property board-name entry")

(defvar *LIST-NAMES*         nil "orgtrello property names of the different lists. This use the standard 'org-todo-keywords property from org-mode.")
(defvar *HMAP-ID-NAME*       nil "orgtrello hash map containing for each id, the associated name (or org keyword).")
(defvar *HMAP-USERS-ID-NAME* nil "orgtrello hash map containing for each user name, the associated id.")
(defvar *HMAP-USERS-NAME-ID* nil "orgtrello hash map containing for each user id, the associated name.")

(defvar *CONFIG-DIR*  (concat (getenv "HOME") "/" ".trello"))
(defvar *CONFIG-FILE* (concat *CONFIG-DIR* "/config.el"))

(defun orgtrello/filtered-kwds () "org keywords used (based on org-todo-keywords-1)."
  org-todo-keywords-1)

(defun orgtrello/--list-user-entries (properties) "List the users entries."
  (--filter (string-match-p *ORGTRELLO-USER-PREFIX* (car it)) properties))

(defun orgtrello/--setup-properties (&optional args) "Setup the properties according to the org-mode setup. Return :ok."
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
                                    :initial-value (make-hash-table :test 'equal)))
         (orgtrello/--list-users (orgtrello/--list-user-entries org-file-properties))
         (orgtrello/--hmap-user-id-name (orgtrello-hash/make-transpose-properties orgtrello/--list-users))
         (orgtrello/--hmap-user-name-id (orgtrello-hash/make-properties orgtrello/--list-users)))
    (setq *LIST-NAMES*   orgtrello/--list-keywords)
    (setq *HMAP-ID-NAME* orgtrello/--hmap-id-name)
    (setq *HMAP-USERS-ID-NAME* orgtrello/--hmap-user-id-name)
    (setq *HMAP-USERS-NAME-ID* orgtrello/--hmap-user-name-id)
    (setq *ORGTRELLO-USER-LOGGED-IN* (orgtrello/--me))
    :ok))

(defun orgtrello/--control-encoding (&optional args) "Use utf-8, otherwise, there will be trouble."
  (progn
    (orgtrello-log/msg *OT/ERROR* "Ensure you use utf-8 encoding for your org buffer.")
    :ok))

(defun orgtrello/--board-name () "Compute the board's name" (assoc-default *BOARD-NAME* org-file-properties))
(defun orgtrello/--board-id () "Compute the board's id" (assoc-default *BOARD-ID* org-file-properties))

(defun orgtrello/--control-properties (&optional args) "org-trello needs the properties board-id and all list id from the trello board to be setuped on header property file. :ok if ok, or the error message if problems."
  (let ((orgtrello/--hmap-count (hash-table-count *HMAP-ID-NAME*)))
    (if (and org-file-properties (orgtrello/--board-id) (= (length *LIST-NAMES*) orgtrello/--hmap-count))
        :ok
        "Setup problem.\nEither you did not connect your org-mode buffer with a trello board, to correct this:\n  * attach to a board through C-c o I or M-x org-trello/install-board-and-lists-ids\n  * or create a board from scratch with C-c o b or M-x org-trello/create-board).\nEither your org-mode's todo keyword list and your trello board lists are not named the same way (which they must).\nFor this, connect to trello and rename your board's list according to your org-mode's todo list.\nAlso, you can specify on your org-mode buffer the todo list you want to work with, for example: #+TODO: TODO DOING | DONE FAIL (hit C-c C-c to refresh the setup)")))

(defun orgtrello/--control-keys (&optional args) "org-trello needs the *consumer-key* and the *access-token* to access the trello resources. Returns :ok if everything is ok, or the error message if problems."
  (if (or (and *consumer-key* *access-token*)
          ;; the data are not set,
          (and (file-exists-p *CONFIG-FILE*)
               ;; trying to load them
               (load *CONFIG-FILE*)
               ;; still not loaded, something is not right!
               (and *consumer-key* *access-token*)))
      :ok
    "Setup problem - You need to install the consumer-key and the read/write access-token - C-c o i or M-x org-trello/install-board-and-lists-ids"))

(defun orgtrello/--retrieve-state-of-card (card-meta) "Given a card, retrieve its state depending on its :keyword metadata. If empty or no keyword then, its equivalence is *TODO*, otherwise, return its current state."
  (-if-let (orgtrello/--card-kwd (orgtrello/--keyword card-meta *TODO*))
           orgtrello/--card-kwd
           *TODO*))

(defun orgtrello/--checks-before-sync-card (card-meta) "Checks done before synchronizing the cards."
  (-if-let (orgtrello/--card-name (orgtrello/--name card-meta)) :ok *ERROR-SYNC-CARD-MISSING-NAME*))

(defun orgtrello/--card (card-meta &optional parent-meta grandparent-meta) "Deal with create/update card query build. If the checks are ko, the error message is returned."
  (let ((checks-ok-or-error-message (orgtrello/--checks-before-sync-card card-meta)))
    ;; name is mandatory
    (if (equal :ok checks-ok-or-error-message)
        ;; parent and grandparent are useless here
        (let* ((orgtrello/--card-kwd  (orgtrello/--retrieve-state-of-card card-meta))
               (orgtrello/--list-id   (assoc-default orgtrello/--card-kwd org-file-properties))
               (orgtrello/--card-id   (orgtrello/--id    card-meta))
               (orgtrello/--card-name (orgtrello/--name card-meta))
               (orgtrello/--card-due  (orgtrello/--due   card-meta))
               (orgtrello/--user-ids-assigned  (orgtrello/--user-assigned-ids card-meta)))
          (if orgtrello/--card-id
              ;; update
              (orgtrello-api/move-card orgtrello/--card-id orgtrello/--list-id orgtrello/--card-name orgtrello/--card-due orgtrello/--user-ids-assigned)
            ;; create
            (orgtrello-api/add-card orgtrello/--card-name orgtrello/--list-id orgtrello/--card-due orgtrello/--user-ids-assigned)))
      checks-ok-or-error-message)))

(defun orgtrello/--checks-before-sync-checklist (checklist-meta card-meta) "Checks done before synchronizing the checklist."
  (let ((orgtrello/--checklist-name (orgtrello/--name checklist-meta))
        (orgtrello/--card-id        (orgtrello/--id card-meta)))
    (if orgtrello/--checklist-name
        (if orgtrello/--card-id
            :ok
          *ERROR-SYNC-CHECKLIST-SYNC-CARD-FIRST*)
      *ERROR-SYNC-CHECKLIST-MISSING-NAME*)))

(defun orgtrello/--checklist (checklist-meta &optional card-meta grandparent-meta) "Deal with create/update checklist query build. If the checks are ko, the error message is returned."
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

(defun orgtrello/--checks-before-sync-item (item-meta checklist-meta card-meta) "Checks done before synchronizing the checklist."
  (let ((orgtrello/--item-name    (orgtrello/--name item-meta))
        (orgtrello/--checklist-id (orgtrello/--id checklist-meta))
        (orgtrello/--card-id      (orgtrello/--id card-meta)))
    (if orgtrello/--item-name
        (if orgtrello/--checklist-id
            (if orgtrello/--card-id :ok *ERROR-SYNC-ITEM-SYNC-CARD-FIRST*)
          *ERROR-SYNC-ITEM-SYNC-CHECKLIST-FIRST*)
      *ERROR-SYNC-ITEM-MISSING-NAME*)))

(defun orgtrello/--item-compute-state-or-check (checklist-update-items-p item-state checklist-state possible-states) "Compute the item's state/check (for creation/update)."
  (let* ((orgtrello/--item-checked   (first possible-states))
         (orgtrello/--item-unchecked (second possible-states)))
    (cond ((and checklist-update-items-p (string= *DONE* checklist-state))                      orgtrello/--item-checked)
          ((and checklist-update-items-p (or checklist-state (string= *TODO* checklist-state))) orgtrello/--item-unchecked)
          ((string= *DONE* item-state)                                                          orgtrello/--item-checked)
          (t                                                                                    orgtrello/--item-unchecked))))

(defun orgtrello/--item-compute-state (checklist-update-items-p item-state checklist-state) "Compute the item's state (for creation)."
  (orgtrello/--item-compute-state-or-check checklist-update-items-p item-state checklist-state '("complete" "incomplete")))

(defun orgtrello/--item-compute-check (checklist-update-items-p item-state checklist-state) "Compute the item's check status (for update)."
    (orgtrello/--item-compute-state-or-check checklist-update-items-p item-state checklist-state '(t nil)))

(defun orgtrello/--compute-state-from-keyword (state) "Given a state, compute the org equivalent (to use with org-todo function)"
  (if (string= *DONE* state) 'done 'none))

(defun orgtrello/--update-item-according-to-checklist-status (checklist-update-items-p checklist-meta) "Update the item of the checklist according to the status of the checklist."
  (if checklist-update-items-p
      (-> checklist-meta
          orgtrello/--keyword
          orgtrello/--compute-state-from-keyword
          org-todo)))

(defun orgtrello/--item (item-meta &optional checklist-meta card-meta) "Deal with create/update item query build. If the checks are ko, the error message is returned."
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

(defun orgtrello/--too-deep-level (meta &optional parent-meta grandparent-meta) "Deal with too deep level."
  "Your arborescence depth is too deep. We only support up to depth 3.\nLevel 1 - card\nLevel 2 - checklist\nLevel 3 - items")

(defvar *MAP-DISPATCH-CREATE-UPDATE* (orgtrello-hash/make-properties `((,*CARD-LEVEL*      . orgtrello/--card)
                                                                       (,*CHECKLIST-LEVEL* . orgtrello/--checklist)
                                                                       (,*ITEM-LEVEL*      . orgtrello/--item))) "Dispatch map for the creation/update of card/checklist/item.")

(defun orgtrello/--dispatch-create (entry-metadata) "Dispatch the creation depending on the nature of the entry."
  (let ((current-meta        (orgtrello-data/current entry-metadata)))
    (-> current-meta
        orgtrello/--level
        (gethash *MAP-DISPATCH-CREATE-UPDATE* 'orgtrello/--too-deep-level)
        (funcall current-meta (orgtrello-data/parent entry-metadata) (orgtrello-data/grandparent entry-metadata)))))

(defun orgtrello/--update-query-with-org-metadata (query-map position buffer-name &optional name success-callback sync) "Given a trello query, add proxy metadata needed to work."
  (puthash :position       position         query-map)
  (puthash :buffername     buffer-name      query-map)
  (when success-callback (puthash :callback success-callback query-map))
  (when sync             (puthash :sync     sync             query-map))
  (when name             (puthash :name     name             query-map))
  query-map)

(defun orgtrello/--set-marker (marker) "Set a marker to get back to later."
  (orgtrello-action/set-property *ORGTRELLO-ID* marker))

(defun orgtrello/--compute-marker-from-entry (entry) "Compute and set the marker (either a sha1 or the id of the entry-metadata)."
  (-if-let (orgtrello/--current-entry-id (orgtrello/--id entry))
           orgtrello/--current-entry-id
           (orgtrello/compute-marker (orgtrello/--buffername entry) (orgtrello/--name entry) (orgtrello/--position entry))))

(defun orgtrello/--right-level-p (entity) "Compute if the level is correct (not higher than level 4)."
  (if (< (-> entity orgtrello-data/current orgtrello/--level) *OUTOFBOUNDS-LEVEL*) :ok "Level too high. Do not deal with entity other than card/checklist/items!"))

(defun orgtrello/--already-synced-p (entity) "Compute if the entity has already been synchronized."
  (if (-> entity orgtrello-data/current orgtrello/--id) :ok "Entity must been synchronized with trello first!"))

(defun orgtrello/--mandatory-name-ok-p (entity) "Ensure entity can be synced regarding the mandatory data."
  (let* ((current (orgtrello-data/current entity))
         (level   (orgtrello/--level current))
         (name    (orgtrello/--name current)))
    (if (and name (< 0 (length name)))
        :ok
        (cond ((= level *CARD-LEVEL*)      *ERROR-SYNC-CARD-MISSING-NAME*)
              ((= level *CHECKLIST-LEVEL*) *ERROR-SYNC-CHECKLIST-MISSING-NAME*)
              ((= level *ITEM-LEVEL*)      *ERROR-SYNC-ITEM-MISSING-NAME*)))))

(defun orgtrello/--set-marker-if-not-present (current-entity marker) "Set the marker to the entry if we never did."
  (unless (string= (orgtrello/--id current-entity) marker) ;; if never created before, we need a marker to add inside the file
          (orgtrello/--set-marker marker)))

(defun orgtrello/--delegate-to-the-proxy (full-meta action) "Execute the delegation to the consumer."
  (let* ((orgtrello/--current          (orgtrello-data/current full-meta))
         (orgtrello/--marker           (orgtrello/--compute-marker-from-entry orgtrello/--current)))
    (orgtrello/--set-marker-if-not-present orgtrello/--current orgtrello/--marker)
;;    (puthash :user-ids  (orgtrello/--user-ids-assigned-to-current-card) orgtrello/--current)
    (puthash :id        orgtrello/--marker                              orgtrello/--current)
    (puthash :action    action                                          orgtrello/--current)
    (orgtrello-proxy/http-producer orgtrello/--current)))

(defun orgtrello/--checks-then-delegate-action-on-entity-to-proxy (functional-controls action) "Execute the functional controls then if all pass, delegate the action 'action' to the proxy."
  (org-action/--functional-controls-then-do functional-controls (orgtrello-data/entry-get-full-metadata) 'orgtrello/--delegate-to-the-proxy action))

(defun orgtrello/do-delete-simple (&optional sync) "Do the deletion of an entity."
  (orgtrello/--checks-then-delegate-action-on-entity-to-proxy '(orgtrello/--right-level-p orgtrello/--already-synced-p) *ORGTRELLO-ACTION-DELETE*))

(defun orgtrello/do-sync-entity () "Do the entity synchronization (if never synchronized, will create it, update it otherwise)."
  (orgtrello/--checks-then-delegate-action-on-entity-to-proxy '(orgtrello/--right-level-p orgtrello/--mandatory-name-ok-p) *ORGTRELLO-ACTION-SYNC*))

(defun orgtrello/do-sync-full-entity () "Do the actual full card creation - from card to item. Beware full side effects..."
  (orgtrello-log/msg *OT/INFO* "Synchronizing full entity with its structure on board '%s'..." (orgtrello/--board-name))
  ;; in any case, we need to show the subtree, otherwise https://github.com/ardumont/org-trello/issues/53
  (org-show-subtree)
  (if (org-at-heading-p)
      (org-map-tree (lambda () (orgtrello/do-sync-entity) (orgtrello/map-sync-checkboxes)))
      (orgtrello/map-sync-checkboxes)))

(defun orgtrello/map-sync-checkboxes () "Map the sync to checkboxes."
  (when *ORGTRELLO-NATURAL-ORG-CHECKLIST* (orgtrello/map-checkboxes 'orgtrello/do-sync-entity)))

(defun orgtrello/org-map-entries (level fn-to-execute) "Map fn-to-execute to a given entities with level level. fn-to-execute is a function without any parameter."
  (org-map-entries (lambda () (when (= level (orgtrello/--current-level)) (funcall fn-to-execute)))))

(defun orgtrello/do-sync-full-file () "Full org-mode file synchronisation."
  (orgtrello-log/msg *OT/WARN* "Synchronizing org-mode file to the board '%s'. This may take some time, some coffee may be a good idea..." (orgtrello/--board-name))
  (orgtrello/org-map-entries *CARD-LEVEL* 'orgtrello/do-sync-full-entity))

(defun orgtrello/justify-file () "Map over the file and justify entries with checkbox."
  (orgtrello/org-map-entries *CARD-LEVEL* 'orgtrello-cbx/--justify-property-current-line))

(defun orgtrello/--compute-card-status (card-id-list) "Given a card's id, compute its status."
  (gethash card-id-list *HMAP-ID-NAME*))

(defun orgtrello/--compute-due-date (due-date) "Compute the format of the due date."
  (if due-date (format "DEADLINE: <%s>\n" due-date) ""))

(defun orgtrello/--private-compute-card-to-org-entry (name status due-date) "Compute the org format for card."
  (format "* %s %s\n%s" (if status status *TODO*) name (orgtrello/--compute-due-date due-date)))

(defun orgtrello/--compute-card-to-org-entry (card &optional orgcheckbox-p) "Given a card, compute its org-mode entry equivalence. orgcheckbox-p is nil"
  (orgtrello/--private-compute-card-to-org-entry (orgtrello-data/entity-name card) (orgtrello-data/entity-state card) (orgtrello-data/entity-due card)))

(defun orgtrello/--compute-checklist-to-orgtrello-entry (name &optional level status) "Compute the orgtrello format checklist"
  (format "** %s\n" name))

(defun orgtrello/--symbol (sym n) "Compute the repetition of a symbol as a string"
  (--> n
       (-repeat it sym)
       (s-join "" it)))

(defun orgtrello/--space (n) "Given a level, compute the number of space for an org checkbox entry."
  (orgtrello/--symbol " "  n))

(defun orgtrello/--star (n) "Given a level, compute the number of space for an org checkbox entry."
  (orgtrello/--symbol "*"  n))

(defun orgtrello/--compute-state-generic (state list-state) "Computing generic."
  (if (or (string= "complete" state)
          (string= *DONE* state)) (first list-state) (second list-state)))

(defun orgtrello/--compute-state-checkbox (state) "Compute the status of the checkbox"
  (orgtrello/--compute-state-generic state '("[X]" "[-]")))

(defun orgtrello/--compute-state-item-checkbox (state) "Compute the status of the item checkbox"
  (orgtrello/--compute-state-generic state '("[X]" "[ ]")))

(defun orgtrello/--compute-state-item (state) "Compute the status of the checkbox"
  (orgtrello/--compute-state-generic state `(,*DONE* ,*TODO*)))

(defun orgtrello/--compute-level-into-spaces (level) "level 2 is 0 space, otherwise 2 spaces."
  (if (equal level *CHECKLIST-LEVEL*) 0 2))

(defun orgtrello/--compute-checklist-to-org-checkbox (name &optional level status) "Compute checklist to the org checkbox format"
  (format "%s- %s %s\n"
          (-> level
              orgtrello/--compute-level-into-spaces
              orgtrello/--space)
          (orgtrello/--compute-state-checkbox status)
          name))

(defun orgtrello/--compute-item-to-org-checkbox (name &optional level status) "Compute item to the org checkbox format"
  (format "%s- %s %s\n"
          (-> level
              orgtrello/--compute-level-into-spaces
              orgtrello/--space)
          (orgtrello/--compute-state-item-checkbox status)
          name))

(defun orgtrello/--compute-item-to-orgtrello-entry (name &optional level status)
  (format "%s %s %s\n"
          (orgtrello/--star level)
          (orgtrello/--compute-state-item status)
          name))

(defun orgtrello/--compute-checklist-to-org-entry (checklist &optional orgcheckbox-p) "Given a checklist, compute its org-mode entry equivalence."
  (funcall (if orgcheckbox-p
               'orgtrello/--compute-checklist-to-org-checkbox
               'orgtrello/--compute-item-to-orgtrello-entry)
           (orgtrello-data/entity-name checklist)
           *CHECKLIST-LEVEL*
           "incomplete"))

(defun orgtrello/--compute-item-to-org-entry (item &optional orgcheckbox-p) "Given a checklist item, compute its org-mode entry equivalence."
  (funcall (if orgcheckbox-p
               'orgtrello/--compute-item-to-org-checkbox
               'orgtrello/--compute-item-to-orgtrello-entry)
           (orgtrello-data/entity-name item)
           *ITEM-LEVEL*
           (orgtrello-data/entity-state item)))

(defun orgtrello/--compute-entity-to-org-entry (entity) "Given an entity, compute its org representation."
  (funcall
   (cond ((orgtrello-data/entity-card-p entity)      'orgtrello/--compute-card-to-org-entry)
         ((orgtrello-data/entity-checklist-p entity) 'orgtrello/--compute-checklist-to-org-entry)
         ((orgtrello-data/entity-item-p entity)      'orgtrello/--compute-item-to-org-entry))
   entity
   *ORGTRELLO-NATURAL-ORG-CHECKLIST*))

(defun orgtrello/--compute-items-from-checklist (checklist entities adjacency) "Given a checklist, retrieve its items and update the entities hash and the adjacency list."
  (let ((checklist-id (orgtrello-data/id checklist)))
    (cl-reduce
     (lambda (acc-entities-hash item)
       (cl-destructuring-bind (entities adjacency) acc-entities-hash
         (list (orgtrello/--add-entity-to-entities item entities) (orgtrello/--add-entity-to-adjacency item checklist adjacency))))
     (orgtrello-data/check-items checklist)
     :initial-value (list entities adjacency))))

(defun orgtrello/--retrieve-checklist-from-card (card) "Given a card, retrieve the checklist of the card (using trello). This gives a list of checklist in the trello order."
  (--> card
       (orgtrello-data/checklist-ids it)                                                            ;; retrieve checklist ids
       (cl-reduce
        (lambda (acc-list checklist-id)
          (cons (-> checklist-id
                    orgtrello-api/get-checklist
                    (orgtrello-query/http-trello *do-sync-query*)) acc-list))
        it :initial-value nil)                                                                         ;; retrieve the trello checklist
       (sort it (lambda (a b) (when (<= (assoc-default 'pos a) (assoc-default 'pos b)) 1)))))          ;; sort them by pos to get back to the right order (reversed)

(defun orgtrello/--compute-checklist-entities-from-card (card entities adjacency) "Given a card, retrieve its checklists (with their items) in the right order."
  (let ((card-id (orgtrello-data/id card)))
    (--> card
         (orgtrello/--retrieve-checklist-from-card it)
         (cl-reduce
          (lambda (acc-entities-hash checklist)
            (cl-destructuring-bind (entities adjacency) acc-entities-hash
              (orgtrello/--compute-items-from-checklist checklist (orgtrello/--add-entity-to-entities checklist entities) (orgtrello/--add-entity-to-adjacency checklist card adjacency))))
          it :initial-value (list entities adjacency)))))                               ;; at last complete checklist with item

;; one map for each complete entity: {entity-id entity} (entity in {card, checklist, item}
;; adjacency list {card-id (checklist-id)
;;                 checklist-id (item-id)}

(defun orgtrello/--compute-full-entities-from-trello (cards) "Given a list of cards, compute the full cards data from the trello board. The order from the trello board is kept. Hash result is of the form: {entity-id '(entity-card {checklist-id (checklist (item))})}"
  (cl-reduce
   (lambda (acc-entities-hash entity-card)
     (orgtrello-log/msg *OT/INFO* "Computing card '%s' data..." (orgtrello-data/name entity-card))
     (cl-destructuring-bind (entities adjacency) acc-entities-hash
       (orgtrello/--compute-checklist-entities-from-card entity-card (orgtrello/--add-entity-to-entities entity-card entities) adjacency)))
   cards
   :initial-value (list (make-hash-table :test 'equal) (make-hash-table :test 'equal))))

(defun orgtrello/--get-entity (id entities-hash) "Update the card entry inside the hash."
  (gethash id entities-hash))

(defun orgtrello/--put-card-with-adjacency (current-meta entities adjacency) "Deal with adding card to entities."
  (-> current-meta
      (orgtrello/--put-entities entities)
      (list adjacency)))

(defun orgtrello/--add-entity-to-entities (entity entities) "Adding entity to the hash entities."
  (let ((entity-id (orgtrello-data/entity-id-or-marker entity)))
    (puthash entity-id entity entities)
    entities))

;; FIXME find an already existing implementation.
(defun orgtrello/--add-to-last-pos (value list) "Adding the value to the list in last position."
  (--> list
       (reverse it)
       (cons value it)
       (reverse it)))

(defun orgtrello/--add-entity-to-adjacency (current-entity parent-entity adjacency) "Adding entity to the adjacency entry."
  (let* ((current-id (orgtrello-data/entity-id-or-marker current-entity))
         (parent-id  (orgtrello-data/entity-id-or-marker parent-entity)))
    (puthash parent-id (orgtrello/--add-to-last-pos current-id (gethash parent-id adjacency)) adjacency)
    adjacency))

(defun orgtrello/--put-entities-with-adjacency (current-meta entities adjacency) "Deal with adding a new item to entities."
  (let ((current-entity (orgtrello-data/current current-meta))
        (parent-entity  (orgtrello-data/parent current-meta)))
    (list (orgtrello/--add-entity-to-entities current-entity entities) (orgtrello/--add-entity-to-adjacency current-entity parent-entity adjacency))))

(defun orgtrello/--dispatch-create-entities-map-with-adjacency (entity) "Dispatch the function to update map depending on the entity level."
  (if (orgtrello-data/entity-card-p entity) 'orgtrello/--put-card-with-adjacency 'orgtrello/--put-entities-with-adjacency))

(defun orgtrello/--compute-full-entities-already-synced-from-org! () "Compute the full entities present in the org buffer which already had been sync'ed previously. Return the list of entities map and adjacency map in this order."
  (let ((entities (make-hash-table :test 'equal))
        (adjacency (make-hash-table :test 'equal)))
    (orgtrello/org-map-entities-without-params! (lambda ()
                                                  (let ((current-meta (orgtrello-data/entry-get-full-metadata)))
                                                    (-> current-meta
                                                        orgtrello-data/current
                                                        orgtrello/--dispatch-create-entities-map-with-adjacency
                                                        (funcall current-meta entities adjacency)))))
    (list entities adjacency)))

;; entities of the form: {entity-id '(entity-card {checklist-id (checklist (item))})}

(defun orgtrello/--compute-full-entities-already-synced-from-org-buffer! (buffername) "Compute the current entities hash from the buffer in the same format as the sync-from-trello routine. Return the list of entities map and adjacency map in this order."
  (set-buffer buffername)
  (save-excursion
    (goto-char (point-min))
    (orgtrello/--compute-full-entities-already-synced-from-org!)))

(defun orgtrello/--put-entities (current-meta entities) "Deal with adding a new item to entities."
  (-> current-meta
      orgtrello-data/current
      (orgtrello/--add-entity-to-entities entities)))

(defun orgtrello/--compute-entities-not-synced-from-org! () "Compute the org-buffer entities with no prior sync'ed activity. Return the list of entities map and the adjacency map."
  (let ((entities  (make-hash-table :test 'equal))
        (adjacency (make-hash-table :test 'equal)))
    (orgtrello/org-map-entities-without-params! (lambda ()
                                                  (let* ((full-meta      (orgtrello-data/entry-get-full-metadata))
                                                         (current-entity (orgtrello-data/current full-meta))
                                                         (current-marker (orgtrello/--compute-marker-from-entry current-entity)))
                                                    (unless (orgtrello/id-p (orgtrello/--id current-entity))
                                                            (orgtrello/--set-marker current-marker)
                                                            (let ((current-meta (orgtrello-data/entry-get-full-metadata)))
                                                              ;; now add the entities to the global list
                                                              (-> current-meta
                                                                  orgtrello-data/current
                                                                  orgtrello/--dispatch-create-entities-map-with-adjacency
                                                                  (funcall current-meta entities adjacency)))))))
    (list entities adjacency)))

(defun orgtrello/--compute-entities-not-synced-from-org-buffer! (buffername) "Compute the list of entities with no prior sync'ed activity from the buffer buffername. Return the list of entities map and the adjacency map."
  (set-buffer buffername)
  (save-excursion
    (goto-char (point-min))
    (orgtrello/--compute-entities-not-synced-from-org!)))

(defun orgtrello/--init-map-from (data) "Init a map from a given data. If data is nil, return an empty hash table."
  (if data data (make-hash-table :test 'equal)))

(defun orgtrello/--merge-item (trello-item org-item) "Merge trello and org item together."
  (if (null trello-item)
      org-item
      (let ((org-item-to-merge (orgtrello/--init-map-from       org-item)))
        (puthash :level *ITEM-LEVEL*                             org-item-to-merge)
        (puthash :id    (orgtrello-data/entity-id trello-item)   org-item-to-merge)
        (puthash :name  (orgtrello-data/entity-name trello-item) org-item-to-merge)
        (--> trello-item
             (orgtrello-data/entity-state it)
             (orgtrello/--compute-state-item it)
             (puthash :keyword it org-item-to-merge))
        org-item-to-merge)))

(defun orgtrello/--merge-checklist (trello-checklist org-checklist) "Merge trello and org checklist together."
  (if (null trello-checklist)
      org-checklist
      (let ((org-checklist-to-merge (orgtrello/--init-map-from org-checklist)))
        (puthash :level *CHECKLIST-LEVEL*                            org-checklist-to-merge)
        (puthash :name (orgtrello-data/entity-name trello-checklist) org-checklist-to-merge)
        (puthash :id   (orgtrello-data/entity-id trello-checklist)   org-checklist-to-merge)
        org-checklist-to-merge)))

(defun orgtrello/--merge-users-assigned (trello-card org-card) "Merge users assigned from trello and org."
  (--> trello-card
       (orgtrello-data/entity-member-ids it)
       (orgtrello-data/merge-2-lists-without-duplicates it (orgtrello/--user-assigned-ids-as-list org-card))
       (orgtrello/--users-to it)))

(defun orgtrello/--merge-card (trello-card org-card) "Merge trello and org card together."
  (if (null trello-card)
      org-card
      (let ((org-card-to-merge (orgtrello/--init-map-from org-card))
            (htrello-card (if (hash-table-p trello-card) trello-card (orgtrello-hash/make-properties trello-card))))
        (puthash :level   *CARD-LEVEL*                                                             org-card-to-merge)
        (puthash :id      (orgtrello-data/entity-id htrello-card)                                  org-card-to-merge)
        (puthash :name    (orgtrello-data/entity-name htrello-card)                                org-card-to-merge)
        (puthash :keyword (-> htrello-card orgtrello-data/entity-list-id orgtrello/--compute-card-status) org-card-to-merge)
        (puthash :users-assigned (orgtrello/--merge-users-assigned htrello-card org-card-to-merge) org-card-to-merge)
        org-card-to-merge)))

(defun orgtrello/--dispatch-merge-fn (entity) "Dispatch the function fn to merge the entity."
  (cond ((orgtrello-data/entity-card-p entity)      'orgtrello/--merge-card)
        ((orgtrello-data/entity-checklist-p entity) 'orgtrello/--merge-checklist)
        ((orgtrello-data/entity-item-p entity)      'orgtrello/--merge-item)))

(defun orgtrello/--merge-entities-trello-and-org (trello-data org-data) "Merge the org-entity entities inside the trello-entities."
  (let ((trello-entities  (first trello-data))
        (trello-adjacency (second trello-data))
        (org-entities     (first org-data))
        (org-adjacency    (second org-data)))

    (maphash (lambda (id trello-entity)
               (puthash id (funcall (orgtrello/--dispatch-merge-fn trello-entity) trello-entity (orgtrello/--get-entity id org-entities)) trello-entities) ;; updating entity to trello
               (puthash id (orgtrello-data/merge-2-lists-without-duplicates (gethash id trello-adjacency) (gethash id org-adjacency))     trello-adjacency)) ;; update entity adjacency to trello
             trello-entities)

    (maphash (lambda (id org-entity)
               (puthash id (funcall (orgtrello/--dispatch-merge-fn org-entity) (-trace (orgtrello/--get-entity id trello-entities) :trello-entity) org-entity)    trello-entities) ;; updating entity to trello
               (puthash id (orgtrello-data/merge-2-lists-without-duplicates (gethash id trello-adjacency) (gethash id org-adjacency))     trello-adjacency)) ;; update entity adjacency to trello
             org-entities)

    (list trello-entities trello-adjacency)))

(defun orgtrello/--update-property (id orgcheckbox-p) "Update the property depending on the nature of thing to sync. Move the cursor position."
  (if orgcheckbox-p
      (progn
        ;; need to get back one line backward for the checkboxes as their properties is at the same level (otherwise, for headings we do not care)
        (forward-line -1)
        (orgtrello-action/set-property *ORGTRELLO-ID* id)
        ;; getting back normally for the rest
        (forward-line))
      (orgtrello-action/set-property *ORGTRELLO-ID* id)))

(defun orgtrello/--write-entity! (entity-id entity) "Write the entity in the buffer to the current position. Move the cursor position."
  (orgtrello-log/msg *OT/INFO* "Synchronizing entity '%s' with id '%s'..." (orgtrello-data/entity-name entity) entity-id)
  (insert (orgtrello/--compute-entity-to-org-entry entity))
  (if entity-id (orgtrello/--update-property entity-id (and *ORGTRELLO-NATURAL-ORG-CHECKLIST* (not (orgtrello-data/entity-card-p entity))))))

;; (defun orgtrello/org-map-entities! (fn-to-execute &optional entities) "Execute fn-to-execute function for all entities from buffer."
;;   (org-map-entries
;;      (lambda ()
;;        (funcall fn-to-execute entities) ;; execute on heading entry
;;        (when *ORGTRELLO-NATURAL-ORG-CHECKLIST*
;;              (orgtrello/map-checkboxes (lambda () (funcall fn-to-execute entities))))) t 'file))
;; execute the same function for each org-checkboxes entry

(defun orgtrello/org-map-entities-without-params! (fn-to-execute) "Execute fn-to-execute function for all entities from buffer - fn-to-execute is a function without any parameters."
  (org-map-entries
     (lambda ()
       (funcall fn-to-execute) ;; execute on heading entry
       (when *ORGTRELLO-NATURAL-ORG-CHECKLIST*
             (orgtrello/map-checkboxes fn-to-execute))) t 'file))

(defun orgtrello/--write-item! (entity-id entities) "Write the item to the org buffer."
  (->> entities
       (gethash entity-id)
       (orgtrello/--write-entity! entity-id)))

(defun orgtrello/--write-checklist! (entity-id entities adjacency) "Write the checklist inside the org buffer."
  (orgtrello/--write-entity! entity-id (gethash entity-id entities))
  (--map (orgtrello/--write-item! it entities) (gethash entity-id adjacency)))

(defun orgtrello/--update-users-assigned-property! (entity) "Update the users assigned property card entry."
  (--> entity
       (orgtrello/--user-assigned-ids it)
       (orgtrello/--csv-user-ids-to-csv-user-names it *HMAP-USERS-ID-NAME*)
       (replace-regexp-in-string *ORGTRELLO-USER-PREFIX* "" it)
       (orgtrello/set-usernames-assigned-property! it)))

(defun orgtrello/--write-card! (entity-id entity entities adjacency) "Write the card inside the org buffer."
  (orgtrello/--write-entity! entity-id entity)
  (orgtrello/--update-users-assigned-property! entity)
  (--map (orgtrello/--write-checklist! it entities adjacency) (gethash entity-id adjacency)))

(defun orgtrello/--sync-buffer-with-trello-data (data buffer-name) "Given all the entities, update the current buffer with those."
  (let ((entities (first data))
        (adjacency (second data)))
    (with-current-buffer buffer-name
      (goto-char (point-max)) ;; go at the end of the file
      (maphash
       (lambda (new-id entity)
         (when (orgtrello-data/entity-card-p entity)
               (orgtrello/--write-card! new-id entity entities adjacency)))
       entities)
      (goto-char (point-min)) ;; go back to the beginning of file
      (org-sort-entries t ?o) ;; sort the entries on their keywords
      (save-buffer))))

(defun orgtrello/--cleanup-org-entries () "Cleanup org-entries from the buffer (FIXME find a suiter way of merging data than removing them all and put them back)."
  (goto-char (point-min))
  (outline-next-heading)
  (kill-region (point-at-bol) (point-max)))

(defun orgtrello/--sync-buffer-with-trello-data-callback (buffername &optional position name) "Generate a callback which knows the buffer with which it must work. (this callback must take a buffer-name and a position)"
  (lexical-let ((buffer-name                              buffername)
                (full-entities-synced-from-buffer         (orgtrello/--compute-full-entities-already-synced-from-org-buffer! buffername))
                (full-entities-not-yet-synced-from-buffer (orgtrello/--compute-entities-not-synced-from-org-buffer! buffername)))
    (function*
     (lambda (&key data &allow-other-keys)
       "Synchronize the buffer with the response data."
       (orgtrello-log/msg *OT/TRACE* "proxy - response data: %S" data)
       ;; compute merge between already sync'ed entries and the trello data
       (-> data
           orgtrello/--compute-full-entities-from-trello                                          ;; slow computation with network access
           (orgtrello/--merge-entities-trello-and-org full-entities-synced-from-buffer)                          ;; slow merge computation
           ((lambda (entry) ;; hack to clean the org entries just before synchronizing the buffer
              (orgtrello/--cleanup-org-entries)
              entry))
           (orgtrello/--sync-buffer-with-trello-data buffer-name)
           (orgtrello-action/safe-wrap (orgtrello-log/msg *OT/INFO* "Synchronizing the merge of trello data and org data - done!")))
       ;; write back the data without prior sync activity
       (-> full-entities-not-yet-synced-from-buffer
           (orgtrello/--sync-buffer-with-trello-data buffer-name)
           (orgtrello-action/safe-wrap (orgtrello-log/msg *OT/INFO* "Simple org data dump  - done!")))))))

(defun orgtrello/do-sync-full-from-trello (&optional sync) "Full org-mode file synchronisation. Beware, this will block emacs as the request is synchronous."
  (orgtrello-log/msg *OT/INFO* "Synchronizing the trello board '%s' to the org-mode file. This may take a moment, some coffee may be a good idea..." (orgtrello/--board-name))
  (--> (orgtrello/--board-id)
       (orgtrello-api/get-cards it)
       (orgtrello/--update-query-with-org-metadata it nil (buffer-name) nil 'orgtrello/--sync-buffer-with-trello-data-callback)
       (orgtrello-proxy/http it sync)))

(defun orgtrello/--card-delete (card-meta &optional parent-meta) "Deal with the deletion query of a card"
  ;; parent is useless here
  (orgtrello-api/delete-card (orgtrello/--id card-meta)))

(defun orgtrello/--checklist-delete (checklist-meta &optional parent-meta) "Deal with the deletion query of a checklist"
  ;; parent is useless here
  (orgtrello-api/delete-checklist (orgtrello/--id checklist-meta)))

(defun orgtrello/--item-delete (item-meta &optional checklist-meta) "Deal with create/update item query build"
  (orgtrello-api/delete-item (orgtrello/--id checklist-meta) (orgtrello/--id item-meta)))

(defvar *MAP-DISPATCH-DELETE* (orgtrello-hash/make-properties `((,*CARD-LEVEL*      . orgtrello/--card-delete)
                                                                (,*CHECKLIST-LEVEL* . orgtrello/--checklist-delete)
                                                                (,*ITEM-LEVEL*      . orgtrello/--item-delete))) "Dispatch map for the deletion query of card/checklist/item.")

(defun orgtrello/--dispatch-delete (meta &optional parent-meta) "Dispatch the delete function to call depending on the level information."
  (-> meta
      orgtrello/--level
      (gethash *MAP-DISPATCH-DELETE* 'orgtrello/--too-deep-level)
      (funcall meta parent-meta)))

(defun orgtrello/--do-delete-card (&optional sync) "Delete the card."
  (when (= *CARD-LEVEL* (-> (orgtrello-data/entry-get-full-metadata)
                            orgtrello-data/current
                            orgtrello/--level))
        (orgtrello/do-delete-simple sync)))

(defun orgtrello/do-delete-entities (&optional sync) "Launch a batch deletion of every single entities present on the buffer."
  (org-map-entries (lambda () (orgtrello/--do-delete-card sync)) t 'file))

(defun orgtrello/--do-install-config-file (*consumer-key* *access-token*) "Persist the file config-file with the input of the user."
  (make-directory *CONFIG-DIR* t)
  (with-temp-file *CONFIG-FILE*
    (erase-buffer)
    (goto-char (point-min))
    (insert (format "(setq *consumer-key* \"%s\")\n" *consumer-key*))
    (insert (format "(setq *access-token* \"%s\")" *access-token*))
    (write-file *CONFIG-FILE* 't)))

(defun orgtrello/do-install-key-and-token () "Procedure to install the *consumer-key* and the token for the user in the config-file."
  (interactive)
  (browse-url (org-trello/https-trello "/1/appKey/generate"))
  (let ((orgtrello/--*consumer-key* (read-string "*consumer-key*: ")))
    (browse-url (org-trello/https-trello (format "/1/authorize?response_type=token&name=org-trello&scope=read,write&expiration=never&key=%s" orgtrello/--*consumer-key*)))
    (let ((orgtrello/--access-token (read-string "Access-token: ")))
      (orgtrello/--do-install-config-file orgtrello/--*consumer-key* orgtrello/--access-token)
      "Install key and read/write access token done!")))

(defun orgtrello/--id-name (entities) "Given a list of entities, return a map of (id, name)."
  (let ((id-name (make-hash-table :test 'equal)))
    (mapc (lambda (it) (puthash (orgtrello-data/id it) (orgtrello-data/name it) id-name)) entities)
    id-name))

(defun orgtrello/--name-id (entities) "Given a list of entities, return a map of (id, name)."
  (let ((name-id (make-hash-table :test 'equal)))
    (mapc (lambda (it) (puthash (orgtrello-data/name it) (orgtrello-data/id it) name-id)) entities)
    name-id))

(defun orgtrello/--list-boards () "Return the map of the existing boards associated to the current account. (Synchronous request)"
  (cl-remove-if-not
   (lambda (board) (equal :json-false (orgtrello-data/close-property board)))
   (orgtrello-query/http-trello (orgtrello-api/get-boards) *do-sync-query*)))

(defun orgtrello/--list-board-lists (board-id) "Return the map of the existing list of the board with id board-id. (Synchronous request)"
  (orgtrello-query/http-trello (orgtrello-api/get-lists board-id) *do-sync-query*))

(defun orgtrello/--choose-board (boards) "Given a map of boards, display the possible boards for the user to choose which one he wants to work with."
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
           (orgtrello/--chosen-board-name ))
      `(,orgtrello/--chosen-board-id ,(gethash orgtrello/--chosen-board-id boards)))))

(defun orgtrello/--convention-property-name (name) "Use the right convention for the property used in the headers of the org-mode file."
  (replace-regexp-in-string " " "-" name))

(defun orgtrello/--delete-buffer-property (property-name) "A simple routine to delete a #+property: entry from the org-mode buffer."
  (save-excursion
    (goto-char (point-min))
    (-when-let (current-point (search-forward property-name nil t))
               (goto-char current-point)
               (beginning-of-line)
               (kill-line)
               (kill-line))))

(defun orgtrello/compute-property (property-name &optional property-value) "Compute a formatted entry in org buffer"
  (format "#+property: %s %s" property-name (if property-value property-value "")))

(defun orgtrello/--compute-hash-name-id-to-list (users-hash-name-id)
  (let ((res-list nil))
    (maphash (lambda (name id) (--> name
                                    (replace-regexp-in-string *ORGTRELLO-USER-PREFIX* "" it)
                                    (format "%s%s" *ORGTRELLO-USER-PREFIX* it)
                                    (orgtrello/compute-property it id)
                                    (push it res-list)))
             users-hash-name-id)
    res-list))

(defun orgtrello/--remove-properties-file! (list-keywords users-hash-name-id user-me &optional update-todo-keywords) "Remove the current org-trello properties"
  (with-current-buffer (current-buffer)
    ;; compute the list of properties to purge
    (->> `(":PROPERTIES"
           ,(orgtrello/compute-property *BOARD-ID*)
           ,(orgtrello/compute-property *BOARD-NAME*)
           ,@(--map (orgtrello/compute-property (orgtrello/--convention-property-name it)) list-keywords)
           ,@(orgtrello/--compute-hash-name-id-to-list users-hash-name-id)
           ,(orgtrello/compute-property *ORGTRELLO-USER-ME* user-me)
           ,(if update-todo-keywords "#+TODO: ")
           ":END:")
         (mapc (lambda (property-to-remove) (orgtrello/--delete-buffer-property property-to-remove))))))

(defun orgtrello/--compute-keyword-separation (name) "Given a keyword done (case insensitive) return a string '| done' or directly the keyword"
  (if (string= "done" (downcase name)) (format "| %s" name) name))

(defun orgtrello/--compute-board-lists-hash-name-id (board-lists-hash-name-id) ""
  (let ((res-list))
    (maphash (lambda (name id) (--> (orgtrello/--convention-property-name name)
                                    (format "#+PROPERTY: %s %s" it id)
                                    (push it res-list)))
             board-lists-hash-name-id)
    res-list))

(defun orgtrello/--properties-compute-todo-keywords-as-string (board-lists-hash-name-id)
  (mapconcat 'identity `("#+TODO: "
                         ,@(let ((res-list))
                           (maphash (lambda (name _) (--> name
                                                          (orgtrello/--convention-property-name it)
                                                          (orgtrello/--compute-keyword-separation it)
                                                          (format "%s " it)
                                                          (push it res-list)))
                                    board-lists-hash-name-id)
                           (nreverse res-list))) ""))

(defun orgtrello/--properties-compute-users-ids (board-users-hash-name-id)
  (let ((res-list))
    (maphash (lambda (name id) (--> name
                                    (format "#+PROPERTY: %s%s %s" *ORGTRELLO-USER-PREFIX* it id)
                                    (push it res-list)))
             board-users-hash-name-id)
    res-list))

(defun orgtrello/--update-orgmode-file-with-properties (board-name board-id board-lists-hash-name-id board-users-hash-name-id user-me &optional update-todo-keywords) "Update the orgmode file with the needed headers for org-trello to work."
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (set-buffer-file-coding-system 'utf-8-auto) ;; force utf-8
    (->> `(":PROPERTIES:"
           ,(format "#+PROPERTY: %s    %s" *BOARD-NAME* board-name)
            ,(format "#+PROPERTY: %s      %s" *BOARD-ID* board-id)
            ,@(orgtrello/--compute-board-lists-hash-name-id board-lists-hash-name-id)
            ,(if update-todo-keywords (orgtrello/--properties-compute-todo-keywords-as-string board-lists-hash-name-id))
            ,@(orgtrello/--properties-compute-users-ids board-users-hash-name-id)
            ,(format "#+PROPERTY: %s %s" *ORGTRELLO-USER-ME* user-me)
            ":END:")
         (mapc (lambda (property-to-insert) (insert property-to-insert "\n"))))
    (goto-char (point-min))
    (org-cycle)
    (save-buffer)
    (orgtrello-action/reload-setup)))

(defun orgtrello/--hash-table-keys (hash-table) "Extract the keys from the hash table."
  (let ((keys ()))
    (maphash (lambda (k v) (push k keys)) hash-table)
    keys))

(defun orgtrello/--user-logged-in () "Compute the current user."
  (--> (orgtrello-api/get-me)
       (orgtrello-query/http-trello it *do-sync-query*)
       (assoc-default 'username it)))

(defun orgtrello/do-install-board-and-lists () "Command to install the list boards."
  (interactive)
  (cl-destructuring-bind
      (orgtrello/--chosen-board-id orgtrello/--chosen-board-name) (-> (orgtrello/--list-boards)
                                                                      orgtrello/--id-name
                                                                      orgtrello/--choose-board)
    (let* ((orgtrello/--board-lists-hname-id (-> orgtrello/--chosen-board-id
                                                 orgtrello/--list-board-lists
                                                 orgtrello/--name-id))
           (orgtrello/--board-list-keywords (orgtrello/--hash-table-keys orgtrello/--board-lists-hname-id))
           (orgtrello/--board-users-name-id (orgtrello/--board-users-information-from-board-id! orgtrello/--chosen-board-id))
           (user-logged-in                  (orgtrello/--user-logged-in)))
      ;; remove any eventual present entry
      (orgtrello/--remove-properties-file! orgtrello/--board-list-keywords orgtrello/--board-users-name-id user-logged-in t)
      ;; update with new ones
      (orgtrello/--update-orgmode-file-with-properties
       orgtrello/--chosen-board-name
       orgtrello/--chosen-board-id
       orgtrello/--board-lists-hname-id
       orgtrello/--board-users-name-id
       user-logged-in
       t))
    "Install board and list ids done!"))

(defun orgtrello/--compute-user-properties (memberships-map) "Given a map, extract the map of user informations."
  (map 'list (lambda (x) (assoc-default 'member x)) memberships-map))

(defun orgtrello/--compute-user-properties-hash (user-properties)
  (-reduce-from (lambda (acc user) (puthash (assoc-default 'username user) (assoc-default 'id user) acc) acc) (make-hash-table :test 'equal) user-properties))

(defun orgtrello/--compute-user-properties-hash-from-board (board-info) "Compute user properties given board's informations."
  (->> board-info
       kvalist->hash
       (gethash 'memberships)
       orgtrello/--compute-user-properties
       orgtrello/--compute-user-properties-hash))

(defun orgtrello/--board-users-information-from-board-id! (board-id) "Compute board users' informations."
  (--> board-id
       (orgtrello-api/get-board it)
       (orgtrello-query/http-trello it *do-sync-query*)
       (orgtrello/--compute-user-properties-hash-from-board it)))

(defun orgtrello/--create-board (board-name &optional board-description) "Create a board with name and eventually a description."
  (orgtrello-log/msg *OT/INFO* "Creating board '%s'" board-name)
  (let ((board-data (orgtrello-query/http-trello (orgtrello-api/add-board board-name board-description) *do-sync-query*)))
    (list (orgtrello-data/id board-data) (orgtrello-data/name board-data))))

(defun orgtrello/--close-lists (list-ids) "Given a list of ids, close those lists."
  (mapc (lambda (list-id)
          (orgtrello-log/msg *OT/INFO* "Closing default list with id %s" list-id)
          (orgtrello-query/http-trello (orgtrello-api/close-list list-id)))
        list-ids))

(defun orgtrello/--create-lists-according-to-keywords (board-id list-keywords) "Given a list of names, build those lists on the trello boards. Return the hashmap (name, id) of the new lists created."
  (cl-reduce
   (lambda (acc-hash-name-id list-name)
     (progn
       (orgtrello-log/msg *OT/INFO* "Board id %s - Creating list '%s'" board-id list-name)
       (puthash list-name (orgtrello-data/id (orgtrello-query/http-trello (orgtrello-api/add-list list-name board-id) *do-sync-query*)) acc-hash-name-id)
       acc-hash-name-id))
   list-keywords
   :initial-value (make-hash-table :test 'equal)))

(defun orgtrello/do-create-board-and-lists () "Command to create a board and the lists."
  (defvar orgtrello/--board-name nil)        (setq orgtrello/--board-name nil)
  (defvar orgtrello/--board-description nil) (setq orgtrello/--board-description nil)
  (while (not orgtrello/--board-name) (setq orgtrello/--board-name (read-string "Please, input the desired board name: ")))
  (setq orgtrello/--board-description (read-string "Please, input the board description (empty for none): "))
  (cl-destructuring-bind (orgtrello/--board-id orgtrello/--board-name) (orgtrello/--create-board orgtrello/--board-name orgtrello/--board-description)
                         (let* ((orgtrello/--board-list-ids       (--map (orgtrello-data/id it) (orgtrello/--list-board-lists orgtrello/--board-id)))  ;; first retrieve the existing lists (created by default on trello)
                                (orgtrello/--lists-to-close       (orgtrello/--close-lists orgtrello/--board-list-ids))                                ;; close those lists (they may surely not match the name we want)
                                (orgtrello/--board-lists-hname-id (orgtrello/--create-lists-according-to-keywords orgtrello/--board-id *LIST-NAMES*))  ;; create the list, this returns the ids list
                                (orgtrello/--board-users-name-id  (orgtrello/--board-users-information-from-board-id! orgtrello/--board-id))           ;; retrieve user informations
                                (user-logged-in                   (orgtrello/--user-logged-in)))
                           (orgtrello/--remove-properties-file! *LIST-NAMES* orgtrello/--board-users-name-id user-logged-in) ;; remove eventual already present entry
                           (orgtrello/--update-orgmode-file-with-properties orgtrello/--board-name orgtrello/--board-id orgtrello/--board-lists-hname-id orgtrello/--board-users-name-id user-logged-in))) ;; update org buffer with new ones
  "Create board and lists done!")

(defun orgtrello/--users-from (string-users) "Compute the users name from the comma separated value in string."
  (when string-users (split-string string-users "," t)))

(defun orgtrello/--add-user (user users) "Add the user to the users list"
  (if (member user users) users (cons user users)))

(defun orgtrello/--remove-user (user users) "Add the user to the users list"
  (if (member user users) (remove user users) users users))

(defun orgtrello/--users-to (users) "Given a list of users, compute the comma separated users."
  (if users (mapconcat 'identity users ",") ""))

(defun orgtrello/--me ()
  (assoc-default *ORGTRELLO-USER-ME* org-file-properties))

(defun orgtrello/--user-ids-assigned-to-current-card () "Compute the user ids assigned to the current card."
  (--> (orgtrello/get-usernames-assigned-property!)
       (orgtrello/--users-from it)
       (--map (gethash (format "%s%s" *ORGTRELLO-USER-PREFIX* it) *HMAP-USERS-NAME-ID*) it)
       (orgtrello/--users-to it)))

(defun orgtrello/--csv-user-ids-to-csv-user-names (csv-users-id users-id-name) "Given a comma separated list of user id and a map, return a comma separated list of username."
  (->> csv-users-id
       orgtrello/--users-from
       (--map (gethash it users-id-name))
       orgtrello/--users-to))

(defun orgtrello/get-usernames-assigned-property! () "Read the org users property from the current entry."
  (org-entry-get nil *ORGTRELLO-USERS-ENTRY*))

(defun orgtrello/set-usernames-assigned-property! (csv-users) "Update users org property."
  (org-entry-put nil *ORGTRELLO-USERS-ENTRY* csv-users))

(defun orgtrello/do-assign-me () "Command to assign oneself to the card."
  (--> (orgtrello/get-usernames-assigned-property!)
       (orgtrello/--users-from it)
       (orgtrello/--add-user *ORGTRELLO-USER-LOGGED-IN* it)
       (orgtrello/--users-to it)
       (orgtrello/set-usernames-assigned-property! it)))

(defun orgtrello/do-unassign-me () "Command to unassign oneself of the card."
  (--> (orgtrello/get-usernames-assigned-property!)
       (orgtrello/--users-from it)
       (orgtrello/--remove-user *ORGTRELLO-USER-LOGGED-IN* it)
       (orgtrello/--users-to it)
       (orgtrello/set-usernames-assigned-property! it)))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello loaded!")



;; #################### org-trello

(defun org-trello/sync-entity () "Control first, then if ok, create a simple entity."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting entity sync"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     'orgtrello/do-sync-entity))

(defun org-trello/sync-full-entity () "Control first, then if ok, create an entity and all its arborescence if need be."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting entity and structure sync"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     'orgtrello/do-sync-full-entity))

(defun org-trello/sync-to-trello () "Control first, then if ok, sync the org-mode file completely to trello."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting sync org buffer to trello board"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     'orgtrello/do-sync-full-file))

(defun org-trello/sync-from-trello () "Control first, then if ok, sync the org-mode file from the trello board."
  (interactive)
  ;; execute the action
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting sync org buffer from trello board"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     'orgtrello/do-sync-full-from-trello
     *do-save-buffer*))

(defun org-trello/kill-entity () "Control first, then if ok, delete the entity and all its arborescence."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting deleting entity"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     'orgtrello/do-delete-simple))

(defun org-trello/kill-all-entities () "Control first, then if ok, delete the entity and all its arborescence."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Requesting deleting entities"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     'orgtrello/do-delete-entities))

(defun org-trello/install-key-and-token () "No control, trigger the setup installation of the key and the read/write token."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
   "Setup key and token"
   nil
   'orgtrello/do-install-key-and-token
   *do-save-buffer*
   *do-reload-setup*))

(defun org-trello/install-board-and-lists-ids () "Control first, then if ok, trigger the setup installation of the trello board to sync with."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Install boards and lists"
     '(orgtrello/--setup-properties orgtrello/--control-keys)
     'orgtrello/do-install-board-and-lists
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/jump-to-card () "Jump to current card in browser."
  (interactive)
  (org-action/--controls-or-actions-then-do
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     (lambda ()
       (let* ((full-meta       (orgtrello-data/entry-get-full-metadata))
              (entity          (orgtrello-data/current full-meta))
              (right-entity-fn (cond ((orgtrello-data/entity-item-p entity)      'orgtrello-data/grandparent)
                                     ((orgtrello-data/entity-checklist-p entity) 'orgtrello-data/parent)
                                     ((orgtrello-data/entity-card-p entity)      'orgtrello-data/current))))
         (-if-let (card-id (->> full-meta (funcall right-entity-fn) orgtrello-data/entity-id))
                  (browse-url (org-trello/https-trello (format "/c/%s" card-id))))))))

(defun org-trello/jump-to-trello-board () "Jump to current trello board."
  (interactive)
  (org-action/--controls-or-actions-then-do
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     (lambda () (browse-url (org-trello/https-trello (format "/b/%s" (orgtrello/--board-id)))))))

(defun org-trello/create-board () "Control first, then if ok, trigger the board creation."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Create board and lists"
     '(orgtrello/--setup-properties orgtrello/--control-keys)
     'orgtrello/do-create-board-and-lists
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/assign-me () "Assign oneself to the card."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Create board and lists"
     '(orgtrello/--setup-properties orgtrello/--control-keys)
     'orgtrello/do-assign-me
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/unassign-me () "Unassign oneself of the card."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
     "Create board and lists"
     '(orgtrello/--setup-properties orgtrello/--control-keys)
     'orgtrello/do-unassign-me
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/check-setup () "Check the current setup."
  (interactive)
  (org-action/--controls-or-actions-then-do
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     (lambda () (orgtrello-log/msg *OT/NOLOG* "Setup ok!"))))

(defun orgtrello/--delete-property (property) "Given a property name (checkbox), if found, delete it from the buffer."
  (org-delete-property-globally property)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":PROPERTIES: {.*" nil t)
      (replace-match "" nil t))))

(defun org-trello/delete-setup () "Delete the current setup."
  (interactive)
  (org-action/--deal-with-consumer-msg-controls-or-actions-then-do
   "Deleting current org-trello setup"
     '(orgtrello/--setup-properties orgtrello/--control-keys orgtrello/--control-properties orgtrello/--control-encoding)
     (lambda ()
       (orgtrello/--remove-properties-file! *LIST-NAMES* *HMAP-USERS-NAME-ID* *ORGTRELLO-USER-LOGGED-IN* t) ;; remove any orgtrello relative entries
       (orgtrello/--delete-property *ORGTRELLO-ID*)          ;; remove all properties orgtrello-id from the buffer
       (orgtrello/--delete-property *ORGTRELLO-USERS-ENTRY*) ;; remove all properties users-assigned
       (orgtrello-log/msg *OT/NOLOG* "Cleanup done!")) ;; a simple message to tell the user that the work is done!
     *do-save-buffer*
     *do-reload-setup*))

(defun org-trello/activate-natural-org-checkboxes () "Activate the natural org-checkboxes - http://orgmode.org/manual/Checkboxes.html"
  (interactive)
  (setq *ORGTRELLO-NATURAL-ORG-CHECKLIST* t)
  (setq *ORGTRELLO-CHECKLIST-UPDATE-ITEMS* nil))

(defun org-trello/deactivate-natural-org-checkboxes () "Activate the natural org-checkboxes - http://orgmode.org/manual/Checkboxes.html"
  (interactive)
  (setq *ORGTRELLO-NATURAL-ORG-CHECKLIST* nil)
  (setq *ORGTRELLO-CHECKLIST-UPDATE-ITEMS* t))

(defun org-trello/--replace-string-prefix-in-string (keybinding string-to-replace)
  (replace-regexp-in-string "#PREFIX#" keybinding string-to-replace t))

(defun org-trello/--startup-message (keybinding)
  (let ((template-string "org-trello/ot is on! To begin with, hit #PREFIX# h or M-x 'org-trello/help-describing-bindings"))
    (replace-regexp-in-string "#PREFIX#" keybinding template-string t)))

(defun org-trello/--help-describing-bindings-template (keybinding list-command-binding-description) "Standard Help message template"
  (->> list-command-binding-description
       (--map (let ((command        (first it))
                    (prefix-binding (second it))
                    (help-msg       (third it)))
                (concat keybinding " " prefix-binding " - M-x " (symbol-name command) " - " help-msg)))
       (s-join "\n")))

(defun org-trello/help-describing-bindings () "A simple message to describe the standard bindings used."
  (interactive)
  (orgtrello-log/msg 0 (org-trello/--help-describing-bindings-template *ORGTRELLO-MODE-PREFIX-KEYBINDING* org-trello/--list-of-interactive-command-binding-couples)))

(defun org-trello/describe-entry () "An helper command to describe org-trello entry."
  (interactive)
  (message "entities: %S" (orgtrello/--compute-full-entities-from-org)))

(defvar org-trello/--list-of-interactive-command-binding-couples
  '((org-trello/version                     "v" "Display the current version installed.")
    (org-trello/install-key-and-token       "i" "Install the keys and the access-token.")
    (org-trello/install-board-and-lists-ids "I" "Select the board and attach the todo, doing and done list.")
    (org-trello/check-setup                 "d" "Check that the setup is ok. If everything is ok, will simply display 'Setup ok!'.")
    (org-trello/assign-me                   "a" "Assign oneself to the card.")
    (org-trello/unassign-me                 "u" "Unassign oneself of the card")
    (org-trello/delete-setup                "D" "Clean up the org buffer from all org-trello informations.")
    (org-trello/create-board                "b" "Create interactively a board and attach the org-mode file to this trello board.")
    (org-trello/sync-from-trello            "S" "Synchronize the org-mode file from the trello board (trello -> org-mode).")
    (org-trello/sync-entity                 "c" "Create/Update an entity (card/checklist/item) depending on its level and status. Do not deal with level superior to 4.")
    (org-trello/sync-full-entity            "C" "Create/Update a complete entity card/checklist/item and its subtree (depending on its level).")
    (org-trello/kill-entity                 "k" "Kill the entity (and its arborescence tree) from the trello board and the org buffer.")
    (org-trello/kill-all-entities           "K" "Kill all the entities (and their arborescence tree) from the trello board and the org buffer.")
    (org-trello/sync-to-trello              "s" "Synchronize the org-mode file to the trello board (org-mode -> trello).")
    (org-trello/jump-to-card                "j" "Jump to card in browser.")
    (org-trello/jump-to-trello-board        "J" "Open the browser to your current trello board.")
    (org-trello/help-describing-bindings    "h" "This help message."))
  "List of command and default binding without the prefix key.")

(defun org-trello/--install-local-keybinding-map! (previous-org-trello-mode-prefix-keybinding org-trello-mode-prefix-keybinding interactive-command-binding-to-install)
  "Install locally the default binding map with the prefix binding of org-trello-mode-prefix-keybinding."
  (mapc (lambda (command-and-binding)
          (let ((command (first command-and-binding))
                (binding (second command-and-binding)))
            ;; unset previous binding
            (local-unset-key (kbd (concat previous-org-trello-mode-prefix-keybinding binding)))
            ;; set new binding
            (local-set-key (kbd (concat org-trello-mode-prefix-keybinding binding)) command)))
        interactive-command-binding-to-install))

(defvar *ORGTRELLO-MODE-PREFIX-KEYBINDING*          "C-c o" "The default prefix keybinding.")
(defvar *PREVIOUS-ORGTRELLO-MODE-PREFIX-KEYBINDING* "C-c o" "The memory default prefix keybinding.")

(defun org-trello/install-local-prefix-mode-keybinding! (keybinding) "Install the new default org-trello mode keybinding."
  (setq *PREVIOUS-ORGTRELLO-MODE-PREFIX-KEYBINDING* *ORGTRELLO-MODE-PREFIX-KEYBINDING*)
  (setq *ORGTRELLO-MODE-PREFIX-KEYBINDING* keybinding)
  (org-trello/--install-local-keybinding-map! *PREVIOUS-ORGTRELLO-MODE-PREFIX-KEYBINDING* *ORGTRELLO-MODE-PREFIX-KEYBINDING* org-trello/--list-of-interactive-command-binding-couples))

;;;###autoload
(define-minor-mode org-trello-mode "Sync your org-mode and your trello together."
  :lighter    " ot"
  :after-hook (org-trello/install-local-prefix-mode-keybinding! *ORGTRELLO-MODE-PREFIX-KEYBINDING*))

(defun org-trello/justify-on-save () "Justify the properties checkbox."
  (if org-trello-mode (orgtrello/justify-file)))

(add-hook 'org-trello-mode-on-hook
          (lambda ()
            ;; hightlight the properties of the checkboxes
            (font-lock-add-keywords 'org-mode '((":PROPERTIES:" 0 font-lock-keyword-face t)))
            (font-lock-add-keywords 'org-mode '((": {\"orgtrello-id\":.*}" 0 font-lock-comment-face t)))
            ;; start the proxy
            (orgtrello-proxy/start)
            ;; installing hooks
            (add-hook 'before-save-hook 'org-trello/justify-on-save)
            ;; a little message in the minibuffer to notify the user
            (orgtrello-log/msg *OT/NOLOG* (org-trello/--startup-message *ORGTRELLO-MODE-PREFIX-KEYBINDING*))))

(add-hook 'org-trello-mode-off-hook
          (lambda ()
            ;; remove the highlight
            (font-lock-remove-keywords 'org-mode '((":PROPERTIES:" 0 font-lock-keyword-face t)))
            (font-lock-remove-keywords 'org-mode '((": {\"orgtrello-id\":.*}" 0 font-lock-comment-face t)))
            ;; stop the proxy
            (orgtrello-proxy/stop)
            ;; uninstalling hooks
            (remove-hook 'before-save-hook 'org-trello/justify-on-save)
            ;; a little message in the minibuffer to notify the user
            (orgtrello-log/msg *OT/NOLOG* "org-trello/ot is off!")))

(orgtrello-log/msg *OT/DEBUG* "org-trello loaded!")

(provide 'org-trello)
