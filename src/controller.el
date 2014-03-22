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

(defun orgtrello-controller/compute-marker (buffer-name name position) "Compute the orgtrello marker which is composed of buffer-name, name and position"
  (->> (list *ORGTRELLO-MARKER* buffer-name name (if (stringp position) position (int-to-string position)))
       (-interpose "-")
       (apply 'concat)
       sha1
       (concat *ORGTRELLO-MARKER* "-")))

(defun orgtrello-controller/id-p (id) "Is the string a trello identifier?"
  (and id (not (string-match-p (format "^%s-" *ORGTRELLO-MARKER*) id))))

(defun orgtrello-controller/filtered-kwds () "org keywords used (based on org-todo-keywords-1)."
  org-todo-keywords-1)

(defun orgtrello-controller/--list-user-entries (properties) "List the users entries."
  (--filter (string-match-p *ORGTRELLO-USER-PREFIX* (car it)) properties))

(defun orgtrello-controller/setup-properties (&optional args) "Setup the properties according to the org-mode setup. Return :ok."
  ;; read the setup
  (orgtrello-action/reload-setup)
  ;; now exploit some
  (let* ((orgtrello-controller/--list-keywords (nreverse (orgtrello-controller/filtered-kwds)))
         (orgtrello-controller/--hmap-id-name
          (--reduce-from (progn
                           (puthash (assoc-default it org-file-properties) it acc)
                           acc)
                         (orgtrello-hash/empty-hash)
                         orgtrello-controller/--list-keywords))
         (orgtrello-controller/--list-users (orgtrello-controller/--list-user-entries org-file-properties))
         (orgtrello-controller/--hmap-user-id-name (orgtrello-hash/make-transpose-properties orgtrello-controller/--list-users))
         (orgtrello-controller/--hmap-user-name-id (orgtrello-hash/make-properties orgtrello-controller/--list-users)))
    (setq *LIST-NAMES*   orgtrello-controller/--list-keywords)
    (setq *HMAP-ID-NAME* orgtrello-controller/--hmap-id-name)
    (setq *HMAP-USERS-ID-NAME* orgtrello-controller/--hmap-user-id-name)
    (setq *HMAP-USERS-NAME-ID* orgtrello-controller/--hmap-user-name-id)
    (setq *ORGTRELLO-USER-LOGGED-IN* (orgtrello-controller/--me))
    :ok))

(defun orgtrello-controller/control-encoding (&optional args) "Use utf-8, otherwise, there will be trouble."
  (progn
    (orgtrello-log/msg *OT/ERROR* "Ensure you use utf-8 encoding for your org buffer.")
    :ok))

(defun orgtrello-controller/--board-name () "Compute the board's name" (assoc-default *BOARD-NAME* org-file-properties))
(defun orgtrello-controller/--board-id () "Compute the board's id" (assoc-default *BOARD-ID* org-file-properties))

(defun orgtrello-controller/control-properties (&optional args) "org-trello needs the properties board-id and all list id from the trello board to be setuped on header property file. :ok if ok, or the error message if problems."
  (let ((orgtrello-controller/--hmap-count (hash-table-count *HMAP-ID-NAME*)))
    (if (and org-file-properties (orgtrello-controller/--board-id) (= (length *LIST-NAMES*) orgtrello-controller/--hmap-count))
        :ok
        "Setup problem.\nEither you did not connect your org-mode buffer with a trello board, to correct this:\n  * attach to a board through C-c o I or M-x org-trello/install-board-and-lists-ids\n  * or create a board from scratch with C-c o b or M-x org-trello/create-board).\nEither your org-mode's todo keyword list and your trello board lists are not named the same way (which they must).\nFor this, connect to trello and rename your board's list according to your org-mode's todo list.\nAlso, you can specify on your org-mode buffer the todo list you want to work with, for example: #+TODO: TODO DOING | DONE FAIL (hit C-c C-c to refresh the setup)")))

(defun orgtrello-controller/control-keys (&optional args) "org-trello needs the *consumer-key* and the *access-token* to access the trello resources. Returns :ok if everything is ok, or the error message if problems."
  (if (or (and *consumer-key* *access-token*)
          ;; the data are not set,
          (and (file-exists-p *CONFIG-FILE*)
               ;; trying to load them
               (load *CONFIG-FILE*)
               ;; still not loaded, something is not right!
               (and *consumer-key* *access-token*)))
      :ok
    "Setup problem - You need to install the consumer-key and the read/write access-token - C-c o i or M-x org-trello/install-board-and-lists-ids"))

(defun orgtrello-controller/--retrieve-state-of-card (card-meta) "Given a card, retrieve its state depending on its :keyword metadata. If empty or no keyword then, its equivalence is *TODO*, otherwise, return its current state."
  (-if-let (orgtrello-controller/--card-kwd (orgtrello-data/entity-keyword card-meta *TODO*))
           orgtrello-controller/--card-kwd
           *TODO*))

(defun orgtrello-controller/--checks-before-sync-card (card-meta) "Checks done before synchronizing the cards."
  (-if-let (orgtrello-controller/--card-name (orgtrello-data/entity-name card-meta)) :ok *ERROR-SYNC-CARD-MISSING-NAME*))

(defun orgtrello-controller/--card (card-meta &optional parent-meta grandparent-meta) "Deal with create/update card query build. If the checks are ko, the error message is returned."
  (let ((checks-ok-or-error-message (orgtrello-controller/--checks-before-sync-card card-meta)))
    ;; name is mandatory
    (if (equal :ok checks-ok-or-error-message)
        ;; parent and grandparent are useless here
        (let* ((orgtrello-controller/--card-kwd           (orgtrello-controller/--retrieve-state-of-card card-meta))
               (orgtrello-controller/--list-id            (assoc-default orgtrello-controller/--card-kwd org-file-properties))
               (orgtrello-controller/--card-id            (orgtrello-data/entity-id          card-meta))
               (orgtrello-controller/--card-name          (orgtrello-data/entity-name        card-meta))
               (orgtrello-controller/--card-due           (orgtrello-data/entity-due         card-meta))
               (orgtrello-controller/--card-desc          (orgtrello-data/entity-description card-meta))
               (orgtrello-controller/--user-ids-assigned  (orgtrello-data/entity-member-ids card-meta)))
          (if orgtrello-controller/--card-id
              ;; update
              (orgtrello-api/move-card orgtrello-controller/--card-id orgtrello-controller/--list-id orgtrello-controller/--card-name orgtrello-controller/--card-due orgtrello-controller/--user-ids-assigned orgtrello-controller/--card-desc)
            ;; create
            (orgtrello-api/add-card orgtrello-controller/--card-name orgtrello-controller/--list-id orgtrello-controller/--card-due orgtrello-controller/--user-ids-assigned orgtrello-controller/--card-desc)))
      checks-ok-or-error-message)))

(defun orgtrello-controller/--checks-before-sync-checklist (checklist-meta card-meta) "Checks done before synchronizing the checklist."
  (let ((orgtrello-controller/--checklist-name (orgtrello-data/entity-name checklist-meta))
        (orgtrello-controller/--card-id        (orgtrello-data/entity-id card-meta)))
    (if orgtrello-controller/--checklist-name
        (if orgtrello-controller/--card-id
            :ok
          *ERROR-SYNC-CHECKLIST-SYNC-CARD-FIRST*)
      *ERROR-SYNC-CHECKLIST-MISSING-NAME*)))

(defun orgtrello-controller/--checklist (checklist-meta &optional card-meta grandparent-meta) "Deal with create/update checklist query build. If the checks are ko, the error message is returned."
  (let ((checks-ok-or-error-message (orgtrello-controller/--checks-before-sync-checklist checklist-meta card-meta)))
    ;; name is mandatory
    (if (equal :ok checks-ok-or-error-message)
        ;; grandparent is useless here
        (let* ((orgtrello-controller/--checklist-id   (orgtrello-data/entity-id checklist-meta))
               (orgtrello-controller/--card-id        (orgtrello-data/entity-id card-meta))
               (orgtrello-controller/--checklist-name (orgtrello-data/entity-name checklist-meta)))
          (if orgtrello-controller/--checklist-id
              ;; update
              (orgtrello-api/update-checklist orgtrello-controller/--checklist-id orgtrello-controller/--checklist-name)
            ;; create
            (orgtrello-api/add-checklist orgtrello-controller/--card-id orgtrello-controller/--checklist-name)))
      checks-ok-or-error-message)))

(defun orgtrello-controller/--checks-before-sync-item (item-meta checklist-meta card-meta) "Checks done before synchronizing the checklist."
  (let ((orgtrello-controller/--item-name    (orgtrello-data/entity-name item-meta))
        (orgtrello-controller/--checklist-id (orgtrello-data/entity-id checklist-meta))
        (orgtrello-controller/--card-id      (orgtrello-data/entity-id card-meta)))
    (if orgtrello-controller/--item-name
        (if orgtrello-controller/--checklist-id
            (if orgtrello-controller/--card-id :ok *ERROR-SYNC-ITEM-SYNC-CARD-FIRST*)
          *ERROR-SYNC-ITEM-SYNC-CHECKLIST-FIRST*)
      *ERROR-SYNC-ITEM-MISSING-NAME*)))

(defun orgtrello-controller/--compute-state-from-keyword (state) "Given a state, compute the org equivalent (to use with org-todo function)"
  (if (string= *DONE* state) 'done 'none))

(defun orgtrello-controller/compute-state (state) "Given a state (TODO/DONE) compute the trello state equivalent."
  (orgtrello-controller/--compute-state-generic state '("complete" "incomplete")))

(defun orgtrello-controller/compute-check (state) "Given a state (TODO/DONE) compute the trello check equivalent."
  (orgtrello-controller/--compute-state-generic state '(t nil)))

(defun orgtrello-controller/--item (item-meta &optional checklist-meta card-meta) "Deal with create/update item query build. If the checks are ko, the error message is returned."
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

(defun orgtrello-controller/--too-deep-level (meta &optional parent-meta grandparent-meta) "Deal with too deep level."
  "Your arborescence depth is too deep. We only support up to depth 3.\nLevel 1 - card\nLevel 2 - checklist\nLevel 3 - items")

(defvar *MAP-DISPATCH-CREATE-UPDATE* (orgtrello-hash/make-properties `((,*CARD-LEVEL*      . orgtrello-controller/--card)
                                                                       (,*CHECKLIST-LEVEL* . orgtrello-controller/--checklist)
                                                                       (,*ITEM-LEVEL*      . orgtrello-controller/--item))) "Dispatch map for the creation/update of card/checklist/item.")

(defun orgtrello-controller/--dispatch-create (entry-metadata) "Dispatch the creation depending on the nature of the entry."
  (let ((current-meta        (orgtrello-data/current entry-metadata)))
    (-> current-meta
        orgtrello-data/entity-level
        (gethash *MAP-DISPATCH-CREATE-UPDATE* 'orgtrello-controller/--too-deep-level)
        (funcall current-meta (orgtrello-data/parent entry-metadata) (orgtrello-data/grandparent entry-metadata)))))

(defun orgtrello-controller/--update-query-with-org-metadata (query-map position buffer-name &optional name success-callback sync) "Given a trello query, add proxy metadata needed to work."
  (puthash :uri          (orgtrello-query/--url-hexify-string (gethash :uri query-map)) query-map) ;; protect the query for transfer to proxy
  (puthash :position     position                                                       query-map)
  (puthash :buffername   buffer-name                                                    query-map)
  (when success-callback (puthash :callback success-callback query-map))
  (when sync             (puthash :sync     sync             query-map))
  (when name             (puthash :name     name             query-map))
  query-map)

(defun orgtrello-controller/--set-marker (marker) "Set a marker to get back to later."
  (orgtrello-action/set-property *ORGTRELLO-ID* marker))

(defun orgtrello-controller/--compute-marker-from-entry (entry) "Compute and set the marker (either a sha1 or the id of the entry-metadata)."
  (-if-let (orgtrello-controller/--current-entry-id (orgtrello-data/entity-id entry))
           orgtrello-controller/--current-entry-id
           (orgtrello-controller/compute-marker (orgtrello-data/entity-buffername entry) (orgtrello-data/entity-name entry) (orgtrello-data/entity-position entry))))

(defun orgtrello-controller/--right-level-p (entity) "Compute if the level is correct (not higher than level 4)."
  (if (< (-> entity orgtrello-data/current orgtrello-data/entity-level) *OUTOFBOUNDS-LEVEL*) :ok "Level too high. Do not deal with entity other than card/checklist/items!"))

(defun orgtrello-controller/--already-synced-p (entity) "Compute if the entity has already been synchronized."
  (if (-> entity orgtrello-data/current orgtrello-data/entity-id) :ok "Entity must been synchronized with trello first!"))

(defun orgtrello-controller/--mandatory-name-ok-p (entity) "Ensure entity can be synced regarding the mandatory data."
  (let* ((current (orgtrello-data/current entity))
         (level   (orgtrello-data/entity-level current))
         (name    (orgtrello-data/entity-name current)))
    (if (and name (< 0 (length name)))
        :ok
        (cond ((= level *CARD-LEVEL*)      *ERROR-SYNC-CARD-MISSING-NAME*)
              ((= level *CHECKLIST-LEVEL*) *ERROR-SYNC-CHECKLIST-MISSING-NAME*)
              ((= level *ITEM-LEVEL*)      *ERROR-SYNC-ITEM-MISSING-NAME*)))))

(defun orgtrello-controller/--set-marker-if-not-present (current-entity marker) "Set the marker to the entry if we never did."
  (unless (string= (orgtrello-data/entity-id current-entity) marker) ;; if never created before, we need a marker to add inside the file
          (orgtrello-controller/--set-marker marker)))

(defun orgtrello-controller/--delegate-to-the-proxy (full-meta action) "Execute the delegation to the consumer."
  (let* ((orgtrello-controller/--current (orgtrello-data/current full-meta))
         (orgtrello-controller/--marker  (orgtrello-controller/--compute-marker-from-entry orgtrello-controller/--current)))
    (orgtrello-controller/--set-marker-if-not-present orgtrello-controller/--current orgtrello-controller/--marker)
    (puthash :id      orgtrello-controller/--marker orgtrello-controller/--current)
    (puthash :action  action             orgtrello-controller/--current)
    (orgtrello-proxy/http-producer orgtrello-controller/--current)))

(defun orgtrello-controller/--checks-then-delegate-action-on-entity-to-proxy (functional-controls action) "Execute the functional controls then if all pass, delegate the action 'action' to the proxy."
  (orgtrello-action/functional-controls-then-do functional-controls (orgtrello-data/entry-get-full-metadata) 'orgtrello-controller/--delegate-to-the-proxy action))

(defun orgtrello-controller/do-delete-simple (&optional sync) "Do the deletion of an entity."
  (orgtrello-controller/--checks-then-delegate-action-on-entity-to-proxy '(orgtrello-controller/--right-level-p orgtrello-controller/--already-synced-p) *ORGTRELLO-ACTION-DELETE*))

(defun orgtrello-controller/do-sync-entity () "Do the entity synchronization (if never synchronized, will create it, update it otherwise)."
  (orgtrello-controller/--checks-then-delegate-action-on-entity-to-proxy '(orgtrello-controller/--right-level-p orgtrello-controller/--mandatory-name-ok-p) *ORGTRELLO-ACTION-SYNC*))

(defun orgtrello-controller/do-sync-full-entity () "Do the actual full card creation - from card to item. Beware full side effects..."
  (orgtrello-log/msg *OT/INFO* "Synchronizing full entity with its structure on board '%s'..." (orgtrello-controller/--board-name))
  ;; in any case, we need to show the subtree, otherwise https://github.com/org-trello/org-trello/issues/53
  (org-show-subtree)
  (if (org-at-heading-p)
      (org-map-tree (lambda () (orgtrello-controller/do-sync-entity) (orgtrello-controller/map-sync-checkboxes)))
      (orgtrello-controller/map-sync-checkboxes)))

(defun orgtrello-controller/map-sync-checkboxes () "Map the sync to checkboxes."
  (orgtrello-cbx/map-checkboxes 'orgtrello-controller/do-sync-entity))

(defun orgtrello-controller/org-map-entries (level fn-to-execute) "Map fn-to-execute to a given entities with level level. fn-to-execute is a function without any parameter."
  (org-map-entries (lambda () (when (= level (orgtrello-data/current-level)) (funcall fn-to-execute)))))

(defun orgtrello-controller/do-sync-full-file () "Full org-mode file synchronisation."
  (orgtrello-log/msg *OT/WARN* "Synchronizing org-mode file to the board '%s'. This may take some time, some coffee may be a good idea..." (orgtrello-controller/--board-name))
  (orgtrello-controller/org-map-entries *CARD-LEVEL* 'orgtrello-controller/do-sync-full-entity))

(defun orgtrello-controller/--compute-card-status (card-id-list) "Given a card's id, compute its status."
  (gethash card-id-list *HMAP-ID-NAME*))

(defun orgtrello-controller/--compute-due-date (due-date) "Compute the format of the due date."
  (if due-date (format "DEADLINE: <%s>\n" due-date) ""))

(defun orgtrello-controller/--private-compute-card-to-org-entry (name status due-date) "Compute the org format for card."
  (format "* %s %s\n%s" (if status status *TODO*) name (orgtrello-controller/--compute-due-date due-date)))

(defun orgtrello-controller/--compute-card-to-org-entry (card) "Given a card, compute its org-mode entry equivalence. orgcheckbox-p is nil"
  (orgtrello-controller/--private-compute-card-to-org-entry (orgtrello-data/entity-name card) (orgtrello-data/entity-keyword card) (orgtrello-data/entity-due card)))

(defun orgtrello-controller/--compute-checklist-to-orgtrello-entry (name &optional level status) "Compute the orgtrello format checklist"
  (format "** %s\n" name))

(defun orgtrello-controller/--symbol (sym n) "Compute the repetition of a symbol as a string"
  (--> n
       (-repeat it sym)
       (s-join "" it)))

(defun orgtrello-controller/--space (n) "Given a level, compute the number of space for an org checkbox entry."
  (orgtrello-controller/--symbol " "  n))

(defun orgtrello-controller/--star (n) "Given a level, compute the number of space for an org checkbox entry."
  (orgtrello-controller/--symbol "*"  n))

(defun orgtrello-controller/--compute-state-generic (state list-state) "Computing generic."
  (if (or (string= "complete" state)
          (string= *DONE* state)) (first list-state) (second list-state)))

(defun orgtrello-controller/--compute-state-checkbox (state) "Compute the status of the checkbox"
  (orgtrello-controller/--compute-state-generic state '("[X]" "[-]")))

(defun orgtrello-controller/--compute-state-item-checkbox (state) "Compute the status of the item checkbox"
  (orgtrello-controller/--compute-state-generic state '("[X]" "[ ]")))

(defun orgtrello-controller/--compute-state-item (state) "Compute the status of the checkbox"
  (orgtrello-controller/--compute-state-generic state `(,*DONE* ,*TODO*)))

(defun orgtrello-controller/--compute-level-into-spaces (level) "level 2 is 0 space, otherwise 2 spaces."
  (if (equal level *CHECKLIST-LEVEL*) 0 2))

(defun orgtrello-controller/--compute-checklist-to-org-checkbox (name &optional level status) "Compute checklist to the org checkbox format"
  (format "%s- %s %s\n"
          (-> level
              orgtrello-controller/--compute-level-into-spaces
              orgtrello-controller/--space)
          (orgtrello-controller/--compute-state-checkbox status)
          name))

(defun orgtrello-controller/--compute-item-to-org-checkbox (name &optional level status) "Compute item to the org checkbox format"
  (format "%s- %s %s\n"
          (-> level
              orgtrello-controller/--compute-level-into-spaces
              orgtrello-controller/--space)
          (orgtrello-controller/--compute-state-item-checkbox status)
          name))

(defun orgtrello-controller/--compute-checklist-to-org-entry (checklist &optional orgcheckbox-p) "Given a checklist, compute its org-mode entry equivalence."
  (orgtrello-controller/--compute-checklist-to-org-checkbox (orgtrello-data/entity-name checklist) *CHECKLIST-LEVEL* "incomplete"))

(defun orgtrello-controller/--compute-item-to-org-entry (item) "Given a checklist item, compute its org-mode entry equivalence."
  (orgtrello-controller/--compute-item-to-org-checkbox (orgtrello-data/entity-name item) *ITEM-LEVEL* (orgtrello-data/entity-keyword item)))

(defun orgtrello-controller/--compute-entity-to-org-entry (entity) "Given an entity, compute its org representation."
  (funcall
   (cond ((orgtrello-data/entity-card-p entity)      'orgtrello-controller/--compute-card-to-org-entry)
         ((orgtrello-data/entity-checklist-p entity) 'orgtrello-controller/--compute-checklist-to-org-entry)
         ((orgtrello-data/entity-item-p entity)      'orgtrello-controller/--compute-item-to-org-entry))
   entity))

(defun orgtrello-controller/--compute-items-from-checklist (checklist entities adjacency) "Given a checklist, retrieve its items and update the entities hash and the adjacency list."
  (let ((checklist-id (orgtrello-data/entity-id checklist)))
    (--reduce-from (cl-destructuring-bind (entities adjacency) acc
                     (list (orgtrello-controller/--add-entity-to-entities it entities)
                           (orgtrello-controller/--add-entity-to-adjacency it checklist adjacency)))
                   (list entities adjacency)
                   (orgtrello-data/entity-items checklist))))

(defun orgtrello-controller/--retrieve-checklist-from-card (card) "Given a card, retrieve the checklist of the card (using trello). This gives a list of checklist in the trello order."
  (--> card
       (orgtrello-data/entity-checklists it)                                                            ;; retrieve checklist ids
       (-reduce-from (lambda (acc-list checklist-id)
                       (cons (-> checklist-id
                                 orgtrello-api/get-checklist
                                 (orgtrello-query/http-trello *do-sync-query*)) acc-list))
                     nil
                     it)                                                                         ;; retrieve the trello checklist
       (sort it (lambda (a b) (when (<= (orgtrello-data/entity-position a) (orgtrello-data/entity-position b)) 1)))))          ;; sort them by pos to get back to the right order (reversed)

(defun orgtrello-controller/--compute-checklist-entities-from-card (card entities adjacency) "Given a card, retrieve its checklists (with their items) in the right order."
  (let ((card-id (orgtrello-data/entity-id card)))
    (--> card
         (orgtrello-controller/--retrieve-checklist-from-card it)
         (-reduce-from (lambda (acc-entities-adj checklist)
                          (cl-destructuring-bind (entities adjacency) acc-entities-adj
                            (orgtrello-controller/--compute-items-from-checklist checklist (orgtrello-controller/--add-entity-to-entities checklist entities) (orgtrello-controller/--add-entity-to-adjacency checklist card adjacency))))
                       (list entities adjacency)
                       it))));; at last complete checklist with item

;; one map for each complete entity: {entity-id entity} (entity in {card, checklist, item}
;; adjacency list {card-id (checklist-id)
;;                 checklist-id (item-id)}

(defun orgtrello-controller/--compute-full-entities-from-trello (cards) "Given a list of cards, compute the full cards data from the trello board. The order from the trello board is kept. Hash result is of the form: {entity-id '(entity-card {checklist-id (checklist (item))})}"
  (--reduce-from (progn
                   (orgtrello-log/msg *OT/INFO* "Computing card '%s' data..."
                                      (orgtrello-data/entity-name it))
                   (cl-destructuring-bind (entities adjacency) acc
                     (orgtrello-controller/--compute-checklist-entities-from-card it (orgtrello-controller/--add-entity-to-entities it entities) adjacency)))
                 (list (orgtrello-hash/empty-hash) (orgtrello-hash/empty-hash))
                 cards))

(defun orgtrello-controller/--get-entity (id entities-hash) "Update the card entry inside the hash."
  (gethash id entities-hash))

(defun orgtrello-controller/--put-card-with-adjacency (current-meta entities adjacency) "Deal with adding card to entities."
  (-> current-meta
      (orgtrello-controller/--put-entities entities)
      (list adjacency)))

(defun orgtrello-controller/--add-entity-to-entities (entity entities) "Adding entity to the hash entities."
  (let ((entity-id (orgtrello-data/entity-id-or-marker entity)))
    (puthash entity-id entity entities)
    entities))

;; FIXME find an already existing implementation.
(defun orgtrello-controller/--add-to-last-pos (value list) "Adding the value to the list in last position."
  (--> list
       (reverse it)
       (cons value it)
       (reverse it)))

(defun orgtrello-controller/--add-entity-to-adjacency (current-entity parent-entity adjacency) "Adding entity to the adjacency entry."
  (let* ((current-id (orgtrello-data/entity-id-or-marker current-entity))
         (parent-id  (orgtrello-data/entity-id-or-marker parent-entity)))
    (puthash parent-id (orgtrello-controller/--add-to-last-pos current-id (gethash parent-id adjacency)) adjacency)
    adjacency))

(defun orgtrello-controller/--put-entities-with-adjacency (current-meta entities adjacency) "Deal with adding a new item to entities."
  (let ((current-entity (orgtrello-data/current current-meta))
        (parent-entity  (orgtrello-data/parent current-meta)))
    (list (orgtrello-controller/--add-entity-to-entities current-entity entities) (orgtrello-controller/--add-entity-to-adjacency current-entity parent-entity adjacency))))

(defun orgtrello-controller/--dispatch-create-entities-map-with-adjacency (entity) "Dispatch the function to update map depending on the entity level."
  (if (orgtrello-data/entity-card-p entity) 'orgtrello-controller/--put-card-with-adjacency 'orgtrello-controller/--put-entities-with-adjacency))

(defun orgtrello-controller/--compute-entities-from-org! () "Compute the full entities present in the org buffer which already had been sync'ed previously. Return the list of entities map and adjacency map in this order."
  (let ((entities (orgtrello-hash/empty-hash))
        (adjacency (orgtrello-hash/empty-hash)))
    (orgtrello-controller/org-map-entities-without-params! (lambda ()
                                                             ;; first will unfold every entries, otherwise https://github.com/org-trello/org-trello/issues/53
                                                             (org-show-subtree)
                                                             (let ((current-entity (-> (orgtrello-data/entry-get-full-metadata) orgtrello-data/current)))
                                                               (unless (-> current-entity orgtrello-data/entity-id orgtrello-controller/id-p) ;; if no id, we set one
                                                                       (orgtrello-controller/--set-marker (orgtrello-controller/--compute-marker-from-entry current-entity)))
                                                               (let ((current-meta (orgtrello-data/entry-get-full-metadata)))
                                                                 (-> current-meta ;; we recompute the metadata because they may have changed
                                                                     orgtrello-data/current
                                                                     orgtrello-controller/--dispatch-create-entities-map-with-adjacency
                                                                     (funcall current-meta entities adjacency))))))
    (list entities adjacency)))

;; entities of the form: {entity-id '(entity-card {checklist-id (checklist (item))})}

(defun orgtrello-controller/--compute-entities-from-org-buffer! (buffername) "Compute the current entities hash from the buffer in the same format as the sync-from-trello routine. Return the list of entities map and adjacency map in this order."
  (set-buffer buffername)
  (save-excursion
    (goto-char (point-min))
    (orgtrello-controller/--compute-entities-from-org!)))

(defun orgtrello-controller/--put-entities (current-meta entities) "Deal with adding a new item to entities."
  (-> current-meta
      orgtrello-data/current
      (orgtrello-controller/--add-entity-to-entities entities)))

(defun orgtrello-controller/--init-map-from (data) "Init a map from a given data. If data is nil, return an empty hash table."
  (if data data (orgtrello-hash/empty-hash)))

(defun orgtrello-controller/--merge-item (trello-item org-item) "Merge trello and org item together."
  (if (null trello-item)
      org-item
      (let ((org-item-to-merge (orgtrello-controller/--init-map-from org-item)))
        (puthash :level *ITEM-LEVEL*                             org-item-to-merge)
        (puthash :id    (orgtrello-data/entity-id trello-item)   org-item-to-merge)
        (puthash :name  (orgtrello-data/entity-name trello-item) org-item-to-merge)
        ;; FIXME find how to populate keyword
        (--> trello-item
             (orgtrello-data/entity-checked it)
             (orgtrello-controller/--compute-state-item it)
             (puthash :keyword it org-item-to-merge))
        org-item-to-merge)))

(defun orgtrello-controller/--merge-checklist (trello-checklist org-checklist) "Merge trello and org checklist together."
  (if (null trello-checklist)
      org-checklist
      (let ((org-checklist-to-merge (orgtrello-controller/--init-map-from org-checklist)))
        (puthash :level *CHECKLIST-LEVEL*                            org-checklist-to-merge)
        (puthash :name (orgtrello-data/entity-name trello-checklist) org-checklist-to-merge)
        (puthash :id   (orgtrello-data/entity-id trello-checklist)   org-checklist-to-merge)
        org-checklist-to-merge)))

(defun orgtrello-controller/--merge-member-ids (trello-card org-card) "Merge users assigned from trello and org."
  (--> trello-card
       (orgtrello-data/entity-member-ids it)
       (orgtrello-data/merge-2-lists-without-duplicates it (orgtrello-data/entity-member-ids-as-list org-card))
       (orgtrello-controller/--users-to it)))

(defun orgtrello-controller/--merge-card (trello-card org-card) "Merge trello and org card together."
  (if (null trello-card)
      org-card
    (let ((org-card-to-merge (orgtrello-controller/--init-map-from org-card)))
        (puthash :comments (orgtrello-data/entity-comments trello-card)                              org-card-to-merge)
        (puthash :level   *CARD-LEVEL*                                                               org-card-to-merge)
        (puthash :id      (orgtrello-data/entity-id trello-card)                                     org-card-to-merge)
        (puthash :name    (orgtrello-data/entity-name trello-card)                                   org-card-to-merge)
        (puthash :keyword (-> trello-card
                              orgtrello-data/entity-list-id
                              orgtrello-controller/--compute-card-status)                            org-card-to-merge)
        (puthash :member-ids (orgtrello-controller/--merge-member-ids trello-card org-card-to-merge) org-card-to-merge)
        (puthash :desc    (orgtrello-data/entity-description trello-card)                            org-card-to-merge)
        org-card-to-merge)))

(defun orgtrello-controller/--dispatch-merge-fn (entity) "Dispatch the function fn to merge the entity."
  (cond ((orgtrello-data/entity-card-p entity)      'orgtrello-controller/--merge-card)
        ((orgtrello-data/entity-checklist-p entity) 'orgtrello-controller/--merge-checklist)
        ((orgtrello-data/entity-item-p entity)      'orgtrello-controller/--merge-item)))

(defun orgtrello-controller/--merge-entities-trello-and-org (trello-data org-data) "Merge the org-entity entities inside the trello-entities."
  (let ((trello-entities  (first trello-data))
        (trello-adjacency (second trello-data))
        (org-entities     (first org-data))
        (org-adjacency    (second org-data)))

    (maphash (lambda (id trello-entity)
               (puthash id (funcall (orgtrello-controller/--dispatch-merge-fn trello-entity) trello-entity (orgtrello-controller/--get-entity id org-entities)) trello-entities) ;; updating entity to trello
               (puthash id (orgtrello-data/merge-2-lists-without-duplicates (gethash id trello-adjacency) (gethash id org-adjacency))     trello-adjacency)) ;; update entity adjacency to trello
             trello-entities)

    ;; copy the entities only present on org files to the trello entities.
    (maphash (lambda (id org-entity)
               (unless (gethash id trello-entities)
                       (puthash id org-entity trello-entities)
                       (puthash id (gethash id org-adjacency) trello-adjacency)))
             org-entities)

    (list trello-entities trello-adjacency)))

(defun orgtrello-controller/--update-property (id orgcheckbox-p) "Update the property depending on the nature of thing to sync. Move the cursor position."
  (if orgcheckbox-p
      (save-excursion
        (forward-line -1) ;; need to get back one line backward for the checkboxes as their properties is at the same level (otherwise, for headings we do not care)
        (orgtrello-action/set-property *ORGTRELLO-ID* id))
      (orgtrello-action/set-property *ORGTRELLO-ID* id)))

(defun orgtrello-controller/--write-entity! (entity-id entity) "Write the entity in the buffer to the current position. Move the cursor position."
  (orgtrello-log/msg *OT/INFO* "Synchronizing entity '%s' with id '%s'..." (orgtrello-data/entity-name entity) entity-id)
  (insert (orgtrello-controller/--compute-entity-to-org-entry entity))
  (when entity-id (orgtrello-controller/--update-property entity-id (not (orgtrello-data/entity-card-p entity)))))

(defun orgtrello-controller/org-map-entities-without-params! (fn-to-execute) "Execute fn-to-execute function for all entities from buffer - fn-to-execute is a function without any parameters."
  (org-map-entries
     (lambda ()
       (funcall fn-to-execute) ;; execute on heading entry
       (orgtrello-cbx/map-checkboxes fn-to-execute)) t 'file))

(defun orgtrello-controller/--write-item! (entity-id entities) "Write the item to the org buffer."
  (->> entities
       (gethash entity-id)
       (orgtrello-controller/--write-entity! entity-id)))

(defun orgtrello-controller/--write-checklist! (entity-id entities adjacency) "Write the checklist inside the org buffer."
  (orgtrello-controller/--write-entity! entity-id (gethash entity-id entities))
  (--map (orgtrello-controller/--write-item! it entities) (gethash entity-id adjacency)))

(defun orgtrello-controller/--update-member-ids-property! (entity) "Update the users assigned property card entry."
  (--> entity
       (orgtrello-data/entity-member-ids it)
       (orgtrello-controller/--csv-user-ids-to-csv-user-names it *HMAP-USERS-ID-NAME*)
       (replace-regexp-in-string *ORGTRELLO-USER-PREFIX* "" it)
       (orgtrello-controller/set-usernames-assigned-property! it)))

(defun orgtrello-controller/--comments-to-list (comments-hash)
  "Given a list of comments hashmap, return the serialized string comment."
  (->> comments-hash
    (--map (s-join ": " (list (gethash :comment-user it) (gethash :comment-text it))))
    (s-join *ORGTRELLO-CARD-COMMENTS-DELIMITER*)))

(defun orgtrello-controller/set-property-comment! (comments)
  "Update comment property."
  (org-entry-put nil *ORGTRELLO-CARD-COMMENTS* comments))

(defun orgtrello-controller/--update-property-card-comments! (entity)
  "Update last comments "
  (->> entity
    orgtrello-data/entity-comments
    orgtrello-controller/--comments-to-list
    orgtrello-controller/set-property-comment!))

(defun orgtrello-controller/--write-card! (entity-id entity entities adjacency) "Write the card inside the org buffer."
  (orgtrello-controller/--write-entity! entity-id entity)
  (orgtrello-controller/--update-member-ids-property! entity)
  (orgtrello-controller/--update-property-card-comments! entity)
  (-when-let (entity-desc (orgtrello-data/entity-description entity))
             (insert (format "%s\n" entity-desc)))
  (--map (orgtrello-controller/--write-checklist! it entities adjacency) (gethash entity-id adjacency)))

(defun orgtrello-controller/--sync-buffer-with-trello-data (data buffer-name) "Given all the entities, update the current buffer with those."
  (let ((entities (first data))
        (adjacency (second data)))
    (with-current-buffer buffer-name
      (goto-char (point-max)) ;; go at the end of the file
      (maphash
       (lambda (new-id entity)
         (when (orgtrello-data/entity-card-p entity)
               (orgtrello-controller/--write-card! new-id entity entities adjacency)))
       entities)
      (goto-char (point-min)) ;; go back to the beginning of file
      (org-sort-entries t ?o) ;; sort the entries on their keywords
      (save-buffer))))

(defun orgtrello-controller/--cleanup-org-entries () "Cleanup org-entries from the buffer (FIXME find a suiter way of merging data than removing them all and put them back)."
  (goto-char (point-min))
  (outline-next-heading)
  (orgtrello-cbx/remove-overlays! (point-at-bol) (point-max))
  (kill-region (point-at-bol) (point-max)))

(defun orgtrello-controller/--sync-buffer-with-trello-data-callback (buffername &optional position name) "Generate a callback which knows the buffer with which it must work. (this callback must take a buffer-name and a position)"
  (lexical-let ((buffer-name              buffername)
                (entities-from-org-buffer (orgtrello-controller/--compute-entities-from-org-buffer! buffername)))
    (function* (lambda (&key data &allow-other-keys) "Synchronize the buffer with the response data."
       (orgtrello-log/msg *OT/TRACE* "proxy - response data: %S" data)
       (-> data                                                                 ;; compute merge between already sync'ed entries and the trello data
           orgtrello-controller/--compute-full-entities-from-trello                        ;; slow computation with network access
           (orgtrello-controller/--merge-entities-trello-and-org entities-from-org-buffer) ;; slow merge computation
           ((lambda (entry) (orgtrello-controller/--cleanup-org-entries) entry))           ;; hack to clean the org entries just before synchronizing the buffer
           (orgtrello-controller/--sync-buffer-with-trello-data buffer-name)
           (orgtrello-action/safe-wrap (orgtrello-log/msg *OT/INFO* "Synchronizing the trello and org data merge - done!")))))))

(defun orgtrello-controller/do-sync-full-from-trello (&optional sync) "Full org-mode file synchronisation. Beware, this will block emacs as the request is synchronous."
  (orgtrello-log/msg *OT/INFO* "Synchronizing the trello board '%s' to the org-mode file. This may take a moment, some coffee may be a good idea..." (orgtrello-controller/--board-name))
  ;; then start the sync computations
  (--> (orgtrello-controller/--board-id)
       (orgtrello-api/get-cards it)
       (orgtrello-controller/--update-query-with-org-metadata it nil (buffer-name) nil 'orgtrello-controller/--sync-buffer-with-trello-data-callback)
       (orgtrello-proxy/http it sync)))

(defun orgtrello-controller/--card-delete (card-meta &optional parent-meta) "Deal with the deletion query of a card" ;; parent is useless here
  (orgtrello-api/delete-card (orgtrello-data/entity-id card-meta)))

(defun orgtrello-controller/--checklist-delete (checklist-meta &optional parent-meta) "Deal with the deletion query of a checklist" ;; parent is useless here
  (orgtrello-api/delete-checklist (orgtrello-data/entity-id checklist-meta)))

(defun orgtrello-controller/--item-delete (item-meta &optional checklist-meta) "Deal with create/update item query build"
  (orgtrello-api/delete-item (orgtrello-data/entity-id checklist-meta) (orgtrello-data/entity-id item-meta)))

(defvar *MAP-DISPATCH-DELETE* (orgtrello-hash/make-properties `((,*CARD-LEVEL*      . orgtrello-controller/--card-delete)
                                                                (,*CHECKLIST-LEVEL* . orgtrello-controller/--checklist-delete)
                                                                (,*ITEM-LEVEL*      . orgtrello-controller/--item-delete))) "Dispatch map for the deletion query of card/checklist/item.")

(defun orgtrello-controller/--dispatch-delete (meta &optional parent-meta) "Dispatch the delete function to call depending on the level information."
  (-> meta
      orgtrello-data/entity-level
      (gethash *MAP-DISPATCH-DELETE* 'orgtrello-controller/--too-deep-level)
      (funcall meta parent-meta)))

(defun orgtrello-controller/--do-delete-card (&optional sync) "Delete the card."
  (when (= *CARD-LEVEL* (-> (orgtrello-data/entry-get-full-metadata)
                            orgtrello-data/current
                            orgtrello-data/entity-level))
        (orgtrello-controller/do-delete-simple sync)))

(defun orgtrello-controller/do-delete-entities (&optional sync) "Launch a batch deletion of every single entities present on the buffer."
  (org-map-entries (lambda () (orgtrello-controller/--do-delete-card sync)) t 'file))

(defun orgtrello-controller/--do-install-config-file (*consumer-key* *access-token*) "Persist the file config-file with the input of the user."
  (make-directory *CONFIG-DIR* t)
  (with-temp-file *CONFIG-FILE*
    (erase-buffer)
    (goto-char (point-min))
    (insert (format "(setq *consumer-key* \"%s\")\n" *consumer-key*))
    (insert (format "(setq *access-token* \"%s\")" *access-token*))
    (write-file *CONFIG-FILE* 't)))

(defun orgtrello-controller/do-install-key-and-token () "Procedure to install the *consumer-key* and the token for the user in the config-file."
  (interactive)
  (browse-url (org-trello/https-trello "/1/appKey/generate"))
  (let ((orgtrello-controller/--*consumer-key* (read-string "*consumer-key*: ")))
    (browse-url (org-trello/https-trello (format "/1/authorize?response_type=token&name=org-trello&scope=read,write&expiration=never&key=%s" orgtrello-controller/--*consumer-key*)))
    (let ((orgtrello-controller/--access-token (read-string "Access-token: ")))
      (orgtrello-controller/--do-install-config-file orgtrello-controller/--*consumer-key* orgtrello-controller/--access-token)
      "Install key and read/write access token done!")))

(defun orgtrello-controller/--id-name (entities) "Given a list of entities, return a map of (id, name)."
  (--reduce-from (progn (puthash (orgtrello-data/entity-id it) (orgtrello-data/entity-name it) acc) acc) (orgtrello-hash/empty-hash) entities))

(defun orgtrello-controller/--name-id (entities) "Given a list of entities, return a map of (id, name)."
  (--reduce-from (progn (puthash (orgtrello-data/entity-name it) (orgtrello-data/entity-id it) acc) acc) (orgtrello-hash/empty-hash) entities))

(defun orgtrello-controller/--list-boards () "Return the map of the existing boards associated to the current account. (Synchronous request)"
  (--remove (orgtrello-data/entity-closed it) (orgtrello-query/http-trello (orgtrello-api/get-boards) *do-sync-query*)))

(defun orgtrello-controller/--list-board-lists (board-id) "Return the map of the existing list of the board with id board-id. (Synchronous request)"
  (orgtrello-query/http-trello (orgtrello-api/get-lists board-id) *do-sync-query*))

(defun orgtrello-controller/--choose-board (boards) "Given a map of boards, display the possible boards for the user to choose which one he wants to work with."  ;; ugliest ever
  (defvar orgtrello-controller/--board-chosen nil)
  (setq orgtrello-controller/--board-chosen nil)
  (let* ((str-key-val  "")
         (i            0)
         (i-id (orgtrello-hash/empty-hash)))
    (maphash (lambda (id name)
               (setq str-key-val (format "%s%d: %s\n" str-key-val i name))
               (puthash (format "%d" i) id i-id)
               (setq i (+ 1 i)))
             boards)
    (while (not (gethash orgtrello-controller/--board-chosen i-id))
      (setq orgtrello-controller/--board-chosen
            (read-string (format "%s\nInput the number of the board desired: " str-key-val))))
    (let* ((orgtrello-controller/--chosen-board-id   (gethash orgtrello-controller/--board-chosen i-id))
           (orgtrello-controller/--chosen-board-name ))
      `(,orgtrello-controller/--chosen-board-id ,(gethash orgtrello-controller/--chosen-board-id boards)))))

(defun orgtrello-controller/--convention-property-name (name) "Use the right convention for the property used in the headers of the org-mode file."
  (replace-regexp-in-string " " "-" name))

(defun orgtrello-controller/--delete-buffer-property (property-name) "A simple routine to delete a #+property: entry from the org-mode buffer."
  (save-excursion
    (goto-char (point-min))
    (-when-let (current-point (search-forward property-name nil t))
               (goto-char current-point)
               (beginning-of-line)
               (kill-line)
               (kill-line))))

(defun orgtrello-controller/compute-property (property-name &optional property-value) "Compute a formatted entry in org buffer"
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

(defun orgtrello-controller/--remove-properties-file! (list-keywords users-hash-name-id user-me &optional update-todo-keywords) "Remove the current org-trello properties"
  (with-current-buffer (current-buffer)
    ;; compute the list of properties to purge
    (->> `(":PROPERTIES"
           ,(orgtrello-controller/compute-property *BOARD-ID*)
           ,(orgtrello-controller/compute-property *BOARD-NAME*)
           ,@(--map (orgtrello-controller/compute-property (orgtrello-controller/--convention-property-name it)) list-keywords)
           ,@(orgtrello-controller/--compute-hash-name-id-to-list users-hash-name-id)
           ,(orgtrello-controller/compute-property *ORGTRELLO-USER-ME* user-me)
           ,(if update-todo-keywords "#+TODO: ")
           ":END:")
         (mapc (lambda (property-to-remove) (orgtrello-controller/--delete-buffer-property property-to-remove))))))

(defun orgtrello-controller/--compute-keyword-separation (name) "Given a keyword done (case insensitive) return a string '| done' or directly the keyword"
  (if (string= "done" (downcase name)) (format "| %s" name) name))

(defun orgtrello-controller/--compute-board-lists-hash-name-id (board-lists-hash-name-id) ""
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

(defun orgtrello-controller/--update-orgmode-file-with-properties (board-name board-id board-lists-hash-name-id board-users-hash-name-id user-me &optional update-todo-keywords) "Update the orgmode file with the needed headers for org-trello to work."
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (set-buffer-file-coding-system 'utf-8-auto) ;; force utf-8
    (->> `(":PROPERTIES:"
           ,(format "#+PROPERTY: %s    %s" *BOARD-NAME* board-name)
            ,(format "#+PROPERTY: %s      %s" *BOARD-ID* board-id)
            ,@(orgtrello-controller/--compute-board-lists-hash-name-id board-lists-hash-name-id)
            ,(if update-todo-keywords (orgtrello-controller/--properties-compute-todo-keywords-as-string board-lists-hash-name-id))
            ,@(orgtrello-controller/--properties-compute-users-ids board-users-hash-name-id)
            ,(format "#+PROPERTY: %s %s" *ORGTRELLO-USER-ME* user-me)
            ":END:")
         (mapc (lambda (property-to-insert) (insert property-to-insert "\n"))))
    (goto-char (point-min))
    (org-cycle)
    (save-buffer)
    (orgtrello-action/reload-setup)))

(defun orgtrello-controller/--hash-table-keys (hash-table) "Extract the keys from the hash table."
  (let ((keys ()))
    (maphash (lambda (k v) (push k keys)) hash-table)
    keys))

(defun orgtrello-controller/--user-logged-in () "Compute the current user."
  (--> (orgtrello-api/get-me)
       (orgtrello-query/http-trello it *do-sync-query*)
       (orgtrello-data/entity-username it)))

(defun orgtrello-controller/do-install-board-and-lists () "Command to install the list boards."
  (interactive)
  (cl-destructuring-bind
      (orgtrello-controller/--chosen-board-id orgtrello-controller/--chosen-board-name) (-> (orgtrello-controller/--list-boards)
                                                                      orgtrello-controller/--id-name
                                                                      orgtrello-controller/--choose-board)
    (let* ((orgtrello-controller/--board-lists-hname-id (-> orgtrello-controller/--chosen-board-id
                                                 orgtrello-controller/--list-board-lists
                                                 orgtrello-controller/--name-id))
           (orgtrello-controller/--board-list-keywords (orgtrello-controller/--hash-table-keys orgtrello-controller/--board-lists-hname-id))
           (orgtrello-controller/--board-users-name-id (orgtrello-controller/--board-users-information-from-board-id! orgtrello-controller/--chosen-board-id))
           (user-logged-in                  (orgtrello-controller/--user-logged-in)))
      ;; remove any eventual present entry
      (orgtrello-controller/--remove-properties-file! orgtrello-controller/--board-list-keywords orgtrello-controller/--board-users-name-id user-logged-in t)
      ;; update with new ones
      (orgtrello-controller/--update-orgmode-file-with-properties
       orgtrello-controller/--chosen-board-name
       orgtrello-controller/--chosen-board-id
       orgtrello-controller/--board-lists-hname-id
       orgtrello-controller/--board-users-name-id
       user-logged-in
       t))
    "Install board and list ids done!"))

(defun orgtrello-controller/--compute-user-properties (memberships-map) "Given a map, extract the map of user informations."
  (mapcar 'orgtrello-data/entity-member memberships-map))

(defun orgtrello-controller/--compute-user-properties-hash (user-properties)
  (--reduce-from (progn (puthash (orgtrello-data/entity-username it) (orgtrello-data/entity-id it) acc) acc) (orgtrello-hash/empty-hash) user-properties))

(defun orgtrello-controller/--compute-user-properties-hash-from-board (board-info) "Compute user properties given board's informations."
  (->> board-info
       orgtrello-data/entity-memberships
       orgtrello-controller/--compute-user-properties
       orgtrello-controller/--compute-user-properties-hash))

(defun orgtrello-controller/--board-users-information-from-board-id! (board-id) "Compute board users' informations."
  (--> board-id
       (orgtrello-api/get-board it)
       (orgtrello-query/http-trello it *do-sync-query*)
       (orgtrello-controller/--compute-user-properties-hash-from-board it)))

(defun orgtrello-controller/--create-board (board-name &optional board-description) "Create a board with name and eventually a description."
  (orgtrello-log/msg *OT/INFO* "Creating board '%s'" board-name)
  (let ((board-data (orgtrello-query/http-trello (orgtrello-api/add-board board-name board-description) *do-sync-query*)))
    (list (orgtrello-data/entity-id board-data) (orgtrello-data/entity-name board-data))))

(defun orgtrello-controller/--close-lists (list-ids) "Given a list of ids, close those lists."
  (mapc (lambda (list-id)
          (orgtrello-log/msg *OT/INFO* "Closing default list with id %s" list-id)
          (orgtrello-query/http-trello (orgtrello-api/close-list list-id)))
        list-ids))

(defun orgtrello-controller/--create-lists-according-to-keywords (board-id list-keywords) "Given a list of names, build those lists on the trello boards. Return the hashmap (name, id) of the new lists created."
  (--reduce-from (progn
                   (orgtrello-log/msg *OT/INFO* "Board id %s - Creating list '%s'"
                                      board-id it)
                   (puthash it (orgtrello-data/entity-id (orgtrello-query/http-trello (orgtrello-api/add-list it board-id) *do-sync-query*)) acc)
                   acc)
                 (orgtrello-hash/empty-hash)
                 list-keywords))

(defun orgtrello-controller/do-create-board-and-lists () "Command to create a board and the lists."
  (defvar orgtrello-controller/--board-name nil)        (setq orgtrello-controller/--board-name nil)
  (defvar orgtrello-controller/--board-description nil) (setq orgtrello-controller/--board-description nil)
  (while (not orgtrello-controller/--board-name) (setq orgtrello-controller/--board-name (read-string "Please, input the desired board name: ")))
  (setq orgtrello-controller/--board-description (read-string "Please, input the board description (empty for none): "))
  (cl-destructuring-bind (orgtrello-controller/--board-id orgtrello-controller/--board-name) (orgtrello-controller/--create-board orgtrello-controller/--board-name orgtrello-controller/--board-description)
                         (let* ((orgtrello-controller/--board-list-ids       (--map (orgtrello-data/entity-id it) (orgtrello-controller/--list-board-lists orgtrello-controller/--board-id)))  ;; first retrieve the existing lists (created by default on trello)
                                (orgtrello-controller/--lists-to-close       (orgtrello-controller/--close-lists orgtrello-controller/--board-list-ids))                                ;; close those lists (they may surely not match the name we want)
                                (orgtrello-controller/--board-lists-hname-id (orgtrello-controller/--create-lists-according-to-keywords orgtrello-controller/--board-id *LIST-NAMES*))  ;; create the list, this returns the ids list
                                (orgtrello-controller/--board-users-name-id  (orgtrello-controller/--board-users-information-from-board-id! orgtrello-controller/--board-id))           ;; retrieve user informations
                                (user-logged-in                   (orgtrello-controller/--user-logged-in)))
                           (orgtrello-controller/--remove-properties-file! *LIST-NAMES* orgtrello-controller/--board-users-name-id user-logged-in) ;; remove eventual already present entry
                           (orgtrello-controller/--update-orgmode-file-with-properties orgtrello-controller/--board-name orgtrello-controller/--board-id orgtrello-controller/--board-lists-hname-id orgtrello-controller/--board-users-name-id user-logged-in))) ;; update org buffer with new ones
  "Create board and lists done!")

(defun orgtrello-controller/--users-from (string-users) "Compute the users name from the comma separated value in string."
  (when string-users (split-string string-users "," t)))

(defun orgtrello-controller/--add-user (user users) "Add the user to the users list"
  (if (member user users) users (cons user users)))

(defun orgtrello-controller/--remove-user (user users) "Add the user to the users list"
  (if (member user users) (remove user users) users users))

(defun orgtrello-controller/--users-to (users) "Given a list of users, compute the comma separated users."
  (if users (mapconcat 'identity users ",") ""))

(defun orgtrello-controller/--me ()
  (assoc-default *ORGTRELLO-USER-ME* org-file-properties))

(defun orgtrello-controller/--user-ids-assigned-to-current-card () "Compute the user ids assigned to the current card."
  (--> (orgtrello-controller/get-usernames-assigned-property!)
       (orgtrello-controller/--users-from it)
       (--map (gethash (format "%s%s" *ORGTRELLO-USER-PREFIX* it) *HMAP-USERS-NAME-ID*) it)
       (orgtrello-controller/--users-to it)))

(defun orgtrello-controller/--csv-user-ids-to-csv-user-names (csv-users-id users-id-name) "Given a comma separated list of user id and a map, return a comma separated list of username."
  (->> csv-users-id
       orgtrello-controller/--users-from
       (--map (gethash it users-id-name))
       orgtrello-controller/--users-to))

(defun orgtrello-controller/get-usernames-assigned-property! () "Read the org users property from the current entry."
  (org-entry-get nil *ORGTRELLO-USERS-ENTRY*))

(defun orgtrello-controller/set-usernames-assigned-property! (csv-users) "Update users org property."
  (org-entry-put nil *ORGTRELLO-USERS-ENTRY* csv-users))

(defun orgtrello-controller/do-assign-me () "Command to assign oneself to the card."
  (--> (orgtrello-controller/get-usernames-assigned-property!)
       (orgtrello-controller/--users-from it)
       (orgtrello-controller/--add-user *ORGTRELLO-USER-LOGGED-IN* it)
       (orgtrello-controller/--users-to it)
       (orgtrello-controller/set-usernames-assigned-property! it)))

(defun orgtrello-controller/do-unassign-me () "Command to unassign oneself of the card."
  (--> (orgtrello-controller/get-usernames-assigned-property!)
       (orgtrello-controller/--users-from it)
       (orgtrello-controller/--remove-user *ORGTRELLO-USER-LOGGED-IN* it)
       (orgtrello-controller/--users-to it)
       (orgtrello-controller/set-usernames-assigned-property! it)))

(defun orgtrello-controller/--delete-property (property) "Given a property name (checkbox), if found, delete it from the buffer."
  (org-delete-property-globally property)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":PROPERTIES: {.*" nil t)
      (remove-overlays (point-at-bol) (point-at-eol)) ;; the current overlay on this line
      (replace-match "" nil t))))                     ;; then remove the property

(defun orgtrello-controller/remove-overlays! () "Remove every org-trello overlays from the current buffer."
  (orgtrello-cbx/remove-overlays! (point-min) (point-max)))

(defun orgtrello-controller/install-overlays! () "Install overlays throughout the all buffers."
  (orgtrello-controller/remove-overlays!)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":PROPERTIES: {.*" nil t)
      (orgtrello-cbx/install-overlays! (match-beginning 0)))))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-controller loaded!")


