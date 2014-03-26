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
                                                                        (buffername     . :buffername)
                                                                        (sync           . :sync)
                                                                        (uri            . :uri)
                                                                        (method         . :method)
                                                                        (params         . :params)
                                                                        (action         . :action)
                                                                        (start          . :start)
                                                                        (callback       . :callback)
                                                                        (pos            . :position)
                                                                        (position       . :position)
                                                                        (keyword        . :keyword)
                                                                        (start          . :start)
                                                                        (level          . :level)
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
  (->> comments
    (s-split *ORGTRELLO-CARD-COMMENTS-DELIMITER*)
    (s-join *ORGTRELLO-CARD-COMMENTS-DELIMITER-PRINT*)))

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


