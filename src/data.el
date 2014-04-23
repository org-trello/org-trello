(defun orgtrello-data/merge-2-lists-without-duplicates (a-list b-list)
  "Merge 2 lists together (no duplicates)."
  (-> a-list
    (append b-list)
    delete-dups))

(defun orgtrello-data/--entity-with-level-p (entity level) "Is the entity with level level?" (-> entity orgtrello-data/entity-level (eq level)))
(defun orgtrello-data/entity-card-p      (entity) "Is this a card?"      (orgtrello-data/--entity-with-level-p entity *ORGTRELLO/CARD-LEVEL*))
(defun orgtrello-data/entity-checklist-p (entity) "Is this a checklist?" (orgtrello-data/--entity-with-level-p entity *ORGTRELLO/CHECKLIST-LEVEL*))
(defun orgtrello-data/entity-item-p      (entity) "Is this an item?"     (orgtrello-data/--entity-with-level-p entity *ORGTRELLO/ITEM-LEVEL*))

(defun orgtrello-data/gethash-data (key map &optional default-value) "Retrieve the map from some query-map" (when map (gethash key map default-value)))

(defun orgtrello-data/entity-id (entity)
  "Dispatch to the rightful function to get the id"
  (let ((id (orgtrello-data/entity-id-or-marker entity)))
    (when (orgtrello-data/id-p id)
      id)))

(defun orgtrello-data/entity-keyword (entity &optional default-value)
  "Retrieve the keyword from the entity."
  (orgtrello-data/gethash-data :keyword entity default-value))

(defun orgtrello-data/entity-name         (entity) (orgtrello-data/gethash-data :name         entity))
(defun orgtrello-data/entity-memberships  (entity) (orgtrello-data/gethash-data :memberships  entity))
(defun orgtrello-data/entity-member       (entity) (orgtrello-data/gethash-data :member       entity))
(defun orgtrello-data/entity-username     (entity) (orgtrello-data/gethash-data :username     entity))
(defun orgtrello-data/entity-action       (entity) (orgtrello-data/gethash-data :action       entity))
(defun orgtrello-data/entity-board-id     (entity) (orgtrello-data/gethash-data :board-id     entity))
(defun orgtrello-data/entity-card-id      (entity) (orgtrello-data/gethash-data :card-id      entity))
(defun orgtrello-data/entity-list-id      (entity) (orgtrello-data/gethash-data :list-id      entity))
(defun orgtrello-data/entity-member-ids   (entity) (orgtrello-data/gethash-data :member-ids   entity))
(defun orgtrello-data/entity-description  (entity) (orgtrello-data/gethash-data :desc         entity))
(defun orgtrello-data/entity-checklists   (entity) (orgtrello-data/gethash-data :checklists   entity))
(defun orgtrello-data/entity-items        (entity) (orgtrello-data/gethash-data :items        entity))
(defun orgtrello-data/entity-position     (entity) (orgtrello-data/gethash-data :position     entity))
(defun orgtrello-data/entity-buffername   (entity) (orgtrello-data/gethash-data :buffername   entity))
(defun orgtrello-data/entity-checked      (entity) (orgtrello-data/gethash-data :checked      entity))
(defun orgtrello-data/entity-due          (entity) (orgtrello-data/gethash-data :due          entity))
(defun orgtrello-data/entity-id-or-marker (entity) (orgtrello-data/gethash-data :id           entity))
(defun orgtrello-data/entity-level        (entity) (orgtrello-data/gethash-data :level        entity))
(defun orgtrello-data/entity-closed       (entity) (orgtrello-data/gethash-data :closed       entity))
(defun orgtrello-data/entity-callback     (entity) (orgtrello-data/gethash-data :callback     entity))
(defun orgtrello-data/entity-start        (entity) (orgtrello-data/gethash-data :start        entity))
(defun orgtrello-data/entity-comments     (entity) (orgtrello-data/gethash-data :comments     entity))
(defun orgtrello-data/entity-labels       (entity) (orgtrello-data/gethash-data :labels       entity))
(defun orgtrello-data/entity-tags         (entity) (orgtrello-data/gethash-data :tags         entity))
(defun orgtrello-data/entity-comment-id   (entity) (orgtrello-data/gethash-data :comment-id   entity))
(defun orgtrello-data/entity-comment-text (entity) (orgtrello-data/gethash-data :comment-text entity))
(defun orgtrello-data/entity-comment-user (entity) (orgtrello-data/gethash-data :comment-user entity))
(defun orgtrello-data/entity-color        (entity) (orgtrello-data/gethash-data :color        entity))

(defun orgtrello-data/entity-method (query-map) (orgtrello-data/gethash-data :method query-map))
(defun orgtrello-data/entity-uri    (query-map) (orgtrello-data/gethash-data :uri    query-map))
(defun orgtrello-data/entity-sync   (query-map) (orgtrello-data/gethash-data :sync   query-map))
(defun orgtrello-data/entity-params (query-map) (orgtrello-data/gethash-data :params query-map))

(defun orgtrello-data/current     (entry-meta) (orgtrello-data/gethash-data :current     entry-meta))
(defun orgtrello-data/parent      (entry-meta) (orgtrello-data/gethash-data :parent      entry-meta))
(defun orgtrello-data/grandparent (entry-meta) (orgtrello-data/gethash-data :grandparent entry-meta))

(defun orgtrello-data/puthash-data (key value entity)
  "Update the map at key with value. Return the entity updated. Nil if the entity is nil."
  (when entity
    (puthash key value entity)
    entity))

(defun orgtrello-data/put-entity-name         (value entity)     (orgtrello-data/puthash-data :name         value entity))
(defun orgtrello-data/put-entity-memberships  (value entity)     (orgtrello-data/puthash-data :memberships  value entity))
(defun orgtrello-data/put-entity-member       (value entity)     (orgtrello-data/puthash-data :member       value entity))
(defun orgtrello-data/put-entity-username     (value entity)     (orgtrello-data/puthash-data :username     value entity))
(defun orgtrello-data/put-entity-action       (value entity)     (orgtrello-data/puthash-data :action       value entity))
(defun orgtrello-data/put-entity-board-id     (value entity)     (orgtrello-data/puthash-data :board-id     value entity))
(defun orgtrello-data/put-entity-card-id      (value entity)     (orgtrello-data/puthash-data :card-id      value entity))
(defun orgtrello-data/put-entity-list-id      (value entity)     (orgtrello-data/puthash-data :list-id      value entity))
(defun orgtrello-data/put-entity-member-ids   (value entity)     (orgtrello-data/puthash-data :member-ids   value entity))
(defun orgtrello-data/put-entity-description  (value entity)     (orgtrello-data/puthash-data :desc         value entity))
(defun orgtrello-data/put-entity-checklists   (value entity)     (orgtrello-data/puthash-data :checklists   value entity))
(defun orgtrello-data/put-entity-items        (value entity)     (orgtrello-data/puthash-data :items        value entity))
(defun orgtrello-data/put-entity-position     (value entity)     (orgtrello-data/puthash-data :position     value entity))
(defun orgtrello-data/put-entity-buffername   (value entity)     (orgtrello-data/puthash-data :buffername   value entity))
(defun orgtrello-data/put-entity-checked      (value entity)     (orgtrello-data/puthash-data :checked      value entity))
(defun orgtrello-data/put-entity-due          (value entity)     (orgtrello-data/puthash-data :due          value entity))
(defun orgtrello-data/put-entity-id           (value entity)     (orgtrello-data/puthash-data :id           value entity))
(defun orgtrello-data/put-entity-level        (value entity)     (orgtrello-data/puthash-data :level        value entity))
(defun orgtrello-data/put-entity-closed       (value entity)     (orgtrello-data/puthash-data :closed       value entity))
(defun orgtrello-data/put-entity-callback     (value entity)     (orgtrello-data/puthash-data :callback     value entity))
(defun orgtrello-data/put-entity-start        (value entity)     (orgtrello-data/puthash-data :start        value entity))
(defun orgtrello-data/put-entity-comments     (value entity)     (orgtrello-data/puthash-data :comments     value entity))
(defun orgtrello-data/put-entity-labels       (value entity)     (orgtrello-data/puthash-data :labels       value entity))
(defun orgtrello-data/put-entity-tags         (value entity)     (orgtrello-data/puthash-data :tags         value entity))
(defun orgtrello-data/put-entity-keyword      (value entity)     (orgtrello-data/puthash-data :keyword      value entity))
(defun orgtrello-data/put-entity-comment-id   (value entity)     (orgtrello-data/puthash-data :comment-id   value entity))
(defun orgtrello-data/put-entity-comment-text (value entity)     (orgtrello-data/puthash-data :comment-text value entity))
(defun orgtrello-data/put-entity-comment-user (value entity)     (orgtrello-data/puthash-data :comment-user value entity))

(defun orgtrello-data/put-entity-method       (value query-map)  (orgtrello-data/puthash-data :method       value query-map))
(defun orgtrello-data/put-entity-uri          (value query-map)  (orgtrello-data/puthash-data :uri          value query-map))
(defun orgtrello-data/put-entity-sync         (value query-map)  (orgtrello-data/puthash-data :sync         value query-map))
(defun orgtrello-data/put-entity-params       (value query-map)  (orgtrello-data/puthash-data :params       value query-map))

(defun orgtrello-data/put-current             (value entry-meta) (orgtrello-data/puthash-data :current      value entry-meta))
(defun orgtrello-data/put-parent              (value entry-meta) (orgtrello-data/puthash-data :parent       value entry-meta))
(defun orgtrello-data/put-grandparent         (value entry-meta) (orgtrello-data/puthash-data :grandparent  value entry-meta))

(defun orgtrello-data/--compute-level (entity-map) "Given a map, compute the entity level"
       (cond ((orgtrello-data/entity-list-id entity-map) *ORGTRELLO/CARD-LEVEL*)
             ((orgtrello-data/entity-card-id entity-map) *ORGTRELLO/CHECKLIST-LEVEL*)
             ((orgtrello-data/entity-checked entity-map) *ORGTRELLO/ITEM-LEVEL*)
             (t nil)))

(defvar *ORGTRELLO/DATA-MAP-KEYWORDS* (orgtrello-hash/make-properties `((url            . :url)
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
                                                                        (pos            . :position)
                                                                        (keyword        . :keyword)
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
  "Given a key, return it as is if it's a keyword or return its mapped version from *ORGTRELLO/DATA-MAP-KEYWORDS*"
  (cond ((keywordp key) key)
        (t             (gethash key *ORGTRELLO/DATA-MAP-KEYWORDS*))))

(defun orgtrello-data/--dispatch-parse-data-fn (key)
  "Given a key, return the function to call to execute the parsing (parse-actions or parse-data)"
  (cond ((eq :comments key) 'orgtrello-data/--parse-actions)
        (t                  'orgtrello-data/parse-data)))

(defun orgtrello-data/--parse-actions (data &optional size)
  "Given a 'comment' (trello action limited to comment in org-trello) structure, return a list of map representing a comment."
  (--map (->> (orgtrello-hash/empty-hash)
           (orgtrello-data/put-entity-comment-id   (assoc-default 'id it))
           (orgtrello-data/put-entity-comment-text (->> it (assoc-default 'data) (assoc-default 'text)))
           (orgtrello-data/put-entity-comment-user (->> it car (assoc-default 'username))))
         data))

(defun orgtrello-data/parse-data (entities)
  "Given a trello entity, convert into org-trello entity"
  (cond ((eq :json-false entities)                                           nil)
        ((--any? (funcall it entities) '(stringp symbolp numberp functionp)) entities)
        ((arrayp entities)                                                   (mapcar 'orgtrello-data/parse-data entities))
        (t
         (let ((hmap (--reduce-from (let ((key (car it))
                                          (val (cdr it)))
                                      (-when-let (new-key (orgtrello-data/--deal-with-key key))
                                        (orgtrello-data/puthash-data new-key (funcall (orgtrello-data/--dispatch-parse-data-fn new-key) val) acc))
                                      acc)
                                    (orgtrello-hash/empty-hash)
                                    entities)))
           (-when-let (level (orgtrello-data/--compute-level hmap)) (orgtrello-data/put-entity-level level hmap))
           hmap))))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-data loaded!")

(defun orgtrello-data/comments-to-list (comments-hash)
  "Given a list of comments hashmap, return the serialized string comment."
  (->> comments-hash
    (--map (s-join ": " (list (orgtrello-data/entity-comment-user it) (orgtrello-data/entity-comment-text it))))
    (s-join *ORGTRELLO/CARD-COMMENTS-DELIMITER*)))

(defun orgtrello-data/format-labels (labels)
  "Given an assoc list of labels, serialize it."
  (->> labels
    (--map (s-join ": " (list (car it) (cdr it))))
    (s-join "\n\n")))

(defun orgtrello-data/unformat-comments (comments)
  "Given a string of comments human readable, transform it into a property format."
  (->> comments
    (s-split *ORGTRELLO/CARD-COMMENTS-DELIMITER-PRINT*)
    (s-join *ORGTRELLO/CARD-COMMENTS-DELIMITER*)))

(defun orgtrello-data/format-comments (comments)
  "Given a property string of comments, work it to permit a human readable display."
  (if comments
      (->> comments
        (s-split *ORGTRELLO/CARD-COMMENTS-DELIMITER*)
        (s-join *ORGTRELLO/CARD-COMMENTS-DELIMITER-PRINT*))
    "No comments to display!"))

(defun orgtrello-data/id-p (id)
  "Is the string a trello identifier?"
  (and id (not (string-match-p (format "^%s-" *ORGTRELLO/MARKER*) id))))

(defun orgtrello-data/merge-item (trello-item org-item)
  "Merge trello and org item together. If trello-item is null, return the org-item"
  (if trello-item
      (let ((org-item-to-merge (orgtrello-hash/init-map-from org-item))) ;; merge
        (orgtrello-data/put-entity-level *ORGTRELLO/ITEM-LEVEL*            org-item-to-merge)
        (orgtrello-data/put-entity-id    (orgtrello-data/entity-id trello-item)   org-item-to-merge)
        (orgtrello-data/put-entity-name  (orgtrello-data/entity-name trello-item) org-item-to-merge)
        (-> trello-item
          orgtrello-data/entity-checked
          orgtrello-data/--compute-state-item
          (orgtrello-data/put-entity-keyword org-item-to-merge)))
    org-item))

(defun orgtrello-data/--compute-state-item-checkbox (state)
  "Compute the status of the item checkbox"
  (orgtrello-data/--compute-state-generic state '("[X]" "[ ]")))

(defun orgtrello-data/--compute-state-item (state)
  "Compute the status of the checkbox"
  (orgtrello-data/--compute-state-generic state `(,*ORGTRELLO/DONE* ,*ORGTRELLO/TODO*)))

(defun orgtrello-data/--merge-checklist (trello-checklist org-checklist)
  "Merge trello and org checklist together. If trello-checklist is null, return org-checklist."
  (if trello-checklist
      (->> (orgtrello-hash/init-map-from org-checklist)
        (orgtrello-data/put-entity-level *ORGTRELLO/CHECKLIST-LEVEL*)
        (orgtrello-data/put-entity-name (orgtrello-data/entity-name trello-checklist))
        (orgtrello-data/put-entity-id (orgtrello-data/entity-id trello-checklist)))
    org-checklist))

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
  "Given a list of tags, return a joined string with : as separator"
  (when labels
    (-when-let (tags (s-join ":" labels))
      (concat ":" tags ":"))))

(defun orgtrello-data/--labels-hash-to-tags (labels)
  "Given a hash map with :labels entry, return a tag string joined by : separator."
  (when labels
    (orgtrello-data/--labels-to-tags (mapcar 'orgtrello-data/entity-color labels))))

(defun orgtrello-data/--from-tags-to-list (tags)
  "Given a : string separated string, return a list of non empty string."
  (->> tags
    (s-split ":")
    (--filter (not (string= "" it)))))

(defun orgtrello-data/--merge-labels-as-tags (trello-labels org-tags)
  "Given trello labels and org-tags, merge both of them"
  (if org-tags
      (let ((org-tags-as-list (orgtrello-data/--from-tags-to-list org-tags))
            (trello-tags-as-list (orgtrello-data/--from-tags-to-list trello-labels)))
        (orgtrello-data/--labels-to-tags (orgtrello-data/merge-2-lists-without-duplicates org-tags-as-list trello-tags-as-list)))
    trello-labels))

(defun orgtrello-data/--merge-card (trello-card org-card)
  "Merge trello and org card together. If trello-card is nil, return org-card."
  (if trello-card
      (->> (orgtrello-hash/init-map-from org-card)
        (orgtrello-data/put-entity-tags (orgtrello-data/--merge-labels-as-tags
                                         (orgtrello-data/--labels-hash-to-tags (orgtrello-data/entity-labels trello-card))
                                         (orgtrello-data/entity-tags org-card)))
        (orgtrello-data/put-entity-comments(orgtrello-data/entity-comments trello-card))
        (orgtrello-data/put-entity-level *ORGTRELLO/CARD-LEVEL*)
        (orgtrello-data/put-entity-id (orgtrello-data/entity-id trello-card))
        (orgtrello-data/put-entity-name (orgtrello-data/entity-name trello-card))
        (orgtrello-data/put-entity-keyword (-> trello-card
                                             orgtrello-data/entity-list-id
                                             orgtrello-data/--compute-card-status))
        (orgtrello-data/put-entity-member-ids (orgtrello-data/--merge-member-ids trello-card org-card))
        (orgtrello-data/put-entity-description (orgtrello-data/entity-description trello-card))
        (orgtrello-data/put-entity-due (orgtrello-data/entity-due trello-card)))
    org-card))

(defun orgtrello-data/--dispatch-merge-fn (entity)
  "Dispatch the function fn to merge the entity."
  (cond ((orgtrello-data/entity-card-p entity)      'orgtrello-data/--merge-card)
        ((orgtrello-data/entity-checklist-p entity) 'orgtrello-data/--merge-checklist)
        ((orgtrello-data/entity-item-p entity)      'orgtrello-data/merge-item)))

(defun orgtrello-data/merge-entities-trello-and-org (trello-data org-data)
  "Merge the org-entity entities inside the trello-entities."
  (let ((trello-entities  (car trello-data))
        (trello-adjacency (cadr trello-data))
        (org-entities     (car org-data))
        (org-adjacency    (cadr org-data)))

    (maphash (lambda (id trello-entity)
               (orgtrello-data/puthash-data id (funcall (orgtrello-data/--dispatch-merge-fn trello-entity) trello-entity (orgtrello-data/--get-entity id org-entities)) trello-entities) ;; updating entity to trello
               (orgtrello-data/puthash-data id (orgtrello-data/merge-2-lists-without-duplicates (gethash id trello-adjacency) (gethash id org-adjacency))     trello-adjacency)) ;; update entity adjacency to trello
             trello-entities)

    ;; copy the entities only present on org files to the trello entities.
    (maphash (lambda (id org-entity)
               (unless (gethash id trello-entities)
                 (orgtrello-data/puthash-data id org-entity trello-entities)
                 (orgtrello-data/puthash-data id (gethash id org-adjacency) trello-adjacency)))
             org-entities)

    (list trello-entities trello-adjacency)))

(defun orgtrello-data/--compute-card-status (card-id-list)
  "Given a card's id, compute its status."
  (gethash card-id-list *ORGTRELLO/HMAP-LIST-ORGKEYWORD-ID-NAME*))

(defun orgtrello-data/--get-entity (id entities-hash)
  "Update the card entry inside the hash."
  (gethash id entities-hash))

(defun orgtrello-data/--compute-state-generic (state list-state)
  "Generic computation of a list depending on the state. If state is \"complete\" or \"DONE\", then the first element of the list is returned, otherwise the second."
  (if (or (string= "complete" state)
          (string= *ORGTRELLO/DONE* state))
      (car list-state)
    (cadr list-state)))

(defun orgtrello-data/--users-from (string-users)
  "Compute the users name from the comma separated value in string."
  (when string-users (split-string string-users "," t)))

(defun orgtrello-data/--users-to (users)
  "Given a list of users, compute the comma separated string of users."
  (if users (mapconcat 'identity users ",") ""))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-data loaded!")


