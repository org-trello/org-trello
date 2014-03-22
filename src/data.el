(defvar *ORGTRELLO-ID* "orgtrello-id" "Key entry used for the trello identifier and the trello marker (the first sync).")

(defun orgtrello-data/merge-2-lists-without-duplicates (a-list b-list)
  "Merge 2 lists together (no duplicates)."
  (-> a-list
      (append b-list)
      (delete-dups)))

(defun orgtrello-data/--convert-orgmode-date-to-trello-date (orgmode-date)
  "Convert the org-mode deadline into a time adapted for trello."
  (if (and orgmode-date (not (string-match-p "T*Z" orgmode-date)))
      (cl-destructuring-bind (sec min hour day mon year dow dst tz)
                             (--map (if it (if (< it 10) (concat "0" (int-to-string it)) (int-to-string it)))
                                    (parse-time-string orgmode-date))
        (concat (concat year "-" mon "-" day "T") (if hour (concat hour ":" min ":" sec) "00:00:00") ".000Z"))
      orgmode-date))

(defun orgtrello-data/org-entity-metadata ()
  "Compute the metadata the org-mode way."
  (org-heading-components))

(defun orgtrello-data/--extract-metadata ()
  "Extract the current metadata depending on the org-trello's checklist policy."
  (funcall (if (orgtrello-cbx/checkbox-p) 'orgtrello-cbx/org-checkbox-metadata 'orgtrello-data/org-entity-metadata)))

(defun orgtrello-data/extract-identifier (point)
  "Extract the identifier from the point."
  (orgtrello-action/org-entry-get point *ORGTRELLO-ID*))

(defun orgtrello-action/set-property (key value)
  "Either set the propery normally (as for entities) or specifically for checklist."
  (funcall (if (orgtrello-cbx/checkbox-p) 'orgtrello-cbx/org-set-property 'org-set-property) key value))

(defun orgtrello-action/org-entry-get (point key)
  "Extract the identifier from the point."
  (funcall (if (orgtrello-cbx/checkbox-p) 'orgtrello-cbx/org-get-property 'org-entry-get) point key))

(defun orgtrello-data/metadata ()
  "Compute the metadata for a given org entry. Also add some metadata identifier/due-data/point/buffer-name/etc..."
  (let ((od/--point (point)))
    (->> (orgtrello-data/--extract-metadata)
         (cons (-> od/--point (orgtrello-action/org-entry-get "DEADLINE") orgtrello-data/--convert-orgmode-date-to-trello-date))
         (cons (orgtrello-data/extract-identifier od/--point))
         (cons od/--point)
         (cons (buffer-name))
         (cons (orgtrello-controller/--user-ids-assigned-to-current-card))
         (cons (orgtrello-buffer/extract-description-from-current-position!))
         orgtrello-data/--convert-to-orgtrello-metadata)))

(defun orgtrello-action/org-up-parent ()
  "A function to get back to the current entry's parent"
  (funcall (if (orgtrello-cbx/checkbox-p) 'orgtrello-cbx/org-up! 'org-up-heading-safe)))

(defun orgtrello-data/--parent-metadata ()
  "Extract the metadata from the current heading's parent."
  (save-excursion
    (orgtrello-action/org-up-parent)
    (orgtrello-data/metadata)))

(defun orgtrello-data/--grandparent-metadata ()
  "Extract the metadata from the current heading's grandparent."
  (save-excursion
    (orgtrello-action/org-up-parent)
    (orgtrello-action/org-up-parent)
    (orgtrello-data/metadata)))

(defun orgtrello-data/entry-get-full-metadata ()
  "Compute metadata needed for entry into a map with keys :current, :parent, :grandparent. Returns nil if the level is superior to 4."
  (let* ((current   (orgtrello-data/metadata))
         (level     (orgtrello-data/entity-level current)))
    (when (< level *OUTOFBOUNDS-LEVEL*)
          (let ((ancestors (cond ((= level *CARD-LEVEL*)      '(nil nil))
                                 ((= level *CHECKLIST-LEVEL*) `(,(orgtrello-data/--parent-metadata) nil))
                                 ((= level *ITEM-LEVEL*)      `(,(orgtrello-data/--parent-metadata) ,(orgtrello-data/--grandparent-metadata))))))
            (orgtrello-hash/make-hierarchy current (first ancestors) (second ancestors))))))

(defun orgtrello-data/--convert-to-orgtrello-metadata (heading-metadata)
  "Given the heading-metadata returned by the function 'org-heading-components, make it a hashmap with key :level, :keyword, :name. and their respective value"
  (cl-destructuring-bind (description member-ids buffer-name point id due level _ keyword _ name &rest) heading-metadata
                         (orgtrello-hash/make-hash-org member-ids level keyword name id due point buffer-name description)))

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
  (let ((id (orgtrello-data/entity-id-or-marker entity))) (when (orgtrello-controller/id-p id) id)))

(defun orgtrello-data/entity-keyword (entity &optional default-value)
  "Retrieve the keyword from the entity."
  (orgtrello-data/gethash-data :keyword entity default-value))

(defun orgtrello-data/entity-member-ids-as-list (entity)
  "Retrieve the users assigned to the entity."
  (-> entity
    orgtrello-data/entity-member-ids
    orgtrello-controller/--users-from))

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


(defun orgtrello-data/entity-method (query-map) (orgtrello-data/gethash-data :method query-map))
(defun orgtrello-data/entity-uri    (query-map) (orgtrello-data/gethash-data :uri    query-map))
(defun orgtrello-data/entity-sync   (query-map) (orgtrello-data/gethash-data :sync   query-map))
(defun orgtrello-data/entity-params (query-map) (orgtrello-data/gethash-data :params query-map))

(defun orgtrello-data/current     (entry-meta) (orgtrello-data/gethash-data :current     entry-meta))
(defun orgtrello-data/parent      (entry-meta) (orgtrello-data/gethash-data :parent      entry-meta))
(defun orgtrello-data/grandparent (entry-meta) (orgtrello-data/gethash-data :grandparent entry-meta))

(defun orgtrello-data/current-level () "Compute the current level's position." (-> (orgtrello-data/metadata) orgtrello-data/entity-level))

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
                                                                        (idMemberCreator . :user-creator-id))))

(defun orgtrello-data/--deal-with-key (key)
  "Given a key, return it as is if it's a keyword or return its mapped version from *ORGTRELLO-DATA-MAP-KEYWORDS*"
  (cond ((keywordp key) key)
        (t             (gethash key *ORGTRELLO-DATA-MAP-KEYWORDS*))))

(defun orgtrello-data/parse-data (entities)
  "Given a trello entity, convert into org-trello entity"
  (cond ((eq :json-false entities)                                           nil)
        ((--any? (funcall it entities) '(stringp symbolp numberp functionp)) entities)
        ((arrayp entities)                                                   (mapcar 'orgtrello-data/parse-data entities))
        (t                                                                   (let ((hmap (--reduce-from (let ((key (car it))
                                                                                                              (val (cdr it)))
                                                                                                          (-when-let (new-key (orgtrello-data/--deal-with-key key))
                                                                                                                     (puthash new-key (orgtrello-data/parse-data val) acc))
                                                                                                          acc)
                                                                                                        (orgtrello-hash/empty-hash)
                                                                                                        entities)))
                                                                               (-when-let (level (orgtrello-data/--compute-level hmap)) (puthash :level level hmap))
                                                                               hmap))))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-data loaded!")


