(require 'org-trello-log)
(require 'org-trello-setup)
(require 'org-trello-hash)
(require 'org-trello-action)

;; #################### orgtrello-data

(defvar *ORGTRELLO-ID* "orgtrello-id" "Key entry used for the trello identifier and the trello marker (the first sync).")

(defun orgtrello-data/merge-2-lists-without-duplicates (a-list b-list) "Merge 2 lists together (no duplicates)."
  (-> a-list
      (append b-list)
      (delete-dups)))

(defun orgtrello-data/--convert-orgmode-date-to-trello-date (orgmode-date) "Convert the org-mode deadline into a time adapted for trello."
  (if (and orgmode-date (not (string-match-p "T*Z" orgmode-date)))
      (cl-destructuring-bind (sec min hour day mon year dow dst tz)
                             (--map (if it (if (< it 10) (concat "0" (int-to-string it)) (int-to-string it)))
                                    (parse-time-string orgmode-date))
        (concat (concat year "-" mon "-" day "T") (if hour (concat hour ":" min ":" sec) "00:00:00") ".000Z"))
      orgmode-date))

(defun orgtrello-data/org-entity-metadata () "Compute the metadata the org-mode way."
  (org-heading-components))

(defun orgtrello-data/--extract-metadata () "Extract the current metadata depending on the org-trello's checklist policy."
  (if (orgtrello-cbx/checkbox-p)
      ;; checklist
      (orgtrello-cbx/org-checkbox-metadata)
      ;; as before, return the heading meta
      (orgtrello-data/org-entity-metadata)))

(defun orgtrello-data/extract-identifier (point) "Extract the identifier from the point."
  (orgtrello-action/org-entry-get point *ORGTRELLO-ID*))

 (defun orgtrello-action/set-property (key value) "Either set the propery normally (as for entities) or specifically for checklist."
  (funcall (if (orgtrello-cbx/checkbox-p) 'orgtrello-cbx/org-set-property 'org-set-property) key value))

(defun orgtrello-action/org-entry-get (point key) "Extract the identifier from the point."
  (funcall (if (orgtrello-cbx/checkbox-p) 'orgtrello-cbx/org-get-property 'org-entry-get) point key))

(defun orgtrello-data/metadata () "Compute the metadata for a given org entry. Also add some metadata identifier/due-data/point/buffer-name/etc..."
  (let ((od/--point (point)))
    (->> (orgtrello-data/--extract-metadata)
         (cons (-> od/--point (orgtrello-action/org-entry-get "DEADLINE") orgtrello-data/--convert-orgmode-date-to-trello-date))
         (cons (orgtrello-data/extract-identifier od/--point))
         (cons od/--point)
         (cons (buffer-name))
         (cons (orgtrello/--user-ids-assigned-to-current-card))
         orgtrello-data/--get-metadata)))

(defun orgtrello-action/org-up-parent () "A function to get back to the current entry's parent"
  (funcall (if (orgtrello-cbx/checkbox-p) 'orgtrello-cbx/org-up! 'org-up-heading-safe)))

(defun orgtrello-data/--parent-metadata () "Extract the metadata from the current heading's parent."
  (save-excursion
    (orgtrello-action/org-up-parent)
    (orgtrello-data/metadata)))

(defun orgtrello-data/--grandparent-metadata () "Extract the metadata from the current heading's grandparent."
  (save-excursion
    (orgtrello-action/org-up-parent)
    (orgtrello-action/org-up-parent)
    (orgtrello-data/metadata)))

(defun orgtrello-data/entry-get-full-metadata () "Compute metadata needed for entry into a map with keys :current, :parent, :grandparent. Returns nil if the level is superior to 4."
  (let* ((current   (orgtrello-data/metadata))
         (level     (orgtrello-data/entity-level current)))
    (when (< level *OUTOFBOUNDS-LEVEL*)
          (let ((ancestors (cond ((= level *CARD-LEVEL*)      '(nil nil))
                                 ((= level *CHECKLIST-LEVEL*) `(,(orgtrello-data/--parent-metadata) nil))
                                 ((= level *ITEM-LEVEL*)      `(,(orgtrello-data/--parent-metadata) ,(orgtrello-data/--grandparent-metadata))))))
            (orgtrello-hash/make-hierarchy current (first ancestors) (second ancestors))))))

(defun orgtrello-data/--get-metadata (heading-metadata) "Given the heading-metadata returned by the function 'org-heading-components, make it a hashmap with key :level, :keyword, :name. and their respective value"
  (cl-destructuring-bind (users-assigned buffer-name point id due level _ keyword _ name &rest) heading-metadata
                         (orgtrello-hash/make-hash-org users-assigned level keyword name id due point buffer-name)))

(defun orgtrello-data/--compute-fn (entity list-dispatch-fn) "Given an entity, compute the result" (funcall (if (hash-table-p entity) (first list-dispatch-fn) (second list-dispatch-fn)) entity))

(defun orgtrello-data/entity-id (entity) "Dispatch to the rightfull function to get the id" (let ((id (orgtrello-data/entity-marker entity))) (when (orgtrello/id-p id) id)))
(defun orgtrello-data/entity-id-or-marker (entity) "Dispatch to the rightful function to get the id" (orgtrello-data/entity-marker entity))

(defun orgtrello/--entity-with-level-p (entity level) "Is the entity with level level?" (-> entity orgtrello-data/entity-level (eq level)))
(defun orgtrello-data/entity-card-p (entity) "Is this a card?"           (orgtrello/--entity-with-level-p entity *CARD-LEVEL*))
(defun orgtrello-data/entity-checklist-p (entity) "Is this a checklist?" (orgtrello/--entity-with-level-p entity *CHECKLIST-LEVEL*))
(defun orgtrello-data/entity-item-p (entity) "Is this an item?"          (orgtrello/--entity-with-level-p entity *ITEM-LEVEL*))

(defun orgtrello-data/gethash-data (key query-map &optional default-value) "Retrieve the data from some query-map" (when query-map (gethash key query-map default-value)))

(defun orgtrello-data/entity-name (entity) "Retrieve the entity name"     (orgtrello-data/gethash-data :name entity))
(defun orgtrello-data/entity-action (entity) "Retrieve the entity name"     (orgtrello-data/gethash-data :action entity))
(defun orgtrello-data/entity-due (entity) "Retrieve the entity due date"  (orgtrello-data/gethash-data :due entity))
(defun orgtrello-data/entity-state (entity) "Retrieve the entity status"  (orgtrello-data/entity-keyword entity))
(defun orgtrello-data/entity-board-id (entity) "Extract the board identitier of the entity from the entity" (orgtrello-data/gethash-data :board-id entity))
(defun orgtrello-data/entity-card-id (entity) "Extract the card identitier of the entity from the entity" (orgtrello-data/gethash-data :card-id entity))
(defun orgtrello-data/entity-list-id (entity) "Extract the list identitier of the entity from the entity" (orgtrello-data/gethash-data :list-id entity))
(defun orgtrello-data/entity-member-ids (entity) "Extract the member ids of the entity" (orgtrello-data/gethash-data :users-assigned entity))
(defun orgtrello-data/entity-checklists (entity) "Extract the checklists params" (orgtrello-data/gethash-data :checklists entity))
(defun orgtrello-data/entity-items (entity) "Extract the checklists params" (orgtrello-data/gethash-data :items entity))
(defun orgtrello-data/entity-position (entity) "Extract the position params" (orgtrello-data/gethash-data :position entity))
(defun orgtrello-data/entity-buffername (entity) "Extract the buffername params" (orgtrello-data/gethash-data :buffername entity))
(defun orgtrello-data/entity-memberids (entity) "Extract the users-assigned/members ids from the entity" (orgtrello-data/gethash-data :users-assigned entity))
(defun orgtrello-data/entity-checked (entity) "Extract the checked param from the entity" (orgtrello-data/gethash-data :checked entity))
(defun orgtrello-data/entity-due (entity) "Extract the checked param from the entity" (orgtrello-data/gethash-data :due entity))
(defun orgtrello-data/entity-keyword (entity &optional default-value) "Retrieve the keyword from the entity." (orgtrello-data/gethash-data :keyword entity default-value))
(defun orgtrello-data/entity-marker (entity) "Retrieve the marker from the entity (id must be a trello id, otherwise, it's the marker)." (orgtrello-data/gethash-data :id entity))
(defun orgtrello-data/entity-level (entity) "Retrieve the level from the entity." (orgtrello-data/gethash-data :level entity))
(defun orgtrello-data/entity-closed (entity) "Retrieve the level from the entity." (orgtrello-data/gethash-data :closed entity))
(defun orgtrello-data/entity-callback (entity) "Retrieve the level from the entity." (orgtrello-data/gethash-data :callback entity))
(defun orgtrello-data/entity-start (entity) "Retrieve the level from the entity." (orgtrello-data/gethash-data :start entity))
(defun orgtrello-data/entity-member-ids-as-list (entity) "Retrieve the users assigned to the entity." (-> entity
                                                                                                               orgtrello-data/entity-member-ids
                                                                                                               orgtrello/--users-from))

(defun orgtrello-data/entity-method (query-map) "Retrieve the http method"    (orgtrello-data/gethash-data :method query-map))
(defun orgtrello-data/entity-uri    (query-map) "Retrieve the http uri"       (orgtrello-data/gethash-data :uri query-map))
(defun orgtrello-data/entity-sync   (query-map) "Retrieve the http sync flag" (orgtrello-data/gethash-data :sync query-map))
(defun orgtrello-data/entity-params (query-map) "Retrieve the http params"    (orgtrello-data/gethash-data :params query-map))

(defun orgtrello-data/current (entry-meta) "Given an entry-meta, return the current entry"         (orgtrello-data/gethash-data :current entry-meta))
(defun orgtrello-data/parent (entry-meta) "Given an entry-meta, return the current entry"          (orgtrello-data/gethash-data :parent entry-meta))
(defun orgtrello-data/grandparent (entry-meta) "Given an entry-meta, return the grandparent entry" (orgtrello-data/gethash-data :grandparent entry-meta))

(defun orgtrello-data/current-level () "Compute the current level's position."
  (-> (orgtrello-data/metadata) orgtrello-data/entity-level))

(defun orgtrello-data/--deal-with-value (values) "Deal with possible values "
  (cond ((stringp values)                                     values)
        ((arrayp values)                                      (mapcar (lambda (e) e) values))
        ((and (listp values) (not (eq 'lambda (car values)))) (mapcar 'orgtrello-data/from-trello values))
        ((eq :json-false values)                              nil)
        (t                                                    values)))

(defun orgtrello-data/--compute-level (entity-map) "Given a map, compute the entity level"
  (cond ((gethash :list-id entity-map) *CARD-LEVEL*)
        ((gethash :card-id entity-map) *CHECKLIST-LEVEL*)
        ((gethash :checked entity-map) *ITEM-LEVEL*)
        (t nil)))

(defvar *ORGTRELLO-DATA-MAP-KEYWORDS* (orgtrello-hash/make-properties `((url . :url)
                                                                        (id . :id)
                                                                        (name . :name)
                                                                        (idMembers . :users-assigned)
                                                                        (idList . :list-id)
                                                                        (idChecklists . :checklists)
                                                                        (idBoard . :board-id)
                                                                        (due . :due)
                                                                        (desc . :description)
                                                                        (closed . :closed)
                                                                        (idCard . :card-id)
                                                                        (checkItems . :items)
                                                                        (state . :checked)
                                                                        (status . :status)
                                                                        (buffername . :buffername)
                                                                        (sync . :sync)
                                                                        (uri . :uri)
                                                                        (method . :method)
                                                                        (params . :params)
                                                                        (action . :action)
                                                                        (start . :start)
                                                                        (callback . :callback)
                                                                        (pos . :position)
                                                                        (position . :position)
                                                                        (keyword . :keyword)
                                                                        (:keyword . :keyword)
                                                                        (start . :start)
                                                                        (level . :level)
                                                                        (users-assigned . :users-assigned))))

(defun orgtrello-data/from-trello (entity-alist) "Given a trello entity, convert into org-trello entity"
  (cond ((arrayp entity-alist) (mapcar 'orgtrello-data/from-trello entity-alist))
        (t                     (let ((hmap (--reduce-from (let ((key (car it))
                                                                (val (cdr it)))
                                                            (-when-let (new-key (gethash key *ORGTRELLO-DATA-MAP-KEYWORDS*))
                                                                       (puthash new-key (orgtrello-data/--deal-with-value val) acc))
                                                            acc)
                                                          (make-hash-table :test 'equal)
                                                          entity-alist)))
                                 (-when-let (level (orgtrello-data/--compute-level hmap))
                                            (puthash :level level hmap))
                                 hmap))))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-data loaded!")

(provide 'org-trello-data)


