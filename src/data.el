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
         (level     (orgtrello/--level current)))
    (when (< level *OUTOFBOUNDS-LEVEL*)
          (let ((ancestors (cond ((= level *CARD-LEVEL*)      '(nil nil))
                                 ((= level *CHECKLIST-LEVEL*) `(,(orgtrello-data/--parent-metadata) nil))
                                 ((= level *ITEM-LEVEL*)      `(,(orgtrello-data/--parent-metadata) ,(orgtrello-data/--grandparent-metadata))))))
            (orgtrello-hash/make-hierarchy current (first ancestors) (second ancestors))))))

(defun orgtrello-data/--get-metadata (heading-metadata) "Given the heading-metadata returned by the function 'org-heading-components, make it a hashmap with key :level, :keyword, :name. and their respective value"
  (cl-destructuring-bind (users-assigned buffer-name point id due level _ keyword _ name &rest) heading-metadata
                         (orgtrello-hash/make-hash-org users-assigned level keyword name id due point buffer-name)))

(defun orgtrello-data/--compute-fn (entity list-dispatch-fn) "Given an entity, compute the result" (funcall (if (hash-table-p entity) (first list-dispatch-fn) (second list-dispatch-fn)) entity))

(defun orgtrello-data/entity-id (entity) "Dispatch to the rightfull function to get the id" (orgtrello-data/--compute-fn entity '(orgtrello/--id orgtrello-data/id)))
(defun orgtrello-data/entity-id-or-marker (entity) "Dispatch to the rightful function to get the id" (orgtrello-data/--compute-fn entity '(orgtrello/--id-or-marker orgtrello-data/id)))

(defun orgtrello-data/entity-card-p (entity) "Is an entity a card?"           (orgtrello-data/--compute-fn entity '(orgtrello/--hcard-p orgtrello/--card-p)))
(defun orgtrello-data/entity-checklist-p (entity) "Is an entity a checklist?" (orgtrello-data/--compute-fn entity '(orgtrello/--hchecklist-p orgtrello/--checklist-p)))
(defun orgtrello-data/entity-item-p (entity) "Is an entity an item?"          (orgtrello-data/--compute-fn entity '(orgtrello/--hitem-p orgtrello/--item-p)))

(defun orgtrello-data/entity-name (entity) "Retrieve the entity name"     (orgtrello-data/--compute-fn entity '(orgtrello/--name orgtrello-data/name)))
(defun orgtrello-data/entity-due (entity) "Retrieve the entity due date"  (orgtrello-data/--compute-fn entity '(orgtrello/--due orgtrello-data/due)))
(defun orgtrello-data/entity-state (entity) "Retrieve the entity status"  (orgtrello-data/--compute-fn entity '(orgtrello/--keyword orgtrello-data/state)))
(defun orgtrello-data/entity-method (entity) "Retrieve the entity method" (orgtrello-data/--compute-fn entity '(orgtrello-data/method orgtrello-data/method-)))
(defun orgtrello-data/entity-uri (entity) "Retrieve the entity uri"       (orgtrello-data/--compute-fn entity '(orgtrello-data/uri orgtrello-data/uri-)))
(defun orgtrello-data/entity-params (entity) "Retrieve the entity params" (orgtrello-data/--compute-fn entity '(orgtrello-data/params orgtrello-data/params-)))
(defun orgtrello-data/entity-list-id (entity) "Extract the list identitier of the entity from the entity" (orgtrello-data/--compute-fn entity '(orgtrello-data/entity-listid orgtrello-data/list-id)))
(defun orgtrello-data/entity-member-ids (entity) "Extract the member ids of the entity" (orgtrello-data/--compute-fn entity '(orgtrello-data/entity-memberids orgtrello-data/member-ids)))
(defun orgtrello-data/entity-checklists (entity) "Extract the checklists params" (orgtrello-data/gethash-data :checklists entity))
(defun orgtrello-data/entity-items (entity) "Extract the checklists params" (orgtrello-data/gethash-data :items entity))
(defun orgtrello-data/entity-position (entity) "Extract the position params" (orgtrello-data/gethash-data :position entity))
(defun orgtrello-data/entity-buffername (entity) "Extract the buffername params" (orgtrello-data/gethash-data :buffername entity))
(defun orgtrello-data/entity-listid (entity) "Extract the list id entry"    (orgtrello-data/gethash-data :list-id entity))
(defun orgtrello-data/entity-memberids (entity) "Extract the users-assigned/members ids from the entity" (orgtrello-data/gethash-data :users-assigned entity))
(defun orgtrello-data/entity-checked (entity) "Extract the checked param from the entity" (orgtrello-data/gethash-data :checked entity))

(defun orgtrello/--card-p (entity) "Is this a card?"           (orgtrello-data/list-id entity))
(defun orgtrello/--checklist-p (entity) "Is this a checklist?" (orgtrello-data/card-id entity))
(defun orgtrello/--item-p (entity) "is this an item?"          (orgtrello-data/state entity))

(defun orgtrello/--entity-with-level-p (entity level) "Is the entity with level level?" (-> entity orgtrello/--level (= level)))
(defun orgtrello/--hcard-p (entity) "Is this a card?"           (orgtrello/--entity-with-level-p entity *CARD-LEVEL*))
(defun orgtrello/--hchecklist-p (entity) "Is this a checklist?" (orgtrello/--entity-with-level-p entity *CHECKLIST-LEVEL*))
(defun orgtrello/--hitem-p (entity) "Is this an item?"          (orgtrello/--entity-with-level-p entity *ITEM-LEVEL*))

;; macro? defmethod?

(defun orgtrello-data/gethash-data (key query-map &optional default-value) "Retrieve the data from some query-map" (gethash key query-map default-value))
(defun orgtrello-data/method (query-map) "Retrieve the http method"    (orgtrello-data/gethash-data :method query-map))
(defun orgtrello-data/uri    (query-map) "Retrieve the http uri"       (orgtrello-data/gethash-data :uri query-map))
(defun orgtrello-data/sync   (query-map) "Retrieve the http sync flag" (orgtrello-data/gethash-data :sync query-map))
(defun orgtrello-data/params (query-map) "Retrieve the http params"    (orgtrello-data/gethash-data :params query-map))

(defun orgtrello-data/current (entry-meta) "Given an entry-meta, return the current entry"         (orgtrello-data/gethash-data :current entry-meta))
(defun orgtrello-data/parent (entry-meta) "Given an entry-meta, return the current entry"          (orgtrello-data/gethash-data :parent entry-meta))
(defun orgtrello-data/grandparent (entry-meta) "Given an entry-meta, return the grandparent entry" (orgtrello-data/gethash-data :grandparent entry-meta))

(defun orgtrello/--keyword (entity-meta &optional default-value) "Retrieve the keyword from the entity."                    (orgtrello-data/gethash-data :keyword entity-meta default-value))
(defun orgtrello/--name (entity-meta) "Retrieve the name from the entity."                                                  (orgtrello-data/gethash-data :name entity-meta (orgtrello-data/gethash-data 'name entity-meta))) ;; hack
(defun orgtrello/--marker (entity-meta) "Retrieve the marker from the entity (id must be a trello id, otherwise, it's the marker)." (orgtrello-data/gethash-data :id entity-meta))
(defun orgtrello/--id (entity-meta) "Retrieve the id from the entity (id must be a trello id, otherwise, it's the marker)." (let ((id (orgtrello/--marker entity-meta))) (when (orgtrello/id-p id) id)))

(defun orgtrello/--id-or-marker (entity-meta) "Retrieve the id or the marker property." (orgtrello/--marker entity-meta))

(defun orgtrello/--level (entity-meta) "Retrieve the level from the entity."                                                (orgtrello-data/gethash-data :level entity-meta))
(defun orgtrello/--due (entity-meta) "Retrieve the due date from the entity."                                               (orgtrello-data/gethash-data :due entity-meta))
(defun orgtrello/--user-assigned-ids (entity-meta) "Retrieve the users assigned to the entity."                             (orgtrello-data/gethash-data :users-assigned entity-meta))
(defun orgtrello/--user-assigned-ids-as-list (entity-meta) "Retrieve the users assigned to the entity."                     (-> entity-meta
                                                                                                                                orgtrello/--user-assigned-ids
                                                                                                                                orgtrello/--users-from))
(defun orgtrello/--buffername (entity-meta) "Retrieve the point from the entity."                                           (orgtrello-data/gethash-data :buffername entity-meta))
(defun orgtrello/--position (entity-meta) "Retrieve the point from the entity."                                             (orgtrello-data/gethash-data :position entity-meta))
(defun orgtrello-data/hlist-id (entity-meta)                                                                                (orgtrello-data/gethash-data 'idList entity-meta))
(defun orgtrello-data/hmember-ids (entity-meta)                                                                             (--> 'idMembers
                                                                                                                                 (orgtrello-data/gethash-data it entity-meta)
                                                                                                                                 (-map (lambda (i) i) it)))

(defun orgtrello-data/retrieve-data  (symbol entity-data) "Own generic accessor"                                    (assoc-default symbol entity-data))
(defun orgtrello-data/buffername     (entity-data) "Extract the buffername of the entity from the entity-data"      (orgtrello-data/retrieve-data 'buffername entity-data))
(defun orgtrello-data/position       (entity-data) "Extract the position of the entity from the entity-data"        (orgtrello-data/retrieve-data 'position entity-data))
(defun orgtrello-data/id             (entity-data) "Extract the id of the entity from the entity"                   (orgtrello-data/retrieve-data 'id entity-data))
(defun orgtrello-data/name           (entity-data) "Extract the name of the entity from the entity"                 (orgtrello-data/retrieve-data 'name entity-data))
(defun orgtrello-data/list-id        (entity-data) "Extract the list identitier of the entity from the entity"      (orgtrello-data/retrieve-data 'idList entity-data))
(defun orgtrello-data/checklist-ids  (entity-data) "Extract the checklist identifier of the entity from the entity" (orgtrello-data/retrieve-data 'idChecklists entity-data))
(defun orgtrello-data/check-items    (entity-data) "Extract the checklist identifier of the entity from the entity" (orgtrello-data/retrieve-data 'checkItems entity-data))
(defun orgtrello-data/card-id        (entity-data) "Extract the card identifier of the entity from the entity"      (orgtrello-data/retrieve-data 'idCard entity-data))
(defun orgtrello-data/due            (entity-data) "Extract the due date of the entity from the query response"     (orgtrello-data/retrieve-data 'due entity-data))
(defun orgtrello-data/state          (entity-data) "Extract the state of the entity"                                (orgtrello-data/retrieve-data 'state entity-data))
(defun orgtrello-data/close-property (entity-data) "Extract the closed property of the entity"                      (orgtrello-data/retrieve-data 'closed entity-data))
(defun orgtrello-data/callback       (entity-data) "Extract the callback property of the entity"                    (orgtrello-data/retrieve-data 'callback entity-data))
(defun orgtrello-data/level          (entity-data) "Extract the callback property of the entity"                    (orgtrello-data/retrieve-data 'level entity-data))
(defun orgtrello-data/start          (entity-data) "Extract the start property of the entity"                       (orgtrello-data/retrieve-data 'start entity-data))
(defun orgtrello-data/action         (entity-data) "Extract the action property of the entity"                      (orgtrello-data/retrieve-data 'action entity-data))
(defun orgtrello-data/member-ids     (entity-data) "Extract the member ids of the entity"                           (--> 'idMembers
                                                                                                                         (orgtrello-data/retrieve-data it entity-data)
                                                                                                                        (-map (lambda (i) i) it)))

(defun orgtrello-data/sync-          (entity-data) "Extract the sync property of the entity"                        (orgtrello-data/retrieve-data 'sync entity-data))
(defun orgtrello-data/method-        (entity-data) "Extract the method property of the entity"                      (orgtrello-data/retrieve-data 'method entity-data))
(defun orgtrello-data/uri-           (entity-data) "Extract the uri property of the entity"                         (orgtrello-data/retrieve-data 'uri entity-data))
(defun orgtrello-data/params-        (entity-data) "Extract the params property of the entity"                      (orgtrello-data/retrieve-data 'params entity-data))

(defun orgtrello/--current-level () "Compute the current level's position."
  (-> (orgtrello-data/metadata) orgtrello/--level))

(defun orgtrello-data/--deal-with-value (values) "Deal with possible values "
  (cond ((stringp values)        values)
        ((arrayp values)         (mapcar (lambda (e) e) values))
        ((eq :json-false values) nil)
        (t                       values)))

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
                                                                        (position . :position)
                                                                        (callback . :callback)
                                                                        (pos . :position)
                                                                        (keyword . :keyword)
                                                                        (:keyword . :keyword))))

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


