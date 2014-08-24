;;; org-trello-proxy.el --- Proxy namespace (in charge of receiving actions to transmit to the consumer).
;;; Commentary:
;;; Code:

(require 'org-trello-log)
(require 'org-trello-setup)
(require 'org-trello-query)
(require 'org-trello-api)
(require 'org-trello-data)
(require 'org-trello-hash)
(require 'org-trello-buffer)
(require 'org-trello-cbx)
(require 'org-trello-action)

(defun orgtrello-proxy/--compute-trello-query (query-map-data)
  "Build a trello query from the content of QUERY-MAP-DATA."
  (orgtrello-api/make-query (orgtrello-data/entity-method query-map-data) (orgtrello-data/entity-uri query-map-data) (orgtrello-data/entity-params query-map-data)))

(defun orgtrello-proxy/sync-from (query-map-data &optional sync)
  "Deal with request QUERY-MAP-DATA from trello.
The query can be synchronous depending on SYNC variable."
  (orgtrello-log/msg *OT/TRACE* "Proxy - Request received. Transmitting...")
  (let* ((position (orgtrello-data/entity-position query-map-data))
         (buffer-name (orgtrello-data/entity-buffername query-map-data))
         (standard-callback-fn (orgtrello-data/entity-callback query-map-data))
         (query-map (orgtrello-proxy/--compute-trello-query query-map-data))
         (name (orgtrello-data/entity-name query-map-data)))
    (orgtrello-query/http-trello query-map sync (when standard-callback-fn (funcall standard-callback-fn buffer-name position name)))))

(defun orgtrello-proxy/--getting-back-to-headline (data)
  "Trying another approach to getting back to header computing the normal form of the entry DATA in the buffer."
  (orgtrello-proxy/--getting-back-to-marker (orgtrello-buffer/--compute-entity-to-org-entry data)))

(defun orgtrello-proxy/--compute-pattern-search-from-marker (marker)
  "Given a MARKER, compute the pattern to look for in the file."
  marker)

(defun orgtrello-proxy/--getting-back-to-marker (marker)
  "Given a MARKER, getting back to marker function.
Move the cursor position."
  (goto-char (point-min))
  (re-search-forward (orgtrello-proxy/--compute-pattern-search-from-marker marker) nil t))

(defun orgtrello-proxy/--get-back-to-marker (marker data)
  "Getting back to MARKER if possible, otherwise return to the DATA headline.
Move the cursor position."
  (-if-let (goto-ok (orgtrello-proxy/--getting-back-to-marker marker))
      goto-ok
    (orgtrello-proxy/--getting-back-to-headline data)))

(defun orgtrello-proxy/update-entities-adjacencies! (old-entity-id entity-synced entities-adjacencies)
  "Given OLD-ENTITY-ID and NEW-ENTITY, update ENTITIES-ADJACENCIES structure.
This will remove the OLD-ENTITY-ID and update with the NEW-ENTITY (after sync).
This will also update the references in arborescence."
  (let ((entities     (car entities-adjacencies))
        (adjacencies  (car (cdr entities-adjacencies)))
        (entry-new-id (orgtrello-data/entity-id entity-synced))
        (children-ids (gethash old-entity adjacencies)))
    ;; update new entries
    (puthash entry-new-id entity-synced entities)
    ;; update children that have now the reference in children
    (puthash entry-new-id children-ids adjacencies)
    ;; update the reference children with the parent
    (mapcar (lambda (child-id)
              (let ((child (gethash child-id entities)))
                (--> child
                  (orgtrello-data/put-parent entity-synced it)
                  (puthash child-id it entities))))
            children-ids)
    ;; remove old entries
    (puthash old-entity-id nil entities)
    (puthash old-entity-id nil adjacencies)))

(defun orgtrello-proxy/--standard-post-or-put-success-callback (entity-to-sync entities-adjacencies)
  "Return a callback fn able to deal with the update of ENTITY-TO-SYNC.
This will update the buffer at the entity synced.
This will also update in place the ENTITIES-ADJACENCIES map lists with new entity information."
  (lexical-let ((entry-position    (orgtrello-data/entity-position entity-to-sync))
                (entry-buffer-name (orgtrello-data/entity-buffername entity-to-sync))
                (level             (orgtrello-data/entity-level entity-to-sync))
                (marker-id         (orgtrello-data/entity-id-or-marker entity-to-sync))
                (entity-name       (orgtrello-data/entity-name entity-to-sync))
                (entities-adj      entities-adjacencies))
    (lambda (response)
      (let* ((entity-synced (request-response-data response))
             (entry-new-id  (orgtrello-data/entity-id entity-synced)))
        (with-current-buffer entry-buffer-name
          ;; will update via tag the trello id of the new persisted entity-synced (if needed)
          (save-excursion
            ;; get back to the buffer and update the id if need be
            (-when-let (str-msg (when (orgtrello-proxy/--get-back-to-marker marker-id entity-synced)
                                  (-if-let (entry-id (when (orgtrello-data/id-p marker-id) marker-id)) ;; Already present, we do nothing on the buffer
                                      (format "Entity '%s' with id '%s' synced!" entity-name entry-id)
                                    (let ((entry-name (orgtrello-data/entity-name entity-synced))) ;; not present, this was just created, we add a simple property
                                      ;; update the buffer with new id
                                      (orgtrello-buffer/set-property *ORGTRELLO/ID* entry-new-id)
                                      ;; update in place the entities-adjacencies
            ;;                          (orgtrello-proxy/update-entities-adjacencies! marker-id entity-synced entities-adj)
                                      (format "Newly entity '%s' with id '%s' synced!" entry-name entry-new-id)))))
              (orgtrello-log/msg *OT/INFO* str-msg))))))))

(defun orgtrello-proxy/--dispatch-action (action)
  "Compute the action function depending on the ACTION (sync, delete) to execute."
  (cond ((string= *ORGTRELLO/ACTION-DELETE* action) 'orgtrello-proxy/--delete)
        ((string= *ORGTRELLO/ACTION-SYNC*   action) 'orgtrello-proxy/--sync-entity)))

(defun orgtrello-proxy/--cleanup-meta (entity)
  "Clean the ENTITY metadata up."
  (unless (orgtrello-data/entity-id entity)
    (orgtrello-cbx/org-delete-property *ORGTRELLO/ID*)))

(defun orgtrello-proxy/--retrieve-state-of-card (card-meta)
  "Given a CARD-META, retrieve its state depending on its :keyword metadata.
If empty or no keyword then, its equivalence is *ORGTRELLO/TODO*, otherwise, return its current state."
  (-if-let (card-kwd (orgtrello-data/entity-keyword card-meta *ORGTRELLO/TODO*)) card-kwd *ORGTRELLO/TODO*))

(defun orgtrello-proxy/--checks-before-sync-card (card-meta)
  "Given the CARD-META, check is done before synchronizing the cards."
  (if (orgtrello-data/entity-name card-meta) :ok *ORGTRELLO/ERROR-SYNC-CARD-MISSING-NAME*))

(defun orgtrello-proxy/--tags-to-labels (tags)
  "Transform org TAGS string to csv labels."
  (when tags
    (let* ((s (s-split ":" tags))
           (ns (if (string= "" (car s)) (cdr s) s)))
      (s-join "," ns))))

(defun orgtrello-proxy/--card (card-meta)
  "Deal with create/update CARD-META query build.
If the checks are ko, the error message is returned."
  (let ((checks-ok-or-error-message (orgtrello-proxy/--checks-before-sync-card card-meta)))
    ;; name is mandatory
    (if (equal :ok checks-ok-or-error-message)
        ;; parent and grandparent are useless here
        (let* ((card-kwd                (orgtrello-proxy/--retrieve-state-of-card card-meta))
               (list-id                 (orgtrello-buffer/org-file-get-property! card-kwd))
               (card-id                 (orgtrello-data/entity-id          card-meta))
               (card-name               (orgtrello-data/entity-name        card-meta))
               (card-due                (orgtrello-data/entity-due         card-meta))
               (card-desc               (orgtrello-data/entity-description card-meta))
               (card-user-ids-assigned  (orgtrello-data/entity-member-ids  card-meta))
               (card-labels             (orgtrello-proxy/--tags-to-labels (orgtrello-data/entity-tags card-meta)))
               (card-pos                (orgtrello-data/entity-position   (orgtrello-data/entity-tags card-meta))))
          (if card-id
              ;; update
              (orgtrello-api/move-card card-id list-id card-name card-due card-user-ids-assigned card-desc card-labels card-pos)
            ;; create
            (orgtrello-api/add-card card-name list-id card-due card-user-ids-assigned card-desc card-labels card-pos)))
      checks-ok-or-error-message)))

(defun orgtrello-proxy/--checks-before-sync-checklist (checklist-meta card-meta)
  "Check all is good before synchronizing the CHECKLIST-META (CARD-META indispensable)."
  (-if-let (checklist-name (orgtrello-data/entity-name checklist-meta))
      (-if-let (card-id (orgtrello-data/entity-id card-meta))
          :ok
        *ORGTRELLO/ERROR-SYNC-CHECKLIST-SYNC-CARD-FIRST*)
    *ORGTRELLO/ERROR-SYNC-CHECKLIST-MISSING-NAME*))

(defun orgtrello-proxy/--checklist (checklist-meta)
  "Deal with create/update CHECKLIST-META query build.
If the checks are ko, the error message is returned."
  (let* ((card-meta                  (orgtrello-data/parent checklist-meta))
         (checks-ok-or-error-message (orgtrello-proxy/--checks-before-sync-checklist checklist-meta card-meta)))
    (if (equal :ok checks-ok-or-error-message)
        (let ((card-id        (orgtrello-data/entity-id card-meta))
              (checklist-name (orgtrello-data/entity-name checklist-meta))
              (checklist-pos  (orgtrello-data/entity-position checklist-meta)))
          (-if-let (checklist-id (orgtrello-data/entity-id checklist-meta))
              ;; update
              (orgtrello-api/update-checklist checklist-id checklist-name checklist-pos)
            ;; create
            (orgtrello-api/add-checklist card-id checklist-name checklist-pos)))
      checks-ok-or-error-message)))

(defun orgtrello-proxy/--compute-state (state)
  "Given a STATE (TODO/DONE) compute the trello state equivalent."
  (orgtrello-data/--compute-state-generic state '("complete" "incomplete")))

(defun orgtrello-proxy/--compute-check (state)
  "Given a STATE (TODO/DONE) compute the trello check equivalent."
  (orgtrello-data/--compute-state-generic state '(t nil)))

(defun orgtrello-proxy/--checks-before-sync-item (item-meta checklist-meta card-meta)
  "Check all is good before synchronizing the item ITEM-META (CHECKLIST-META and CARD-META indispensable)."
  (let ((item-name    (orgtrello-data/entity-name item-meta))
        (checklist-id (orgtrello-data/entity-id checklist-meta))
        (card-id      (orgtrello-data/entity-id card-meta)))
    (if item-name
        (if checklist-id
            (if card-id :ok *ORGTRELLO/ERROR-SYNC-ITEM-SYNC-CARD-FIRST*)
          *ORGTRELLO/ERROR-SYNC-ITEM-SYNC-CHECKLIST-FIRST*)
      *ORGTRELLO/ERROR-SYNC-ITEM-MISSING-NAME*)))

(defun orgtrello-proxy/--item (item-meta)
  "Deal with create/update ITEM-META query build.
If the checks are ko, the error message is returned."
  (let* ((checklist-meta (orgtrello-data/parent item-meta))
         (card-meta      (orgtrello-data/parent checklist-meta))
         (checks-ok-or-error-message (orgtrello-proxy/--checks-before-sync-item item-meta checklist-meta card-meta)))
    ;; name is mandatory
    (if (equal :ok checks-ok-or-error-message)
        (let* ((item-id         (orgtrello-data/entity-id item-meta))
               (checklist-id    (orgtrello-data/entity-id checklist-meta))
               (card-id         (orgtrello-data/entity-id card-meta))
               (item-name       (orgtrello-data/entity-name item-meta))
               (item-state      (orgtrello-data/entity-keyword item-meta))
               (item-pos        (orgtrello-data/entity-position item-meta)))
          ;; update/create items
          (if item-id
              ;; update - rename, check or uncheck the item
              (orgtrello-api/update-item card-id
                                         checklist-id
                                         item-id
                                         item-name
                                         (orgtrello-proxy/--compute-state item-state)
                                         item-pos)
            ;; create
            (orgtrello-api/add-items checklist-id
                                     item-name
                                     (orgtrello-proxy/--compute-check item-state)
                                     item-pos)))
      checks-ok-or-error-message)))

(defun orgtrello-proxy/compute-dispatch-fn (map-dispatch-fn entity)
  "Generic function to dispatch, depending on the ENTITY level, functions.
MAP-DISPATCH-FN is a map of function taking the one parameter ENTITY."
  (-> entity
    orgtrello-data/entity-level
    (gethash map-dispatch-fn 'orgtrello-action/--too-deep-level)
    (funcall entity)))

(defvar *MAP-DISPATCH-CREATE-UPDATE* (orgtrello-hash/make-properties `((,*ORGTRELLO/CARD-LEVEL*      . orgtrello-proxy/--card)
                                                                       (,*ORGTRELLO/CHECKLIST-LEVEL* . orgtrello-proxy/--checklist)
                                                                       (,*ORGTRELLO/ITEM-LEVEL*      . orgtrello-proxy/--item)))
  "Dispatch map for the creation/update of card/checklist/item.")

(defun orgtrello-proxy/--compute-sync-query-request (entity)
  "Dispatch the ENTITY creation/update depending on the nature of the entry."
  (orgtrello-proxy/compute-dispatch-fn entity *MAP-DISPATCH-CREATE-UPDATE*))

(defun orgtrello-proxy/--sync-entity (entity-data entities-adjacencies)
  "Compute the sync action on entity ENTITY-DATA.
Use ENTITY-FULL-METADATA and ENTITIES-ADJACENCIES to provide further information."
  (lexical-let ((query-map        (orgtrello-proxy/--compute-sync-query-request entity-data))
                (entity-to-sync   entity-data))
    (if (hash-table-p query-map)
        (orgtrello-query/http-trello
         query-map
         nil ; async
         (orgtrello-proxy/--standard-post-or-put-success-callback entity-data entities-adjacencies)
         (lambda (response)
           (orgtrello-proxy/--cleanup-meta entity-to-sync)
           (orgtrello-log/msg *OT/ERROR* "client - Problem during the sync request to the proxy- error-thrown: %s" (request-response-error-thrown response))))
      (progn ;; cannot execute the request
        (orgtrello-proxy/--cleanup-meta entity-to-sync)
        (orgtrello-log/msg *OT/ERROR* query-map)
        query-map))))

(defun orgtrello-proxy/do-action-on-entity (entity-data entities-adjacencies)
  "Compute the action on an entity ENTITY-DATA.
Retrieve needed information from the buffer and/or ENTITIES-ADJACENCIES if needed."
  (let* ((position    (orgtrello-data/entity-position entity-data))   ;; position is mandatory
         (buffer-name (orgtrello-data/entity-buffername entity-data))    ;; buffer-name too
         (marker      (orgtrello-data/entity-id-or-marker entity-data))  ;; retrieve the id (which serves as a marker too)
         (level       (orgtrello-data/entity-level entity-data)))
    (orgtrello-log/msg *OT/TRACE* "do-action-on-entity - Searching entity metadata from buffer '%s' at point '%s' to sync..." buffer-name position)
    (with-current-buffer buffer-name
      (when (orgtrello-proxy/--get-back-to-marker marker entity-data)
        (-> entity-data
          orgtrello-data/entity-action
          orgtrello-proxy/--dispatch-action
          (funcall entity-data entities-adjacencies))))))

(defun orgtrello-proxy/--delete-region (start end)
  "Delete a region defined by START and END bound."
  (remove-overlays start end) ;; remove overlays on the card region
  (delete-region start end))

(defun orgtrello-proxy/--delete-card-region ()
  "Delete the card region (including overlays and line)."
  (org-back-to-heading)
  (let ((starting-point (point))
        (ending-point   (save-excursion (if (org-goto-sibling) (point) (point-max))))) ;; next card or point-max
    (orgtrello-proxy/--delete-region starting-point ending-point)))

(defun orgtrello-proxy/--delete-checkbox-checklist-region ()
  "Delete the checklist region."
  (let ((starting-point (point-at-bol))
        (ending-point (save-excursion (-if-let (result (orgtrello-cbx/--goto-next-checkbox-with-same-level! *ORGTRELLO/CHECKLIST-LEVEL*))
                                          result
                                        (orgtrello-cbx/compute-next-card-point!))))) ;; next checkbox or next card or point-max
    (orgtrello-proxy/--delete-region starting-point ending-point)))

(defun orgtrello-proxy/--delete-checkbox-item-region ()
  "Delete the item region."
  (let ((starting-point (point-at-bol))
        (ending-point (1+ (point-at-eol))))
    (orgtrello-proxy/--delete-region starting-point ending-point)))

(defun orgtrello-proxy/delete-region (entity)
  "Compute the delete region function depending on the ENTITY's nature."
  (cond ((orgtrello-data/entity-card-p entity) 'orgtrello-proxy/--delete-card-region)
        ((orgtrello-data/entity-checklist-p entity) 'orgtrello-proxy/--delete-checkbox-checklist-region)
        ((orgtrello-data/entity-item-p entity) 'orgtrello-proxy/--delete-checkbox-item-region)))

(defun orgtrello-proxy/--standard-delete-success-callback (entity-to-del)
  "Return a callback function able to deal with the ENTITY-TO-DEL deletion."
  (lexical-let ((entry-position    (orgtrello-data/entity-position entity-to-del))
                (entry-buffer-name (orgtrello-data/entity-buffername entity-to-del))
                (entry-level       (orgtrello-data/entity-level entity-to-del))
                (marker            (orgtrello-data/entity-id entity-to-del))
                (level             (orgtrello-data/entity-level entity-to-del)))
    (lambda (response)
      (with-current-buffer entry-buffer-name
        (save-excursion
          (when (orgtrello-proxy/--getting-back-to-marker marker)
            (-> (orgtrello-buffer/entry-get-full-metadata!)
              orgtrello-data/current
              orgtrello-proxy/delete-region
              funcall)))))))

(defun orgtrello-proxy/--card-delete (card-meta)
  "Deal with the deletion query of a CARD-META."
  (orgtrello-api/delete-card (orgtrello-data/entity-id card-meta)))

(defun orgtrello-proxy/--checklist-delete (checklist-meta)
  "Deal with the deletion query of a CHECKLIST-META."
  (orgtrello-api/delete-checklist (orgtrello-data/entity-id checklist-meta)))

(defun orgtrello-proxy/--item-delete (item-meta)
  "Deal with create/update query of an ITEM-META in CHECKLIST-META."
  (let ((checklist-meta (orgtrello-data/parent item-meta)))
    (orgtrello-api/delete-item (orgtrello-data/entity-id checklist-meta) (orgtrello-data/entity-id item-meta))))

(defvar *MAP-DISPATCH-DELETE* (orgtrello-hash/make-properties `((,*ORGTRELLO/CARD-LEVEL*      . orgtrello-proxy/--card-delete)
                                                                (,*ORGTRELLO/CHECKLIST-LEVEL* . orgtrello-proxy/--checklist-delete)
                                                                (,*ORGTRELLO/ITEM-LEVEL*      . orgtrello-proxy/--item-delete)))
  "Dispatch map for the deletion query of card/checklist/item.")

(defun orgtrello-proxy/--dispatch-delete (entity)
  "Dispatch the call to the delete function depending on ENTITY level info."
  (orgtrello-proxy/compute-dispatch-fn entity *MAP-DISPATCH-DELETE*))

(defun orgtrello-proxy/--delete (entity-data &optional entities-adjacencies)
  "Compute the delete action to remove ENTITY-DATA.
This uses ENTITY-FULL-METADATA to help provide further information.
ENTITIES-ADJACENCIES is not used."
  (lexical-let ((query-map        (orgtrello-proxy/--dispatch-delete entity-data))
                (entity-to-delete entity-data)
                (level            (orgtrello-data/entity-level entity-data)))
    (if (hash-table-p query-map)
        (orgtrello-query/http-trello
         query-map
         nil ; async
         (orgtrello-proxy/--standard-delete-success-callback entity-data)
         (lambda (response)
           (orgtrello-log/msg *OT/ERROR* "client - Problem during the deletion request to the proxy - error-thrown: %s" (request-response-error-thrown response))
           (orgtrello-proxy/--cleanup-meta entity-to-delete)))
      (orgtrello-log/msg *OT/ERROR* query-map))))

(orgtrello-log/msg *OT/DEBUG* "orgtrello-proxy loaded!")

(provide 'org-trello-proxy)
;;; org-trello-proxy.el ends here
