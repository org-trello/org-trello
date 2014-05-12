;;; org-trello-api.el --- Interface functions to Trello API
;;; Commentary:
;;; Code:

(require 'dash)
(require 'org-trello-setup)
(require 'org-trello-log)
(require 'org-trello-hash)
(require 'org-trello-data)

(defun orgtrello-api/make-query (method uri &optional params)
  "Make a query hash map from a METHOD, URI and optional parameter PARAMS."
  (let ((h (orgtrello-hash/empty-hash)))
    (->> (if params (orgtrello-data/put-entity-params params h) h)
      (orgtrello-data/put-entity-method method)
      (orgtrello-data/put-entity-uri uri))))

(defun orgtrello-api/--deal-with-optional-value (optional-entry value entries)
  "Add the optional value in entries depending on optional-entry.
If OPTIONAL-ENTRY is non nil, cons the VALUE to ENTRIES and return it.
Otherwise,return ENTRIES."
  (if optional-entry (cons value entries) entries))

(defun orgtrello-api/--deal-with-optional-values (optional-entries-values entries)
  "Add the optional entry/value OPTIONAL-ENTRIES-VALUES in ENTRIES.
Return entries updated with value if entry, entries untouched otherwise."
  (--reduce-from (orgtrello-api/--deal-with-optional-value (car it) (cdr it) acc)
                 entries
                 optional-entries-values))

(defun orgtrello-api/add-board (name &optional description)
  "Create a board query from NAME and optional DESCRIPTION."
  (orgtrello-api/make-query "POST" "/boards" (orgtrello-api/--deal-with-optional-value description `("desc" . ,description) `(("name" . ,name)))))

(defun orgtrello-api/get-boards ()
  "Create a retrieve boards of the current user query."
  (orgtrello-api/make-query "GET" "/members/me/boards"))

(defun orgtrello-api/get-board (id)
  "Create a retrieve board with board ID query."
  (orgtrello-api/make-query "GET" (format "/boards/%s" id) '(("memberships" . "active")
                                                             ("memberships_member" . "true")
                                                             ("fields" . "name,memberships,closed,labelNames"))))

(defun orgtrello-api/get-cards (board-id)
  "Create a cards retrieval from the board with BOARD-ID query."
  (orgtrello-api/make-query "GET" (format "/boards/%s/cards" board-id)
                            '(("actions" .  "commentCard")
                              ("fields" . "closed,desc,due,idBoard,idChecklists,idList,idMembers,name,pos"))))

(defun orgtrello-api/get-card (card-id)
  "Create a get-card with CARD-ID query."
  (orgtrello-api/make-query "GET" (format "/cards/%s" card-id)
                            '(("actions" . "commentCard")
                              ("action_fields" . "data")
                              ("action_memberCreator_fields" . "username")
                              ("fields" .  "closed,dateLastActivity,desc,due,idChecklists,idList,idMembers,labels,name,pos"))))

(defun orgtrello-api/delete-card (card-id)
  "Create a delete card with id CARD-ID query."
  (orgtrello-api/make-query "DELETE" (format "/cards/%s" card-id)))

(defun orgtrello-api/get-lists (board-id)
  "Create a get-lists of the board with BOARD-ID."
  (orgtrello-api/make-query "GET" (format "/boards/%s/lists" board-id)))

(defun orgtrello-api/close-list (list-id)
  "Create a close list with id LIST-ID query."
  (orgtrello-api/make-query "PUT" (format "/lists/%s/closed" list-id) '((value . t))))

(defun orgtrello-api/add-list (name idBoard)
  "Create an add a list with NAME and the IDBOARD."
  (orgtrello-api/make-query "POST" "/lists/" `(("name" . ,name) ("idBoard" . ,idBoard))))

(defun orgtrello-api/add-card (name idList &optional due id-members desc labels)
  "Create an add a card with NAME to the list IDLIST, with optional DUE, ID-MEMBERS, DESC and LABELS query."
  (orgtrello-api/make-query "POST" "/cards/"
                            (orgtrello-api/--deal-with-optional-values `((,id-members . ("idMembers" . ,id-members))
                                                                         (,due . ("due" . ,due))
                                                                         (,desc . ("desc" . ,desc))
                                                                         (,labels . ("labels" . ,labels)))
                                                                       `(("name" . ,name)
                                                                         ("idList" . ,idList)))))

(defun orgtrello-api/move-card (card-id idList &optional name due id-members desc labels)
  "Create an update a card CARD-ID to IDLIST with optional NAME, DUE date, ID-MEMBERS, DESC and LABELS query."
  (->> (orgtrello-api/--deal-with-optional-values `((,name . ("name" . ,name))
                                                    (,id-members . ("idMembers" . ,id-members))
                                                    (,due . ("due" . ,due))
                                                    (,desc . ("desc" . ,desc))
                                                    (,labels . ("labels" . ,labels)))
                                                  `(("idList" . ,idList)))
    (orgtrello-api/make-query "PUT" (format "/cards/%s" card-id))))

(defun orgtrello-api/add-checklist (card-id name)
  "Create an add a checklist to a card CARD-ID, checklist with NAME query."
  (orgtrello-api/make-query "POST" (format "/cards/%s/checklists" card-id) `(("name" . ,name))))

(defun orgtrello-api/update-checklist (checklist-id name)
  "Create an update the checklist CHECKLIST-ID with NAME query."
  (orgtrello-api/make-query "PUT" (format "/checklists/%s" checklist-id) `(("name" . ,name))))

(defun orgtrello-api/get-checklist (checklist-id &optional without-items)
  "Create a retrieve a checklist CHECKLIST-ID with optional WITHOUT-ITEMS flag query."
  (let ((default-params '(("fields" . "name,pos,idCard") ;; the id card is useful for us to determine if we deal with a checklist
                          ("checkItem_fields" . "name,pos,state"))))
    (orgtrello-api/make-query "GET"
                              (format "/checklists/%s" checklist-id)
                              (if without-items (cons '("checkItems" . "none") default-params) default-params))))

(defun orgtrello-api/delete-checklist (checklist-id)
  "Create a delete a checklist CHECKLIST-ID."
  (orgtrello-api/make-query "DELETE" (format "/checklists/%s" checklist-id)))

(defun orgtrello-api/add-items (checklist-id name &optional checked)
  "Create an add items to a checklist CHECKLIST-ID with NAME and optional CHECKED state query."
  (->> (orgtrello-api/--deal-with-optional-value checked `("checked" . ,checked) `(("name" . ,name)))
    (orgtrello-api/make-query "POST" (format "/checklists/%s/checkItems" checklist-id) )))

(defun orgtrello-api/update-item (card-id checklist-id item-id name &optional state)
  "Create an update an item from the CARD-ID, CHECKLIST-ID and ITEM-ID with NAME and optional STATE query."
  (->> (orgtrello-api/--deal-with-optional-value state `("state" . ,state) `(("name" . ,name)))
    (orgtrello-api/make-query "PUT" (format "/cards/%s/checklist/%s/checkItem/%s" card-id checklist-id item-id))))

(defun orgtrello-api/get-item (checklist-id item-id)
  "Create a get item from the checklist CHECKLIST-ID and item ITEM-ID query."
  (orgtrello-api/make-query "GET" (format "/checklists/%s/checkItems/%s" checklist-id item-id) '(("fields" . "name,pos,state"))))

(defun orgtrello-api/delete-item (checklist-id item-id)
  "Create a delete from the checklist CHECKLIST-ID the item ITEM-ID query."
  (orgtrello-api/make-query "DELETE" (format "/checklists/%s/checkItems/%s" checklist-id item-id)))

(defun orgtrello-api/get-me ()
  "Retrieve the current user's member informations query."
  (orgtrello-api/make-query "GET" "/members/me"))

(defun orgtrello-api/add-card-comment (card-id comment-text)
  "Create a add a card comment to the card with id CARD-ID with the COMMENT-TEXT."
  (orgtrello-api/make-query "POST" (format "/cards/%s/actions/comments" card-id) `(("text" . ,comment-text))))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-api loaded!")

(provide 'org-trello-api)
;;; org-trello-api.el ends here
