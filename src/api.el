(defun orgtrello-api/make-query (method uri &optional params)
  "Utility function to ease the creation of the map - wait, where are my clojure data again!?"
  (let ((h (orgtrello-hash/empty-hash)))
    (puthash :method method h)
    (puthash :uri    uri    h)
    (if params (puthash :params params h))
    h))

(defun orgtrello-api/--deal-with-optional-value (optional-entry value entries)
  "Add the optional value depending on the entry. Return entries updated with value if entry, entries untouched otherwise."
  (if optional-entry (cons value entries) entries))

(defun orgtrello-api/--deal-with-optional-values (optional-entries-values entries)
  "Add the optional entry/value depending on their entry. Return entries updated with value if entry, entries untouched otherwise."
  (--reduce-from (orgtrello-api/--deal-with-optional-value (car it) (cdr it) acc)
                 entries
                 optional-entries-values))

(defun orgtrello-api/add-board (name &optional description)
  "Create a board."
  (orgtrello-api/make-query "POST" "/boards" (orgtrello-api/--deal-with-optional-value description `("desc" . ,description) `(("name" . ,name)))))

(defun orgtrello-api/get-boards ()
  "Retrieve the boards of the current user."
  (orgtrello-api/make-query "GET" "/members/me/boards"))

(defun orgtrello-api/get-board (id)
  "Retrieve the boards of the current user."
  (orgtrello-api/make-query "GET" (format "/boards/%s" id) '(("memberships" . "active")
                                                             ("memberships_member" . "true")
                                                             ("fields" . "name,memberships,closed,labelNames"))))

(defun orgtrello-api/get-cards (board-id)
  "cards of a board"
  (orgtrello-api/make-query "GET" (format "/boards/%s/cards" board-id)
                            '(("actions" .  "commentCard")
                              ("fields" . "closed,desc,due,idBoard,idChecklists,idList,idMembers,name,pos"))))

(defun orgtrello-api/get-card (card-id)
  "Detail of a card with id card-id."
  (orgtrello-api/make-query "GET" (format "/cards/%s" card-id)
                            '(("actions" . "commentCard")
                              ("action_fields" . "data")
                              ("action_memberCreator_fields" . "username")
                              ("fields" .  "closed,dateLastActivity,desc,due,idChecklists,idList,idMembers,labels,name,pos"))))

(defun orgtrello-api/delete-card (card-id)
  "Delete a card with id card-id."
  (orgtrello-api/make-query "DELETE" (format "/cards/%s" card-id)))

(defun orgtrello-api/get-lists (board-id)
  "Display the lists of the board"
  (orgtrello-api/make-query "GET" (format "/boards/%s/lists" board-id)))

(defun orgtrello-api/close-list (list-id)
  "'Close' the list with id list-id."
  (orgtrello-api/make-query "PUT" (format "/lists/%s/closed" list-id) '((value . t))))

(defun orgtrello-api/add-list (name idBoard)
  "Add a list - the name and the board id are mandatory (so i say!)."
  (orgtrello-api/make-query "POST" "/lists/" `(("name" . ,name) ("idBoard" . ,idBoard))))

(defun orgtrello-api/add-card (name idList &optional due id-members desc labels)
  "Add a card to a board, optional due date (formatted string date), id-members (csv id members) and description desc."
  (orgtrello-api/make-query "POST" "/cards/"
                            (orgtrello-api/--deal-with-optional-values `((,id-members . ("idMembers" . ,id-members))
                                                                         (,due . ("due" . ,due))
                                                                         (,desc . ("desc" . ,desc))
                                                                         (,labels . ("labels" . ,labels)))
                                                                       `(("name" . ,name)
                                                                         ("idList" . ,idList)))))

(defun orgtrello-api/move-card (card-id idList &optional name due id-members desc labels)
  "Move a card to another list - optional entries (name, due date, id-members, desc)"
  (->> (orgtrello-api/--deal-with-optional-values `((,name . ("name" . ,name))
                                                    (,id-members . ("idMembers" . ,id-members))
                                                    (,due . ("due" . ,due))
                                                    (,desc . ("desc" . ,desc))
                                                    (,labels . ("labels" . ,labels)))
                                                  `(("idList" . ,idList)))
    (orgtrello-api/make-query "PUT" (format "/cards/%s" card-id))))

(defun orgtrello-api/add-checklist (card-id name)
  "Add a checklist to a card"
  (orgtrello-api/make-query "POST" (format "/cards/%s/checklists" card-id) `(("name" . ,name))))

(defun orgtrello-api/update-checklist (checklist-id name)
  "Update the checklist's name"
  (orgtrello-api/make-query "PUT" (format "/checklists/%s" checklist-id) `(("name" . ,name))))

(defun orgtrello-api/get-checklist (checklist-id &optional without-items)
  "Retrieve all the information from a checklist"
  (let ((default-params '(("fields" . "name,pos,idCard") ;; the id card is useful for us to determine if we deal with a checklist
                          ("checkItem_fields" . "name,pos,state"))))
    (orgtrello-api/make-query "GET"
                              (format "/checklists/%s" checklist-id)
                              (if without-items (cons '("checkItems" . "none") default-params) default-params))))

(defun orgtrello-api/delete-checklist (checklist-id)
  "Delete a checklist with checklist-id"
  (orgtrello-api/make-query "DELETE" (format "/checklists/%s" checklist-id)))

(defun orgtrello-api/add-items (checklist-id name &optional checked)
  "Add todo items (trello items) to a checklist with id 'id'"
  (->> (orgtrello-api/--deal-with-optional-value checked `("checked" . ,checked) `(("name" . ,name)))
    (orgtrello-api/make-query "POST" (format "/checklists/%s/checkItems" checklist-id) )))

(defun orgtrello-api/update-item (card-id checklist-id item-id name &optional state)
  "Update an item"
  (->> (orgtrello-api/--deal-with-optional-value state `("state" . ,state) `(("name" . ,name)))
    (orgtrello-api/make-query "PUT" (format "/cards/%s/checklist/%s/checkItem/%s" card-id checklist-id item-id))))

(defun orgtrello-api/get-item (checklist-id item-id)
  "List the checklist items."
  (orgtrello-api/make-query "GET" (format "/checklists/%s/checkItems/%s" checklist-id item-id) '(("fields" . "name,pos,state"))))

(defun orgtrello-api/delete-item (checklist-id item-id)
  "Delete a item with id item-id"
  (orgtrello-api/make-query "DELETE" (format "/checklists/%s/checkItems/%s" checklist-id item-id)))

(defun orgtrello-api/get-me ()
  "Retrieve the current user's member informations."
  (orgtrello-api/make-query "GET" "/members/me"))

(defun orgtrello-api/add-card-comment (card-id comment-text)
  "Add a comment to a card"
  (orgtrello-api/make-query "POST" (format "/cards/%s/actions/comments" card-id) `(("text" . ,comment-text))))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-api loaded!")


