(require 'orgtrello-hash)

(defun get-boards ()
  "Retrieve the boards of the current user."
  (make-hash :get "/members/me/boards"))

(defun get-board (id)
  "Retrieve the boards of the current user."
  (make-hash :get (format "/boards/%s" id)))

(defun get-cards (board-id)
  "cards of a board"
  (make-hash :get (format "/boards/%s/cards" board-id)))

(defun get-card (card-id)
  "Detail of a card with id card-id."
  (make-hash :get (format "/cards/%s" card-id)))

(defun get-lists (board-id)
  "Display the lists of the board"
  (make-hash :get (format "/boards/%s/lists" board-id)))

(defun get-list (list-id)
  "Get a list by id"
  (make-hash :get (format "/lists/%s" list-id)))

(defun add-list (name idBoard)
  "Add a list - the name and the board id are mandatory (so i say!)."
  (make-hash :post "/lists/" `(("name" . ,name) ("idBoard" . ,idBoard))))

(defun add-card (name idList)
  "Add a card to a board"
  (make-hash :post "/cards/" `(("name" . ,name) ("idList" . ,idList))))

(defun get-cards (list-id)
  "List all the cards"
  (make-hash :get (format "/lists/%s/cards" list-id)))

(defun move-card (id idList name)
  "Move a card to another list"
  (make-hash :put (format "/cards/%s" id) `(("name" . ,name)
                                            ("idList" . ,idList))))

(defun add-checklist (card-id name)
  "Add a checklist to a card"
  (make-hash :post
             (format "/cards/%s/checklists" card-id)
             `(("name" . ,name))))

(defun get-checklists (card-id)
  "List the checklists of a card"
  (make-hash :get (format "/cards/%s/checklists" card-id)))

(defun get-checklist (checklist-id)
  "Retrieve all the information from a checklist"
  (make-hash :get (format "/checklists/%s" checklist-id)))

(defun add-tasks (name checklist-id)
  "Add todo tasks (trello items) to a checklist with id 'id'"
  (make-hash :post (format "/checklists/%s/checkItems" checklist-id) `(("name" . ,name))))

(defun check-or-uncheck-tasks (card-id checklist-id task-id state)
  "Update a task"
  (make-hash
   :put
   (format "/cards/%s/checklist/%s/checkItem/%s" card-id checklist-id task-id)
   `(("state" . ,state))))

(provide 'orgtrello-api)

;;; orgtrello-api ends here
