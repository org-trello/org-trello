;;; orgtrello-api.el

(require 'orgtrello-hash)

(defun orgtrello-api--get-boards ()
  "Retrieve the boards of the current user."
  (orgtrello-hash--make-hash :get "/members/me/boards"))

(defun orgtrello-api--get-board (id)
  "Retrieve the boards of the current user."
  (orgtrello-hash--make-hash :get (format "/boards/%s" id)))

(defun orgtrello-api--get-cards (board-id)
  "cards of a board"
  (orgtrello-hash--make-hash :get (format "/boards/%s/cards" board-id)))

(defun orgtrello-api--get-card (card-id)
  "Detail of a card with id card-id."
  (orgtrello-hash--make-hash :get (format "/cards/%s" card-id)))

(defun orgtrello-api--get-lists (board-id)
  "Display the lists of the board"
  (orgtrello-hash--make-hash :get (format "/boards/%s/lists" board-id)))

(defun orgtrello-api--get-list (list-id)
  "Get a list by id"
  (orgtrello-hash--make-hash :get (format "/lists/%s" list-id)))

(defun orgtrello-api--add-list (name idBoard)
  "Add a list - the name and the board id are mandatory (so i say!)."
  (orgtrello-hash--make-hash :post "/lists/" `(("name" . ,name) ("idBoard" . ,idBoard))))

(defun orgtrello-api--add-card (name idList)
  "Add a card to a board"
  (orgtrello-hash--make-hash :post "/cards/" `(("name" . ,name) ("idList" . ,idList))))

(defun orgtrello-api--get-cards (list-id)
  "List all the cards"
  (orgtrello-hash--make-hash :get (format "/lists/%s/cards" list-id)))

(defun orgtrello-api--move-card (card-id idList &optional name)
  "Move a card to another list"
  (if name
      (setq data `(("name"   . ,name)
                   ("idList" . ,idList)))
    (setq data `(("idList" . ,idList))))
  (orgtrello-hash--make-hash :put (format "/cards/%s" card-id) data))

(defun orgtrello-api--add-checklist (card-id name)
  "Add a checklist to a card"
  (orgtrello-hash--make-hash :post
             (format "/cards/%s/checklists" card-id)
             `(("name" . ,name))))

(defun orgtrello-api--update-checklist (checklist-id name)
  "Update the checklist's name"
  (orgtrello-hash--make-hash :put
             (format "/checklists/%s" checklist-id)
             `(("name" . ,name))))

(defun orgtrello-api--get-checklists (card-id)
  "List the checklists of a card"
  (orgtrello-hash--make-hash :get (format "/cards/%s/checklists" card-id)))

(defun orgtrello-api--get-checklist (checklist-id)
  "Retrieve all the information from a checklist"
  (orgtrello-hash--make-hash :get (format "/checklists/%s" checklist-id)))

(defun orgtrello-api--add-tasks (checklist-id name &optional checked)
  "Add todo tasks (trello items) to a checklist with id 'id'"
  (let* ((payload (if checked
                      `(("name"  . ,name) ("checked" . ,checked))
                    `(("name" . ,name)))))
    (orgtrello-hash--make-hash :post (format "/checklists/%s/checkItems" checklist-id) payload)))

(defun orgtrello-api--update-task (card-id checklist-id task-id name &optional state)
  "Update a task"
  (let* ((payload (if state
                      `(("name"  . ,name) ("state" . ,state))
                    `(("name" . ,name)))))
    (orgtrello-hash--make-hash
     :put
     (format "/cards/%s/checklist/%s/checkItem/%s" card-id checklist-id task-id)
     payload)))

(provide 'orgtrello-api)

;;; orgtrello-api.el ends here
