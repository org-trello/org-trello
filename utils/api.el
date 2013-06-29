(require 'hash)

(defun get-boards ()
  "Retrieve the boards of the current user."
  (make-hash :get "/members/me/board"))

(ert-deftest testing-get-boards ()
  (let ((h (get-boards)))
    (should (equal (gethash :method h) :get))
    (should (equal (gethash :uri    h) "/members/me/board"))
    (should (equal (gethash :params h) nil))))

(defun get-board (id)
  "Retrieve the boards of the current user."
  (make-hash :get (format "/boards/%s" id)))

(ert-deftest testing-get-board ()
  (let ((h (get-board :id)))
    (should (equal (gethash :method h) :get))
    (should (equal (gethash :uri    h) "/boards/:id"))
    (should (equal (gethash :params h) nil))))

(defun get-cards (board-id)
  "cards of a board"
  (make-hash :get (format "/boards/%s/cards" board-id)))

(ert-deftest testing-get-cards ()
  (let ((h (get-cards :board-id)))
    (should (equal (gethash :method h) :get))
    (should (equal (gethash :uri    h) "/boards/:board-id/cards"))
    (should (equal (gethash :params h) nil))))

(defun get-card (card-id)
  "Detail of a card with id card-id."
  (make-hash :get (format "/cards/%s" card-id)))

(ert-deftest testing-get-card ()
  (let ((h (get-card :card-id)))
    (should (equal (gethash :method h) :get))
    (should (equal (gethash :uri    h) "/cards/:card-id"))
    (should (equal (gethash :params h) nil))))

(defun get-lists (board-id)
  "Display the lists of the board"
  (make-hash :get (format "/boards/%s/lists" board-id)))

(ert-deftest testing-get-lists ()
  (let ((h (get-lists :board-id)))
    (should (equal (gethash :method h) :get))
    (should (equal (gethash :uri    h) "/boards/:board-id/lists"))
    (should (equal (gethash :params h) nil))))

(defun get-list (list-id)
  "Get a list by id"
  (make-hash :get (format "/lists/%s" list-id)))

(ert-deftest testing-get-list ()
  (let ((h (get-list :list-id)))
    (should (equal (gethash :method h) :get))
    (should (equal (gethash :uri    h) "/lists/:list-id"))
    (should (equal (gethash :params h) nil))))

(defun add-list (name idBoard)
  "Add a list - the name and the board id are mandatory (so i say!)."
  (make-hash :post "/lists/" `(("name" . ,name) ("idBoard" . ,idBoard))))

(ert-deftest testing-add-list ()
  (let ((h (add-list "list-name" "board-id")))
    (should (equal (gethash :method h) :post))
    (should (equal (gethash :uri    h) "/lists/"))
    (should (equal (gethash :params h) '(("name" . "list-name")
                                         ("idBoard" . "board-id"))))))

(defun add-card (name idList)
  "Add a card to a board"
  (make-hash :post "/cards/" `(("name" . ,name) ("idList" . ,idList))))

(ert-deftest testing-add-card ()
  (let ((h (add-card "card-name" "list-id")))
    (should (equal (gethash :method h) :post))
    (should (equal (gethash :uri    h) "/cards/"))
    (should (equal (gethash :params h) '(("name" . "card-name") ("idList" . "list-id"))))))

(defun get-cards (list-id)
  "List all the cards"
  (make-hash :get (format "/lists/%s/cards" list-id)))

(ert-deftest testing-get-cards ()
  (let ((h (get-cards :list-id)))
    (should (equal (gethash :method h) :get))
    (should (equal (gethash :uri    h) "/lists/:list-id/cards"))
    (should (equal (gethash :params h) nil))))

(defun move-card (id idList name)
  "Move a card to another list"
  (make-hash :put (format "/cards/%s" id) `(("name" . ,name)
                                            ("idList" . ,idList))))

(ert-deftest testing-move-card ()
  (let ((h (move-card :id-card :id-list "name-card")))
    (should (equal (gethash :method h) :put))
    (should (equal (gethash :uri    h) "/cards/:id-card"))
    (should (equal (gethash :params h) '(("name" . "name-card")
                                         ("idList" . :id-list))))))

(defun add-checklist (card-id name)
  "Add a checklist to a card"
  (make-hash :post
             (format "/cards/%s/checklists" card-id)
             `(("name" . ,name))))

(ert-deftest testing-add-checklist ()
  (let ((h (add-checklist "id-card" "name-checklist")))
    (should (equal (gethash :method h) :post))
    (should (equal (gethash :uri    h) "/cards/id-card/checklists"))
    (should (equal (gethash :params h) '(("name" . "name-checklist"))))))

(defun get-checklists (card-id)
  "List the checklists of a card"
  (make-hash :get (format "/cards/%s/checklists" card-id)))

(ert-deftest testing-get-checklists ()
  (let ((h (get-checklists :card-id)))
    (should (equal (gethash :method h) :get))
    (should (equal (gethash :uri    h) "/cards/:card-id/checklists"))
    (should (equal (gethash :params h) nil))))

(defun get-checklist (checklist-id)
  "Retrieve all the information from a checklist"
  (make-hash :get (format "/checklists/%s" checklist-id)))

(ert-deftest testing-get-checklist ()
  (let ((h (get-checklist :checklist-id)))
    (should (equal (gethash :method h) :get))
    (should (equal (gethash :uri    h) "/checklists/:checklist-id"))
    (should (equal (gethash :params h) nil))))

(defun add-tasks (name checklist-id)
  "Add todo tasks (trello items) to a checklist with id 'id'"
  (make-hash :post (format "/checklists/%s/checkItems" checklist-id) `(("name" . ,name))))

(ert-deftest testing-add-tasks ()
  (let ((h (add-tasks "task-name" :checklist-id)))
    (should (equal (gethash :method h) :post))
    (should (equal (gethash :uri    h) "/checklists/:checklist-id/checkItems"))
    (should (equal (gethash :params h) '(("name" . "task-name"))))))

(defun check-or-uncheck-tasks (card-id checklist-id task-id state)
  "Update a task"
  (make-hash
   :put
   (format "/cards/%s/checklist/%s/checkItem/%s" card-id checklist-id task-id)
   `(("state" . ,state))))

(ert-deftest testing-check-or-uncheck-states ()
  (let ((h (check-or-uncheck-tasks :card-id :checklist-id :task-id "incomplete")))
    (should (equal (gethash :method h) :put))
    (should (equal (gethash :uri    h) "/cards/:card-id/checklist/:checklist-id/checkItem/:task-id"))
    (should (equal (gethash :params h) '(("state" ."incomplete"))))))

(provide 'api)

;;; api ends here
