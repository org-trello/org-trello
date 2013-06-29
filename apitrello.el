(load "./hash-util.el")

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
