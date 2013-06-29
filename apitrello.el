(defun make-hash (method uri &optional params)
  "Utility function to ease the creation of the map - wait, where are my clojure data again!?"
  (setq h (make-hash-table :test 'equal))
  (puthash :method method h)
  (puthash :uri    uri    h)
  (if params (puthash :params params h))
  h)

(ert-deftest testing-make-hash ()
  (let ((h (make-hash :some-method :some-uri)))
    (should (equal (gethash :method h) :some-method))
    (should (equal (gethash :uri    h) :some-uri))
    (should (equal (gethash :params h) nil))))

(defun get-boards ()
  "Retrieve the boards of the current user."
  (make-hash :get "/members/me/board"))

(ert-deftest testing-get-boards ()
  (let ((h (get-boards)))
    (should (equal (gethash :method h) :get))
    (should (equal (gethash :uri    h) "/members/me/board"))
    (should (equal (gethash :params h) nil))))
