(require 'orgtrello-hash)
(require 'cl-lib)

(ert-deftest testing-orgtrello-hash--make-hash-org ()
  (let* ((meta (orgtrello-hash--make-hash-org 0 "IN PROGRESS" "some title" "some id" "point")))
    (should (equal (gethash :title   meta) "some title"))
    (should (equal (gethash :keyword meta) "IN PROGRESS"))
    (should (equal (gethash :level   meta) 0))
    (should (equal (gethash :id      meta) "some id"))
    (should (equal (gethash :point   meta) "point"))))

(ert-deftest testing-orgtrello-hash--make-hash ()
  (let ((h (orgtrello-hash--make-hash :some-method :some-uri)))
    (should (equal (gethash :method h) :some-method))
    (should (equal (gethash :uri    h) :some-uri))
    (should (equal (gethash :params h) nil))))

(provide 'orgtrello-hash-tests)

;;; orgtrello-hash-tests.el end here
