(require 'orgtrello-hash)

(ert-deftest testing-make-hash ()
  (let ((h (make-hash :some-method :some-uri)))
    (should (equal (gethash :method h) :some-method))
    (should (equal (gethash :uri    h) :some-uri))
    (should (equal (gethash :params h) nil))))

(provide 'orgtrello-hash-tests)

;;; orgtrello-hash-tests.el end here
