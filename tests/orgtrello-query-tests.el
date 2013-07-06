(require 'orgtrello-query)
(eval-when-compile (require 'cl-lib))

(ert-deftest testing-orgtrello-query--compute-method ()
  (should (equal (orgtrello-query--compute-method :get)    "GET"))
  (should (equal (orgtrello-query--compute-method :post)   "POST"))
  (should (equal (orgtrello-query--compute-method :put)    "PUT"))
  (should (equal (orgtrello-query--compute-method :delete) "DELETE")))

(ert-deftest testing-orgtrello-query--compute-url ()
  (should (equal (orgtrello-query--compute-url "/uri")
                 (format "%s%s" *TRELLO-URL* "/uri")))
  (should (equal (orgtrello-query--compute-url "/uri/other")
                 (format "%s%s" *TRELLO-URL* "/uri/other")))
  (should (equal (orgtrello-query--compute-url "/uri/some/other")
                 (format "%s%s" *TRELLO-URL* "/uri/some/other"))))

(provide 'orgtrello-query-tests)

;;; orgtrello-query-tests.el end here
