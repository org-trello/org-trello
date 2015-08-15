(require 'org-trello-query)
(require 'ert)
(require 'el-mock)

(ert-deftest test-orgtrello-query--compute-url ()
  (should (equal (format "%s%s" orgtrello-query--trello-url "/uri")            (orgtrello-query--compute-url orgtrello-query--trello-url "/uri")))
  (should (equal (format "%s%s" orgtrello-query--trello-url "/uri/other")      (orgtrello-query--compute-url orgtrello-query--trello-url "/uri/other")))
  (should (equal (format "some-server/uri/some/other")          (orgtrello-query--compute-url "some-server" "/uri/some/other"))))


(ert-deftest test-orgtrello-query--dispatch-http-query ()
  (should (equal 'orgtrello-query--get         (orgtrello-query--dispatch-http-query "GET")))
  (should (equal 'orgtrello-query--post-or-put (orgtrello-query--dispatch-http-query "POST")))
  (should (equal 'orgtrello-query--post-or-put (orgtrello-query--dispatch-http-query "PUT")))
  (should (equal 'orgtrello-query--delete      (orgtrello-query--dispatch-http-query "DELETE"))))

(provide 'org-trello-query-test)
;;; org-trello-query-test.el ends here
