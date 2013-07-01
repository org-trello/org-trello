;;; orgtrello-data-tests

(require 'orgtrello-data)

(ert-deftest testing-orgtrello-data--get-level ()
  (should (equal (orgtrello-data--get-level '(0 0 "IN PROGRESS" nil "HEADING_LABEL" nil)) 0))
  (should (equal (orgtrello-data--get-level '(1 0 "IN PROGRESS" nil "HEADING_LABEL" nil)) 1)))

(ert-deftest testing-orgtrello-data--get-keyword ()
  (should (equal (orgtrello-data--get-keyword '(0  0 "IN PROGRESS" nil "HEADING_LABEL" nil)) "IN PROGRESS"))
  (should (equal (orgtrello-data--get-keyword '(1  0 "TODO"        nil "HEADING_LABEL" nil)) "TODO")))

(ert-deftest testing-orgtrello-data--get-title ()
;;  (should (equal (orgtrello-data--get-title '(0  0 "IN PROGRESS" :some nil :tags))              nil))
  (should (equal (orgtrello-data--get-title '(0  0 "IN PROGRESS" :some "title 0" nil))          "title 0"))
  (should (equal (orgtrello-data--get-title '(1  0 "TODO"        :some "some other title" nil)) "some other title")))

(ert-deftest testing-orgtrello-data--get-id ()
  (should (equal (orgtrello-data--get-id '(1  0 "TODO"        nil "some other title :orgtrello-id-bloubiboulga"  nil))               "bloubiboulga"))
  (should (equal (orgtrello-data--get-id '(1  0 "TODO"        nil "some other title :orgtrello-id-bloubiboulga:" nil))               "bloubiboulga"))
  (should (equal (orgtrello-data--get-id '(1  0 "TODO"        nil "some other title :meta2:"                     nil))                nil))
  (should (equal (orgtrello-data--get-id '(1  0 "TODO"        nil "some other title :orgtrello-id-bloubiboulga:" ":some-tag:"))       "bloubiboulga"))
  (should (equal (orgtrello-data--get-id '(1  0 "TODO"        nil "some other title :meta2:"                     ":some-other-tag:")) nil))  )

(ert-deftest testing-orgtrello-data--extract-id ()
  (should (equal (orgtrello-data--extract-id-from-title nil) nil))
  (should (equal (orgtrello-data--extract-id-from-title "some other title") nil))
  (should (equal (orgtrello-data--extract-id-from-title "some other title :orgtrello-id-bloubiboulga:") "bloubiboulga")))

(ert-deftest testing-orgtrello-data--get-metadata ()
  (let* ((meta (orgtrello-data--get-metadata '(0 1 "IN PROGRESS" nil "some title :orgtrello-id-identifier:" nil))))
    (should (equal (gethash :title   meta) "some title :orgtrello-id-identifier:"))
    (should (equal (gethash :keyword meta) "IN PROGRESS"))
    (should (equal (gethash :level   meta) 0))
    (should (equal (gethash :id      meta) "identifier"))))

(provide 'orgtrello-data-tests)

;;; orgtrello-data-tests.el ends here
