;;; orgtrello-data-tests

(require 'orgtrello-data)
(eval-when-compile (require 'cl-lib))

(ert-deftest testing-orgtrello-data--get-level ()
  (should (equal (orgtrello-data--get-level '(:pt :id 0 0 "IN PROGRESS" nil "HEADING_LABEL" nil)) 0))
  (should (equal (orgtrello-data--get-level '(:pt :id 1 0 "IN PROGRESS" nil "HEADING_LABEL" nil)) 1)))

(ert-deftest testing-orgtrello-data--get-keyword ()
  (should (equal (orgtrello-data--get-keyword '(:pt :id 0  0 "IN PROGRESS" nil "HEADING_LABEL" nil)) "IN PROGRESS"))
  (should (equal (orgtrello-data--get-keyword '(:pt :id 1  0 "TODO"        nil "HEADING_LABEL" nil)) "TODO")))

(ert-deftest testing-orgtrello-data--get-title ()
;;  (should (equal (orgtrello-data--get-title '(:pt :id 0  0 "IN PROGRESS" :some nil :tags))              nil))
  (should (equal (orgtrello-data--get-title '(:pt :id 0  0 "IN PROGRESS" :some "title 0" nil))          "title 0"))
  (should (equal (orgtrello-data--get-title '(:pt :id 1  0 "TODO"        :some "some other title" nil)) "some other title")))

(ert-deftest testing-orgtrello-data--get-id ()
  (should (equal (orgtrello-data--get-id '(:pt nil 1  0 "TODO"  nil "some other title" nil)) nil))
  (should (equal (orgtrello-data--get-id '(:pt :id0 1  0 "TODO" nil "some other title" nil)) :id0)))

(ert-deftest testing-orgtrello-data--get-point ()
  (should (equal (orgtrello-data--get-point '(:pt nil 1  0 "TODO"  nil "some other title" nil)) :pt))
  (should (equal (orgtrello-data--get-point '(:pt2 :id0 1  0 "TODO" nil "some other title" nil)) :pt2)))

(ert-deftest testing-orgtrello-data--get-metadata ()
  (let* ((meta (orgtrello-data--get-metadata '(:pt :id 0 1 "IN PROGRESS" nil "some title :orgtrello-id-identifier:" nil))))
    (should (equal (gethash :title   meta) "some title :orgtrello-id-identifier:"))
    (should (equal (gethash :keyword meta) "IN PROGRESS"))
    (should (equal (gethash :level   meta) 0))
    (should (equal (gethash :id      meta) :id))
    (should (equal (gethash :point   meta) :pt))))

(provide 'orgtrello-data-tests)

;;; orgtrello-data-tests.el ends here
