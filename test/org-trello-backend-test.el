(require 'org-trello-backend)

(ert-deftest test-orgtrello-backend-add-entity-to-entities ()
  (should (orgtrello-tests-hash-equal
           (orgtrello-hash-make-properties `((:id . "identifier-became-the-key")))
           (orgtrello-hash-gethash-data "identifier-became-the-key"
                                        (orgtrello-backend-add-entity-to-entities
                                         (orgtrello-hash-make-properties `((:id . "identifier-became-the-key")))
                                         (orgtrello-hash-empty-hash))))))

(provide 'org-trello-backend-test)
;;; org-trello-backend-test.el ends here
