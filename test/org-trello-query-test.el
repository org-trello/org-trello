(require 'org-trello-query)
(require 'ert)
(require 'el-mock)

(ert-deftest test-orgtrello-query--compute-url ()
  (should (equal (format "%s%s" orgtrello-query--trello-url "/uri")            (orgtrello-query--compute-url orgtrello-query--trello-url "/uri")))
  (should (equal (format "%s%s" orgtrello-query--trello-url "/uri/other")      (orgtrello-query--compute-url orgtrello-query--trello-url "/uri/other")))
  (should (equal (format "some-server/uri/some/other")          (orgtrello-query--compute-url "some-server" "/uri/some/other"))))

(ert-deftest test-orgtrello-query-entity-getters ()
  (should (equal :some-get (orgtrello-data-entity-method (orgtrello-hash-make-properties `((:method . :some-get) (:uri . :some-uri) (:sync . :some-sync) (:params . :some-params))))))
  (should (equal :some-uri (orgtrello-data-entity-uri (orgtrello-hash-make-properties `((:method . :some-get) (:uri . :some-uri) (:sync . :some-sync) (:params . :some-params))))))
  (should (equal :some-sync (orgtrello-data-entity-sync (orgtrello-hash-make-properties `((:method . :some-get) (:uri . :some-uri) (:sync . :some-sync) (:params . :some-params))))))
  (should (equal :some-params (orgtrello-data-entity-params (orgtrello-hash-make-properties `((:method . :some-get) (:uri . :some-uri) (:sync . :some-sync) (:params . :some-params))))))

  (should (equal "some-id" (orgtrello-data-entity-id (orgtrello-hash-make-properties '((:id . "some-id"))))))
  (should (equal nil       (orgtrello-data-entity-id (orgtrello-hash-make-properties '((noid . "some-id"))))))

  (should (equal :some-name (orgtrello-data-entity-name (orgtrello-hash-make-properties '((:name . :some-name))))))
  (should (equal nil        (orgtrello-data-entity-name (orgtrello-hash-make-properties '((noname . :some-name))))))

  (should (equal :some-list-id (orgtrello-data-entity-list-id (orgtrello-hash-make-properties '((:list-id . :some-list-id))))))
  (should (equal nil           (orgtrello-data-entity-list-id (orgtrello-hash-make-properties '((noIdList . :some-list-id))))))

  (should (equal :some-clist-ids (orgtrello-data-entity-checklists (orgtrello-hash-make-properties '((:checklists . :some-clist-ids))))))
  (should (equal nil             (orgtrello-data-entity-checklists (orgtrello-hash-make-properties '((no . :some-clist-ids))))))

  (should (equal :some-check-items (orgtrello-data-entity-items (orgtrello-hash-make-properties '((:items . :some-check-items))))))
  (should (equal nil               (orgtrello-data-entity-items (orgtrello-hash-make-properties '((no . :some-check-items))))))

  (should (equal :some-card-id (orgtrello-data-entity-card-id (orgtrello-hash-make-properties '((:card-id . :some-card-id))))))
  (should (equal nil           (orgtrello-data-entity-card-id (orgtrello-hash-make-properties '((no . :some-card-id))))))

  (should (equal :some-due (orgtrello-data-entity-due (orgtrello-hash-make-properties '((:due . :some-due))))))
  (should (equal nil       (orgtrello-data-entity-due (orgtrello-hash-make-properties '((no . :some-due))))))

  (should (equal :some-state (orgtrello-data-entity-keyword (orgtrello-hash-make-properties '((:keyword . :some-state))))))
  (should (equal nil         (orgtrello-data-entity-keyword (orgtrello-hash-make-properties '((no . :some-state))))))

  (should (equal :closed (orgtrello-data-entity-closed (orgtrello-hash-make-properties '((:closed . :closed))))))
  (should (equal nil     (orgtrello-data-entity-closed (orgtrello-hash-make-properties '((no . :some-state)))))))

(ert-deftest test-orgtrello-query--dispatch-http-query ()
  (should (equal 'orgtrello-query--get         (orgtrello-query--dispatch-http-query "GET")))
  (should (equal 'orgtrello-query--post-or-put (orgtrello-query--dispatch-http-query "POST")))
  (should (equal 'orgtrello-query--post-or-put (orgtrello-query--dispatch-http-query "PUT")))
  (should (equal 'orgtrello-query--delete      (orgtrello-query--dispatch-http-query "DELETE"))))

(provide 'org-trello-query-tests)
;;; org-trello-query-tests.el ends here
