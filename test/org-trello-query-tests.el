(require 'org-trello-query)
(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(expectations (desc "orgtrello-query/--compute-url")
  (expect (format "%s%s" *ORGTRELLO/TRELLO-URL* "/uri")            (orgtrello-query/--compute-url *ORGTRELLO/TRELLO-URL* "/uri"))
  (expect (format "%s%s" *ORGTRELLO/TRELLO-URL* "/uri/other")      (orgtrello-query/--compute-url *ORGTRELLO/TRELLO-URL* "/uri/other"))
  (expect (format "some-server/uri/some/other")          (orgtrello-query/--compute-url "some-server" "/uri/some/other")))

(expectations (desc "orgtrello-data/entity-* - 1")
  (expect :some-get (orgtrello-data/entity-method (orgtrello-hash/make-properties `((:method . :some-get) (:uri . :some-uri) (:sync . :some-sync) (:params . :some-params)))))
  (expect :some-uri (orgtrello-data/entity-uri (orgtrello-hash/make-properties `((:method . :some-get) (:uri . :some-uri) (:sync . :some-sync) (:params . :some-params)))))
  (expect :some-sync (orgtrello-data/entity-sync (orgtrello-hash/make-properties `((:method . :some-get) (:uri . :some-uri) (:sync . :some-sync) (:params . :some-params)))))
  (expect :some-params (orgtrello-data/entity-params (orgtrello-hash/make-properties `((:method . :some-get) (:uri . :some-uri) (:sync . :some-sync) (:params . :some-params))))))

(expectations (desc "orgtrello-data/entity-* - 2")
  (expect "some-id" (orgtrello-data/entity-id (orgtrello-hash/make-properties '((:id . "some-id")))))
  (expect nil       (orgtrello-data/entity-id (orgtrello-hash/make-properties '((noid . "some-id"))))))

(expectations (desc "orgtrello-data/entity-* - 3")
  (expect :some-name (orgtrello-data/entity-name (orgtrello-hash/make-properties '((:name . :some-name)))))
  (expect nil        (orgtrello-data/entity-name (orgtrello-hash/make-properties '((noname . :some-name))))))

(expectations (desc "orgtrello-data/entity-* - 4")
  (expect :some-list-id (orgtrello-data/entity-list-id (orgtrello-hash/make-properties '((:list-id . :some-list-id)))))
  (expect nil           (orgtrello-data/entity-list-id (orgtrello-hash/make-properties '((noIdList . :some-list-id))))))

(expectations (desc "orgtrello-data/entity-* - 5")
  (expect :some-clist-ids (orgtrello-data/entity-checklists (orgtrello-hash/make-properties '((:checklists . :some-clist-ids)))))
  (expect nil             (orgtrello-data/entity-checklists (orgtrello-hash/make-properties '((no . :some-clist-ids))))))

(expectations (desc "orgtrello-data/entity-* - 6")
  (expect :some-check-items (orgtrello-data/entity-items (orgtrello-hash/make-properties '((:items . :some-check-items)))))
  (expect nil               (orgtrello-data/entity-items (orgtrello-hash/make-properties '((no . :some-check-items))))))

(expectations (desc "orgtrello-data/entity-* - 7")
  (expect :some-card-id (orgtrello-data/entity-card-id (orgtrello-hash/make-properties '((:card-id . :some-card-id)))))
  (expect nil           (orgtrello-data/entity-card-id (orgtrello-hash/make-properties '((no . :some-card-id))))))

(expectations (desc "orgtrello-data/entity-* - 8")
  (expect :some-due (orgtrello-data/entity-due (orgtrello-hash/make-properties '((:due . :some-due)))))
  (expect nil       (orgtrello-data/entity-due (orgtrello-hash/make-properties '((no . :some-due))))))

(expectations (desc "orgtrello-data/entity-* - 9")
  (expect :some-state (orgtrello-data/entity-keyword (orgtrello-hash/make-properties '((:keyword . :some-state)))))
  (expect nil         (orgtrello-data/entity-keyword (orgtrello-hash/make-properties '((no . :some-state))))))

(expectations (desc "orgtrello-data/entity-* - 10")
  (expect :closed (orgtrello-data/entity-closed (orgtrello-hash/make-properties '((:closed . :closed)))))
  (expect nil     (orgtrello-data/entity-closed (orgtrello-hash/make-properties '((no . :some-state))))))

(expectations (desc "orgtrello-query/--dispatch-http-query")
  (expect 'orgtrello-query/--get         (orgtrello-query/--dispatch-http-query "GET"))
  (expect 'orgtrello-query/--post-or-put (orgtrello-query/--dispatch-http-query "POST"))
  (expect 'orgtrello-query/--post-or-put (orgtrello-query/--dispatch-http-query "PUT"))
  (expect 'orgtrello-query/--delete      (orgtrello-query/--dispatch-http-query "DELETE")))

(provide 'org-trello-query-tests)
;;; org-trello-query-tests.el ends here
