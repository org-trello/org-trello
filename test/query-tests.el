(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(expectations (desc "orgtrello-query/--compute-url")
  (expect (format "%s%s" *TRELLO-URL* "/uri")            (orgtrello-query/--compute-url *TRELLO-URL* "/uri"))
  (expect (format "%s%s" *TRELLO-URL* "/uri/other")      (orgtrello-query/--compute-url *TRELLO-URL* "/uri/other"))
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

(expectations (desc "orgtrello-query/--prepare-params-assoc!")
  (expect '((id . "id") (name . "some%20content%20to%20escape%20voila"))
    (orgtrello-query/--prepare-params-assoc! '((id . "id") (name . "some content to escape voila"))))
  (expect '((id . "id") (name . "some%20content%20to%20escape%20voila") (any . "content%20is%20escaped%20this%20is%20fun"))
    (orgtrello-query/--prepare-params-assoc! '((id . "id") (name . "some content to escape voila") (any . "content is escaped this is fun"))))
  (expect '((id) (name . "some%20content%20to%20escape%20voila") (any . "content%20is%20escaped%20this%20is%20fun"))
    (orgtrello-query/--prepare-params-assoc! '((id) (name . "some content to escape voila") (any . "content is escaped this is fun"))))
  (expect '((ok . t) (name . "some%20content%20to%20escape%20voila") (any . "content%20is%20escaped%20this%20is%20fun"))
    (orgtrello-query/--prepare-params-assoc! '((ok . t) (name . "some content to escape voila") (any . "content is escaped this is fun")))))

(expectations (desc "orgtrello-query/--prepare-query-params!")
  (expect '((name . "some%20content%20to%20escape%20voila") (id . "id"))
    (orgtrello-query/--prepare-query-params! '((id . "id") (name . "some content to escape voila"))))
  (expect '((any . "content%20is%20escaped%20this%20is%20fun") (name . "some%20content%20to%20escape%20voila") (id . "id"))
    (orgtrello-query/--prepare-query-params! '((id . "id") (name . "some content to escape voila") (any . "content is escaped this is fun"))))
  (expect '((any . "content%20is%20escaped%20this%20is%20fun") (name . "some%20content%20to%20escape%20voila") (id))
    (orgtrello-query/--prepare-query-params! '((id) (name . "some content to escape voila") (any . "content is escaped this is fun"))))
  (expect '((any . "content%20is%20escaped%20this%20is%20fun") (name . "some%20content%20to%20escape%20voila") (ok . t))
    (orgtrello-query/--prepare-query-params! '((ok . t) (name . "some content to escape voila") (any . "content is escaped this is fun")))))

(expectations (desc "orgtrello-query/--prepare-params-assoc!")
  (expect '((name . "some data with & keywords hexified") (id . "abc") (other-field . "hexified string"))
    (->> '((other-field . "hexified string") (id . "abc") (name . "some data with & keywords hexified"))
         orgtrello-query/--prepare-params-assoc!
         json-encode
         orgtrello-proxy/--json-read-from-string)))

(expectations (desc "orgtrello-query/--prepare-params-assoc!")
  (expect '((name . "some%20data%20with%20keywords%20hexified") (id . "abc") (other-field . "hexified%20string"))
    (-> '((other-field . "hexified string") (id . "abc") (name . "some data with keywords hexified"))
        orgtrello-query/--prepare-params-assoc!
        json-encode
        orgtrello-proxy/--unhexify-data))
  (expect '((name . "some data with keywords hexified") (id . "abc") (other-field . "hexified string"))
    (-> '((other-field . "hexified string") (id . "abc") (name . "some data with keywords hexified"))
        orgtrello-query/--prepare-params-assoc!
        json-encode
        (orgtrello-proxy/--unhexify-data t))))

(expectations (desc "orgtrello-query/--dispatch-http-query")
  (expect 'orgtrello-query/--get         (orgtrello-query/--dispatch-http-query "GET"))
  (expect 'orgtrello-query/--post-or-put (orgtrello-query/--dispatch-http-query "POST"))
  (expect 'orgtrello-query/--post-or-put (orgtrello-query/--dispatch-http-query "PUT"))
  (expect 'orgtrello-query/--delete      (orgtrello-query/--dispatch-http-query "DELETE")))

(expectations
  (expect (downcase "%28%29%2A%25%21%24%26%27%2B%2C%3B%3D%20content%20is%20escaped%20this%20is%20fun%20ain%20t%20it")
    (downcase (funcall orgtrello-query/--hexify "()*%!$&'+,;= content is escaped this is fun ain t it"))))
