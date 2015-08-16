(require 'org-trello-proxy)
(require 'ert)
(require 'el-mock)

(ert-deftest test-orgtrello-proxy--compute-pattern-search-from-marker ()
  (should (equal "marker-is-a-trello-id" (orgtrello-proxy--compute-pattern-search-from-marker "marker-is-a-trello-id")))
  (should (equal "orgtrello-marker-tony" (orgtrello-proxy--compute-pattern-search-from-marker "orgtrello-marker-tony"))))

(ert-deftest test-orgtrello-proxy-map-dispatch-delete ()
  (should (equal 'orgtrello-proxy--card-delete      (gethash org-trello--card-level orgtrello-proxy--map-fn-dispatch-delete)))
  (should (equal 'orgtrello-proxy--checklist-delete (gethash org-trello--checklist-level orgtrello-proxy--map-fn-dispatch-delete)))
  (should (equal 'orgtrello-proxy--item-delete      (gethash org-trello--item-level orgtrello-proxy--map-fn-dispatch-delete))))

(ert-deftest test-orgtrello-proxy--card ()
  (should (equal 'orgtrello-proxy--card      (gethash org-trello--card-level orgtrello-proxy--map-fn-dispatch-create-update)))
  (should (equal 'orgtrello-proxy--checklist (gethash org-trello--checklist-level orgtrello-proxy--map-fn-dispatch-create-update)))
  (should (equal 'orgtrello-proxy--item      (gethash org-trello--item-level orgtrello-proxy--map-fn-dispatch-create-update))))

(ert-deftest test-orgtrello-proxy--compute-state ()
  (should (equal "complete" (orgtrello-proxy--compute-state org-trello--done)))
  (should (equal "incomplete" (orgtrello-proxy--compute-state "anything-else"))))

(ert-deftest test-orgtrello-proxy--compute-check ()
  (should (equal t   (orgtrello-proxy--compute-check org-trello--done)))
  (should (equal nil (orgtrello-proxy--compute-check "anything-else"))))

(ert-deftest test-orgtrello-proxy--tags-to-labels ()
  (should (string= "a,b,c" (orgtrello-proxy--tags-to-labels ":a:b:c")))
  (should (string= "a,b,c" (orgtrello-proxy--tags-to-labels "a:b:c")))
  (should (string= "a," (orgtrello-proxy--tags-to-labels ":a:")))
  (should (string= "a," (orgtrello-proxy--tags-to-labels "a:")))
  (should (string= ""  (orgtrello-proxy--tags-to-labels nil))))

(ert-deftest test-orgtrello-proxy--compute-pattern-search-from-marker ()
  (should (eq 'x (orgtrello-proxy--compute-pattern-search-from-marker 'x))))

(ert-deftest test-orgtrello-proxy--compute-sync-query-request ()
  (should (equal :res
                 (with-mock
                   (mock (orgtrello-proxy--compute-dispatch-fn :entity
                                                               orgtrello-proxy--map-fn-dispatch-create-update) => :res)
                   (orgtrello-proxy--compute-sync-query-request :entity)))))

(ert-deftest test-orgtrello-proxy--cleanup-meta ()
  ;; should clean up since it's not an id
  (should (equal :done
                 (with-mock
                   (mock (orgtrello-data-entity-id :entity) => nil)
                   (mock (orgtrello-cbx-org-delete-property org-trello--label-key-id) => :done)
                   (orgtrello-proxy--cleanup-meta :entity))))
  ;; should not clean up since it's an id
  (should-not (with-mock
                (mock (orgtrello-data-entity-id :entity) => :id)
                (orgtrello-proxy--cleanup-meta :entity))))

(ert-deftest test-orgtrello-proxy-sync-entity ()
  ;; not a query so fails
  (should (equal :not-a-query
                 (with-mock
                   (mock (orgtrello-proxy--compute-sync-query-request :entity) => :not-a-query)
                   (mock (hash-table-p :not-a-query) => nil)
                   (mock (orgtrello-proxy--cleanup-meta :entity) => :done)
                   (mock (orgtrello-log-msg orgtrello-log-error :not-a-query) => :done)
                   (orgtrello-proxy-sync-entity :entity :entities-adjacencies))))
  ;; success call to api
  (should (equal :result
                 (with-mock
                   (mock (orgtrello-proxy--compute-sync-query-request :entity) => :query-map)
                   (mock (hash-table-p :query-map) => t)
                   (mock (orgtrello-proxy--standard-post-or-put-success-callback :entity :entities-adjacencies) => :success-cbk)
                   (mock (orgtrello-query-http-trello :query-map
                                                      nil
                                                      :success-cbk
                                                      *) => :result)
                   (orgtrello-proxy-sync-entity :entity :entities-adjacencies)))))

(provide 'org-trello-proxy-test)
;;; org-trello-proxy-test.el ends here
