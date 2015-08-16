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

(ert-deftest test-orgtrello-proxy--compute-delete-query-request ()
  (should (equal :res
                 (with-mock
                   (mock (orgtrello-proxy--compute-dispatch-fn :entity orgtrello-proxy--map-fn-dispatch-delete) => :res)
                   (orgtrello-proxy--compute-delete-query-request :entity)))))

(ert-deftest test-orgtrello-proxy--delete-entity-region ()
  (should (equal 'orgtrello-proxy--delete-card-region
                 (with-mock
                   (mock (orgtrello-data-entity-card-p :entity) => t)
                   (orgtrello-proxy--delete-entity-region :entity))))
  (should (equal 'orgtrello-proxy--delete-checkbox-checklist-region
                 (with-mock
                   (mock (orgtrello-data-entity-card-p :entity) => nil)
                   (mock (orgtrello-data-entity-checklist-p :entity) => t)
                   (orgtrello-proxy--delete-entity-region :entity))))
  (should (equal 'orgtrello-proxy--delete-checkbox-item-region
                 (with-mock
                   (mock (orgtrello-data-entity-card-p :entity) => nil)
                   (mock (orgtrello-data-entity-checklist-p :entity) => nil)
                   (mock (orgtrello-data-entity-item-p :entity) => t)
                   (orgtrello-proxy--delete-entity-region :entity)))))

(ert-deftest test-orgtrello-proxy--card-delete ()
  "Deal with the deletion query of a CARD-META."
  (should (equal :delete-card-done
                 (with-mock
                   (mock (orgtrello-data-entity-id :card-meta) => :id)
                   (mock (orgtrello-api-delete-card :id) => :delete-card-done)
                   (orgtrello-proxy--card-delete :card-meta)))))

(ert-deftest test-orgtrello-proxy--checklist-delete ()
  (should (equal :delete-checklist-done
                 (with-mock
                   (mock (orgtrello-data-entity-id :checklist-meta) => :id)
                   (mock (orgtrello-api-delete-checklist :id) => :delete-checklist-done)
                   (orgtrello-proxy--checklist-delete :checklist-meta)))))

(ert-deftest test-orgtrello-proxy--item-delete ()
  (should (equal :delete-item-done
                 (let ((item-meta (orgtrello-hash-make-properties `((:id . "item-id")
                                                                    (:parent . ,(orgtrello-hash-make-properties '((:id . "checklist-id"))))))))
                   (with-mock
                     (mock (orgtrello-api-delete-item "checklist-id" "item-id") => :delete-item-done)
                     (orgtrello-proxy--item-delete item-meta))))))

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

(ert-deftest test-orgtrello-proxy-delete-entity ()
  (should (equal :not-a-query
                 (with-mock
                   (mock (orgtrello-proxy--compute-delete-query-request :entity) => :not-a-query)
                   (mock (hash-table-p :not-a-query) => nil)
                   (mock (orgtrello-log-msg orgtrello-log-error :not-a-query) => :not-a-query)
                   (orgtrello-proxy-delete-entity :entity))))
  (should (equal :result
                 (with-mock
                   (mock (orgtrello-proxy--compute-delete-query-request :entity) => :query-map)
                   (mock (hash-table-p :query-map) => t)
                   (mock (orgtrello-proxy--standard-delete-success-callback :entity) => :success-cbk)
                   (mock (orgtrello-query-http-trello :query-map
                                                      nil
                                                      :success-cbk
                                                      *) => :result)
                   (orgtrello-proxy-delete-entity :entity)))))

(ert-deftest test-orgtrello-proxy-sync-entity ()
  ;; not a query so fails
  (should (equal :not-a-query
                 (with-mock
                   (mock (orgtrello-proxy--compute-sync-query-request :entity) => :not-a-query)
                   (mock (hash-table-p :not-a-query) => nil)
                   (mock (orgtrello-proxy--cleanup-meta :entity) => :done)
                   (mock (orgtrello-log-msg orgtrello-log-error :not-a-query) => :not-a-query)
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
