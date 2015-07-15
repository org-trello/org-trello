(require 'org-trello-proxy)
(require 'ert)
(require 'el-mock)

(ert-deftest test-orgtrello-proxy/--compute-pattern-search-from-marker ()
  (should (equal "marker-is-a-trello-id" (orgtrello-proxy/--compute-pattern-search-from-marker "marker-is-a-trello-id")))
  (should (equal "orgtrello-marker-tony" (orgtrello-proxy/--compute-pattern-search-from-marker "orgtrello-marker-tony"))))

(ert-deftest test-orgtrello-proxy/map-dispatch-delete ()
  (should (equal 'orgtrello-proxy/--card-delete      (gethash org-trello--card-level orgtrello-proxy--map-fn-dispatch-delete)))
  (should (equal 'orgtrello-proxy/--checklist-delete (gethash org-trello--checklist-level orgtrello-proxy--map-fn-dispatch-delete)))
  (should (equal 'orgtrello-proxy/--item-delete      (gethash org-trello--item-level orgtrello-proxy--map-fn-dispatch-delete))))

(ert-deftest test-orgtrello-proxy/--card ()
  (should (equal 'orgtrello-proxy/--card      (gethash org-trello--card-level orgtrello-proxy--map-fn-dispatch-create-update)))
  (should (equal 'orgtrello-proxy/--checklist (gethash org-trello--checklist-level orgtrello-proxy--map-fn-dispatch-create-update)))
  (should (equal 'orgtrello-proxy/--item      (gethash org-trello--item-level orgtrello-proxy--map-fn-dispatch-create-update))))

(ert-deftest test-orgtrello-proxy/--compute-state ()
  (should (equal "complete" (orgtrello-proxy/--compute-state org-trello--done)))
  (should (equal "incomplete" (orgtrello-proxy/--compute-state "anything-else"))))

(ert-deftest test-orgtrello-proxy/--compute-check ()
  (should (equal t   (orgtrello-proxy/--compute-check org-trello--done)))
  (should (equal nil (orgtrello-proxy/--compute-check "anything-else"))))

(ert-deftest test-orgtrello-proxy/--tags-to-labels ()
  (should (string= "a,b,c" (orgtrello-proxy/--tags-to-labels ":a:b:c")))
  (should (string= "a,b,c" (orgtrello-proxy/--tags-to-labels "a:b:c")))
  (should (string= "a," (orgtrello-proxy/--tags-to-labels ":a:")))
  (should (string= "a," (orgtrello-proxy/--tags-to-labels "a:")))
  (should (string= ""  (orgtrello-proxy/--tags-to-labels nil))))

(provide 'org-trello-proxy-tests)
;;; org-trello-proxy-tests.el ends here
