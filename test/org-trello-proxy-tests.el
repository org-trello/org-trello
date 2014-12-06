(require 'org-trello-proxy)
(require 'ert)
(require 'el-mock)

(ert-deftest test-orgtrello-proxy/--compute-pattern-search-from-marker ()
  (should (equal "marker-is-a-trello-id" (orgtrello-proxy/--compute-pattern-search-from-marker "marker-is-a-trello-id")))
  (should (equal "orgtrello-marker-tony" (orgtrello-proxy/--compute-pattern-search-from-marker "orgtrello-marker-tony"))))

(ert-deftest test-orgtrello-proxy/map-dispatch-delete ()
  (should (equal 'orgtrello-proxy/--card-delete      (gethash *ORGTRELLO/CARD-LEVEL* *MAP-DISPATCH-DELETE*)))
  (should (equal 'orgtrello-proxy/--checklist-delete (gethash *ORGTRELLO/CHECKLIST-LEVEL* *MAP-DISPATCH-DELETE*)))
  (should (equal 'orgtrello-proxy/--item-delete      (gethash *ORGTRELLO/ITEM-LEVEL* *MAP-DISPATCH-DELETE*))))

(ert-deftest test-orgtrello-proxy/--card ()
  (should (equal 'orgtrello-proxy/--card      (gethash *ORGTRELLO/CARD-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*)))
  (should (equal 'orgtrello-proxy/--checklist (gethash *ORGTRELLO/CHECKLIST-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*)))
  (should (equal 'orgtrello-proxy/--item      (gethash *ORGTRELLO/ITEM-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*))))

(ert-deftest test-orgtrello-proxy/--compute-state ()
  (should (equal "complete" (orgtrello-proxy/--compute-state *ORGTRELLO/DONE*)))
  (should (equal "incomplete" (orgtrello-proxy/--compute-state "anything-else"))))

(ert-deftest test-orgtrello-proxy/--compute-check ()
  (should (equal t   (orgtrello-proxy/--compute-check *ORGTRELLO/DONE*)))
  (should (equal nil (orgtrello-proxy/--compute-check "anything-else"))))

(ert-deftest test-orgtrello-proxy/--tags-to-labels ()
  (should (equal "a,b,c" (orgtrello-proxy/--tags-to-labels ":a:b:c")))
  (should (equal "a,b,c" (orgtrello-proxy/--tags-to-labels "a:b:c")))
  (should (equal "a," (orgtrello-proxy/--tags-to-labels ":a:")))
  (should (equal "a," (orgtrello-proxy/--tags-to-labels "a:")))
  (should (equal nil  (orgtrello-proxy/--tags-to-labels nil))))

(provide 'org-trello-proxy-tests)
;;; org-trello-proxy-tests.el ends here
