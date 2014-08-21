(require 'org-trello-proxy)
(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(expectations (desc "orgtrello-proxy/--dispatch-action")
              (expect 'orgtrello-proxy/--delete      (orgtrello-proxy/--dispatch-action "delete"))
              (expect 'orgtrello-proxy/--sync-entity (orgtrello-proxy/--dispatch-action "sync-entity"))
              (expect nil                            (orgtrello-proxy/--dispatch-action "nothing")))

(expectations (desc "orgtrello-proxy/--compute-pattern-search-from-marker")
              (expect "marker-is-a-trello-id" (orgtrello-proxy/--compute-pattern-search-from-marker "marker-is-a-trello-id"))
              (expect "orgtrello-marker-tony" (orgtrello-proxy/--compute-pattern-search-from-marker "orgtrello-marker-tony")))

(expectations (desc "orgtrello-proxy/--update-buffer-to-save")
              (expect '(:a) (orgtrello-proxy/--update-buffer-to-save :a nil))
              (expect '(:a) (orgtrello-proxy/--update-buffer-to-save :a '(:a)))
              (expect '(:a :b) (orgtrello-proxy/--update-buffer-to-save :a '(:b))))

(expectations (desc "map-dispatch-delete")
              (expect 'orgtrello-proxy/--card-delete      (gethash *ORGTRELLO/CARD-LEVEL* *MAP-DISPATCH-DELETE*))
              (expect 'orgtrello-proxy/--checklist-delete (gethash *ORGTRELLO/CHECKLIST-LEVEL* *MAP-DISPATCH-DELETE*))
              (expect 'orgtrello-proxy/--item-delete      (gethash *ORGTRELLO/ITEM-LEVEL* *MAP-DISPATCH-DELETE*)))

(expectations (desc "orgtrello-proxy/--card")
              (expect 'orgtrello-proxy/--card      (gethash *ORGTRELLO/CARD-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*))
              (expect 'orgtrello-proxy/--checklist (gethash *ORGTRELLO/CHECKLIST-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*))
              (expect 'orgtrello-proxy/--item      (gethash *ORGTRELLO/ITEM-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*)))

(expectations
 (expect "complete" (orgtrello-proxy/--compute-state *ORGTRELLO/DONE*))
 (expect "incomplete" (orgtrello-proxy/--compute-state "anything-else")))

(expectations
 (expect t   (orgtrello-proxy/--compute-check *ORGTRELLO/DONE*))
 (expect nil (orgtrello-proxy/--compute-check "anything-else")))

(expectations
 (expect "a,b,c" (orgtrello-proxy/--tags-to-labels ":a:b:c"))
 (expect "a,b,c" (orgtrello-proxy/--tags-to-labels "a:b:c"))
 (expect "a," (orgtrello-proxy/--tags-to-labels ":a:"))
 (expect "a," (orgtrello-proxy/--tags-to-labels "a:"))
 (expect nil  (orgtrello-proxy/--tags-to-labels nil)))

(provide 'org-trello-proxy-tests)
;;; org-trello-proxy-tests.el ends here
