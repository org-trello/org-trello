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

(expectations (desc "orgtrello-proxy/update-buffer-to-save!")
              (setq *ORGTRELLO/LIST-BUFFERS-TO-SAVE* nil)
              (expect '(:a) (orgtrello-proxy/update-buffer-to-save! :a))
              (expect '(:a) (orgtrello-proxy/update-buffer-to-save! :a))
              (expect '(:b :a) (orgtrello-proxy/update-buffer-to-save! :b)))

(expectations
 (expect t
         (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                   (:callback "orgtrello-controller/--sync-buffer-with-trello-data-callback" :buffername "todo-TESTS-simple.org" :position nil :params
                                              ((actions . "commentCard")
                                               (field . "closed,desc,due,idBoard,idChecklists,idList,idMembers,name,pos"))
                                              :uri "/boards/51d99bbc1e1d8988390047f2/cards"
                                              :method "GET"
                                              :sync t
                                              :name "some name"
                                              :id "some id"
                                              :action "some action"
                                              :level 3
                                              :start nil))
                     (orgtrello-proxy/parse-query '((callback . "orgtrello-controller/--sync-buffer-with-trello-data-callback")
                                                    (buffername . "todo-TESTS-simple.org")
                                                    (position)
                                                    (params (actions . "commentCard") (field . "closed,desc,due,idBoard,idChecklists,idList,idMembers,name,pos"))
                                                    (uri . "/boards/51d99bbc1e1d8988390047f2/cards")
                                                    (method . "GET")
                                                    (sync . t)
                                                    (name . "some name")
                                                    (id . "some id")
                                                    (action . "some action")
                                                    (level . 3)
                                                    (start . nil))))))

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
