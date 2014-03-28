(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(expectations
 (desc "testing orgtrello-api/make-query")
 (expect :some-method (gethash :method (orgtrello-api/make-query :some-method :some-uri)))
 (expect :some-uri    (gethash :uri    (orgtrello-api/make-query :some-method :some-uri)))
 (expect nil          (gethash :params (orgtrello-api/make-query :some-method :some-uri))))

(expectations (desc "testing orgtrello-api/get-boards")
  (expect "GET"                (gethash :method (orgtrello-api/get-boards)))
  (expect "/members/me/boards" (gethash :uri    (orgtrello-api/get-boards)))
  (expect nil                  (gethash :params (orgtrello-api/get-boards))))

(expectations (desc "testing orgtrello-api/get-board")
  (expect "GET"                                                                                                           (gethash :method (orgtrello-api/get-board :id)))
  (expect "/boards/:id"                                                                                                   (gethash :uri    (orgtrello-api/get-board :id)))
  (expect '(("memberships" . "active") ("memberships_member" . "true") ("fields" . "name,memberships,closed,labelNames")) (gethash :params (orgtrello-api/get-board :id))))

(expectations (desc "testing orgtrello-api/get-cards")
  (expect "GET"                                                                                                       (gethash :method (orgtrello-api/get-cards :board-id)))
  (expect "/boards/:board-id/cards"                                                                                   (gethash :uri    (orgtrello-api/get-cards :board-id)))
  (expect '(("actions" . "commentCard") ("field" . "closed,desc,due,idBoard,idChecklists,idList,idMembers,name,pos")) (gethash :params (orgtrello-api/get-cards :board-id))))

(expectations (desc "orgtrello-api/get-card")
  (expect "GET"                                                                                                  (gethash :method (orgtrello-api/get-card :card-id)))
  (expect "/cards/:card-id"                                                                                      (gethash :uri    (orgtrello-api/get-card :card-id)))
  (expect '(("actions" . "commentCard") ("action_fields" . "data") ("action_memberCreator_fields" . "username")) (gethash :params (orgtrello-api/get-card :card-id))))

(expectations (desc "orgtrello-api/delete-card")
  (expect "DELETE"           (gethash :method (orgtrello-api/delete-card :card-id)))
  (expect "/cards/:card-id" (gethash :uri    (orgtrello-api/delete-card :card-id)))
  (expect nil               (gethash :params (orgtrello-api/delete-card :card-id))))

(expectations (desc "orgtrello-api/get-lists")
  (expect "GET"                      (gethash :method (orgtrello-api/get-lists :board-id)))
  (expect "/boards/:board-id/lists" (gethash :uri    (orgtrello-api/get-lists :board-id)))
  (expect nil                       (gethash :params (orgtrello-api/get-lists :board-id))))

(expectations (desc "orgtrello-api/get-list")
  (expect "GET"              (gethash :method (orgtrello-api/get-list :list-id)))
  (expect "/lists/:list-id" (gethash :uri    (orgtrello-api/get-list :list-id)))
  (expect nil               (gethash :params (orgtrello-api/get-list :list-id))))

(expectations (desc "orgtrello-api/close-list")
  (expect "PUT"                     (gethash :method (orgtrello-api/close-list :list-id)))
  (expect "/lists/:list-id/closed" (gethash :uri    (orgtrello-api/close-list :list-id)))
  (expect '((value . t))           (gethash :params (orgtrello-api/close-list :list-id))))

(expectations (desc "orgtrello-api/add-list")
  (expect "POST"                      (gethash :method (orgtrello-api/add-list "list-name" "board-id")))
  (expect "/lists/"                   (gethash :uri    (orgtrello-api/add-list "list-name" "board-id")))
  (expect '(("name" . "list-name")
            ("idBoard" . "board-id")) (gethash :params (orgtrello-api/add-list "list-name" "board-id"))))


(expectations (desc "orgtrello-api/add-card - name, idList, due-date")
  (expect "POST"                                                              (gethash :method (orgtrello-api/add-card :name-card :id-list "due-date")))
  (expect "/cards/"                                                           (gethash :uri    (orgtrello-api/add-card :name-card :id-list "due-date")))
  (expect '(("due" . "due-date") ("name" . :name-card) ("idList" . :id-list)) (gethash :params (orgtrello-api/add-card :name-card :id-list "due-date"))))

(expectations (desc "orgtrello-api/add-card - name, idList, due-date, idMember")
  (expect "POST"                                                                                                    (gethash :method (orgtrello-api/add-card :name-card :id-list "due-date" "idmember0,idmember1")))
  (expect "/cards/"                                                                                                 (gethash :uri    (orgtrello-api/add-card :name-card :id-list "due-date" "idmember0,idmember1")))
  (expect '(("due" . "due-date") ("idMembers" . "idmember0,idmember1") ("name" . :name-card) ("idList" . :id-list)) (gethash :params (orgtrello-api/add-card :name-card :id-list "due-date" "idmember0,idmember1"))))

(expectations (desc "orgtrello-api/add-card - name, idList, description")
  (expect "POST"                                                                                                    (gethash :method (orgtrello-api/add-card :name-card :id-list nil nil :description)))
  (expect "/cards/"                                                                                                 (gethash :uri    (orgtrello-api/add-card :name-card :id-list nil nil :description)))
  (expect '(("desc" . :description) ("name" . :name-card) ("idList" . :id-list))                                    (gethash :params (orgtrello-api/add-card :name-card :id-list nil nil :description))))

(expectations (desc "orgtrello-api/add-card - name, idList, labels")
  (expect "POST"                                                              (gethash :method (orgtrello-api/add-card :name-card :id-list nil nil nil :labels)))
  (expect "/cards/"                                                           (gethash :uri    (orgtrello-api/add-card :name-card :id-list nil nil nil :labels)))
  (expect '(("labels" . :labels) ("name" . :name-card) ("idList" . :id-list)) (gethash :params (orgtrello-api/add-card :name-card :id-list nil nil nil :labels))))

(expectations (desc "orgtrello-api/move-card - card-id, list-id, name")
  (expect "PUT"                                             (gethash :method (orgtrello-api/move-card :id-card :id-list "name-card")))
  (expect "/cards/:id-card"                                 (gethash :uri    (orgtrello-api/move-card :id-card :id-list "name-card")))
  (expect '(("name"   . "name-card") ("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list "name-card"))))

(expectations (desc "orgtrello-api/move-card - card-id, list-id, name")
  (expect "PUT"                                     (gethash :method (orgtrello-api/move-card :id-card :id-list :name)))
  (expect "/cards/:id-card"                         (gethash :uri    (orgtrello-api/move-card :id-card :id-list :name)))
  (expect '(("name" . :name) ("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list :name))))

(expectations (desc "orgtrello-api/move-card - card-id, list-id")
  (expect "PUT"                    (gethash :method (orgtrello-api/move-card :id-card :id-list)))
  (expect "/cards/:id-card"        (gethash :uri    (orgtrello-api/move-card :id-card :id-list)))
  (expect '(("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list))))

(expectations (desc "orgtrello-api/move-card - card-id, list-id, no name, due date")
  (expect "PUT"                                        (gethash :method (orgtrello-api/move-card :id-card :id-list nil :due-date)))
  (expect "/cards/:id-card"                            (gethash :uri    (orgtrello-api/move-card :id-card :id-list nil :due-date)))
  (expect '(("due" . :due-date) ("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list nil :due-date))))

(expectations (desc "orgtrello-api/move-card - card-id, list-id, name, due-date")
  (expect "PUT"                                                         (gethash :method (orgtrello-api/move-card :id-card :id-list :name :due-date)))
  (expect "/cards/:id-card"                                             (gethash :uri    (orgtrello-api/move-card :id-card :id-list :name :due-date)))
  (expect '(("due" . :due-date) ("name" . :name) ("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list :name :due-date))))

(expectations (desc "orgtrello-api/move-card - card-id, list-id, name, no due-date, idMembers")
  (expect "PUT"                                                                                 (gethash :method (orgtrello-api/move-card :id-card :id-list "name-card" nil "idmember0,idmember1")))
  (expect "/cards/:id-card"                                                                     (gethash :uri    (orgtrello-api/move-card :id-card :id-list "name-card" nil "idmember0,idmember1")))
  (expect '(("idMembers" . "idmember0,idmember1") ("name" . "name-card") ("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list "name-card" nil "idmember0,idmember1"))))

(expectations (desc "orgtrello-api/move-card - card-id, list-id, no name, no due date, no idMembers, description")
  (expect "PUT"                                            (gethash :method (orgtrello-api/move-card :id-card :id-list nil nil nil :description)))
  (expect "/cards/:id-card"                                (gethash :uri    (orgtrello-api/move-card :id-card :id-list nil nil nil :description)))
  (expect '(("desc" . :description) ("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list nil nil nil :description))))

(expectations (desc "orgtrello-api/get-cards-from-list")
  (expect "GET"                    (gethash :method (orgtrello-api/get-cards-from-list :list-id)))
  (expect "/lists/:list-id/cards" (gethash :uri    (orgtrello-api/get-cards-from-list :list-id)))
  (expect nil                     (gethash :params (orgtrello-api/get-cards-from-list :list-id))))

(expectations (desc "orgtrello-api/add-checklist")
  (expect "POST"                          (gethash :method (orgtrello-api/add-checklist "id-card" "name-checklist")))
  (expect "/cards/id-card/checklists"    (gethash :uri    (orgtrello-api/add-checklist "id-card" "name-checklist")))
  (expect '(("name" . "name-checklist")) (gethash :params (orgtrello-api/add-checklist "id-card" "name-checklist"))))

(expectations (desc "orgtrello-api/update-checklist")
  (expect "PUT"                           (gethash :method (orgtrello-api/update-checklist :id-checklist "name-checklist")))
  (expect "/checklists/:id-checklist"    (gethash :uri    (orgtrello-api/update-checklist :id-checklist "name-checklist")))
  (expect '(("name" . "name-checklist")) (gethash :params (orgtrello-api/update-checklist :id-checklist "name-checklist"))))

(expectations (desc "orgtrello-api/delete-checklist")
  (expect "DELETE"                     (gethash :method (orgtrello-api/delete-checklist :id-checklist)))
  (expect "/checklists/:id-checklist" (gethash :uri    (orgtrello-api/delete-checklist :id-checklist)))
  (expect nil                         (gethash :params (orgtrello-api/delete-checklist :id-checklist))))

(expectations (desc "orgtrello-api/get-checklists")
  (expect "GET"                         (gethash :method (orgtrello-api/get-checklists :card-id)))
  (expect "/cards/:card-id/checklists" (gethash :uri    (orgtrello-api/get-checklists :card-id)))
  (expect nil                          (gethash :params (orgtrello-api/get-checklists :card-id))))

(expectations (desc "orgtrello-api/get-checklist")
  (expect "GET"                        (gethash :method (orgtrello-api/get-checklist :checklist-id)))
  (expect "/checklists/:checklist-id" (gethash :uri    (orgtrello-api/get-checklist :checklist-id)))
  (expect nil                         (gethash :params (orgtrello-api/get-checklist :checklist-id))))

(expectations (desc "orgtrello-api/add-items - 1")
  (expect "POST"                                     (gethash :method (orgtrello-api/add-items :checklist-id "item-name" t)))
  (expect "/checklists/:checklist-id/checkItems"     (gethash :uri    (orgtrello-api/add-items :checklist-id "item-name" t)))
  (expect '(("checked" . t) ("name"  . "item-name")) (gethash :params (orgtrello-api/add-items :checklist-id "item-name" t))))

(expectations (desc "orgtrello-api/add-items - 2")
  (expect "POST"                                 (gethash :method (orgtrello-api/add-items :checklist-id "item-name")))
  (expect "/checklists/:checklist-id/checkItems" (gethash :uri    (orgtrello-api/add-items :checklist-id "item-name")))
  (expect '(("name"  . "item-name"))             (gethash :params (orgtrello-api/add-items :checklist-id "item-name"))))

(expectations (desc "orgtrello-api/add-items - 3")
  (expect "POST"                                 (gethash :method (orgtrello-api/add-items :checklist-id "item-name" nil)))
  (expect "/checklists/:checklist-id/checkItems" (gethash :uri    (orgtrello-api/add-items :checklist-id "item-name" nil)))
  (expect '(("name"  . "item-name"))             (gethash :params (orgtrello-api/add-items :checklist-id "item-name" nil))))

(expectations (desc "orgtrello-api/update-item - 1")
  (expect "PUT"                                                        (gethash :method (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name "incomplete")))
  (expect "/cards/:card-id/checklist/:checklist-id/checkItem/:item-id" (gethash :uri    (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name "incomplete")))
  (expect '(("state" ."incomplete") ("name"  . :item-name))            (gethash :params (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name "incomplete"))))

(expectations (desc "orgtrello-api/update-item - 2")
  (expect "PUT"                                                         (gethash :method (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name)))
  (expect "/cards/:card-id/checklist/:checklist-id/checkItem/:item-id" (gethash :uri    (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name)))
  (expect '(("name"  . :item-name))                                    (gethash :params (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name))))

(expectations (desc "orgtrello-api/update-item - 3")
  (expect "PUT"                                                         (gethash :method (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name nil)))
  (expect "/cards/:card-id/checklist/:checklist-id/checkItem/:item-id" (gethash :uri    (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name nil)))
  (expect '(("name"  . :item-name))                                    (gethash :params (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name nil))))

(expectations (desc "orgtrello-api/delete-item")
  (expect "DELETE"                                         (gethash :method (orgtrello-api/delete-item :checklist-id :item-id)))
  (expect "/checklists/:checklist-id/checkItems/:item-id" (gethash :uri    (orgtrello-api/delete-item :checklist-id :item-id))))

(expectations (desc "orgtrello-api/get-items")
  (expect "GET"                                   (gethash :method (orgtrello-api/get-items :checklist-id)))
  (expect "/checklists/:checklist-id/checkItems/" (gethash :uri    (orgtrello-api/get-items :checklist-id))))

(expectations (desc "orgtrello-api/get-item")
  (expect "GET"                                           (gethash :method (orgtrello-api/get-item :checklist-id :item-id)))
  (expect "/checklists/:checklist-id/checkItems/:item-id" (gethash :uri    (orgtrello-api/get-item :checklist-id :item-id))))

(expectations (desc "orgtrello-api/get-member")
  (expect "GET"          (gethash :method (orgtrello-api/get-member :id)))
  (expect "/members/:id" (gethash :uri (orgtrello-api/get-member :id))))

(expectations (desc "orgtrello-api/get-me")
  (expect "GET"         (gethash :method (orgtrello-api/get-me)))
  (expect "/members/me" (gethash :uri (orgtrello-api/get-me))))

(expectations (desc "orgtrello-api/--deal-with-optional-value")
  (expect nil                    (orgtrello-api/--deal-with-optional-value nil nil nil))
  (expect nil                    (orgtrello-api/--deal-with-optional-value nil :a nil))
  (expect :existing-list         (orgtrello-api/--deal-with-optional-value nil :a :existing-list))
  (expect :existing-list         (orgtrello-api/--deal-with-optional-value nil nil :existing-list))
  (expect '(:value-a)            (orgtrello-api/--deal-with-optional-value :a :value-a nil))
  (expect '(:value-a :value-b)   (orgtrello-api/--deal-with-optional-value :a :value-a '(:value-b)))
  (expect '(nil :value-b) (orgtrello-api/--deal-with-optional-value :a nil '(:value-b))))

(expectations (desc "orgtrello-api/--deal-with-optional-values")
  (expect nil                    (orgtrello-api/--deal-with-optional-values '((nil . nil)) nil))
  (expect nil                    (orgtrello-api/--deal-with-optional-values '((nil . :a)) nil))
  (expect :existing-list         (orgtrello-api/--deal-with-optional-values '((nil . :a)) :existing-list))
  (expect :existing-list         (orgtrello-api/--deal-with-optional-values '((nil . nil)) :existing-list))

  (expect '(:value-a)            (orgtrello-api/--deal-with-optional-values '((:a . :value-a)) nil))
  (expect '(:value-a :value-b)   (orgtrello-api/--deal-with-optional-values '((:a . :value-a)) '(:value-b)))
  (expect '(nil :value-b)        (orgtrello-api/--deal-with-optional-values '((:a . nil)) '(:value-b))))

(expectations (desc "orgtrello-api/--deal-with-optional-values")
  (expect nil                           (orgtrello-api/--deal-with-optional-values '((nil . nil) (nil . nil)) nil))
  (expect nil                           (orgtrello-api/--deal-with-optional-values '((nil . :a)  (nil . :a)) nil))
  (expect :existing-list                (orgtrello-api/--deal-with-optional-values '((nil . :a) (nil . :a)) :existing-list))
  (expect :existing-list                (orgtrello-api/--deal-with-optional-values '((nil . nil) (nil . nil)) :existing-list))

  (expect '(:value-c :value-a)          (orgtrello-api/--deal-with-optional-values '((:a . :value-a) (:c . :value-c)) nil))
  (expect '(:value-c :value-a :value-b) (orgtrello-api/--deal-with-optional-values '((:a . :value-a) (:c . :value-c)) '(:value-b)))
  (expect '(nil nil :value-b)           (orgtrello-api/--deal-with-optional-values '((:a . nil) (:c . nil)) '(:value-b))))

(expectations (desc "orgtrello-api/add-board")
  (expect "POST"                      (gethash :method (orgtrello-api/add-board ":some-board")))
  (expect "/boards"                   (gethash :uri    (orgtrello-api/add-board ":some-board")))
  (expect '(("name" . ":some-board")) (gethash :params (orgtrello-api/add-board ":some-board"))))

(expectations (desc "orgtrello-api/add-board")
  (expect "POST"                           (gethash :method (orgtrello-api/add-board "some-board" "some-description")))
  (expect "/boards"                        (gethash :uri    (orgtrello-api/add-board "some-board" "some-description")))
  (expect '(("desc" . "some-description")
            ("name" . "some-board")) (gethash :params (orgtrello-api/add-board "some-board" "some-description"))))

(expectations (desc "orgtrello-api/add-card")
  (expect "POST"                                           (gethash :method (orgtrello-api/add-card "card-name" "list-id")))
  (expect "/cards/"                                        (gethash :uri    (orgtrello-api/add-card "card-name" "list-id")))
  (expect '(("name" . "card-name") ("idList" . "list-id")) (gethash :params (orgtrello-api/add-card "card-name" "list-id"))))

(expectations (desc "orgtrello-api/add-card")
  (expect "POST"                                                                (gethash :method (orgtrello-api/add-card "card-name" "list-id" "due-date")))
  (expect "/cards/"                                                             (gethash :uri    (orgtrello-api/add-card "card-name" "list-id" "due-date")))
  (expect '(("due" . "due-date") ("name" . "card-name") ("idList" . "list-id")) (gethash :params (orgtrello-api/add-card "card-name" "list-id" "due-date"))))

(expectations
 (desc "orgtrello-api/add-card-comment")
 (expect "POST"                             (gethash :method (orgtrello-api/add-card-comment :card-id "some comment text")))
 (expect "/cards/:card-id/actions/comments" (gethash :uri    (orgtrello-api/add-card-comment :card-id "some comment text")))
 (expect '(("text" . "some comment text"))  (gethash :params (orgtrello-api/add-card-comment :card-id "some comment text"))))
