(require 'cl-lib)
(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(message "Launching tests!")

(load-file "load-namespaces.el")

;; ########################## util test function

(defun hash-equal (hash1 hash2) "Compare two hash tables to see whether they are equal."
  (and (= (hash-table-count hash1) (hash-table-count hash2))
       (catch 'flag (maphash (lambda (x y) (or (equal (gethash x hash2) y) (throw 'flag nil))) hash1)
              (throw 'flag t))))

(expectations (desc "hash-equal")
 (expect t (hash-equal (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO")))
                       (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO")))))
 (expect nil (hash-equal (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO")))
                         (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "DONE"))))))

(expectations (desc "orgtrello-hash/make-transpose-properties")
  (expect t (hash-equal (orgtrello-hash/make-properties `(("some other name" . :name) ("TODO" . :keyword)))
                        (orgtrello-hash/make-transpose-properties `((:name . "some other name") (:keyword . "TODO"))))))

(expectations (desc "orgtrello-hash/empty-hash")
 (expect t (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ())
                       (orgtrello-hash/empty-hash))))

;; ########################## orgtrello-hash

(expectations (desc "testing orgtrello-hash/make-hash-org")
  (expect "some name"       (gethash :name           (orgtrello-hash/make-hash-org "" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc)))
  (expect "IN PROGRESS"     (gethash :keyword        (orgtrello-hash/make-hash-org "" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc)))
  (expect 0                 (gethash :level          (orgtrello-hash/make-hash-org "" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc)))
  (expect "some id"         (gethash :id             (orgtrello-hash/make-hash-org "" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc)))
  (expect "due-date"        (gethash :due            (orgtrello-hash/make-hash-org "" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc)))
  (expect :point            (gethash :position       (orgtrello-hash/make-hash-org "" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc)))
  (expect "buffer-name.org" (gethash :buffername     (orgtrello-hash/make-hash-org "" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc)))
  (expect "1,2,3"           (gethash :member-ids     (orgtrello-hash/make-hash-org "1,2,3" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc)))
  (expect :desc             (gethash :desc           (orgtrello-hash/make-hash-org "1,2,3" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc))))

(expectations (desc "testing orgtrello-hash/make-hash")
  (expect :some-method (gethash :method (orgtrello-hash/make-hash :some-method :some-uri)))
  (expect :some-uri    (gethash :uri    (orgtrello-hash/make-hash :some-method :some-uri)))
  (expect nil          (gethash :params (orgtrello-hash/make-hash :some-method :some-uri))))

;; ########################## orgtrello-data

(expectations (desc "testing orgtrello-data/--convert-to-orgtrello-metadata")
  (expect "some name :orgtrello-id-identifier:"  (gethash :name       (orgtrello-data/--convert-to-orgtrello-metadata '("" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect "IN PROGRESS"                          (gethash :keyword    (orgtrello-data/--convert-to-orgtrello-metadata '("" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect 0                                      (gethash :level      (orgtrello-data/--convert-to-orgtrello-metadata '("" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect :id                                    (gethash :id         (orgtrello-data/--convert-to-orgtrello-metadata '("" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect :due                                   (gethash :due        (orgtrello-data/--convert-to-orgtrello-metadata '("" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect :point                                 (gethash :position   (orgtrello-data/--convert-to-orgtrello-metadata '("" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect "1,2,3"                                (gethash :member-ids (orgtrello-data/--convert-to-orgtrello-metadata '("" "1,2,3" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect :desc                                  (gethash :desc       (orgtrello-data/--convert-to-orgtrello-metadata '(:desc "1,2,3" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil)))))

(expectations (desc "testing orgtrello-data/--convert-orgmode-date-to-trello-date")
  (expect "2013-07-18T02:00:00.000Z" (orgtrello-data/--convert-orgmode-date-to-trello-date "2013-07-18T02:00:00.000Z"))
  (expect "2013-07-29T14:00:00.000Z" (orgtrello-data/--convert-orgmode-date-to-trello-date "2013-07-29 lun. 14:00"))
  (expect "2013-07-29T00:00:00.000Z" (orgtrello-data/--convert-orgmode-date-to-trello-date "2013-07-29"))
  (expect nil                        (orgtrello-data/--convert-orgmode-date-to-trello-date nil)))

;; ########################## orgtrello-api

(expectations (desc "testing orgtrello-api/get-boards")
  (expect "GET"                (gethash :method (orgtrello-api/get-boards)))
  (expect "/members/me/boards" (gethash :uri    (orgtrello-api/get-boards)))
  (expect nil                  (gethash :params (orgtrello-api/get-boards))))

(expectations (desc "testing orgtrello-api/get-board")
  (expect "GET"                                                                                                (gethash :method (orgtrello-api/get-board :id)))
  (expect "/boards/:id"                                                                                        (gethash :uri    (orgtrello-api/get-board :id)))
  (expect '(("memberships" . "active") ("memberships_member" . "true") ("fields" . "name,memberships,closed")) (gethash :params (orgtrello-api/get-board :id))))

(expectations (desc "testing orgtrello-api/get-board")
  (expect "GET"                     (gethash :method (orgtrello-api/get-cards :board-id)))
  (expect "/boards/:board-id/cards" (gethash :uri    (orgtrello-api/get-cards :board-id)))
  (expect nil                       (gethash :params (orgtrello-api/get-cards :board-id))))

(expectations (desc "orgtrello-api/get-card")
  (expect "GET"              (gethash :method (orgtrello-api/get-card :card-id)))
  (expect "/cards/:card-id" (gethash :uri    (orgtrello-api/get-card :card-id)))
  (expect nil               (gethash :params (orgtrello-api/get-card :card-id))))

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
  (expect "GET"                                    (gethash :method (orgtrello-api/get-items :checklist-id)))
  (expect "/checklists/:checklist-id/checkItems/" (gethash :uri    (orgtrello-api/get-items :checklist-id))))

(expectations (desc "orgtrello-api/get-member")
  (expect "GET"          (gethash :method (orgtrello-api/get-member :id)))
  (expect "/members/:id" (gethash :uri (orgtrello-api/get-member :id))))

(expectations (desc "orgtrello-api/get-me")
  (expect "GET"         (gethash :method (orgtrello-api/get-me)))
  (expect "/members/me" (gethash :uri (orgtrello-api/get-me))))

;; ########################## orgtrello-query

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

;; ########################## orgtrello-tests

(ert-deftest testing-orgtrello-controller/--compute-data-from-entity-meta ()
  (let* ((entry   (orgtrello-hash/make-hash-org :member-ids :some-level :some-keyword :some-name "some-id" :some-due :some-point :some-buffername :desc)))
    (should (equal (orgtrello-data/entity-id entry)          "some-id"))
    (should (equal (orgtrello-data/entity-name entry)        :some-name))
    (should (equal (orgtrello-data/entity-keyword entry)     :some-keyword))
    (should (equal (orgtrello-data/entity-level entry)       :some-level))
    (should (equal (orgtrello-data/entity-due entry)         :some-due))
    (should (equal (orgtrello-data/entity-position entry)    :some-point))
    (should (equal (orgtrello-data/entity-buffername entry)  :some-buffername))
    (should (equal (orgtrello-data/entity-member-ids entry)  :member-ids))
    (should (equal (orgtrello-data/entity-description entry) :desc))))

(ert-deftest testing-orgtrello-controller/--id-name ()
  (let* ((entities (orgtrello-data/parse-data [((id . "id")
                                                 (shortUrl . "https://trello.com/b/ePrdEnzC")
                                                 (url . "https://trello.com/board/devops/4f96a984dbb00d733b04d8b5")
                                                 (name . "testing board"))
                                                ((id . "another-id")
                                                 (shortUrl . "https://trello.com/b/ePrdEnzC")
                                                 (url . "https://trello.com/board/devops/4f96a984dbb00d733b04d8b5")
                                                 (name . "testing board 2"))
                                                ((id . "yet-another-id")
                                                 (shortUrl . "https://trello.com/b/ePrdEnzC")
                                                 (url . "https://trello.com/board/devops/4f96a984dbb00d733b04d8b5")
                                                 (name . "testing board 3"))]))
         (hashtable-result (orgtrello-controller/--id-name entities))
         (hashtable-expected (make-hash-table :test 'equal)))
    (puthash "id" "testing board" hashtable-expected)
    (puthash "another-id" "testing board 2" hashtable-expected)
    (puthash "yet-another-id" "testing board 3" hashtable-expected)
    (should (equal (gethash "id" hashtable-result) (gethash "id" hashtable-expected)))
    (should (equal (gethash "another-id" hashtable-result) (gethash "another-id" hashtable-expected)))
    (should (equal (gethash "yet-another-id" hashtable-result) (gethash "yet-another-id" hashtable-expected)))
    (should (equal (hash-table-count hashtable-result) (hash-table-count hashtable-expected)))))

(ert-deftest testing-orgtrello-controller/--name-id ()
  (let* ((entities (orgtrello-data/parse-data [((id . "id")
                                                 (shortUrl . "https://trello.com/b/ePrdEnzC")
                                                 (name . "testing board"))
                                                ((id . "another-id")
                                                 (shortUrl . "https://trello.com/b/ePrdEnzC")
                                                 (name . "testing board 2"))
                                                ((id . "yet-another-id")
                                                 (shortUrl . "https://trello.com/b/ePrdEnzC")
                                                 (name . "testing board 3"))]))
         (hashtable-result (orgtrello-controller/--name-id entities))
         (hashtable-expected (make-hash-table :test 'equal)))
    (puthash "testing board" "id" hashtable-expected)
    (puthash "testing board 2" "another-id"  hashtable-expected)
    (puthash "testing board 3" "yet-another-id"  hashtable-expected)
    (should (equal (gethash "testing board" hashtable-result) (gethash "testing board" hashtable-expected)))
    (should (equal (gethash "testing board 2" hashtable-result) (gethash "testing board 2" hashtable-expected)))
    (should (equal (gethash "testing board 3" hashtable-result) (gethash "testing board 3" hashtable-expected)))
    (should (equal (hash-table-count hashtable-result) (hash-table-count hashtable-expected)))))

(expectations (desc "orgtrello-controller/--compute-state-from-keyword")
  (expect 'none (orgtrello-controller/--compute-state-from-keyword ""))
  (expect 'none (orgtrello-controller/--compute-state-from-keyword *TODO*))
  (expect 'done (orgtrello-controller/--compute-state-from-keyword *DONE*))
  (expect 'none (orgtrello-controller/--compute-state-from-keyword "IN")))

(expectations (desc "orgtrello-controller/--item-compute-state")
  (expect "complete" (orgtrello-controller/--item-compute-state t "DONE" "DONE"))
  (expect "complete" (orgtrello-controller/--item-compute-state t "TODO" "DONE"))
  (expect "incomplete" (orgtrello-controller/--item-compute-state t "DONE" "TODO"))
  (expect "incomplete" (orgtrello-controller/--item-compute-state t "TODO" "TODO"))
  (expect "complete" (orgtrello-controller/--item-compute-state nil "DONE" "DONE"))
  (expect "incomplete" (orgtrello-controller/--item-compute-state nil "TODO" "DONE"))
  (expect "complete" (orgtrello-controller/--item-compute-state nil "DONE" "TODO"))
  (expect "incomplete" (orgtrello-controller/--item-compute-state nil "TODO" "TODO")) )

(expectations (desc "orgtrello-controller/--item-compute-check")
  (expect t (orgtrello-controller/--item-compute-check t "DONE" "DONE"))
  (expect t (orgtrello-controller/--item-compute-check t "TODO" "DONE"))
  (expect nil (orgtrello-controller/--item-compute-check t "DONE" "TODO"))
  (expect nil (orgtrello-controller/--item-compute-check t "TODO" "TODO"))
  (expect t (orgtrello-controller/--item-compute-check nil "DONE" "DONE"))
  (expect nil (orgtrello-controller/--item-compute-check nil "TODO" "DONE"))
  (expect t (orgtrello-controller/--item-compute-check nil "DONE" "TODO"))
  (expect nil (orgtrello-controller/--item-compute-check nil "TODO" "TODO")) )

(expectations (desc "orgtrello-elnode/--dictionary-lessp")
 (expect t (and (orgtrello-elnode/--dictionary-lessp "a" "b")
                (null (orgtrello-elnode/--dictionary-lessp "b" "a"))
                (null (orgtrello-elnode/--dictionary-lessp "a" "a"))
                (orgtrello-elnode/--dictionary-lessp "1" "2")
                (null (orgtrello-elnode/--dictionary-lessp "2" "1"))
                (null (orgtrello-elnode/--dictionary-lessp "1" "1"))
                (orgtrello-elnode/--dictionary-lessp "1" "a")
                (null (orgtrello-elnode/--dictionary-lessp "a" "1"))
                (orgtrello-elnode/--dictionary-lessp "" "a")
                (null (orgtrello-elnode/--dictionary-lessp "a" ""))

                (orgtrello-elnode/--dictionary-lessp "ab12" "ab34")
                (orgtrello-elnode/--dictionary-lessp "ab12" "ab123")
                (orgtrello-elnode/--dictionary-lessp "ab12" "ab12d")
                (orgtrello-elnode/--dictionary-lessp "ab132" "ab132z")

                (orgtrello-elnode/--dictionary-lessp "132zzzzz" "ab132z")
                (null (orgtrello-elnode/--dictionary-lessp "1.32" "1ab")))))

(expectations (desc "orgtrello-proxy/--dispatch-action")
 (expect 'orgtrello-proxy/--delete      (orgtrello-proxy/--dispatch-action "delete"))
 (expect 'orgtrello-proxy/--sync-entity (orgtrello-proxy/--dispatch-action "sync-entity"))
 (expect nil                            (orgtrello-proxy/--dispatch-action "nothing")))

(expectations (desc "orgtrello-controller/--compute-marker-from-entry")
  (expect "id"                                                        (orgtrello-controller/--compute-marker-from-entry (orgtrello-hash/make-hash-org :users :level :kwd :name      "id"  :due :position :buffername :desc)))
  (expect "orgtrello-marker-2a0b98e652ce6349a0659a7a8eeb3783ffe9a11a" (orgtrello-controller/--compute-marker-from-entry (orgtrello-hash/make-hash-org :users :level :kwd "some-name" nil :due 1234      "buffername" :desc)))
  (expect "orgtrello-marker-6c59c5dcf6c83edaeb3f4923bfd929a091504bb3" (orgtrello-controller/--compute-marker-from-entry (orgtrello-hash/make-hash-org :users :level :kwd "some-name" nil :due 4321      "some-other-buffername" :desc))))

(expectations (desc "orgtrello-marker-2a0b98e652ce6349a0659a7a8eeb3783ffe9a11a")
  (expect "orgtrello-marker-2a0b98e652ce6349a0659a7a8eeb3783ffe9a11a" (orgtrello-controller/compute-marker "buffername" "some-name" 1234))
  (expect "orgtrello-marker-6c59c5dcf6c83edaeb3f4923bfd929a091504bb3" (orgtrello-controller/compute-marker "some-other-buffername" "some-name" 4321)))

(expectations (desc "orgtrello-proxy/--compute-pattern-search-from-marker")
  (expect "marker-is-a-trello-id" (orgtrello-proxy/--compute-pattern-search-from-marker "marker-is-a-trello-id"))
  (expect "orgtrello-marker-tony" (orgtrello-proxy/--compute-pattern-search-from-marker "orgtrello-marker-tony")))

(expectations (desc "orgtrello-controller/id-p")
  (expect t   (orgtrello-controller/id-p "anything-that-does-not-start-with-orgtrello-marker"))
  (expect t   (orgtrello-controller/id-p "agfgdsfgbdfgbdfgbdfshouldbetrue"))
  (expect t   (orgtrello-controller/id-p "orgtrello-markeragfgdsfgbdfgbdfgbdfshouldbetrue"))
  (expect t   (orgtrello-controller/id-p "should-be-true-orgtrello-marker-agfgdsfgbdfgbdfgbdf"))
  (expect nil (orgtrello-controller/id-p "orgtrello-marker-shouldbenil"))
  (expect nil (orgtrello-controller/id-p nil)))

(expectations (desc "compose-fn")
  (expect '(3 5 7) (--map (funcall (compose-fn '((lambda (it) (+ 1 it)) (lambda (it) (* 2 it)))) it) '(1 2 3))))

(expectations (desc "orgtrello-webadmin/--detail-entity")
  (expect "entity name" (orgtrello-webadmin/--detail-entity 3 (orgtrello-hash/make-properties '((:name . "entity name")))))
  (expect "entity name" (gethash :name (orgtrello-webadmin/--detail-entity 5 (orgtrello-hash/make-properties '((:name . "entity name")))))))

(expectations (desc "orgtrello-action/--filter-error-message")
 (expect '("error0" "error1") (orgtrello-action/--filter-error-messages '("error0" :ok "error1")))
 (expect nil                  (orgtrello-action/--filter-error-messages '(:ok :ok :ok))))

(expectations
  (expect '(:ok) (orgtrello-action/--execute-controls '((lambda (e) :ok))))
  (expect '(:ok "ko") (orgtrello-action/--execute-controls '((lambda (e) :ok)
                                                       (lambda (e) "ko"))))
  (expect '(:ok) (orgtrello-action/--execute-controls '((lambda (a) :ok)) 'args))
  (expect '(:ok "ko") (orgtrello-action/--execute-controls '((lambda (a) :ok)
                                                       (lambda (a) "ko")) 'arg0)))

(expectations  (desc "orgtrello-action/--function-controls-then-do - 1")
  (expect   "List of errors:
 - Level too high. Do not deal with entity other than card/checklist/items!
"
      (orgtrello-action/--functional-controls-then-do
       '(orgtrello-controller/--right-level-p)
       (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org :users 4 :kwd :name nil :due :position :buffer-name :desc))
       (lambda (entity s) (format "%S %s" entity s))
       "- hello"))

  (expect "#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:current #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:buffername :buffer-name :position :position :level 3 :keyword :kwd :name :name :id nil :due :due :member-ids :users :desc :desc)) :parent nil :grandparent nil)) - hello"
    (orgtrello-action/--functional-controls-then-do
     '(orgtrello-controller/--right-level-p)
     (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org :users 3 :kwd :name nil :due :position :buffer-name :desc))
     (lambda (entity s) (format "%S %s" entity s))
     "- hello")))

(expectations (desc "orgtrello-action/--function-controls-then-do - 2")
  (expect "List of errors:
 - Entity must been synchronized with trello first!
"
    (orgtrello-action/--functional-controls-then-do
     '(orgtrello-controller/--right-level-p orgtrello-controller/--already-synced-p)
     (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org :users 1 :kwd :name nil :due :position :buffer-name :desc))
     (lambda (entity s) (format "%S %s" entity s))
     "- hello"))
  (expect "#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:current #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:buffername :buffer-name :position :position :level 1 :keyword :kwd :name :name :id \"some-id\" :due :due :member-ids :users :desc :desc)) :parent nil :grandparent nil)) - hello"

    (orgtrello-action/--functional-controls-then-do
     '(orgtrello-controller/--right-level-p orgtrello-controller/--already-synced-p)
     (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org :users 1 :kwd :name "some-id" :due :position :buffer-name :desc))
     (lambda (entity s) (format "%S %s" entity s))
     "- hello")))

(expectations (desc "orgtrello-action/--compute-error-message")
  (expect "- message 1\n- message 2\n" (orgtrello-action/--compute-error-message '("message 1" "message 2"))))

(expectations (desc "orgtrello-hash/key")
 (expect ":key:" (orgtrello-hash/key "key")))

(expectations (desc "orgtrello-cbx/--level")
  (expect 2 (orgtrello-cbx/--level '("-" "[X]" "call" "people" "[4/4]")))
  (expect 3 (orgtrello-cbx/--level '("" "-" "[X]" "Peter")))
  (expect 3 (orgtrello-cbx/--level '("" "" "-" "[X]" "Peter")))
  (expect 3 (orgtrello-cbx/--level '("" "" "" "-" "[X]" "Peter")))
  (expect 4 (orgtrello-cbx/--level '("" "" "" "" "" "Peter"))))

(expectations (desc "orgtrello-cbx/--status")
  (expect "DONE" (orgtrello-cbx/--status"[X]"))
  (expect "TODO" (orgtrello-cbx/--status"[ ]"))
  (expect "TODO" (orgtrello-cbx/--status"[]"))
  (expect "TODO" (orgtrello-cbx/--status"[-]"))
  (expect "TODO" (orgtrello-cbx/--status"")))

(expectations (desc "orgtrello-cbx/--org-split-data")
  (expect '("-" "[X]" "call" "people" "[4/4]")                                             (orgtrello-cbx/--org-split-data "- [X] call people [4/4]"))
  (expect '("-" "[X]" "call" "people" "[4/4]" ":PROPERTIES:" "{\"orgtrello-id\":\"456\"}") (orgtrello-cbx/--org-split-data "- [X] call people [4/4] :PROPERTIES: {\"orgtrello-id\":\"456\"}"))
  (expect '("" "" "-" "[X]" "Peter")                                                       (orgtrello-cbx/--org-split-data "  - [X] Peter"))
  (expect '("" "" "-" "[X]" "Peter" ":PROPERTIES:" "{\"orgtrello-id\":\"456\"}")           (orgtrello-cbx/--org-split-data "  - [X] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}"))

  (expect '("-" "[]" "call" "people" "[4/4]")                                             (orgtrello-cbx/--org-split-data "- [] call people [4/4]"))
  (expect '("-" "[]" "call" "people" "[4/4]" ":PROPERTIES:" "{\"orgtrello-id\":\"456\"}") (orgtrello-cbx/--org-split-data "- [] call people [4/4] :PROPERTIES: {\"orgtrello-id\":\"456\"}"))
  (expect '("" "" "-" "[]" "Peter")                                                       (orgtrello-cbx/--org-split-data "  - [] Peter"))
  (expect '("" "" "-" "[]" "Peter" ":PROPERTIES:" "{\"orgtrello-id\":\"456\"}")           (orgtrello-cbx/--org-split-data "  - [] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}"))

  (expect '("-" "[]" "call" "people" "[4/4]")                                             (orgtrello-cbx/--org-split-data "- [ ] call people [4/4]"))
  (expect '("-" "[]" "call" "people" "[4/4]" ":PROPERTIES:" "{\"orgtrello-id\":\"456\"}") (orgtrello-cbx/--org-split-data "- [ ] call people [4/4] :PROPERTIES: {\"orgtrello-id\":\"456\"}"))
  (expect '("" "" "-" "[]" "Peter")                                                       (orgtrello-cbx/--org-split-data "  - [ ] Peter"))
  (expect '("" "" "-" "[]" "Peter" ":PROPERTIES:" "{\"orgtrello-id\":\"456\"}")           (orgtrello-cbx/--org-split-data "  - [ ] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}")))

(expectations (desc "orgtrello-cbx/--retrieve-status")
  (expect "[X]" (orgtrello-cbx/--retrieve-status '("" "" "-" "[X]" "Peter")))
  (expect "[]" (orgtrello-cbx/--retrieve-status '("" "" "-" "[]" "Peter")))
  (expect "[-]" (orgtrello-cbx/--retrieve-status '("" "" "-" "[-]" "Peter")))
  (expect "[ ]" (orgtrello-cbx/--retrieve-status '("" "" "-" "[ ]" "Peter"))))

(expectations (desc "orgtrello-cbx/--name")
  (expect "call people [4/4]" (orgtrello-cbx/--name "- [X] call people [4/4]"   "[X]"))
  (expect "call people [4/4]" (orgtrello-cbx/--name "- [] call people [4/4]"    "[]"))
  (expect "call people [4/4]" (orgtrello-cbx/--name "- [-] call people [4/4]"   "[-]"))
  (expect "call people [4/4]" (orgtrello-cbx/--name "-[X] call people [4/4]"    "[X]"))
  (expect "call people [4/4]" (orgtrello-cbx/--name "-[] call people [4/4]"     "[]"))
  (expect "call people [4/4]" (orgtrello-cbx/--name "-[-] call people [4/4]"    "[-]"))
  (expect "call people [4/4]" (orgtrello-cbx/--name "  - [X] call people [4/4]" "[X]"))
  (expect "call people [4/4]" (orgtrello-cbx/--name "  - [] call people [4/4]"  "[]"))
  (expect "call people [4/4]" (orgtrello-cbx/--name "  - [-] call people [4/4]" "[-]"))
  (expect "call people [4/4]" (orgtrello-cbx/--name "  -[X] call people [4/4]"  "[X]"))
  (expect "call people [4/4]" (orgtrello-cbx/--name "  -[] call people [4/4]"   "[]"))
  (expect "call people [4/4]" (orgtrello-cbx/--name "  -[-] call people [4/4]"  "[-]")))

(expectations (desc "orgtrello-id\\\":\\\"123\\\"}")
  (expect "{\"orgtrello-id\":\"123\"}"                               (orgtrello-cbx/--to-properties `((,*ORGTRELLO-ID* . "123"))))
  (expect "{\"orgtrello-id\":\"456\"}"                               (orgtrello-cbx/--to-properties `((,*ORGTRELLO-ID* . "123") (,*ORGTRELLO-ID* . "456"))))
  (expect "{\"orgtrello-id\":\"def\", \"orgtrello-marker\":\"456\", \"orgtrello-id\":\"abc\"}" (orgtrello-cbx/--to-properties `(("orgtrello-id" . "abc") (orgtrello-marker . "456") (orgtrello-id . "def"))))
  (expect "{\"orgtrello-marker\":\"456\", \"orgtrello-id\":\"def\"}" (orgtrello-cbx/--to-properties `(("orgtrello-id" . "abc") (orgtrello-marker . "456") ("orgtrello-id" . "def"))))
  (expect "{\"orgtrello-marker\":\"456\", \"orgtrello-id\":\"def\"}" (orgtrello-cbx/--to-properties `((orgtrello-id . "abc") (orgtrello-marker . "456") (orgtrello-id . "def")))))

(expectations  (desc "orgtrello-cbx/--from-properties")
  (expect '((orgtrello-id . "123")) (orgtrello-cbx/--from-properties "{\"orgtrello-id\":\"123\"}")))

(expectations (desc "orgtrello-cbx/--read-properties")
  (expect '((orgtrello-id . "123")) (orgtrello-cbx/--read-properties "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}")))

(expectations (desc "orgtrello-cbx/--update-properties")
  (expect "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}"
    (orgtrello-cbx/--update-properties "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"abc\"}" `((,*ORGTRELLO-ID* . "123"))))
  (expect "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"456\"}"
    (orgtrello-cbx/--update-properties "- [X] some checkbox" `((,*ORGTRELLO-ID* . "456"))))
  (expect "- [X] some checkbox :PROPERTIES: {}"
    (orgtrello-cbx/--update-properties "- [X] some checkbox" nil))
  (expect "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"789\"}"
    (orgtrello-cbx/--update-properties "- [X] some checkbox :PROPERTIES:" `((,*ORGTRELLO-ID* . "789"))))
  (expect "- [X] some checkbox :PROPERTIES: {}"
    (orgtrello-cbx/--update-properties "- [X] some checkbox :PROPERTIES:" nil)))

(expectations (desc "orgtrello-cbx/--org-get-property")
  (expect "123"    (orgtrello-cbx/--org-get-property "orgtrello-id" `((orgtrello-id . "123"))))
  (expect nil      (orgtrello-cbx/--org-get-property "orgtrello-id" `(("orgtrello-id" . "123"))))
  (expect nil      (orgtrello-cbx/--org-get-property 'orgtrello-id `(("orgtrello-id" . "123"))))
  (expect "123"    (orgtrello-cbx/--org-get-property 'orgtrello-id `((orgtrello-id . "123"))))
  (expect "marker" (orgtrello-cbx/--org-get-property "orgtrello-marker" `(("orgtrello-id" . "123") (orgtrello-marker . "marker")))))

(expectations (desc "orgtrello-cbx/--org-update-property")
  (expect `((orgtrello-id . "10") (orgtrello-marker . "123"))    (orgtrello-cbx/--org-update-property "orgtrello-id" "10" `((orgtrello-marker . "123"))))
  (expect `((orgtrello-toto . "abc") (orgtrello-marker . "456")) (orgtrello-cbx/--org-update-property "orgtrello-toto" "abc" `((orgtrello-marker . "456"))))
  (expect `((orgtrello-id . "abc") (orgtrello-marker . "456"))   (orgtrello-cbx/--org-update-property "orgtrello-id" "abc" `((orgtrello-marker . "456") (orgtrello-id . "def")))))

(expectations (desc "orgtrello-cbx/--org-delete-property")
  (expect `(("orgtrello-id" . "123") (orgtrello-marker . "marker")) (orgtrello-cbx/--org-delete-property "orgtrello-id" `(("orgtrello-id" . "123") (orgtrello-marker . "marker"))))
  (expect `((orgtrello-marker . "marker")) (orgtrello-cbx/--org-delete-property "orgtrello-id" `((orgtrello-id . "123") (orgtrello-marker . "marker"))))
  (expect `((orgtrello-marker . "marker")) (orgtrello-cbx/--org-delete-property 'orgtrello-id `((orgtrello-id . "123") (orgtrello-marker . "marker"))))
  (expect `(("orgtrello-id" . "123") (orgtrello-marker . "marker")) (orgtrello-cbx/--org-delete-property 'orgtrello-id `(("orgtrello-id" . "123") (orgtrello-marker . "marker")))))

(expectations (desc "orgtrello-cbx/--checkbox-split")
  (expect '("  - [X] Peter " " {\"orgtrello-id\":\"456\"}") (orgtrello-cbx/--checkbox-split "  - [X] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}")))

(expectations (desc "orgtrello-id\\\":\\\"456\\\"}")
  (expect "{\"orgtrello-id\":\"456\"}" (orgtrello-cbx/--checkbox-metadata "  - [X] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}"))
  (expect ""                           (orgtrello-cbx/--checkbox-metadata "  - [X] Peter :PROPERTIES:"))
  (expect ""                           (orgtrello-cbx/--checkbox-metadata "  - [X] Peter :PROPERTIES: "))
  (expect nil                          (orgtrello-cbx/--checkbox-metadata "  - [X] Peter")))

(expectations (desc "orgtrello-cbx/--checkbox-data")
  (expect "  - [X] Peter" (orgtrello-cbx/--checkbox-data "  - [X] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}")))

(expectations (desc "orgtrello-cbx/--key-to-search")
  (expect 'some-key (orgtrello-cbx/--key-to-search "some-key"))
  (expect 'some-key (orgtrello-cbx/--key-to-search 'some-key))
  (expect :some-key (orgtrello-cbx/--key-to-search :some-key)))

(expectations (desc "orgtrello-cbx/--list-is-checkbox-p")
  (expect nil (orgtrello-cbx/--list-is-checkbox-p '("1" "2" "3")))
  (expect nil (orgtrello-cbx/--list-is-checkbox-p '("1" "2" "3" "-")))
  (expect t   (orgtrello-cbx/--list-is-checkbox-p '("-" "1" "2" "3")))
  (expect t   (orgtrello-cbx/--list-is-checkbox-p '("" "" "-" "1" "2" "3"))))

(expectations (desc "orgtrello-cbx/--get-level")
  (expect 1 (orgtrello-cbx/--get-level '(1 2 3)))
  (expect 2 (orgtrello-cbx/--get-level '(2 3))))

(expectations (desc "orgtrello-controller/--compute-due-date")
  (expect "DEADLINE: <some-date>
" (orgtrello-controller/--compute-due-date "some-date"))
  (expect "" (orgtrello-controller/--compute-due-date nil)))

(expectations (desc "orgtrello-controller/--private-compute-card-to-org-entry")
  (expect "* name TODO
DEADLINE: <some-date>
" (orgtrello-controller/--private-compute-card-to-org-entry "TODO" "name" "some-date"))
  (expect "* name TODO
" (orgtrello-controller/--private-compute-card-to-org-entry "TODO" "name" nil)))

(expectations (desc "orgtrello-controller/--compute-checklist-to-orgtrello-entry")
  (expect "** name\n" (orgtrello-controller/--compute-checklist-to-orgtrello-entry "name"))
  (expect "** name\n" (orgtrello-controller/--compute-checklist-to-orgtrello-entry "name" nil))
  (expect "** name\n" (orgtrello-controller/--compute-checklist-to-orgtrello-entry "name" 't))
  (expect "** name\n" (orgtrello-controller/--compute-checklist-to-orgtrello-entry "name" nil 't))
  (expect "** name\n" (orgtrello-controller/--compute-checklist-to-orgtrello-entry "name" 't nil))
  (expect "** name\n" (orgtrello-controller/--compute-checklist-to-orgtrello-entry "name" nil nil))
  (expect "** name\n" (orgtrello-controller/--compute-checklist-to-orgtrello-entry "name" 't 't)))

(expectations (desc "orgtrello-controller/--symbol")
  (expect ""      (orgtrello-controller/--symbol " "  0))
  (expect "*"     (orgtrello-controller/--symbol "*"  1))
  (expect "****"  (orgtrello-controller/--symbol "**" 2))
  (expect "   "   (orgtrello-controller/--symbol " "  3)) )

(expectations (desc "orgtrello-controller/--space")
  (expect ""    (orgtrello-controller/--space 0))
  (expect " "   (orgtrello-controller/--space 1))
  (expect "  "  (orgtrello-controller/--space 2))
  (expect "   " (orgtrello-controller/--space 3)) )

(expectations (desc "orgtrello-controller/--star")
  (expect ""    (orgtrello-controller/--star 0))
  (expect "*"   (orgtrello-controller/--star 1))
  (expect "**"  (orgtrello-controller/--star 2))
  (expect "***" (orgtrello-controller/--star 3)) )

(expectations (desc "orgtrello-controller/--compute-level-into-spaces")
  (expect 0 (orgtrello-controller/--compute-level-into-spaces 2))
  (expect 2 (orgtrello-controller/--compute-level-into-spaces nil))
  (expect 2 (orgtrello-controller/--compute-level-into-spaces 'any)))

(expectations (desc "orgtrello-controller/--compute-checklist-to-org-checkbox")
  (expect "- [X] name
" (orgtrello-controller/--compute-checklist-to-org-checkbox "name" 2 "complete"))
  (expect "  - [X] name
" (orgtrello-controller/--compute-checklist-to-org-checkbox "name" 3 "complete"))
  (expect "- [X] name
" (orgtrello-controller/--compute-checklist-to-org-checkbox "name" 2 "complete"))
  (expect "  - [-] name
" (orgtrello-controller/--compute-checklist-to-org-checkbox "name" 3 "incomplete")))

(expectations (desc "orgtrello-controller/--compute-item-to-org-checkbox")
  (expect "- [X] name
" (orgtrello-controller/--compute-item-to-org-checkbox "name" 2 "complete"))
  (expect "  - [X] name
" (orgtrello-controller/--compute-item-to-org-checkbox "name" 3 "complete"))
  (expect "- [X] name
" (orgtrello-controller/--compute-item-to-org-checkbox "name" 2 "complete"))
  (expect "  - [ ] name
" (orgtrello-controller/--compute-item-to-org-checkbox "name" 3 "incomplete")))

(expectations (desc "orgtrello-controller/--compute-item-to-orgtrello-entry")
  (expect "*** DONE name
" (orgtrello-controller/--compute-item-to-orgtrello-entry "name" 3 "complete"))
  (expect "*** TODO name
" (orgtrello-controller/--compute-item-to-orgtrello-entry "name" 3 "incomplete")))

(expectations (desc "orgtrello-controller/--compute-checklist-to-org-entry")
  (expect "- [-] name
" (orgtrello-controller/--compute-checklist-to-org-entry (orgtrello-hash/make-properties `((:name . "name"))) t))
  (expect "- [-] name
" (orgtrello-controller/--compute-checklist-to-org-entry (orgtrello-hash/make-properties `((:name . "name"))) t)))

(expectations (desc "orgtrello-controller/--compute-checklist-to-org-entry")
  (expect "** TODO name
" (orgtrello-controller/--compute-checklist-to-org-entry (orgtrello-hash/make-properties `((:name . "name"))) nil))
  (expect "** TODO name
" (orgtrello-controller/--compute-checklist-to-org-entry (orgtrello-hash/make-properties `((:name . "name"))) nil)))

(expectations (desc "orgtrello-controller/--compute-item-to-org-entry - 1")
  (expect "  - [X] name
" (orgtrello-controller/--compute-item-to-org-entry (orgtrello-hash/make-properties `((:name . "name") (:keyword . "complete"))) t))
  (expect "  - [ ] name
" (orgtrello-controller/--compute-item-to-org-entry (orgtrello-hash/make-properties `((:name . "name") (:keyword . "incomplete"))) t)))

(expectations (desc "orgtrello-controller/--compute-item-to-org-entry - 2")
  (expect "*** DONE name
" (orgtrello-controller/--compute-item-to-org-entry (orgtrello-hash/make-properties `((:name . "name") (:keyword . "complete"))) nil))
  (expect "*** TODO name
" (orgtrello-controller/--compute-item-to-org-entry (orgtrello-hash/make-properties `((:name . "name") (:keyword . "incomplete"))) nil)))

(expectations (desc "orgtrello-data/entity-*")
  (expect "test" (orgtrello-data/entity-buffername            (orgtrello-hash/make-properties `((:buffername . "test")))))
  (expect nil (orgtrello-data/entity-buffername               (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-position              (orgtrello-hash/make-properties `((:position . "test")))))
  (expect nil (orgtrello-data/entity-position                 (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-id                    (orgtrello-hash/make-properties `((:id . "test")))))
  (expect nil (orgtrello-data/entity-id                       (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-name                  (orgtrello-hash/make-properties `((:name . "test")))))
  (expect nil (orgtrello-data/entity-name                     (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-list-id               (orgtrello-hash/make-properties `((:list-id . "test")))))
  (expect nil (orgtrello-data/entity-list-id                  (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-checklists         (orgtrello-hash/make-properties `((:checklists . "test")))))
  (expect nil (orgtrello-data/entity-checklists            (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-items           (orgtrello-hash/make-properties `((:items . "test")))))
  (expect nil (orgtrello-data/entity-items              (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-card-id               (orgtrello-hash/make-properties `((:card-id . "test")))))
  (expect nil (orgtrello-data/entity-card-id                  (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-due                   (orgtrello-hash/make-properties `((:due . "test")))))
  (expect nil (orgtrello-data/entity-due                      (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-keyword                 (orgtrello-hash/make-properties `((:keyword . "test")))))
  (expect nil (orgtrello-data/entity-keyword                    (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-closed        (orgtrello-hash/make-properties `((:closed . "test")))))
  (expect nil (orgtrello-data/entity-closed           (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-callback              (orgtrello-hash/make-properties `((:callback . "test")))))
  (expect nil (orgtrello-data/entity-callback                 (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-sync                 (orgtrello-hash/make-properties `((:sync . "test")))))
  (expect nil (orgtrello-data/entity-sync                    (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-level                 (orgtrello-hash/make-properties `((:level . "test")))))
  (expect nil (orgtrello-data/entity-level                    (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-method               (orgtrello-hash/make-properties `((:method . "test")))))
  (expect nil (orgtrello-data/entity-method                  (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-uri                  (orgtrello-hash/make-properties `((:uri . "test")))))
  (expect nil (orgtrello-data/entity-uri                     (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-params               (orgtrello-hash/make-properties `((:params . "test")))))
  (expect nil (orgtrello-data/entity-params                  (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-start                 (orgtrello-hash/make-properties `((:start . "test")))))
  (expect nil (orgtrello-data/entity-start                    (orgtrello-hash/make-properties `((inexistant . "test")))))
  (expect "test" (orgtrello-data/entity-action                (orgtrello-hash/make-properties `((:action . "test")))))
  (expect nil (orgtrello-data/entity-action                   (orgtrello-hash/make-properties `((inexistant . "test"))))))

(expectations (desc "orgtrello-data/entity-* - 2")
  (expect :some-method (orgtrello-data/entity-method (orgtrello-hash/make-properties `((:method . :some-method )))))
  (expect nil (orgtrello-data/entity-method (orgtrello-hash/make-properties `((:inexistant . :some-method )))))
  (expect :some-uri (orgtrello-data/entity-method (orgtrello-hash/make-properties `((:method . :some-uri )))))
  (expect nil (orgtrello-data/entity-method (orgtrello-hash/make-properties `((:inexistant . :some-method )))))
  (expect :some-sync (orgtrello-data/entity-method (orgtrello-hash/make-properties `((:method . :some-sync )))))
  (expect nil (orgtrello-data/entity-method (orgtrello-hash/make-properties `((:inexistant . :some-method )))))
  (expect :some-params (orgtrello-data/entity-method (orgtrello-hash/make-properties `((:method . :some-params )))))
  (expect nil (orgtrello-data/entity-method (orgtrello-hash/make-properties `((:inexistant . :some-method ))))))

(expectations (desc "orgtrello-controller/--compute-state-generic")
  (expect "DONE" (orgtrello-controller/--compute-state-generic "complete" '("DONE" "TODO")))
  (expect "TODO" (orgtrello-controller/--compute-state-generic "incomplete" '("DONE" "TODO")))
  (expect "DONE" (orgtrello-controller/--compute-state-generic "DONE" '("DONE" "TODO")))

  (expect "[X]" (orgtrello-controller/--compute-state-generic "complete" '("[X]" "[-]")))
  (expect "[-]" (orgtrello-controller/--compute-state-generic "incomplete" '("[X]" "[-]")))
  (expect "[X]" (orgtrello-controller/--compute-state-generic "DONE" '("[X]" "[-]"))))


(expectations (desc "orgtrello-controller/--compute-state-checkbox")
  (expect "[X]" (orgtrello-controller/--compute-state-checkbox "complete"))
  (expect "[-]" (orgtrello-controller/--compute-state-checkbox "incomplete")))

(expectations (desc "orgtrello-controller/--compute-state-item-checkbox")
  (expect "[X]" (orgtrello-controller/--compute-state-item-checkbox "complete"))
  (expect "[ ]" (orgtrello-controller/--compute-state-item-checkbox "incomplete")))

(expectations (desc "orgtrello-controller/--compute-state-item")
 (expect "DONE" (orgtrello-controller/--compute-state-item "complete"))
 (expect "TODO" (orgtrello-controller/--compute-state-item "incomplete")))
(message "Tests done!")

(expectations (desc "orgtrello-data/--entity-with-level-p")
 (expect nil (orgtrello-data/--entity-with-level-p nil 1))
 (expect t   (orgtrello-data/--entity-with-level-p (orgtrello-hash/make-properties `((:level . ,*CARD-LEVEL*))) *CARD-LEVEL*))
 (expect nil (orgtrello-data/--entity-with-level-p (orgtrello-hash/make-properties `((:level . ,*CHECKLIST-LEVEL*))) *CARD-LEVEL*)))

(expectations (desc "orgtrello-data/entity-card-p")
 (expect t   (orgtrello-data/entity-card-p (orgtrello-hash/make-properties `((:level . ,*CARD-LEVEL*)))))
 (expect nil (orgtrello-data/entity-card-p (orgtrello-hash/make-properties `((:level . ,*CHECKLIST-LEVEL*)))))
 (expect nil (orgtrello-data/entity-card-p (orgtrello-hash/make-properties `((idList . 1)))))
 (expect nil (orgtrello-data/entity-card-p (orgtrello-hash/make-properties `((id . 1)))))
 (expect nil (orgtrello-data/entity-card-p (orgtrello-hash/make-properties `((:list-id . "this is a card")))))
 (expect nil (orgtrello-data/entity-card-p (orgtrello-hash/make-properties `((anything-else . "this is not a card"))))))

(expectations (desc "orgtrello-data/entity-checklist-p")
 (expect t   (orgtrello-data/entity-checklist-p (orgtrello-hash/make-properties `((:level . ,*CHECKLIST-LEVEL*)))))
 (expect nil (orgtrello-data/entity-checklist-p (orgtrello-hash/make-properties `((:level . ,*ITEM-LEVEL*)))))
 (expect nil (orgtrello-data/entity-checklist-p (orgtrello-hash/make-properties `((idCard . 1)))))
 (expect nil (orgtrello-data/entity-checklist-p (orgtrello-hash/make-properties `((id . 1)))))
 (expect nil (orgtrello-data/entity-checklist-p (orgtrello-hash/make-properties `((:card-id . "this is a checklist")))))
 (expect nil (orgtrello-data/entity-checklist-p (orgtrello-hash/make-properties `((anything-else . "this is not a checklist"))))))

(expectations (desc "orgtrello-data/entity-item-p")
  (expect t   (orgtrello-data/entity-item-p (orgtrello-hash/make-properties `((:level . ,*ITEM-LEVEL*)))))
  (expect nil (orgtrello-data/entity-item-p (orgtrello-hash/make-properties `((:checked . "this is an item")))))
  (expect nil (orgtrello-data/entity-item-p (orgtrello-hash/make-properties `((anything-else . "this is not a item")))))
  (expect nil (orgtrello-data/entity-item-p (orgtrello-hash/make-properties `((:level . ,*CARD-LEVEL*)))))
  (expect nil (orgtrello-data/entity-item-p (orgtrello-hash/make-properties `((:state . 1)))))
  (expect nil (orgtrello-data/entity-item-p (orgtrello-hash/make-properties `((id . 1))))))

(expectations (desc "orgtrello-webadmin/--compute-filename-from-entity")
  (expect (format "%sorg-trello/3/test.org-123.el" elnode-webserver-docroot) (orgtrello-webadmin/--compute-filename-from-entity (orgtrello-hash/make-properties '((:level . 3) (:buffername . "test.org") (:position . "123"))))))

(with-temp-buffer
  (insert "- [X] call people [4/4] :PROPERTIES: {\"orgtrello-id\":\"456\"}")
  (forward-line -1))

(expectations
  (expect 1
    (with-temp-buffer
      (insert "* heading\n")
      (insert "- [ ] some checklist\n")
      (insert "  - [ ] some item\n")
      (orgtrello-cbx/--point-at-beg-of-region-for-justify)))
  (expect 21
    (with-temp-buffer
      (insert "#+TODO: TODO | DONE\n")
      (insert "* heading\n")
      (insert "- [ ] some checklist\n")
      (insert "  - [ ] some item\n")
      (orgtrello-cbx/--point-at-beg-of-region-for-justify))))

(expectations (desc "orgtrello-cbx/--read-properties-from-point")
  (expect '((orgtrello-id . "123")) (with-temp-buffer
                                      (insert "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}")
                                      (forward-line -1)
                                      (orgtrello-cbx/--read-properties-from-point (point))))
  (expect nil (with-temp-buffer
                (insert "- [X] some checkbox :PROPERTIES: {}")
                (forward-line -1)
                (orgtrello-cbx/--read-properties-from-point (point))))
  (expect nil (with-temp-buffer
                (insert "- [X] some checkbox")
                (forward-line -1)
                (orgtrello-cbx/--read-properties-from-point (point)))))

(expectations (desc "orgtrello-cbx/--write-properties-at-point")
  (expect "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":456}"
    (with-temp-buffer
      (insert "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}")
      (forward-line -1)
      (orgtrello-cbx/--write-properties-at-point (point) `(("orgtrello-id" . 456))))))

(expectations (desc "orgtrello-cbx/org-get-property")
  (expect "abc"
    (with-temp-buffer
      (insert "- [X] some checkbox                                                                :PROPERTIES: {\"orgtrello-id\":\"abc\"}")
      (forward-line -1)
      (orgtrello-cbx/org-get-property (point) "orgtrello-id")))
  (expect nil
    (with-temp-buffer
      (insert "- [X] some checkbox                                                                :PROPERTIES: {\"orgtrello-id\":\"abc\"}")
      (forward-line -1)
      (orgtrello-cbx/org-get-property (point) "inexistant-id"))))

(expectations (desc "orgtrello-cbx/org-set-property")
  (expect "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"abc\"}"
    (with-temp-buffer
      (insert "- [X] some checkbox")
      (forward-line -1)
      (orgtrello-cbx/org-set-property "orgtrello-id" "abc")
      (buffer-string)))
  (expect "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"abc\"}"
    (with-temp-buffer
      (insert "- [X] some checkbox :PROPERTIES: {}")
      (forward-line -1)
      (orgtrello-cbx/org-set-property "orgtrello-id" "abc")
      (buffer-string)))
  (expect "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"def\"}"
    (with-temp-buffer
      (insert "- [X] some checkbox                                                                                                    :PROPERTIES: {\"orgtrello-id\":\"abc\"}")
      (forward-line -1)
      (orgtrello-cbx/org-set-property "orgtrello-id" "def")
      (buffer-string))))

(expectations (desc "orgtrello-cbx/org-delete-property")
  (expect "- [X] some checkbox :PROPERTIES: {}"
    (with-temp-buffer
      (insert "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}")
      (forward-line -1)
      (orgtrello-cbx/org-delete-property "orgtrello-id")
      (buffer-string)))
  (expect "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"def\"}"
    (with-temp-buffer
      (insert "- [X] some checkbox                                                                                         :PROPERTIES: {\"orgtrello-id\":\"def\"}")
      (forward-line -1)
      (orgtrello-cbx/org-delete-property "inexistant")
      (buffer-string)))
  (expect "- [X] some checkbox :PROPERTIES: {}"
    (with-temp-buffer
      (insert "- [X] some checkbox")
      (forward-line -1)
      (orgtrello-cbx/org-delete-property "inexistant")
      (buffer-string))))

(expectations (desc "orgtrello-query/--prepare-params-assoc!")
  (expect '((id . "id") (name . "some%20content%20to%20escape%20%26%20voila%21"))
    (orgtrello-query/--prepare-params-assoc! '((id . "id") (name . "some content to escape & voila!"))))
  (expect '((id . "id") (name . "some%20content%20to%20escape%20%26%20voila%21") (any . "content%20is%20escaped%20%26%20%3E%20this%20is%20fun%21"))
    (orgtrello-query/--prepare-params-assoc! '((id . "id") (name . "some content to escape & voila!") (any . "content is escaped & > this is fun!"))))
  (expect '((id) (name . "some%20content%20to%20escape%20%26%20voila%21") (any . "content%20is%20escaped%20%26%20%3E%20this%20is%20fun%21"))
    (orgtrello-query/--prepare-params-assoc! '((id) (name . "some content to escape & voila!") (any . "content is escaped & > this is fun!"))))
  (expect '((ok . t) (name . "some%20content%20to%20escape%20%26%20voila%21") (any . "content%20is%20escaped%20%26%20%3E%20this%20is%20fun%21"))
    (orgtrello-query/--prepare-params-assoc! '((ok . t) (name . "some content to escape & voila!") (any . "content is escaped & > this is fun!")))))

(expectations (desc "orgtrello-query/--prepare-query-params!")
  (expect '((name . "some%20content%20to%20escape%20%26%20voila%21") (id . "id"))
    (orgtrello-query/--prepare-query-params! '((id . "id") (name . "some content to escape & voila!"))))
  (expect '((any . "content%20is%20escaped%20%26%20%3E%20this%20is%20fun%21") (name . "some%20content%20to%20escape%20%26%20voila%21") (id . "id"))
    (orgtrello-query/--prepare-query-params! '((id . "id") (name . "some content to escape & voila!") (any . "content is escaped & > this is fun!"))))
  (expect '((any . "content%20is%20escaped%20%26%20%3E%20this%20is%20fun%21") (name . "some%20content%20to%20escape%20%26%20voila%21") (id))
    (orgtrello-query/--prepare-query-params! '((id) (name . "some content to escape & voila!") (any . "content is escaped & > this is fun!"))))
  (expect '((any . "content%20is%20escaped%20%26%20%3E%20this%20is%20fun%21") (name . "some%20content%20to%20escape%20%26%20voila%21") (ok . t))
    (orgtrello-query/--prepare-query-params! '((ok . t) (name . "some content to escape & voila!") (any . "content is escaped & > this is fun!")))))

(expectations (desc "orgtrello-query/--prepare-params-assoc!")
  (expect '((name . "some data with & keywords hexified") (id . "abc") (other-field . "hexified string"))
    (->> '((other-field . "hexified string") (id . "abc") (name . "some data with & keywords hexified"))
         orgtrello-query/--prepare-params-assoc!
         json-encode
         orgtrello-proxy/--json-read-from-string)))

(expectations (desc "orgtrello-query/--prepare-params-assoc!")
  (expect '((name . "some%20data%20with%20%26%20keywords%20hexified") (id . "abc") (other-field . "hexified%20string"))
    (-> '((other-field . "hexified string") (id . "abc") (name . "some data with & keywords hexified"))
        orgtrello-query/--prepare-params-assoc!
        json-encode
        orgtrello-proxy/--unhexify-data))
  (expect '((name . "some data with & keywords hexified") (id . "abc") (other-field . "hexified string"))
    (-> '((other-field . "hexified string") (id . "abc") (name . "some data with & keywords hexified"))
        orgtrello-query/--prepare-params-assoc!
        json-encode
        (orgtrello-proxy/--unhexify-data t))))

(expectations (desc "orgtrello-query/--dispatch-http-query")
  (expect 'orgtrello-query/--get         (orgtrello-query/--dispatch-http-query "GET"))
  (expect 'orgtrello-query/--post-or-put (orgtrello-query/--dispatch-http-query "POST"))
  (expect 'orgtrello-query/--post-or-put (orgtrello-query/--dispatch-http-query "PUT"))
  (expect 'orgtrello-query/--delete      (orgtrello-query/--dispatch-http-query "DELETE")))

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

(expectations (desc "orgtrello-controller/--card")
  (expect 'orgtrello-controller/--card      (gethash *CARD-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*))
  (expect 'orgtrello-controller/--checklist (gethash *CHECKLIST-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*))
  (expect 'orgtrello-controller/--item      (gethash *ITEM-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*)))

(expectations (desc "orgtrello-controller/--card-delete")
  (expect 'orgtrello-controller/--card-delete      (gethash *CARD-LEVEL* *MAP-DISPATCH-DELETE*))
  (expect 'orgtrello-controller/--checklist-delete (gethash *CHECKLIST-LEVEL* *MAP-DISPATCH-DELETE*))
  (expect 'orgtrello-controller/--item-delete      (gethash *ITEM-LEVEL* *MAP-DISPATCH-DELETE*)))

(expectations (desc "orgtrello-elnode/compute-entity-level-dir")
 (expect (format "%sorg-trello/1/" elnode-webserver-docroot) (orgtrello-elnode/compute-entity-level-dir *CARD-LEVEL*))
 (expect (format "%sorg-trello/2/" elnode-webserver-docroot) (orgtrello-elnode/compute-entity-level-dir *CHECKLIST-LEVEL*))
 (expect (format "%sorg-trello/3/" elnode-webserver-docroot) (orgtrello-elnode/compute-entity-level-dir *ITEM-LEVEL*)))

(expectations (desc "orgtrello-cbx/--compute-next-card-point")
  (expect 50
    (with-temp-buffer
      (insert "* heading\n")
      (insert "- [ ] some checklist\n")
      (insert "  - [ ] some item\n")
      (orgtrello-cbx/--compute-next-card-point)))
  (expect 70
    (with-temp-buffer
      (insert "#+TODO: TODO | DONE\n")
      (insert "* heading\n")
      (insert "- [ ] some checklist\n")
      (insert "  - [ ] some item\n")
      (forward-line -2)
      (orgtrello-cbx/--compute-next-card-point)))
  (expect 65
    (with-temp-buffer
      (insert "* heading\n")
      (insert "- [ ] some checklist\n")
      (insert "  - [ ] some item\n")
      (insert "* next heading\n")
      (forward-line -2)
      (orgtrello-cbx/--compute-next-card-point)))
  (expect 85
    (with-temp-buffer
      (insert "#+TODO: TODO | DONE\n")
      (insert "* heading\n")
      (insert "- [ ] some checklist\n")
      (insert "  - [ ] some item\n")
      (insert "* next heading\n")
      (forward-line -2)
      (orgtrello-cbx/--compute-next-card-point)))
  (expect 85
    (with-temp-buffer
      (insert "#+TODO: TODO | DONE\n")
      (insert "* heading\n")
      (insert "- [ ] some checklist\n")
      (insert "  - [ ] some item\n")
      (insert "* next heading\n")
      (forward-line -3)
      (orgtrello-cbx/--compute-next-card-point)))
  (expect 85
    (with-temp-buffer
      (insert "#+TODO: TODO | DONE\n")
      (insert "* heading\n")
      (insert "- [ ] some checklist\n")
      (insert "  - [ ] some item\n")
      (insert "* next heading\n")
      (forward-line -4)
      (orgtrello-cbx/--compute-next-card-point))))

(expectations (desc "orgtrello-webadmin/--header-table")
  (expect '(tr nil (td nil) (td nil "Action") (td nil "Entity") (td nil "Delete")) (orgtrello-webadmin/--header-table)))

(expectations (desc "orgtrello-webadmin/--delete-action")
  (expect '(input ((class . "btn btn-danger btn-mini") (type . "button") (onclick . "deleteEntities('/proxy/admin/entities/delete/id');") (value . "x"))) (orgtrello-webadmin/--delete-action
                                                                                                                                                           (orgtrello-hash/make-properties '((:id . "id")))))
  (expect ""                                          (orgtrello-webadmin/--delete-action (orgtrello-hash/make-properties '((:name . "name"))))))

(expectations (desc "orgtrello-webadmin/--entity")
  (expect '(tr
            ((class . "success"))
            (td nil
                (i
                 ((class . "icon-play"))))
            (td nil "test")
            (td nil "name")
            (td nil
                (input
                 ((class . "btn btn-danger btn-mini")
                  (type . "button")
                  (onclick . "deleteEntities('/proxy/admin/entities/delete/id');")
                  (value . "x")))))
    (orgtrello-webadmin/--entity (orgtrello-hash/make-properties '((:action . "test") (:id . "id") (:name . "name"))) "icon-play"))

  (expect '(tr
            ((class . "warning"))
            (td nil
                (i
                 ((class . "icon-pause"))))
            (td nil "delete")
            (td nil "name")
            (td nil
                (input
                 ((class . "btn btn-danger btn-mini")
                  (type . "button")
                  (onclick . "deleteEntities('/proxy/admin/entities/delete/id');")
                  (value . "x")))))
    (orgtrello-webadmin/--entity (orgtrello-hash/make-properties '((:action . "delete") (:id . "id") (:name . "name"))) "icon-pause"))

  (expect '(tr
            ((class . "success"))
            (td nil
                (i
                 ((class . "icon-play"))))
            (td nil "test")
            (td nil "name 0")
            (td nil
                (input
                 ((class . "btn btn-danger btn-mini")
                  (type . "button")
                  (onclick . "deleteEntities('/proxy/admin/entities/delete/id');")
                  (value . "x")))))
    (orgtrello-webadmin/--entity (orgtrello-hash/make-properties '((:action . "test") (:name . "name 0") (:id . "id"))) "icon-play"))

  (expect '(tr
            ((class . "warning"))
            (td nil
                (i
                 ((class . "icon-pause"))))
            (td nil "delete")
            (td nil "name 1")
            (td nil
                (input
                 ((class . "btn btn-danger btn-mini")
                  (type . "button")
                  (onclick . "deleteEntities('/proxy/admin/entities/delete/id');")
                  (value . "x")))))
    (orgtrello-webadmin/--entity (orgtrello-hash/make-properties '((:action . "delete") (:name . "name 1") (:id . "id"))) "icon-pause")))

(expectations (desc "orgtrello-webadmin/--input-button-html")
  (expect '(input
            ((class . "btn btn-danger btn-mini")
             (type . "button")
             (onclick . "deleteEntities('/proxy/admin/entities/delete/');")
             (value . "x")))
    (orgtrello-webadmin/--input-button-html "deleteEntities('/proxy/admin/entities/delete/');" "x")))

(expectations (desc "orgtrello-webadmin/--main-body")
  (expect '(div
            ((class . "row-fluid marketing"))
            (div
             ((class . "span6"))
             (div
              ((style . "font-size: 2em;margin-right: 10px;margin-bottom: 10px"))
              "Current action")
             (span
              ((id . "current-action"))))
            (div
             ((class . "span6"))
             (div
              ((style . "margin-bottom:10px"))
              (span
               ((style . "font-size: 2em;margin-right: 10px"))
               "Next actions")
              (span nil
                    (input
                     ((class . "btn btn-danger btn-mini")
                      (type . "button")
                      (onclick . "deleteEntities('/proxy/admin/entities/delete/');")
                      (value . "Delete all")))))
             (span
              ((id . "next-actions")))))
    (orgtrello-webadmin/--main-body)))

(expectations (desc "orgtrello-webadmin/--render-html")
  (expect
      (esxml-to-xml `(div ((class . "hello")) "world"))
    (orgtrello-webadmin/--render-html `(div ((class . "hello")) "world"))))

(expectations (desc "orgtrello-webadmin/--entities-as-html")
  (expect "None" (orgtrello-webadmin/--entities-as-html nil))
  (expect "None" (orgtrello-webadmin/--entities-as-html nil "icon-arrow-right"))
  (expect "None" (orgtrello-webadmin/--entities-as-html nil "icon-arrow-right" "icon-arrow-left"))
  (expect '(table
            ((class . "table table-striped table-bordered table-hover")
             (style . "font-size: 0.75em"))
            (tr nil
                (td nil)
                (td nil "Action")
                (td nil "Entity")
                (td nil "Delete"))
            (tr
             ((class . ""))
             (td nil
                 (i
                  ((class . "icon-arrow-right"))))
             (td nil "create")
             (td nil "name 0")
             (td nil
                 (input
                  ((class . "btn btn-danger btn-mini")
                   (type . "button")
                   (onclick . "deleteEntities('/proxy/admin/entities/delete/id 0');")
                   (value . "x")))))
            (tr
             ((class . ""))
             (td nil
                 (i
                  ((class . "icon-arrow-up"))))
             (td nil "delete")
             (td nil "name 1")
             (td nil "")))
    (orgtrello-webadmin/--entities-as-html (mapcar 'orgtrello-hash/make-properties
                                                   '(((:action . "create") (:name . "name 0") (:id . "id 0"))
                                                     ((:action . "delete") (:name . "name 1"))))))

  (expect '(table
            ((class . "table table-striped table-bordered table-hover")
             (style . "font-size: 0.75em"))
            (tr nil
                (td nil)
                (td nil "Action")
                (td nil "Entity")
                (td nil "Delete"))
            (tr
             ((class . ""))
             (td nil
                 (i
                  ((class . "icon-arrow-right"))))
             (td nil "create")
             (td nil "name 0")
             (td nil ""))
            (tr
             ((class . ""))
             (td nil
                 (i
                  ((class . "icon-arrow-up"))))
             (td nil "delete")
             (td nil "name 1")
             (td nil "")))
    (orgtrello-webadmin/--entities-as-html (mapcar 'orgtrello-hash/make-properties
                                                   '(((:action . "create") (:name . "name 0"))
                                                     ((:action . "delete") (:name . "name 1")))) "icon-arrow-right"))

  (expect '(table
            ((class . "table table-striped table-bordered table-hover")
             (style . "font-size: 0.75em"))
            (tr nil
                (td nil)
                (td nil "Action")
                (td nil "Entity")
                (td nil "Delete"))
            (tr
             ((class . ""))
             (td nil
                 (i
                  ((class . "icon-arrow-right"))))
             (td nil "create")
             (td nil "name 0")
             (td nil ""))
            (tr
             ((class . ""))
             (td nil
                 (i
                  ((class . "icon-arrow-up"))))
             (td nil "delete")
             (td nil "name 1")
             (td nil "")))
    (orgtrello-webadmin/--entities-as-html (mapcar 'orgtrello-hash/make-properties '(((:action . "create") (:name . "name 0"))
                                                                                     ((:action . "delete") (:name . "name 1")))) nil "icon-arrow-up"))

  (expect '(table
            ((class . "table table-striped table-bordered table-hover")
             (style . "font-size: 0.75em"))
            (tr nil
                (td nil)
                (td nil "Action")
                (td nil "Entity")
                (td nil "Delete"))
            (tr
             ((class . "success"))
             (td nil
                 (i
                  ((class . "icon-play"))))
             (td nil "create")
             (td nil "name 0")
             (td nil ""))
            (tr
             ((class . "warning"))
             (td nil
                 (i
                  ((class . "icon-pause"))))
             (td nil "delete")
             (td nil "name 1")
             (td nil "")))
    (orgtrello-webadmin/--entities-as-html (mapcar 'orgtrello-hash/make-properties
                                                   '(((:action . "create") (:name . "name 0"))
                                                     ((:action . "delete") (:name . "name 1")))) "icon-play" "icon-pause")))

(expectations (desc "orgtrello-webadmin/--list-entities-as-html")
  (expect '((tr
             ((class . ""))
             (td nil
                 (i
                  ((class . "next"))))
             (td nil "action")
             (td nil "nil")
             (td nil
                 (input
                  ((class . "btn btn-danger btn-mini")
                   (type . "button")
                   (onclick . "deleteEntities('/proxy/admin/entities/delete/id');")
                   (value . "x")))))
            (tr
             ((class . ""))
             (td nil
                 (i
                  ((class . "next"))))
             (td nil "action")
             (td nil "nil")
             (td nil
                 (input
                  ((class . "btn btn-danger btn-mini")
                   (type . "button")
                   (onclick . "deleteEntities('/proxy/admin/entities/delete/id2');")
                   (value . "x"))))))
    (orgtrello-webadmin/--list-entities-as-html (mapcar 'orgtrello-hash/make-properties
                                                        '(((:action . "action") (:id . "id") (:marker . "marker"))
                                                          ((:action . "action") (:id . "id2") (:marker . "marker2")))) "next")))

(expectations (desc "orgtrello-webadmin/--compute-class")
  (expect '(class . "success") (orgtrello-webadmin/--compute-class "icon-play"))
  (expect '(class . "warning") (orgtrello-webadmin/--compute-class "icon-pause"))
  (expect '(class . "")        (orgtrello-webadmin/--compute-class nil))
  (expect '(class . "")        (orgtrello-webadmin/--compute-class "any")))

(expectations (desc "orgtrello-hash/make-hierarchy")
 (expect :current (gethash :current (orgtrello-hash/make-hierarchy :current)))
 (expect nil (gethash :parent (orgtrello-hash/make-hierarchy :current)))
 (expect nil (gethash :grandparent (orgtrello-hash/make-hierarchy :current))))

(expectations (desc "orgtrello-hash/make-hierarchy")
 (expect :current (gethash :current (orgtrello-hash/make-hierarchy :current :parent)))
 (expect :parent (gethash :parent (orgtrello-hash/make-hierarchy :current :parent)))
 (expect nil (gethash :grandparent (orgtrello-hash/make-hierarchy :current :parent))))

(expectations (desc "orgtrello-hash/make-hierarchy")
 (expect :current (gethash :current (orgtrello-hash/make-hierarchy :current :parent :grandparent)))
 (expect :parent (gethash :parent (orgtrello-hash/make-hierarchy :current :parent :grandparent)))
 (expect :grandparent (gethash :grandparent (orgtrello-hash/make-hierarchy :current :parent :grandparent))))

(expectations (desc "orgtrello-hash/make-hierarchy")
 (expect :current (gethash :current (orgtrello-hash/make-hierarchy :current nil :grandparent)))
 (expect nil (gethash :parent (orgtrello-hash/make-hierarchy :current nil :grandparent)))
 (expect :grandparent (gethash :grandparent (orgtrello-hash/make-hierarchy nil :parent :grandparent))))

;; (expectations
;;   (expect :ok                                      (-> (orgtrello-hash/make-hash-org 1 :keyword :name :id :due :position :buffer-name)
;;                                                        orgtrello-hash/make-hierarchy
;;                                                        orgtrello-controller/--can-be-synced-p))
;;   (expect :ok                                      (-> (orgtrello-hash/make-hash-org 2 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name "some-id" :due :position :buffer-name))
;;                                                        orgtrello-controller/--can-be-synced-p))
;;   (expect *ERROR-SYNC-CHECKLIST-SYNC-CARD-FIRST*   (-> (orgtrello-hash/make-hash-org 2 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name nil :due :position :buffer-name))
;;                                                        orgtrello-controller/--can-be-synced-p))
;;   (expect *ERROR-SYNC-CHECKLIST-SYNC-CARD-FIRST*   (-> (orgtrello-hash/make-hash-org 2 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name "orgtrello-marker-bad-id-equiv-nil" :due :position :buffer-name))
;;                                                        orgtrello-controller/--can-be-synced-p))
;;   (expect :ok                                      (-> (orgtrello-hash/make-hash-org 3 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name "some-id" :due :position :buffer-name)
;;                                                                                       (orgtrello-hash/make-hash-org 2 :keyword :name "some-id" :due :position :buffer-name))
;;                                                        orgtrello-controller/--can-be-synced-p))
;;   (expect *ERROR-SYNC-ITEM-SYNC-UPPER-LAYER-FIRST* (-> (orgtrello-hash/make-hash-org 3 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name nil :due :position :buffer-name)
;;                                                                                       (orgtrello-hash/make-hash-org 2 :keyword :name "some-id" :due :position :buffer-name))
;;                                                        orgtrello-controller/--can-be-synced-p))
;;   (expect *ERROR-SYNC-ITEM-SYNC-UPPER-LAYER-FIRST* (-> (orgtrello-hash/make-hash-org 3 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name "orgtrello-marker-nil" :due :position :buffer-name)
;;                                                                                       (orgtrello-hash/make-hash-org 2 :keyword :name "some-id" :due :position :buffer-name))
;;                                                        orgtrello-controller/--can-be-synced-p))
;;   (expect *ERROR-SYNC-ITEM-SYNC-UPPER-LAYER-FIRST* (-> (orgtrello-hash/make-hash-org 3 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name "some-id" :due :position :buffer-name)
;;                                                                                       (orgtrello-hash/make-hash-org 2 :keyword :name nil :due :position :buffer-name))
;;                                                        orgtrello-controller/--can-be-synced-p))
;;   (expect *ERROR-SYNC-ITEM-SYNC-UPPER-LAYER-FIRST* (-> (orgtrello-hash/make-hash-org 3 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name "some-id" :due :position :buffer-name)
;;                                                                                       (orgtrello-hash/make-hash-org 2 :keyword :name "orgtrello-marker-nil" :due :position :buffer-name))
;;                                                        orgtrello-controller/--can-be-synced-p))
;;   (expect *ERROR-SYNC-ITEM-SYNC-UPPER-LAYER-FIRST* (-> (orgtrello-hash/make-hash-org 3 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name nil :due :position :buffer-name)
;;                                                                                       (orgtrello-hash/make-hash-org 2 :keyword :name nil :due :position :buffer-name))
;;                                                        orgtrello-controller/--can-be-synced-p))
;;   (expect *ERROR-SYNC-ITEM-SYNC-UPPER-LAYER-FIRST* (-> (orgtrello-hash/make-hash-org 3 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name "orgtrello-marker-nil" :due :position :buffer-name)
;;                                                                                       (orgtrello-hash/make-hash-org 2 :keyword :name "orgtrello-marker-nil" :due :position :buffer-name))
;;                                                        orgtrello-controller/--can-be-synced-p)))

(expectations (desc "orgtrello-hash/make-hierarchy")
  (expect :ok                                 (-> (orgtrello-hash/make-hash-org :users 1 :keyword "some name" :id :due :position :buffer-name :desc)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-CARD-MISSING-NAME*      (-> (orgtrello-hash/make-hash-org :users 1 :keyword "" :id :due :position :buffer-name :desc)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-CARD-MISSING-NAME*      (-> (orgtrello-hash/make-hash-org :users 1 :keyword nil :id :due :position :buffer-name :desc)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p))
  (expect :ok                                 (-> (orgtrello-hash/make-hash-org :users 2 :keyword "some name" :id :due :position :buffer-name :desc)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-CHECKLIST-MISSING-NAME* (-> (orgtrello-hash/make-hash-org :users 2 :keyword "" :id :due :position :buffer-name :desc)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-CHECKLIST-MISSING-NAME* (-> (orgtrello-hash/make-hash-org :users 2 :keyword nil :id :due :position :buffer-name :desc)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p))
  (expect :ok                                 (-> (orgtrello-hash/make-hash-org :users 3 :keyword "some name" :id :due :position :buffer-name :desc)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-ITEM-MISSING-NAME*      (-> (orgtrello-hash/make-hash-org :users 3 :keyword "" :id :due :position :buffer-name :desc)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-ITEM-MISSING-NAME*      (-> (orgtrello-hash/make-hash-org :users 3 :keyword nil :id :due :position :buffer-name :desc)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p)))

(expectations (desc "orgtrello-data/entry-get-full-metadata")
  (expect nil    (->> (with-temp-buffer
                        (org-mode)
                        (insert "* card")
                        (orgtrello-data/entry-get-full-metadata))
                      (orgtrello-data/parent)))
  (expect nil    (->> (with-temp-buffer
                        (org-mode)
                        (insert "* card")
                        (orgtrello-data/entry-get-full-metadata))
                      (orgtrello-data/grandparent)))
  (expect "card" (->> (with-temp-buffer
                        (org-mode)
                        (insert "* card")
                        (orgtrello-data/entry-get-full-metadata))
                      (orgtrello-data/current)
                      orgtrello-data/entity-name)))

(expectations (desc "orgtrello-data/entry-get-full-metadata")
  (expect "card"      (->> (with-temp-buffer
                             (org-mode)
                             (insert "* card\n")
                             (insert "- [ ] checklist")
                             (orgtrello-data/entry-get-full-metadata))
                           (orgtrello-data/parent)
                           orgtrello-data/entity-name))
  (expect nil         (->> (with-temp-buffer
                             (org-mode)
                             (insert "* card\n")
                             (insert "- [ ] checklist")
                             (orgtrello-data/entry-get-full-metadata))
                           (orgtrello-data/grandparent)))
  (expect "checklist" (->> (with-temp-buffer
                             (org-mode)
                             (insert "* card\n")
                             (insert "- [ ] checklist")
                             (orgtrello-data/entry-get-full-metadata))
                           (orgtrello-data/current)
                           orgtrello-data/entity-name)))

(expectations (desc "orgtrello-data/entry-get-full-metadata")
  (expect "checklist" (->> (with-temp-buffer
                             (org-mode)
                             (insert "* card\n")
                             (insert "- [ ] checklist\n")
                             (insert "  - [ ] item")
                             (orgtrello-data/entry-get-full-metadata))
                           (orgtrello-data/parent)
                           orgtrello-data/entity-name))
  (expect "card"      (->> (with-temp-buffer
                             (org-mode)
                             (insert "* card\n")
                             (insert "- [ ] checklist\n")
                             (insert "  - [ ] item")
                             (orgtrello-data/entry-get-full-metadata))
                           (orgtrello-data/grandparent)
                           orgtrello-data/entity-name))
  (expect "item"      (->> (with-temp-buffer
                             (org-mode)
                             (insert "* card\n")
                             (insert "- [ ] checklist\n")
                             (insert "  - [ ] item")
                             (orgtrello-data/entry-get-full-metadata))
                           (orgtrello-data/current)
                           orgtrello-data/entity-name)))

(expectations (desc "orgtrello-cbx/--read-properties-from-point")
  (expect '((orgtrello-id . "orgtrello-marker-123")) (with-temp-buffer
                                                       (org-mode)
                                                       (insert "* card\n")
                                                       (insert "- [X] hello :PROPERTIES: {\"orgtrello-id\":\"orgtrello-marker-123\"}")
                                                       (orgtrello-cbx/--read-properties-from-point (point))))

  (expect nil (with-temp-buffer
                (org-mode)
                (insert "* card\n")
                (insert "- [X] hello :PROPERTIES: {\"orgtrello-id\":\"orgtrello-marker-123\"}")
                (orgtrello-proxy/--cleanup-meta (orgtrello-data/entry-get-full-metadata))
                (orgtrello-cbx/--read-properties-from-point (point))))

  (expect nil (with-temp-buffer
                (org-mode)
                (insert "* card\n")
                (insert "- [X] hello :PROPERTIES: {\"orgtrello-id\":\"orgtrello-marker-123\"}")
                (orgtrello-proxy/--cleanup-meta (orgtrello-data/entry-get-full-metadata))
                (orgtrello-cbx/--read-properties-from-point (point))))

  (expect nil (with-temp-buffer
                (org-mode)
                (insert "* card\n")
                (insert "- [X] cl :PROPERTIES: {\"orgtrello-id\":\"abc\"}\n")
                (insert "  - [X] item :PROPERTIES: {\"orgtrello-id\":\"orgtrello-marker-123\"}")
                (orgtrello-proxy/--cleanup-meta (orgtrello-data/entry-get-full-metadata))
                (orgtrello-cbx/--read-properties-from-point (point)))))

(expectations (desc "orgtrello-elnode/archived-scanning-dir")
  (expect "tests.scanning" (orgtrello-elnode/archived-scanning-dir "tests"))
  (expect "nil.scanning" (orgtrello-elnode/archived-scanning-dir nil)))

(expectations (desc "orgtrello-proxy/--archived-scanning-file")
  (expect "test/folder/.scanning/filename" (orgtrello-proxy/--archived-scanning-file "test/folder/filename")))

(expectations (desc "orgtrello-proxy/--update-buffer-to-save")
 (expect '(:a) (orgtrello-proxy/--update-buffer-to-save :a nil))
 (expect '(:a) (orgtrello-proxy/--update-buffer-to-save :a '(:a)))
 (expect '(:a :b) (orgtrello-proxy/--update-buffer-to-save :a '(:b))))

(expectations (desc "orgtrello-proxy/update-buffer-to-save!")
 (setq *ORGTRELLO-LIST-BUFFERS-TO-SAVE* nil)
 (expect '(:a) (orgtrello-proxy/update-buffer-to-save! :a))
 (expect '(:a) (orgtrello-proxy/update-buffer-to-save! :a))
 (expect '(:b :a) (orgtrello-proxy/update-buffer-to-save! :b)))

(expectations (desc "orgtrello-controller/--dispatch-create-entities-map-with-adjacency")
  (expect 'orgtrello-controller/--put-card-with-adjacency     (orgtrello-controller/--dispatch-create-entities-map-with-adjacency (orgtrello-hash/make-hash-org :users *CARD-LEVEL* nil nil nil nil nil nil nil)))
  (expect 'orgtrello-controller/--put-entities-with-adjacency (orgtrello-controller/--dispatch-create-entities-map-with-adjacency (orgtrello-hash/make-hash-org :users *CHECKLIST-LEVEL* nil nil nil nil nil nil nil)))
  (expect 'orgtrello-controller/--put-entities-with-adjacency (orgtrello-controller/--dispatch-create-entities-map-with-adjacency (orgtrello-hash/make-hash-org :users *ITEM-LEVEL* nil nil nil nil nil nil nil))))

(ert-deftest testing-orgtrello-controller/--init-map-from ()
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ()) (orgtrello-controller/--init-map-from nil))))

(expectations (desc "orgtrello-controller/--init-map-from")
  (expect :data (orgtrello-controller/--init-map-from :data)))

(ert-deftest testing-orgtrello-controller/--merge-item ()
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id nil :level 3))
                      (orgtrello-controller/--merge-item (orgtrello-hash/make-properties `((:checked . "anything") (:name . "some name")))
                                              (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "DONE" :id nil :level 3))
                      (orgtrello-controller/--merge-item (orgtrello-hash/make-properties `((:checked . "complete") (:name . "some name")))
                                               nil)))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id nil :level 3))
                      (orgtrello-controller/--merge-item (orgtrello-hash/make-properties `((:checked . "anything") (:name . "some name")))
                                              (orgtrello-hash/make-properties `((:name . "some other name") (:keyword . "TODO") (:id . "1"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id "1" :level 3))
                      (orgtrello-controller/--merge-item (orgtrello-hash/make-properties `((:checked . "anything") (:name . "some name") (:id . "1")))
                                              (orgtrello-hash/make-properties `((:name . "some other name") (:keyword . "TODO"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id "1" :level 3))
                      (orgtrello-controller/--merge-item (orgtrello-hash/make-properties `((:checked . "anything") (:name . "some name") (:id . "1")))
                                              (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO") (:id . "2"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id "1" :level 3))
                      (orgtrello-controller/--merge-item (orgtrello-hash/make-properties `((:checked . "anything") (:name . "some name") (:id . "1")))
                                              (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "DONE" :id "1" :level 3))
                      (orgtrello-controller/--merge-item (orgtrello-hash/make-properties `((:checked . "complete") (:name . "some name") (:id . "1")))
                                              nil))))

(ert-deftest testing-orgtrello-controller/--merge-checklist ()
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :id nil :level 2))
                      (orgtrello-controller/--merge-checklist (orgtrello-hash/make-properties `((:id . nil) (:name . "some name")))
                                                   (orgtrello-hash/make-properties `((:name . "some other name"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :id nil :level 2))
                      (orgtrello-controller/--merge-checklist (orgtrello-hash/make-properties `((:id . nil) (:name . "some name")))
                                                   (orgtrello-hash/make-properties `((:name . "some other name"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :id nil :level 2))
                      (orgtrello-controller/--merge-checklist (orgtrello-hash/make-properties `((:id . nil) (:name . "some name")))
                                                   nil)))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :id nil :level 2))
                      (orgtrello-controller/--merge-checklist (orgtrello-hash/make-properties `((:id . nil) (:name . "some name")))
                                                   (orgtrello-hash/make-properties `((:name . "some other name") (:id "1"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :id "1" :level 2))
                      (orgtrello-controller/--merge-checklist (orgtrello-hash/make-properties `((:id . "1") (:name . "some name") (:id . "1")))
                                                   (orgtrello-hash/make-properties `((:name . "some other name"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :id "1" :level 2))
                      (orgtrello-controller/--merge-checklist (orgtrello-hash/make-properties `((:id . "1") (:name . "some name") (:id . "1")))
                                                   (orgtrello-hash/make-properties `((:name . "some other name") (:id 2))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :id "1" :level 2))
                      (orgtrello-controller/--merge-checklist (orgtrello-hash/make-properties `((:id . "1") (:name . "some name") (:id . "1")))
                                                   (orgtrello-hash/make-properties `((:name . "some other name"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :id "1" :level 2))
                      (orgtrello-controller/--merge-checklist (orgtrello-hash/make-properties `((:id . "1") (:name . "some name")))
                                                   nil))))

;; (ert-deftest testing-orgtrello-controller/--merge-card ()
;;   (let ((*HMAP-ID-NAME* (orgtrello-hash/make-properties `((1 . "TODO") (2 . "DONE") (3 . "IN-PROGRESS")))))
;;     (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id nil :level 1 :member-ids ""))
;;                         (orgtrello-controller/--merge-card `((id . nil) (name . "some name") (idList . 1)) (orgtrello-hash/make-properties `((:name . "some other name"))))))
;;     (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "DONE" :id nil :level 1 :member-ids ""))
;;                         (orgtrello-controller/--merge-card `((id . nil) (name . "some name") (idList . 2)) (orgtrello-hash/make-properties `((:name . "some other name"))))))
;;     (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id nil :level 1 :member-ids ""))
;;                         (orgtrello-controller/--merge-card `((id . nil) (name . "some name") (idList . 1)) nil)))
;;     (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id nil :level 1 :member-ids ""))
;;                         (orgtrello-controller/--merge-card `((id . nil) (name . "some name") (idList . 1)) (orgtrello-hash/make-properties `((:name . "some other name") (:id 1))))))
;;     (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id 1 :level 1 :member-ids ""))
;;                         (orgtrello-controller/--merge-card `((id . 1) (name . "some name") (id . 1) (idList . 1)) (orgtrello-hash/make-properties `((:name . "some other name"))))))
;;     (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id 1 :level 1 :member-ids ""))
;;                         (orgtrello-controller/--merge-card `((id . 1) (name . "some name") (id . 1) (idList . 1)) (orgtrello-hash/make-properties `((:name . "some other name") (:id 2))))))
;;     (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id 1 :level 1 :member-ids ""))
;;                         (orgtrello-controller/--merge-card `((id . 1) (name . "some name") (id . 1) (idList . 1)) (orgtrello-hash/make-properties `((:name . "some other name"))))))
;;     (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id 1 :level 1 :member-ids ""))
;;                         (orgtrello-controller/--merge-card `((id . 1) (name . "some name") (idList . 1)) nil)))
;;     (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id 1 :level 1 :member-ids "1,2,3"))
;;                         (orgtrello-controller/--merge-card `((id . 1) (name . "some name") (idList . 1) (idMembers . ["1" "2" "3"])) nil)))
;;     (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :member-ids "1,2,3,4,5" :level 1 :id 1 :keyword nil))
;;                         (orgtrello-controller/--merge-card `((id . 1) (name . "some name") (idList . 1) (idMembers . ["1" "2" "3"])) (orgtrello-hash/make-properties `((:name . "some other name") (:member-ids . "4,5,3"))))))))

(expectations (desc "orgtrello-controller/--add-to-last-pos")
 (expect '(1 2 3 4) (orgtrello-controller/--add-to-last-pos 4 '(1 2 3))))

(expectations (desc "orgtrello-data/merge-2-lists-without-duplicates")
  (expect '(1 2 3 4) (orgtrello-data/merge-2-lists-without-duplicates '(1 2 3) '(4 1 2)))
  (expect '(4 1 2)   (orgtrello-data/merge-2-lists-without-duplicates nil '(4 1 2)))
  (expect '(4 1 2)   (orgtrello-data/merge-2-lists-without-duplicates '(4 1 2) nil))
  (expect nil        (orgtrello-data/merge-2-lists-without-duplicates nil nil)))

(expectations (desc "orgtrello-data/entity-card-p")
 (expect t (orgtrello-data/entity-card-p (orgtrello-hash/make-properties `((:level . ,*CARD-LEVEL*)))))
 (expect nil (orgtrello-data/entity-card-p (orgtrello-hash/make-properties `((:level . ,*CHECKLIST-LEVEL*))))))

(expectations
  (expect "some-method" (orgtrello-data/gethash-data :method (orgtrello-hash/make-properties `((:method . "some-method")))))
  (expect nil           (orgtrello-data/gethash-data :method (orgtrello-hash/make-properties `((:inexistant . "some-method")))))
  (expect nil           (orgtrello-data/gethash-data :key nil))
  (expect :value        (orgtrello-data/gethash-data :key (orgtrello-hash/make-properties `((:key . :value)))))
  (expect nil           (orgtrello-data/gethash-data :key (orgtrello-hash/make-properties `((:other-key . :value)))))
  (expect nil           (orgtrello-data/gethash-data :key (orgtrello-hash/make-properties `((:key . nil))))))

(expectations (desc "orgtrello-controller/--compute-user-properties")
  (expect t (hash-equal
             #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                           (:username "ardumont" :full-name "Antoine R. Dumont" :id "4f2baa2f72b7c1293501cad3"))
             (first (orgtrello-controller/--compute-user-properties '(#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                                                         (:member #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                                                                                (:username "ardumont" :full-name "Antoine R. Dumont" :id "4f2baa2f72b7c1293501cad3"))
                                                                                  :id "51d99bbc1e1d8988390047f6"))
                                                             #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                                                           (:member #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                                                                                  (:username "orgmode" :full-name "org trello" :id "5203a0c833fc36360800177f"))
                                                                                    :id "524855ff8193aec160002cfa")))))))
  (expect t (hash-equal
             #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                           (:username "orgmode" :full-name "org trello" :id "5203a0c833fc36360800177f"))
             (second (orgtrello-controller/--compute-user-properties '(#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                                                          (:member #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                                                                                 (:username "ardumont" :full-name "Antoine R. Dumont" :id "4f2baa2f72b7c1293501cad3"))
                                                                                   :id "51d99bbc1e1d8988390047f6"))
                                                              #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                                                            (:member #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                                                                                   (:username "orgmode" :full-name "org trello" :id "5203a0c833fc36360800177f"))
                                                                                     :id "524855ff8193aec160002cfa"))))))))

(expectations (desc "testing-orgtrello-controller/--compute-user-properties-hash")
  (expect t (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ("ardumont" "4f2baa2f72b7c1293501cad3" "orgmode" "5203a0c833fc36360800177f"))
                        (orgtrello-controller/--compute-user-properties-hash '(#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                                                                  (:username "ardumont" :full-name "Antoine R. Dumont" :id "4f2baa2f72b7c1293501cad3"))
                                                                      #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                                                                    (:username "orgmode" :full-name "org trello" :id "5203a0c833fc36360800177f")))))))

(expectations (desc "orgtrello-controller/--compute-user-properties-hash-from-board")
  (expect t (hash-equal
             #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                           ("ardumont" "4f2baa2f72b7c1293501cad3"
                            "orgmode" "5203a0c833fc36360800177f"))
             (orgtrello-controller/--compute-user-properties-hash-from-board #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                                                                (:closed nil :memberships
                                                                                         (#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                                                                                        (:member #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                                                                                                               (:username "ardumont" :full-name "Antoine R. Dumont" :id "4f2baa2f72b7c1293501cad3"))
                                                                                                                 :id "51d99bbc1e1d8988390047f6"))
                                                                                            #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                                                                                          (:member #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                                                                                                                 (:username "orgmode" :full-name "org trello" :id "5203a0c833fc36360800177f"))
                                                                                                                   :id "524855ff8193aec160002cfa")))
                                                                                         :name "api test board" :id "51d99bbc1e1d8988390047f2"))))))

(expectations (desc "orgtrello-controller/--list-user-entries")
 (expect
  '(("orgtrello-user-ardumont" . "4f2baa2f72b7c1293501cad3") ("orgtrello-user-orgmode" . "5203a0c833fc36360800177f"))
  (orgtrello-controller/--list-user-entries '(("board-name" . "api test board") ("board-id" . "51d99bbc1e1d8988390047f2") ("TODO" . "51d99bbc1e1d8988390047f3") ("IN-PROGRESS" . "51d99bbc1e1d8988390047f4") ("DONE" . "51d99bbc1e1d8988390047f5") ("PENDING" . "51e53898ea3d1780690015ca") ("DELEGATED" . "51e538a89c05f1e25c0027c6") ("FAIL" . "51e538a26f75d07902002d25") ("CANCELLED" . "51e538e6c7a68fa0510014ee") ("orgtrello-user-ardumont" . "4f2baa2f72b7c1293501cad3") ("orgtrello-user-orgmode" . "5203a0c833fc36360800177f")))))

(expectations (desc "orgtrello-controller/--users-from")
  (expect '("a" "b" "c") (orgtrello-controller/--users-from "a,b,c,,"))
  (expect '() (orgtrello-controller/--users-from ",,,"))
  (expect '() (orgtrello-controller/--users-from ""))
  (expect '() (orgtrello-controller/--users-from nil)))

(expectations (desc "orgtrello-controller/--users-to")
  (expect "" (orgtrello-controller/--users-to nil))
  (expect "a,b,c," (orgtrello-controller/--users-to '("a" "b" "c" ""))))

(expectations (desc "orgtrello-controller/--add-user")
  (expect '("a" "b" "c") (orgtrello-controller/--add-user "a" '("a" "b" "c")))
  (expect '("a" "b" "c") (orgtrello-controller/--add-user "a" '("b" "c"))))

(expectations (desc "orgtrello-controller/--remove-user")
  (expect '("b")     (orgtrello-controller/--remove-user "a" '("a" "b")))
  (expect '("a" "b") (orgtrello-controller/--remove-user "c" '("a" "b")))
  (expect nil        (orgtrello-controller/--remove-user "c" nil))
  (expect nil        (orgtrello-controller/--remove-user nil nil))
  (expect '("a")     (orgtrello-controller/--remove-user nil '("a"))))

(expectations (desc "orgtrello-controller/--csv-user-ids-to-csv-user-names")
  (expect "user0,user1,user2" (orgtrello-controller/--csv-user-ids-to-csv-user-names "id0,id1,id2" (orgtrello-hash/make-properties '(("id0". "user0") ("id1". "user1") ("id2". "user2")))))
  (expect "user0,user1," (orgtrello-controller/--csv-user-ids-to-csv-user-names "id0,id1,id2" (orgtrello-hash/make-properties '(("id0". "user0") ("id1". "user1")))))
  (expect "user0" (orgtrello-controller/--csv-user-ids-to-csv-user-names "id0" (orgtrello-hash/make-properties '(("id0". "user0"))))))

(expectations (desc "orgtrello-controller/compute-property")
 (expect "#+property: test "      (orgtrello-controller/compute-property "test"))
 (expect "#+property: test value" (orgtrello-controller/compute-property "test" "value")))

(expectations (desc "ORGTRELLO-MODE-PREFIX-KEYBINDING*")
 (expect "C-c o a - M-x some-action - some-description
C-c o 2 - M-x action2 - some other description" (org-trello/--help-describing-bindings-template "C-c o" '((some-action "a" "some-description")
                                                                                                          (action2 "2" "some other description")))))

(expectations (desc "org-trello/--help-describing-bindings-template")
  (expect
      "C-c o v - M-x org-trello/version - Display the current version installed.
C-c o i - M-x org-trello/install-key-and-token - Install the keys and the access-token.
C-c o I - M-x org-trello/install-board-and-lists-ids - Select the board and attach the todo, doing and done list.
C-c o d - M-x org-trello/check-setup - Check that the setup is ok. If everything is ok, will simply display 'Setup ok!'.
C-c o a - M-x org-trello/assign-me - Assign oneself to the card.
C-c o u - M-x org-trello/unassign-me - Unassign oneself of the card
C-c o D - M-x org-trello/delete-setup - Clean up the org buffer from all org-trello informations.
C-c o b - M-x org-trello/create-board - Create interactively a board and attach the org-mode file to this trello board.
C-c o S - M-x org-trello/sync-from-trello - Synchronize the org-mode file from the trello board (trello -> org-mode).
C-c o c - M-x org-trello/sync-entity - Create/Update an entity (card/checklist/item) depending on its level and status. Do not deal with level superior to 4.
C-c o C - M-x org-trello/sync-full-entity - Create/Update a complete entity card/checklist/item and its subtree (depending on its level).
C-c o k - M-x org-trello/kill-entity - Kill the entity (and its arborescence tree) from the trello board and the org buffer.
C-c o K - M-x org-trello/kill-all-entities - Kill all the entities (and their arborescence tree) from the trello board and the org buffer.
C-c o s - M-x org-trello/sync-to-trello - Synchronize the org-mode file to the trello board (org-mode -> trello).
C-c o j - M-x org-trello/jump-to-card - Jump to card in browser.
C-c o J - M-x org-trello/jump-to-trello-board - Open the browser to your current trello board.
C-c o h - M-x org-trello/help-describing-bindings - This help message."
      (org-trello/--help-describing-bindings-template *ORGTRELLO-MODE-PREFIX-KEYBINDING* org-trello/--list-of-interactive-command-binding-couples)))

(expectations (desc "orgtrello-controller/--merge-member-ids")
  (expect "'some-keybinding' is fun 'some-keybinding'" (org-trello/--replace-string-prefix-in-string "some-keybinding" "'#PREFIX#' is fun '#PREFIX#'")))

(expectations (desc "orgtrello-controller/--merge-member-ids")
 (expect "org-trello/ot is on! To begin with, hit C-c o h or M-x 'org-trello/help-describing-bindings" (org-trello/--startup-message "C-c o")))

(expectations (desc "orgtrello-controller/--merge-member-ids")
  (expect "1,5,2,3,4"
    (orgtrello-controller/--merge-member-ids (orgtrello-hash/make-properties `((:member-ids . ("1" "5"))))
                                      (orgtrello-hash/make-properties `((:member-ids . "2,3,4"))))))

(expectations (desc "orgtrello-controller/--merge-member-ids")
  (expect "1,5,2,3,4"
    (orgtrello-controller/--merge-member-ids (orgtrello-hash/make-properties `((:member-ids . ["1" "5"])))
                                      (orgtrello-hash/make-properties `((:member-ids . "2,3,4"))))))

(expectations (desc "orgtrello-data/--compute-level")
  (expect *CARD-LEVEL*      (orgtrello-data/--compute-level (orgtrello-hash/make-properties '((:list-id . 0)))))
  (expect *CHECKLIST-LEVEL* (orgtrello-data/--compute-level (orgtrello-hash/make-properties '((:card-id . 0)))))
  (expect *ITEM-LEVEL*      (orgtrello-data/--compute-level (orgtrello-hash/make-properties '((:checked . 0)))))
  (expect nil               (orgtrello-data/--compute-level (orgtrello-hash/make-properties '()))))

(ert-deftest testing-orgtrello-data/parse-data-card ()
  ;; check card
  (should (hash-equal (orgtrello-hash/make-properties `((:url . "https://trello.com/c/9XPLuJhi/2515-joy-of-fun-ctional-languages")
                                                        (:name . "Joy of FUN(ctional) LANGUAGES")
                                                        (:level . 1)
                                                        (:member-ids . (1 2 3))
                                                        (:checklists . (4 5 6))
                                                        (:list-id . "51d99bbc1e1d8988390047f3")
                                                        (:desc . "some-desc")
                                                        (:board-id . "51d99bbc1e1d8988390047f2")
                                                        (:closed . nil)
                                                        (:id . "52c09056d84eeca156001a24")
                                                        (:due . "some-due-date")
                                                        (:position . "98304")))

                      (orgtrello-data/parse-data '((url . "https://trello.com/c/9XPLuJhi/2515-joy-of-fun-ctional-languages")
                                                    (shortUrl . "https://trello.com/c/9XPLuJhi")
                                                    (pos . "98304")
                                                    (name . "Joy of FUN(ctional) LANGUAGES")
                                                    (labels . [])
                                                    (manualCoverAttachment . :json-false)
                                                    (idAttachmentCover)
                                                    (idShort . 2515)
                                                    (idMembers . [1 2 3])
                                                    (idList . "51d99bbc1e1d8988390047f3")
                                                    (idChecklists . [4 5 6])
                                                    (idBoard . "51d99bbc1e1d8988390047f2")
                                                    (due . "some-due-date")
                                                    (descData (emoji))
                                                    (desc . "some-desc")
                                                    (dateLastActivity . "2013-12-29T21:12:54.164Z")
                                                    (closed . :json-false)
                                                    (checkItemStates . [])
                                                    (badges (due)
                                                            (description . :json-false)
                                                            (attachments . 0)
                                                            (comments . 0)
                                                            (checkItemsChecked . 0)
                                                            (checkItems . 0)
                                                            (fogbugz)
                                                            (subscribed . :json-false)
                                                            (viewingMemberVoted . :json-false)
                                                            (votes . 0))
                                                    (id . "52c09056d84eeca156001a24"))))))

(ert-deftest testing-orgtrello-data/parse-data-checklist ()
  ;; check checklist
  (should (hash-equal (orgtrello-hash/make-properties `((:items . ())
                                                        (:name . "LISP family")
                                                        (:level . 2)
                                                        (:id . "52c0a36886b7bdd67c008cf1")
                                                        (:card-id . "52c09056d84eeca156001a24")
                                                        (:board-id . "51d99bbc1e1d8988390047f2")
                                                        (:id . "52c0a36886b7bdd67c008cf1")
                                                        (:position . 16384)))
                      (orgtrello-data/parse-data '((checkItems . [])
                                                    (pos . 16384)
                                                    (idCard . "52c09056d84eeca156001a24")
                                                    (idBoard . "51d99bbc1e1d8988390047f2")
                                                    (name . "LISP family")
                                                    (id . "52c0a36886b7bdd67c008cf1"))))))

(ert-deftest testing-orgtrello-data/parse-data-item ()
  (should (hash-equal
           (orgtrello-hash/make-properties `((:name . "Emacs-Lisp")
                                             (:level . 3)
                                             (:id . "52c0a64cfb34123369015393")
                                             (:checked . "complete")
                                             (:position . 16384)))
           (orgtrello-data/parse-data '((pos . 16384)
                                         (nameData (emoji))
                                         (name . "Emacs-Lisp")
                                         (id . "52c0a64cfb34123369015393")
                                         (state . "complete")))))
  (should (hash-equal
           (orgtrello-hash/make-properties `((:name . "Emacs-Lisp")
                                             (:level . 3)
                                             (:id . "52c0a64cfb34123369015393")
                                             (:checked . "incomplete")
                                             (:position . 16384)))
           (orgtrello-data/parse-data '((pos . 16384)
                                         (nameData (emoji))
                                         (name . "Emacs-Lisp")
                                         (id . "52c0a64cfb34123369015393")
                                         (state . "incomplete"))))))

(ert-deftest testing-orgtrello-data/parse-data-http-response ()
  (should (hash-equal
           (orgtrello-hash/make-properties `((:status . ok)))
           (orgtrello-data/parse-data '((status . ok))))))

(ert-deftest testing-orgtrello-data/parse-data-remaining-possible-inputs ()
  (should (hash-equal
           (orgtrello-hash/make-properties `((:buffername . ":buffername")
                                             (:sync . ":sync")
                                             (:uri . ":uri")
                                             (:method . ":method")
                                             (:params . ":params")
                                             (:action . ":action")
                                             (:start . ":start")
                                             (:position . ":position")
                                             (:keyword . ":keyword")
                                             (:callback . (lambda (id) id))))
           (orgtrello-data/parse-data '((buffername . ":buffername")
                                         (sync . ":sync")
                                         (uri . ":uri")
                                         (method . ":method")
                                         (params . ":params")
                                         (action . ":action")
                                         (start . ":start")
                                         (position . ":position")
                                         (:keyword . ":keyword")
                                         (callback . (lambda (id) id)))))))

(ert-deftest testing-orgtrello-data/parse-data-with-list-of-results ()
  (let ((list-hash (orgtrello-data/parse-data '[((shortUrl . "https://trello.com/b/o9oY3NlQ")
                                                 (dateLastView . "2013-08-08T14:07:03.382Z")
                                                 (dateLastActivity)
                                                 (powerUps . [])
                                                 (labelNames
                                                  (purple . "")
                                                  (blue . "")
                                                  (green . "")
                                                  (yellow . "")
                                                  (orange . "")
                                                  (red . ""))
                                                 (subscribed . :json-false)
                                                 (shortLink . "o9oY3NlQ")
                                                 (invitations . [])
                                                 (prefs
                                                  (canInvite . t)
                                                  (canBePrivate . t)
                                                  (canBeOrg . t)
                                                  (canBePublic . t)
                                                  (bg . "blue")
                                                  (backgroundBrightness . "unknown")
                                                  (backgroundTile . :json-false)
                                                  (backgroundImageScaled)
                                                  (backgroundImage)
                                                  (backgroundColor . "#205C7E")
                                                  (cardCovers . t)
                                                  (selfJoin . :json-false)
                                                  (invitations . "members")
                                                  (comments . "members")
                                                  (voting . "disabled")
                                                  (permissionLevel . "private"))
                                                 (url . "https://trello.com/b/o9oY3NlQ/1-board-to-rule-them-all")
                                                 (pinned . t)
                                                 (invited . :json-false)
                                                 (idOrganization)
                                                 (closed . t)
                                                 (descData)
                                                 (desc . "")
                                                 (name . "1-board-to-rule-them-all")
                                                 (id . "5203a4fd0ac2f5b75c001d1d"))
                                                ((shortUrl . "https://trello.com/b/xzOJmxzy")
                                                 (dateLastView . "2013-04-15T09:58:13.992Z")
                                                 (dateLastActivity)
                                                 (powerUps .
                                                           [])
                                                 (labelNames
                                                  (blue . "Projet")
                                                  (green . "Modèle")
                                                  (orange . "API")
                                                  (purple . "Devops")
                                                  (red . "WCI")
                                                  (yellow . "IHM"))
                                                 (subscribed . :json-false)
                                                 (shortLink . "xzOJmxzy")
                                                 (invitations . [])
                                                 (prefs
                                                  (canInvite . t)
                                                  (canBePrivate . t)
                                                  (canBeOrg . t)
                                                  (canBePublic . t)
                                                  (bg . "blue")
                                                  (backgroundBrightness . "unknown")
                                                  (backgroundTile . :json-false)
                                                  (backgroundImageScaled)
                                                  (backgroundImage)
                                                  (backgroundColor . "#205C7E")
                                                  (cardCovers . t)
                                                  (selfJoin . :json-false)
                                                  (invitations . "members")
                                                  (comments . "members")
                                                  (voting . "disabled")
                                                  (permissionLevel . "org"))
                                                 (url . "https://trello.com/b/xzOJmxzy/demandes-infra")
                                                 (pinned . t)
                                                 (invited . :json-false)
                                                 (idOrganization . "5044ce9d5371b0384813bba6")
                                                 (closed . t)
                                                 (descData)
                                                 (desc . "")
                                                 (name . "Demandes Infra")
                                                 (id . "50aa59502ddab2fc1100115b"))])))
    (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                      (:url "https://trello.com/b/o9oY3NlQ/1-board-to-rule-them-all" :closed t :desc "" :name "1-board-to-rule-them-all" :id "5203a4fd0ac2f5b75c001d1d"))
                        (first list-hash)))
    (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                      (:url "https://trello.com/b/xzOJmxzy/demandes-infra" :closed t :desc "" :name "Demandes Infra" :id "50aa59502ddab2fc1100115b"))
                        (second list-hash)))))

(expectations (desc "orgtrello-data/parse-data - with nested assoc list.")
              (defvar actual-result)
              (setq actual-result (orgtrello-data/parse-data '((checkItems . [((pos . 16384)
                                                                                (nameData)
                                                                                (name . "introduction")
                                                                                (id . "52c0b537ad469b9d6d044fa1")
                                                                                (state . "incomplete"))
                                                                               ((pos . 32768)
                                                                                (nameData)
                                                                                (name . "Ch. 1 - A scalable language")
                                                                                (id . "52c0b5386548fde20105ea4e")
                                                                                (state . "incomplete"))])
                                                                (idCard . "52c0b529bdbf2ab3770570b7")
                                                                (idBoard . "51d99bbc1e1d8988390047f2")
                                                                (name .  "chapters")
                                                                (id .  "52c0b52ece09f28f6801fe5e")
                                                                (level . 2))))
              (expect "52c0b529bdbf2ab3770570b7" (orgtrello-data/entity-card-id actual-result))
              (expect "51d99bbc1e1d8988390047f2" (orgtrello-data/entity-board-id actual-result))
              (expect "chapters" (orgtrello-data/entity-name actual-result))
              (expect 2 (orgtrello-data/entity-level actual-result))
              (expect "52c0b52ece09f28f6801fe5e" (orgtrello-data/entity-id actual-result))
              (expect 2 (length (orgtrello-data/entity-items actual-result)))
              (expect t (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:position 16384 :name "introduction" :id "52c0b537ad469b9d6d044fa1" :checked "incomplete" :level 3))
                                    (first (orgtrello-data/entity-items actual-result))))
              (expect t (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:position 32768 :name "Ch. 1 - A scalable language" :id "52c0b5386548fde20105ea4e" :checked "incomplete" :level 3))
                                    (second (orgtrello-data/entity-items actual-result)))))

(expectations
 (expect :keyword (orgtrello-data/--deal-with-key :keyword))
 (expect :name    (orgtrello-data/--deal-with-key 'name))
 (expect nil      (orgtrello-data/--deal-with-key 'something-that-does-not-exist)))

(expectations
  (expect "hello there\nhow are you today\nthis is a hell of a ride" (orgtrello-buffer/filter-out-properties ":PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
hello there
how are you today
this is a hell of a ride"))
  (expect "hello there\nhow are you today\nthis is a hell of a ride" (orgtrello-buffer/filter-out-properties "hello there
how are you today
this is a hell of a ride")))

(expectations
  (expect "hello there"
    (with-temp-buffer
      (insert "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
hello there
")
      (orgtrello-buffer/extract-description-from-current-position)))

    (expect "hello there"
     (with-temp-buffer
       (insert "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
  - [X] Common-Lisp :PROPERTIES: {\"orgtrello-id\":\"52c94518b2c5b28e37012ba4\"}")
       (orgtrello-buffer/extract-description-from-current-position)))

    (expect "hello there\n"
     (with-temp-buffer
       (insert "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:

hello there

- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
  - [X] Common-Lisp :PROPERTIES: {\"orgtrello-id\":\"52c94518b2c5b28e37012ba4\"}")
       (orgtrello-buffer/extract-description-from-current-position))))

(provide 'org-trello-tests)
;;; org-trello-tests ends here
