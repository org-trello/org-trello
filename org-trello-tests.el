(require 'cl-lib)
(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(message "Launching tests!")

(load-file "org-trello.el")

;; ########################## orgtrello-hash

(expectations
;;  (desc "testing orgtrello-hash/make-hash-org")
  (expect "some title"  (gethash :title   (orgtrello-hash/make-hash-org 0 "IN PROGRESS" "some title" "some id" "due-date")))
  (expect "IN PROGRESS" (gethash :keyword (orgtrello-hash/make-hash-org 0 "IN PROGRESS" "some title" "some id" "due-date")))
  (expect 0             (gethash :level   (orgtrello-hash/make-hash-org 0 "IN PROGRESS" "some title" "some id" "due-date")))
  (expect "some id"     (gethash :id      (orgtrello-hash/make-hash-org 0 "IN PROGRESS" "some title" "some id" "due-date")))
  (expect "due-date"    (gethash :due     (orgtrello-hash/make-hash-org 0 "IN PROGRESS" "some title" "some id" "due-date"))))

(expectations
  (expect :some-method (gethash :method (orgtrello-hash/make-hash :some-method :some-uri)))
  (expect :some-uri    (gethash :uri    (orgtrello-hash/make-hash :some-method :some-uri)))
  (expect nil          (gethash :params (orgtrello-hash/make-hash :some-method :some-uri))))

;; ########################## orgtrello-data

(expectations
  (expect "some title :orgtrello-id-identifier:" (gethash :title   (orgtrello-data/--get-metadata '(:id :due 0 1 "IN PROGRESS" nil "some title :orgtrello-id-identifier:" nil))))
  (expect "IN PROGRESS"                          (gethash :keyword (orgtrello-data/--get-metadata '(:id :due 0 1 "IN PROGRESS" nil "some title :orgtrello-id-identifier:" nil))))
  (expect 0                                      (gethash :level   (orgtrello-data/--get-metadata '(:id :due 0 1 "IN PROGRESS" nil "some title :orgtrello-id-identifier:" nil))))
  (expect :id                                    (gethash :id      (orgtrello-data/--get-metadata '(:id :due 0 1 "IN PROGRESS" nil "some title :orgtrello-id-identifier:" nil))))
  (expect :due                                   (gethash :due     (orgtrello-data/--get-metadata '(:id :due 0 1 "IN PROGRESS" nil "some title :orgtrello-id-identifier:" nil)))))

(expectations
  (expect "2013-07-18T02:00:00.000Z" (orgtrello-data/--convert-orgmode-date-to-trello-date "2013-07-18T02:00:00.000Z"))
  (expect "2013-07-29T14:00:00.000Z" (orgtrello-data/--convert-orgmode-date-to-trello-date "2013-07-29 lun. 14:00"))
  (expect "2013-07-29T00:00:00.000Z" (orgtrello-data/--convert-orgmode-date-to-trello-date "2013-07-29"))
  (expect nil                        (orgtrello-data/--convert-orgmode-date-to-trello-date nil)))

;; ########################## orgtrello-api

(expectations
  (expect :post                       (gethash :method (orgtrello-api/add-board ":some-board")))
  (expect "/boards"                   (gethash :uri    (orgtrello-api/add-board ":some-board")))
  (expect '(("name" . ":some-board")) (gethash :params (orgtrello-api/add-board ":some-board"))))

(expectations
  (expect :post                            (gethash :method (orgtrello-api/add-board "some-board" "some-description")))
  (expect "/boards"                        (gethash :uri    (orgtrello-api/add-board "some-board" "some-description")))
  (expect '(("name" . "some-board")
            ("desc" . "some-description")) (gethash :params (orgtrello-api/add-board "some-board" "some-description"))))

(expectations
  (expect :get                 (gethash :method (orgtrello-api/get-boards)))
  (expect "/members/me/boards" (gethash :uri    (orgtrello-api/get-boards)))
  (expect nil                  (gethash :params (orgtrello-api/get-boards))))

(expectations
  (expect :get          (gethash :method (orgtrello-api/get-board :id)))
  (expect "/boards/:id" (gethash :uri    (orgtrello-api/get-board :id)))
  (expect nil           (gethash :params (orgtrello-api/get-board :id))))

(expectations
  (expect :get                      (gethash :method (orgtrello-api/get-cards :board-id)))
  (expect "/boards/:board-id/cards" (gethash :uri    (orgtrello-api/get-cards :board-id)))
  (expect nil                       (gethash :params (orgtrello-api/get-cards :board-id))))

(expectations
  (expect :get              (gethash :method (orgtrello-api/get-card :card-id)))
  (expect "/cards/:card-id" (gethash :uri    (orgtrello-api/get-card :card-id)))
  (expect nil               (gethash :params (orgtrello-api/get-card :card-id))))

(expectations
  (expect :delete           (gethash :method (orgtrello-api/delete-card :card-id)))
  (expect "/cards/:card-id" (gethash :uri    (orgtrello-api/delete-card :card-id)))
  (expect nil               (gethash :params (orgtrello-api/delete-card :card-id))))

(expectations
  (expect :get                      (gethash :method (orgtrello-api/get-lists :board-id)))
  (expect "/boards/:board-id/lists" (gethash :uri    (orgtrello-api/get-lists :board-id)))
  (expect nil                       (gethash :params (orgtrello-api/get-lists :board-id))))

(expectations
  (expect :get              (gethash :method (orgtrello-api/get-list :list-id)))
  (expect "/lists/:list-id" (gethash :uri    (orgtrello-api/get-list :list-id)))
  (expect nil               (gethash :params (orgtrello-api/get-list :list-id))))

(expectations
  (expect :put                     (gethash :method (orgtrello-api/close-list :list-id)))
  (expect "/lists/:list-id/closed" (gethash :uri    (orgtrello-api/close-list :list-id)))
  (expect '((value . t))           (gethash :params (orgtrello-api/close-list :list-id))))

(expectations
  (expect :post                       (gethash :method (orgtrello-api/add-list "list-name" "board-id")))
  (expect "/lists/"                   (gethash :uri    (orgtrello-api/add-list "list-name" "board-id")))
  (expect '(("name" . "list-name")
            ("idBoard" . "board-id")) (gethash :params (orgtrello-api/add-list "list-name" "board-id"))))

(expectations
  (expect :post                                            (gethash :method (orgtrello-api/add-card "card-name" "list-id")))
  (expect "/cards/"                                        (gethash :uri    (orgtrello-api/add-card "card-name" "list-id")))
  (expect '(("name" . "card-name") ("idList" . "list-id")) (gethash :params (orgtrello-api/add-card "card-name" "list-id"))))

(expectations
  (expect :post                                                                 (gethash :method (orgtrello-api/add-card "card-name" "list-id" "due-date")))
  (expect "/cards/"                                                             (gethash :uri    (orgtrello-api/add-card "card-name" "list-id" "due-date")))
  (expect '(("due" . "due-date") ("name" . "card-name") ("idList" . "list-id")) (gethash :params (orgtrello-api/add-card "card-name" "list-id" "due-date"))))

(expectations
  (expect :get                    (gethash :method (orgtrello-api/get-cards-from-list :list-id)))
  (expect "/lists/:list-id/cards" (gethash :uri    (orgtrello-api/get-cards-from-list :list-id)))
  (expect nil                     (gethash :params (orgtrello-api/get-cards-from-list :list-id))))

(expectations
  (expect :put                                                                                          (gethash :method (orgtrello-api/move-card :id-card :id-list "name-card")))
  (expect "/cards/:id-card"                                                                             (gethash :uri    (orgtrello-api/move-card :id-card :id-list "name-card")))
  (expect '(("name"   . "name-card")
                                                                                 ("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list "name-card"))))
(expectations
  (expect :put                     (gethash :method (orgtrello-api/move-card :id-card :id-list)))
  (expect "/cards/:id-card"        (gethash :uri    (orgtrello-api/move-card :id-card :id-list)))
  (expect '(("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list))))

(expectations
  (expect :put                                         (gethash :method (orgtrello-api/move-card :id-card :id-list nil :due-date)))
  (expect "/cards/:id-card"                            (gethash :uri    (orgtrello-api/move-card :id-card :id-list nil :due-date)))
  (expect '(("due" . :due-date) ("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list nil :due-date))))

(expectations
  (expect :put                                                          (gethash :method (orgtrello-api/move-card :id-card :id-list :name :due-date)))
  (expect "/cards/:id-card"                                             (gethash :uri    (orgtrello-api/move-card :id-card :id-list :name :due-date)))
  (expect '(("due" . :due-date) ("name" . :name) ("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list :name :due-date))))

(expectations
  (expect :post                          (gethash :method (orgtrello-api/add-checklist "id-card" "name-checklist")))
  (expect "/cards/id-card/checklists"    (gethash :uri    (orgtrello-api/add-checklist "id-card" "name-checklist")))
  (expect '(("name" . "name-checklist")) (gethash :params (orgtrello-api/add-checklist "id-card" "name-checklist"))))

(expectations
  (expect :put                           (gethash :method (orgtrello-api/update-checklist :id-checklist "name-checklist")))
  (expect "/checklists/:id-checklist"    (gethash :uri    (orgtrello-api/update-checklist :id-checklist "name-checklist")))
  (expect '(("name" . "name-checklist")) (gethash :params (orgtrello-api/update-checklist :id-checklist "name-checklist"))))

(expectations
  (expect :delete                     (gethash :method (orgtrello-api/delete-checklist :id-checklist)))
  (expect "/checklists/:id-checklist" (gethash :uri    (orgtrello-api/delete-checklist :id-checklist)))
  (expect nil                         (gethash :params (orgtrello-api/delete-checklist :id-checklist))))

(expectations
  (expect :get                         (gethash :method (orgtrello-api/get-checklists :card-id)))
  (expect "/cards/:card-id/checklists" (gethash :uri    (orgtrello-api/get-checklists :card-id)))
  (expect nil                          (gethash :params (orgtrello-api/get-checklists :card-id))))

(expectations
  (expect :get                        (gethash :method (orgtrello-api/get-checklist :checklist-id)))
  (expect "/checklists/:checklist-id" (gethash :uri    (orgtrello-api/get-checklist :checklist-id)))
  (expect nil                         (gethash :params (orgtrello-api/get-checklist :checklist-id))))

(expectations
  (expect :post                                  (gethash :method (orgtrello-api/add-tasks :checklist-id "task-name" t)))
  (expect "/checklists/:checklist-id/checkItems" (gethash :uri    (orgtrello-api/add-tasks :checklist-id "task-name" t)))
  (expect '(("name"  . "task-name")
            ("checked" . t))                     (gethash :params (orgtrello-api/add-tasks :checklist-id "task-name" t))))

(expectations
  (expect :post                                  (gethash :method (orgtrello-api/add-tasks :checklist-id "task-name")))
  (expect "/checklists/:checklist-id/checkItems" (gethash :uri    (orgtrello-api/add-tasks :checklist-id "task-name")))
  (expect '(("name"  . "task-name"))             (gethash :params (orgtrello-api/add-tasks :checklist-id "task-name"))))

(expectations
  (expect :post                                  (gethash :method (orgtrello-api/add-tasks :checklist-id "task-name" nil)))
  (expect "/checklists/:checklist-id/checkItems" (gethash :uri    (orgtrello-api/add-tasks :checklist-id "task-name" nil)))
  (expect '(("name"  . "task-name"))             (gethash :params (orgtrello-api/add-tasks :checklist-id "task-name" nil))))

(expectations
  (expect :put                                                                                                                        (gethash :method (orgtrello-api/update-task :card-id :checklist-id :task-id :task-name "incomplete")))
  (expect "/cards/:card-id/checklist/:checklist-id/checkItem/:task-id"                                                                (gethash :uri    (orgtrello-api/update-task :card-id :checklist-id :task-id :task-name "incomplete")))
  (expect '(("name"  . :task-name)
                                                                                                             ("state" ."incomplete")) (gethash :params (orgtrello-api/update-task :card-id :checklist-id :task-id :task-name "incomplete"))))

(expectations
  (expect :put                                                         (gethash :method (orgtrello-api/update-task :card-id :checklist-id :task-id :task-name)))
  (expect "/cards/:card-id/checklist/:checklist-id/checkItem/:task-id" (gethash :uri    (orgtrello-api/update-task :card-id :checklist-id :task-id :task-name)))
  (expect '(("name"  . :task-name))                                    (gethash :params (orgtrello-api/update-task :card-id :checklist-id :task-id :task-name))))

(expectations
  (expect :put                                                         (gethash :method (orgtrello-api/update-task :card-id :checklist-id :task-id :task-name nil)))
  (expect "/cards/:card-id/checklist/:checklist-id/checkItem/:task-id" (gethash :uri    (orgtrello-api/update-task :card-id :checklist-id :task-id :task-name nil)))
  (expect '(("name"  . :task-name))                                    (gethash :params (orgtrello-api/update-task :card-id :checklist-id :task-id :task-name nil))))

(expectations
  (expect :delete                                         (gethash :method (orgtrello-api/delete-task :checklist-id :task-id)))
  (expect "/checklists/:checklist-id/checkItems/:task-id" (gethash :uri    (orgtrello-api/delete-task :checklist-id :task-id))))

(expectations
  (expect :get                                    (gethash :method (orgtrello-api/get-tasks :checklist-id)))
  (expect "/checklists/:checklist-id/checkItems/" (gethash :uri    (orgtrello-api/get-tasks :checklist-id))))

(expectations
  (expect '(((pos . 16384)
             (name . "yes")
             (id . "51da82abf4deb8010b003850")
             (state . "incomplete"))
            ((pos . 32768)
             (name . "empty")
             (id . "51da82abc2b917772100240e")
             (state . "incomplete"))
            ((pos . 49152)
             (name . "no")
             (id . "51da82ac6054b8c35300ba98")
             (state . "incomplete")))  (orgtrello/--do-retrieve-checklists-and-items '((checkItems . [((pos . 16384)
                                                                                                       (name . "yes")
                                                                                                       (id . "51da82abf4deb8010b003850")
                                                                                                       (state . "incomplete"))
                                                                                                      ((pos . 32768)
                                                                                                       (name . "empty")
                                                                                                       (id . "51da82abc2b917772100240e")
                                                                                                       (state . "incomplete"))
                                                                                                      ((pos . 49152)
                                                                                                       (name . "no")
                                                                                                       (id . "51da82ac6054b8c35300ba98")
                                                                                                       (state . "incomplete"))])))))

;; ########################## orgtrello-query

(expectations
  (expect "GET"    (orgtrello-query/--compute-method :get))
  (expect "POST"   (orgtrello-query/--compute-method :post))
  (expect "PUT"    (orgtrello-query/--compute-method :put))
  (expect "DELETE" (orgtrello-query/--compute-method :delete)))

(expectations
  (expect (format "%s%s" *TRELLO-URL* "/uri")            (orgtrello-query/--compute-url "/uri")     )
  (expect (format "%s%s" *TRELLO-URL* "/uri/other")      (orgtrello-query/--compute-url "/uri/other")     )
  (expect (format "%s%s" *TRELLO-URL* "/uri/some/other") (orgtrello-query/--compute-url "/uri/some/other")     ))

;; ########################## orgtrello-tests

(ert-deftest testing-orgtrello/--merge-map ()
  (let* ((entry   (orgtrello-hash/make-hash-org :level :method "the name of the entry" nil "due"))
         (map-ids (make-hash-table :test 'equal)))
    (puthash "the name of the entry" :some-id map-ids)
    (should (equal (gethash :id (orgtrello/--merge-map entry map-ids)) :some-id))))

(ert-deftest testing-orgtrello/--merge-map2 ()
  (let* ((entry   (orgtrello-hash/make-hash-org :level :method :title :id-already-there "due"))
         (map-ids (make-hash-table :test 'equal)))
    (puthash :title :some-id map-ids)
    (should (equal (gethash :id (orgtrello/--merge-map entry map-ids)) :id-already-there))))

(ert-deftest testing-orgtrello/--merge-map3 ()
  (let* ((entry   (orgtrello-hash/make-hash-org :level :method :title :id-already-there "due"))
         (map-ids (make-hash-table :test 'equal)))
    (should (equal (gethash :id (orgtrello/--merge-map entry map-ids)) :id-already-there))))

(ert-deftest testing-orgtrello/--id-name ()
  (let* ((entities [((id . "id")
                     (shortUrl . "https://trello.com/b/ePrdEnzC")
                     (url . "https://trello.com/board/devops/4f96a984dbb00d733b04d8b5") (name . "testing board"))
                    ((id . "another-id")
                     f(shortUrl . "https://trello.com/b/ePrdEnzC")
                     (url . "https://trello.com/board/devops/4f96a984dbb00d733b04d8b5")
                     (name . "testing board 2"))
                    ((id . "yet-another-id")
                     (shortUrl . "https://trello.com/b/ePrdEnzC")
                     (url . "https://trello.com/board/devops/4f96a984dbb00d733b04d8b5")
                     (name . "testing board 3"))])
         (hashtable-result (orgtrello/--id-name entities))
         (hashtable-expected (make-hash-table :test 'equal)))
    (puthash "id" "testing board" hashtable-expected)
    (puthash "another-id" "testing board 2" hashtable-expected)
    (puthash "yet-another-id" "testing board 3" hashtable-expected)
    (should (equal (gethash "id" hashtable-result) (gethash "id" hashtable-expected)))
    (should (equal (gethash "another-id" hashtable-result) (gethash "another-id" hashtable-expected)))
    (should (equal (gethash "yet-another-id" hashtable-result) (gethash "yet-another-id" hashtable-expected)))
    (should (equal (hash-table-count hashtable-result) (hash-table-count hashtable-expected)))))

(ert-deftest testing-orgtrello/--name-id ()
  (let* ((entities [((id . "id")
                     (shortUrl . "https://trello.com/b/ePrdEnzC")
                     (name . "testing board"))
                    ((id . "another-id")
                     f(shortUrl . "https://trello.com/b/ePrdEnzC")
                     (name . "testing board 2"))
                    ((id . "yet-another-id")
                     (shortUrl . "https://trello.com/b/ePrdEnzC")
                     (name . "testing board 3"))])
         (hashtable-result (orgtrello/--name-id entities))
         (hashtable-expected (make-hash-table :test 'equal)))
    (puthash "testing board" "id" hashtable-expected)
    (puthash "testing board 2" "another-id"  hashtable-expected)
    (puthash "testing board 3" "yet-another-id"  hashtable-expected)
    (should (equal (gethash "testing board" hashtable-result) (gethash "testing board" hashtable-expected)))
    (should (equal (gethash "testing board 2" hashtable-result) (gethash "testing board 2" hashtable-expected)))
    (should (equal (gethash "testing board 3" hashtable-result) (gethash "testing board 3" hashtable-expected)))
    (should (equal (hash-table-count hashtable-result) (hash-table-count hashtable-expected)))))

(expectations
  (expect ""     (orgtrello/--compute-state-from-keyword ""))
  (expect "TODO" (orgtrello/--compute-state-from-keyword *TODO*))
  (expect "DONE" (orgtrello/--compute-state-from-keyword *DONE*))
  (expect "TODO" (orgtrello/--compute-state-from-keyword "IN")))

(expectations
  (expect "complete" (orgtrello/--task-compute-state t "DONE" "DONE"))
  (expect "complete" (orgtrello/--task-compute-state t "TODO" "DONE"))
  (expect "incomplete" (orgtrello/--task-compute-state t "DONE" "TODO"))
  (expect "incomplete" (orgtrello/--task-compute-state t "TODO" "TODO"))
  (expect "complete" (orgtrello/--task-compute-state nil "DONE" "DONE"))
  (expect "incomplete" (orgtrello/--task-compute-state nil "TODO" "DONE"))
  (expect "complete" (orgtrello/--task-compute-state nil "DONE" "TODO"))
  (expect "incomplete" (orgtrello/--task-compute-state nil "TODO" "TODO")) )

(expectations
  (expect t (orgtrello/--task-compute-check t "DONE" "DONE"))
  (expect t (orgtrello/--task-compute-check t "TODO" "DONE"))
  (expect nil (orgtrello/--task-compute-check t "DONE" "TODO"))
  (expect nil (orgtrello/--task-compute-check t "TODO" "TODO"))
  (expect t (orgtrello/--task-compute-check nil "DONE" "DONE"))
  (expect nil (orgtrello/--task-compute-check nil "TODO" "DONE"))
  (expect t (orgtrello/--task-compute-check nil "DONE" "TODO"))
  (expect nil (orgtrello/--task-compute-check nil "TODO" "TODO")) )

(message "Tests done!")
