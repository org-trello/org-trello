(require 'cl-lib)
(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(message "Launching tests!")

(load-file "org-trello.el")

;; ########################## orgtrello-hash

(expectations
;;  (desc "testing orgtrello-hash/make-hash-org")
  (expect "some name"       (gethash :name      (orgtrello-hash/make-hash-org 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org")))
  (expect "IN PROGRESS"     (gethash :keyword    (orgtrello-hash/make-hash-org 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org")))
  (expect 0                 (gethash :level      (orgtrello-hash/make-hash-org 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org")))
  (expect "some id"         (gethash :id         (orgtrello-hash/make-hash-org 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org")))
  (expect "due-date"        (gethash :due        (orgtrello-hash/make-hash-org 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org")))
  (expect :point            (gethash :position   (orgtrello-hash/make-hash-org 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org")))
  (expect "buffer-name.org" (gethash :buffername (orgtrello-hash/make-hash-org 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org"))))

(expectations
  (expect :some-method (gethash :method (orgtrello-hash/make-hash :some-method :some-uri)))
  (expect :some-uri    (gethash :uri    (orgtrello-hash/make-hash :some-method :some-uri)))
  (expect nil          (gethash :params (orgtrello-hash/make-hash :some-method :some-uri))))

;; ########################## orgtrello-data

(expectations
  (expect "some name :orgtrello-id-identifier:" (gethash :name      (orgtrello-data/--get-metadata '("buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect "IN PROGRESS"                          (gethash :keyword  (orgtrello-data/--get-metadata '("buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect 0                                      (gethash :level    (orgtrello-data/--get-metadata '("buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect :id                                    (gethash :id       (orgtrello-data/--get-metadata '("buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect :due                                   (gethash :due      (orgtrello-data/--get-metadata '("buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect :point                                 (gethash :position (orgtrello-data/--get-metadata '("buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil)))))

(expectations
  (expect "2013-07-18T02:00:00.000Z" (orgtrello-data/--convert-orgmode-date-to-trello-date "2013-07-18T02:00:00.000Z"))
  (expect "2013-07-29T14:00:00.000Z" (orgtrello-data/--convert-orgmode-date-to-trello-date "2013-07-29 lun. 14:00"))
  (expect "2013-07-29T00:00:00.000Z" (orgtrello-data/--convert-orgmode-date-to-trello-date "2013-07-29"))
  (expect nil                        (orgtrello-data/--convert-orgmode-date-to-trello-date nil)))

;; ########################## orgtrello-api

(expectations
  (expect "POST"                       (gethash :method (orgtrello-api/add-board ":some-board")))
  (expect "/boards"                   (gethash :uri    (orgtrello-api/add-board ":some-board")))
  (expect '(("name" . ":some-board")) (gethash :params (orgtrello-api/add-board ":some-board"))))

(expectations
  (expect "POST"                            (gethash :method (orgtrello-api/add-board "some-board" "some-description")))
  (expect "/boards"                        (gethash :uri    (orgtrello-api/add-board "some-board" "some-description")))
  (expect '(("name" . "some-board")
            ("desc" . "some-description")) (gethash :params (orgtrello-api/add-board "some-board" "some-description"))))

(expectations
  (expect "GET"                 (gethash :method (orgtrello-api/get-boards)))
  (expect "/members/me/boards" (gethash :uri    (orgtrello-api/get-boards)))
  (expect nil                  (gethash :params (orgtrello-api/get-boards))))

(expectations
  (expect "GET"          (gethash :method (orgtrello-api/get-board :id)))
  (expect "/boards/:id" (gethash :uri    (orgtrello-api/get-board :id)))
  (expect nil           (gethash :params (orgtrello-api/get-board :id))))

(expectations
  (expect "GET"                      (gethash :method (orgtrello-api/get-cards :board-id)))
  (expect "/boards/:board-id/cards" (gethash :uri    (orgtrello-api/get-cards :board-id)))
  (expect nil                       (gethash :params (orgtrello-api/get-cards :board-id))))

(expectations
  (expect "GET"              (gethash :method (orgtrello-api/get-card :card-id)))
  (expect "/cards/:card-id" (gethash :uri    (orgtrello-api/get-card :card-id)))
  (expect nil               (gethash :params (orgtrello-api/get-card :card-id))))

(expectations
  (expect "DELETE"           (gethash :method (orgtrello-api/delete-card :card-id)))
  (expect "/cards/:card-id" (gethash :uri    (orgtrello-api/delete-card :card-id)))
  (expect nil               (gethash :params (orgtrello-api/delete-card :card-id))))

(expectations
  (expect "GET"                      (gethash :method (orgtrello-api/get-lists :board-id)))
  (expect "/boards/:board-id/lists" (gethash :uri    (orgtrello-api/get-lists :board-id)))
  (expect nil                       (gethash :params (orgtrello-api/get-lists :board-id))))

(expectations
  (expect "GET"              (gethash :method (orgtrello-api/get-list :list-id)))
  (expect "/lists/:list-id" (gethash :uri    (orgtrello-api/get-list :list-id)))
  (expect nil               (gethash :params (orgtrello-api/get-list :list-id))))

(expectations
  (expect "PUT"                     (gethash :method (orgtrello-api/close-list :list-id)))
  (expect "/lists/:list-id/closed" (gethash :uri    (orgtrello-api/close-list :list-id)))
  (expect '((value . t))           (gethash :params (orgtrello-api/close-list :list-id))))

(expectations
  (expect "POST"                       (gethash :method (orgtrello-api/add-list "list-name" "board-id")))
  (expect "/lists/"                   (gethash :uri    (orgtrello-api/add-list "list-name" "board-id")))
  (expect '(("name" . "list-name")
            ("idBoard" . "board-id")) (gethash :params (orgtrello-api/add-list "list-name" "board-id"))))

(expectations
  (expect "POST"                                            (gethash :method (orgtrello-api/add-card "card-name" "list-id")))
  (expect "/cards/"                                        (gethash :uri    (orgtrello-api/add-card "card-name" "list-id")))
  (expect '(("name" . "card-name") ("idList" . "list-id")) (gethash :params (orgtrello-api/add-card "card-name" "list-id"))))

(expectations
  (expect "POST"                                                                 (gethash :method (orgtrello-api/add-card "card-name" "list-id" "due-date")))
  (expect "/cards/"                                                             (gethash :uri    (orgtrello-api/add-card "card-name" "list-id" "due-date")))
  (expect '(("due" . "due-date") ("name" . "card-name") ("idList" . "list-id")) (gethash :params (orgtrello-api/add-card "card-name" "list-id" "due-date"))))

(expectations
  (expect "GET"                    (gethash :method (orgtrello-api/get-cards-from-list :list-id)))
  (expect "/lists/:list-id/cards" (gethash :uri    (orgtrello-api/get-cards-from-list :list-id)))
  (expect nil                     (gethash :params (orgtrello-api/get-cards-from-list :list-id))))

(expectations
  (expect "PUT"                                                                                          (gethash :method (orgtrello-api/move-card :id-card :id-list "name-card")))
  (expect "/cards/:id-card"                                                                             (gethash :uri    (orgtrello-api/move-card :id-card :id-list "name-card")))
  (expect '(("name"   . "name-card")
                                                                                 ("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list "name-card"))))
(expectations
  (expect "PUT"                     (gethash :method (orgtrello-api/move-card :id-card :id-list)))
  (expect "/cards/:id-card"        (gethash :uri    (orgtrello-api/move-card :id-card :id-list)))
  (expect '(("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list))))

(expectations
  (expect "PUT"                                         (gethash :method (orgtrello-api/move-card :id-card :id-list nil :due-date)))
  (expect "/cards/:id-card"                            (gethash :uri    (orgtrello-api/move-card :id-card :id-list nil :due-date)))
  (expect '(("due" . :due-date) ("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list nil :due-date))))

(expectations
  (expect "PUT"                                                          (gethash :method (orgtrello-api/move-card :id-card :id-list :name :due-date)))
  (expect "/cards/:id-card"                                             (gethash :uri    (orgtrello-api/move-card :id-card :id-list :name :due-date)))
  (expect '(("due" . :due-date) ("name" . :name) ("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list :name :due-date))))

(expectations
  (expect "POST"                          (gethash :method (orgtrello-api/add-checklist "id-card" "name-checklist")))
  (expect "/cards/id-card/checklists"    (gethash :uri    (orgtrello-api/add-checklist "id-card" "name-checklist")))
  (expect '(("name" . "name-checklist")) (gethash :params (orgtrello-api/add-checklist "id-card" "name-checklist"))))

(expectations
  (expect "PUT"                           (gethash :method (orgtrello-api/update-checklist :id-checklist "name-checklist")))
  (expect "/checklists/:id-checklist"    (gethash :uri    (orgtrello-api/update-checklist :id-checklist "name-checklist")))
  (expect '(("name" . "name-checklist")) (gethash :params (orgtrello-api/update-checklist :id-checklist "name-checklist"))))

(expectations
  (expect "DELETE"                     (gethash :method (orgtrello-api/delete-checklist :id-checklist)))
  (expect "/checklists/:id-checklist" (gethash :uri    (orgtrello-api/delete-checklist :id-checklist)))
  (expect nil                         (gethash :params (orgtrello-api/delete-checklist :id-checklist))))

(expectations
  (expect "GET"                         (gethash :method (orgtrello-api/get-checklists :card-id)))
  (expect "/cards/:card-id/checklists" (gethash :uri    (orgtrello-api/get-checklists :card-id)))
  (expect nil                          (gethash :params (orgtrello-api/get-checklists :card-id))))

(expectations
  (expect "GET"                        (gethash :method (orgtrello-api/get-checklist :checklist-id)))
  (expect "/checklists/:checklist-id" (gethash :uri    (orgtrello-api/get-checklist :checklist-id)))
  (expect nil                         (gethash :params (orgtrello-api/get-checklist :checklist-id))))

(expectations
  (expect "POST"                                  (gethash :method (orgtrello-api/add-items :checklist-id "item-name" t)))
  (expect "/checklists/:checklist-id/checkItems" (gethash :uri    (orgtrello-api/add-items :checklist-id "item-name" t)))
  (expect '(("name"  . "item-name")
            ("checked" . t))                     (gethash :params (orgtrello-api/add-items :checklist-id "item-name" t))))

(expectations
  (expect "POST"                                  (gethash :method (orgtrello-api/add-items :checklist-id "item-name")))
  (expect "/checklists/:checklist-id/checkItems" (gethash :uri    (orgtrello-api/add-items :checklist-id "item-name")))
  (expect '(("name"  . "item-name"))             (gethash :params (orgtrello-api/add-items :checklist-id "item-name"))))

(expectations
  (expect "POST"                                  (gethash :method (orgtrello-api/add-items :checklist-id "item-name" nil)))
  (expect "/checklists/:checklist-id/checkItems" (gethash :uri    (orgtrello-api/add-items :checklist-id "item-name" nil)))
  (expect '(("name"  . "item-name"))             (gethash :params (orgtrello-api/add-items :checklist-id "item-name" nil))))

(expectations
  (expect "PUT"                                                                                                                        (gethash :method (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name "incomplete")))
  (expect "/cards/:card-id/checklist/:checklist-id/checkItem/:item-id"                                                                (gethash :uri    (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name "incomplete")))
  (expect '(("name"  . :item-name)
                                                                                                             ("state" ."incomplete")) (gethash :params (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name "incomplete"))))

(expectations
  (expect "PUT"                                                         (gethash :method (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name)))
  (expect "/cards/:card-id/checklist/:checklist-id/checkItem/:item-id" (gethash :uri    (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name)))
  (expect '(("name"  . :item-name))                                    (gethash :params (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name))))

(expectations
  (expect "PUT"                                                         (gethash :method (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name nil)))
  (expect "/cards/:card-id/checklist/:checklist-id/checkItem/:item-id" (gethash :uri    (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name nil)))
  (expect '(("name"  . :item-name))                                    (gethash :params (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name nil))))

(expectations
  (expect "DELETE"                                         (gethash :method (orgtrello-api/delete-item :checklist-id :item-id)))
  (expect "/checklists/:checklist-id/checkItems/:item-id" (gethash :uri    (orgtrello-api/delete-item :checklist-id :item-id))))

(expectations
  (expect "GET"                                    (gethash :method (orgtrello-api/get-items :checklist-id)))
  (expect "/checklists/:checklist-id/checkItems/" (gethash :uri    (orgtrello-api/get-items :checklist-id))))

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
  (expect (format "%s%s" *TRELLO-URL* "/uri")            (orgtrello-query/--compute-url *TRELLO-URL* "/uri"))
  (expect (format "%s%s" *TRELLO-URL* "/uri/other")      (orgtrello-query/--compute-url *TRELLO-URL* "/uri/other"))
  (expect (format "some-server/uri/some/other")          (orgtrello-query/--compute-url "some-server" "/uri/some/other")))

(expectations
  (expect :some-get (orgtrello-query/--method (orgtrello-hash/make-properties `((:method . :some-get) (:uri . :some-uri) (:sync . :some-sync) (:params . :some-params)))))
  (expect :some-uri (orgtrello-query/--uri (orgtrello-hash/make-properties `((:method . :some-get) (:uri . :some-uri) (:sync . :some-sync) (:params . :some-params)))))
  (expect :some-sync (orgtrello-query/--sync (orgtrello-hash/make-properties `((:method . :some-get) (:uri . :some-uri) (:sync . :some-sync) (:params . :some-params)))))
  (expect :some-params (orgtrello-query/--params (orgtrello-hash/make-properties `((:method . :some-get) (:uri . :some-uri) (:sync . :some-sync) (:params . :some-params))))))

(expectations
  (expect "some-id" (orgtrello-query/--id '((id . "some-id"))))
  (expect nil       (orgtrello-query/--id '((noid . "some-id")))))

(expectations
  (expect :some-name (orgtrello-query/--name '((name . :some-name))))
  (expect nil        (orgtrello-query/--name '((noname . :some-name)))))

(expectations
  (expect :some-list-id (orgtrello-query/--list-id '((idList . :some-list-id))))
  (expect nil           (orgtrello-query/--list-id '((noIdList . :some-list-id)))))

(expectations
  (expect :some-clist-ids (orgtrello-query/--checklist-ids '((idChecklists . :some-clist-ids))))
  (expect nil             (orgtrello-query/--checklist-ids '((no . :some-clist-ids)))))

(expectations
  (expect :some-check-items (orgtrello-query/--check-items '((checkItems . :some-check-items))))
  (expect nil               (orgtrello-query/--check-items '((no . :some-check-items)))))

(expectations
  (expect :some-card-id (orgtrello-query/--card-id '((idCard . :some-card-id))))
  (expect nil           (orgtrello-query/--card-id '((no . :some-card-id)))))

(expectations
  (expect :some-due (orgtrello-query/--due '((due . :some-due))))
  (expect nil       (orgtrello-query/--due '((no . :some-due)))))

(expectations
  (expect :some-state (orgtrello-query/--state '((state . :some-state))))
  (expect nil         (orgtrello-query/--state '((no . :some-state)))))

(expectations
  (expect :closed (orgtrello-query/--close-property '((closed . :closed))))
  (expect nil     (orgtrello-query/--close-property '((no . :some-state)))))

;; ########################## orgtrello-tests

(ert-deftest testing-orgtrello/--compute-data-from-entity-meta ()
  (let* ((entry   (orgtrello-hash/make-hash-org :some-level :some-keyword :some-name "some-id" :some-due :some-point :some-buffername)))
    (should (equal (orgtrello/--id entry)         "some-id"))
    (should (equal (orgtrello/--name entry)       :some-name))
    (should (equal (orgtrello/--keyword entry)    :some-keyword))
    (should (equal (orgtrello/--level entry)      :some-level))
    (should (equal (orgtrello/--due entry)        :some-due))
    (should (equal (orgtrello/--position entry)   :some-point))
    (should (equal (orgtrello/--buffername entry) :some-buffername))))

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
  (expect 'none (orgtrello/--compute-state-from-keyword ""))
  (expect 'none (orgtrello/--compute-state-from-keyword *TODO*))
  (expect 'done (orgtrello/--compute-state-from-keyword *DONE*))
  (expect 'none (orgtrello/--compute-state-from-keyword "IN")))

(expectations
  (expect "complete" (orgtrello/--item-compute-state t "DONE" "DONE"))
  (expect "complete" (orgtrello/--item-compute-state t "TODO" "DONE"))
  (expect "incomplete" (orgtrello/--item-compute-state t "DONE" "TODO"))
  (expect "incomplete" (orgtrello/--item-compute-state t "TODO" "TODO"))
  (expect "complete" (orgtrello/--item-compute-state nil "DONE" "DONE"))
  (expect "incomplete" (orgtrello/--item-compute-state nil "TODO" "DONE"))
  (expect "complete" (orgtrello/--item-compute-state nil "DONE" "TODO"))
  (expect "incomplete" (orgtrello/--item-compute-state nil "TODO" "TODO")) )

(expectations
  (expect t (orgtrello/--item-compute-check t "DONE" "DONE"))
  (expect t (orgtrello/--item-compute-check t "TODO" "DONE"))
  (expect nil (orgtrello/--item-compute-check t "DONE" "TODO"))
  (expect nil (orgtrello/--item-compute-check t "TODO" "TODO"))
  (expect t (orgtrello/--item-compute-check nil "DONE" "DONE"))
  (expect nil (orgtrello/--item-compute-check nil "TODO" "DONE"))
  (expect t (orgtrello/--item-compute-check nil "DONE" "TODO"))
  (expect nil (orgtrello/--item-compute-check nil "TODO" "TODO")) )

(expectations
 (expect t (and (dictionary-lessp "a" "b")
                (null (dictionary-lessp "b" "a"))
                (null (dictionary-lessp "a" "a"))
                (dictionary-lessp "1" "2")
                (null (dictionary-lessp "2" "1"))
                (null (dictionary-lessp "1" "1"))
                (dictionary-lessp "1" "a")
                (null (dictionary-lessp "a" "1"))
                (dictionary-lessp "" "a")
                (null (dictionary-lessp "a" ""))

                (dictionary-lessp "ab12" "ab34")
                (dictionary-lessp "ab12" "ab123")
                (dictionary-lessp "ab12" "ab12d")
                (dictionary-lessp "ab132" "ab132z")

                (dictionary-lessp "132zzzzz" "ab132z")
                (null (dictionary-lessp "1.32" "1ab")))))

(expectations
 (expect 'orgtrello-proxy/--delete      (orgtrello-proxy/--dispatch-action "delete"))
 (expect 'orgtrello-proxy/--sync-entity (orgtrello-proxy/--dispatch-action "sync-entity"))
 (expect nil                            (orgtrello-proxy/--dispatch-action "nothing")))

(expectations
  (expect "id"                                                        (orgtrello/--compute-marker-from-entry (orgtrello-hash/make-hash-org :level :kwd :name      "id"  :due :position :buffername)))
  (expect "orgtrello-marker-2a0b98e652ce6349a0659a7a8eeb3783ffe9a11a" (orgtrello/--compute-marker-from-entry (orgtrello-hash/make-hash-org :level :kwd "some-name" nil :due 1234      "buffername")))
  (expect "orgtrello-marker-6c59c5dcf6c83edaeb3f4923bfd929a091504bb3" (orgtrello/--compute-marker-from-entry (orgtrello-hash/make-hash-org :level :kwd "some-name" nil :due 4321      "some-other-buffername"))))

(expectations
  (expect "orgtrello-marker-2a0b98e652ce6349a0659a7a8eeb3783ffe9a11a" (orgtrello/compute-marker "buffername" "some-name" 1234))
  (expect "orgtrello-marker-6c59c5dcf6c83edaeb3f4923bfd929a091504bb3" (orgtrello/compute-marker "some-other-buffername" "some-name" 4321)))

(expectations
  (expect "marker-is-a-trello-id" (orgtrello-proxy/--compute-pattern-search-from-marker "marker-is-a-trello-id"))
  (expect "orgtrello-marker-tony" (orgtrello-proxy/--compute-pattern-search-from-marker "orgtrello-marker-tony")))

(expectations
  (expect t   (orgtrello/id-p "anything-that-does-not-start-with-orgtrello-marker"))
  (expect t   (orgtrello/id-p "agfgdsfgbdfgbdfgbdfshouldbetrue"))
  (expect t   (orgtrello/id-p "orgtrello-markeragfgdsfgbdfgbdfgbdfshouldbetrue"))
  (expect t   (orgtrello/id-p "should-be-true-orgtrello-marker-agfgdsfgbdfgbdfgbdf"))
  (expect nil (orgtrello/id-p "orgtrello-marker-shouldbenil"))
  (expect nil (orgtrello/id-p nil)))

(expectations
  (expect '(3 5 7) (--map (funcall (compose-fn '((lambda (it) (+ 1 it)) (lambda (it) (* 2 it)))) it) '(1 2 3))))

(expectations
  (expect "entity name"             (orgtrello-admin/--detail-entity 3 '((name . "entity name"))))
  (expect '((name . "entity name")) (orgtrello-admin/--detail-entity 5 '((name . "entity name")))))

(expectations
 (expect '("error0" "error1") (org-action/--filter-error-messages '("error0" :ok "error1")))
 (expect nil                  (org-action/--filter-error-messages '(:ok :ok :ok))))

(expectations
  (expect '(:ok) (org-action/--execute-controls '((lambda (e) :ok))))
  (expect '(:ok "ko") (org-action/--execute-controls '((lambda (e) :ok)
                                                       (lambda (e) "ko"))))
  (expect '(:ok) (org-action/--execute-controls '((lambda (a) :ok)) 'args))
  (expect '(:ok "ko") (org-action/--execute-controls '((lambda (a) :ok)
                                                       (lambda (a) "ko")) 'arg0)))

(expectations
  (expect   "List of errors:
 - Level too high. Do not deal with entity other than card/checklist/items!
"
      (org-action/--functional-controls-then-do
       '(orgtrello/--right-level-p)
       (orgtrello-hash/make-hash-org 4 :kwd :name nil :due :position :buffer-name)
       (lambda (entity s) (format "%S %s" entity s))
       "- hello"))

  (expect "#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:buffername :buffer-name :position :position :level 3 :keyword :kwd :name :name :id nil :due :due)) - hello"
    (org-action/--functional-controls-then-do
     '(orgtrello/--right-level-p)
     (orgtrello-hash/make-hash-org 3 :kwd :name nil :due :position :buffer-name)
     (lambda (entity s) (format "%S %s" entity s))
     "- hello")))

(expectations
  (expect "List of errors:
 - Entity must been synchronized with trello first!
"
    (org-action/--functional-controls-then-do
     '(orgtrello/--right-level-p orgtrello/--already-synced-p)
     (orgtrello-hash/make-hash-org 1 :kwd :name nil :due :position :buffer-name)
     (lambda (entity s) (format "%S %s" entity s))
     "- hello"))
  (expect "#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:buffername :buffer-name :position :position :level 1 :keyword :kwd :name :name :id \"some-id\" :due :due)) - hello"

    (org-action/--functional-controls-then-do
     '(orgtrello/--right-level-p orgtrello/--already-synced-p)
     (orgtrello-hash/make-hash-org 1 :kwd :name "some-id" :due :position :buffer-name)
     (lambda (entity s) (format "%S %s" entity s))
     "- hello")))

(expectations
  (expect "- message 1\n- message 2\n" (org-action/--compute-error-message '("message 1" "message 2"))))

(expectations
 (expect ":key:" (orgtrello-hash/key "key")))

(expectations
  (expect 2 (orgtrello-cbx/--level '("-" "[X]" "call" "people" "[4/4]")))
  (expect 3 (orgtrello-cbx/--level '("" "-" "[X]" "Peter")))
  (expect 3 (orgtrello-cbx/--level '("" "" "-" "[X]" "Peter")))
  (expect 3 (orgtrello-cbx/--level '("" "" "" "-" "[X]" "Peter")))
  (expect 4 (orgtrello-cbx/--level '("" "" "" "" "" "Peter"))))

(expectations
  (expect "DONE" (orgtrello-cbx/--status"[X]"))
  (expect "TODO" (orgtrello-cbx/--status"[ ]"))
  (expect "TODO" (orgtrello-cbx/--status"[]"))
  (expect "TODO" (orgtrello-cbx/--status"[-]"))
  (expect "TODO" (orgtrello-cbx/--status"")))

(expectations
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

(expectations
  (expect "[X]" (orgtrello-cbx/--retrieve-status '("" "" "-" "[X]" "Peter")))
  (expect "[]" (orgtrello-cbx/--retrieve-status '("" "" "-" "[]" "Peter")))
  (expect "[-]" (orgtrello-cbx/--retrieve-status '("" "" "-" "[-]" "Peter")))
  (expect "[ ]" (orgtrello-cbx/--retrieve-status '("" "" "-" "[ ]" "Peter"))))

(expectations
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

(expectations
  (expect "{\"orgtrello-id\":\"123\"}"                               (orgtrello-cbx/--to-properties `((,*ORGTRELLO-ID* . "123"))))
  (expect "{\"orgtrello-id\":\"456\"}"                               (orgtrello-cbx/--to-properties `((,*ORGTRELLO-ID* . "123") (,*ORGTRELLO-ID* . "456"))))
  (expect "{\"orgtrello-id\":\"def\", \"orgtrello-marker\":\"456\", \"orgtrello-id\":\"abc\"}" (orgtrello-cbx/--to-properties `(("orgtrello-id" . "abc") (orgtrello-marker . "456") (orgtrello-id . "def"))))
  (expect "{\"orgtrello-marker\":\"456\", \"orgtrello-id\":\"def\"}" (orgtrello-cbx/--to-properties `(("orgtrello-id" . "abc") (orgtrello-marker . "456") ("orgtrello-id" . "def"))))
  (expect "{\"orgtrello-marker\":\"456\", \"orgtrello-id\":\"def\"}" (orgtrello-cbx/--to-properties `((orgtrello-id . "abc") (orgtrello-marker . "456") (orgtrello-id . "def")))))

(expectations
  (expect '((orgtrello-id . "123")) (orgtrello-cbx/--from-properties "{\"orgtrello-id\":\"123\"}")))

(expectations
  (expect '((orgtrello-id . "123")) (orgtrello-cbx/--read-properties "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}")))

(expectations
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

(expectations
  (expect "123"    (orgtrello-cbx/--org-get-property "orgtrello-id" `((orgtrello-id . "123"))))
  (expect nil      (orgtrello-cbx/--org-get-property "orgtrello-id" `(("orgtrello-id" . "123"))))
  (expect nil      (orgtrello-cbx/--org-get-property 'orgtrello-id `(("orgtrello-id" . "123"))))
  (expect "123"    (orgtrello-cbx/--org-get-property 'orgtrello-id `((orgtrello-id . "123"))))
  (expect "marker" (orgtrello-cbx/--org-get-property "orgtrello-marker" `(("orgtrello-id" . "123") (orgtrello-marker . "marker")))))

(expectations
  (expect `((orgtrello-id . "10") (orgtrello-marker . "123"))    (orgtrello-cbx/--org-update-property "orgtrello-id" "10" `((orgtrello-marker . "123"))))
  (expect `((orgtrello-toto . "abc") (orgtrello-marker . "456")) (orgtrello-cbx/--org-update-property "orgtrello-toto" "abc" `((orgtrello-marker . "456"))))
  (expect `((orgtrello-id . "abc") (orgtrello-marker . "456"))   (orgtrello-cbx/--org-update-property "orgtrello-id" "abc" `((orgtrello-marker . "456") (orgtrello-id . "def")))))

(expectations
  (expect `(("orgtrello-id" . "123") (orgtrello-marker . "marker")) (orgtrello-cbx/--org-delete-property "orgtrello-id" `(("orgtrello-id" . "123") (orgtrello-marker . "marker"))))
  (expect `((orgtrello-marker . "marker")) (orgtrello-cbx/--org-delete-property "orgtrello-id" `((orgtrello-id . "123") (orgtrello-marker . "marker"))))
  (expect `((orgtrello-marker . "marker")) (orgtrello-cbx/--org-delete-property 'orgtrello-id `((orgtrello-id . "123") (orgtrello-marker . "marker"))))
  (expect `(("orgtrello-id" . "123") (orgtrello-marker . "marker")) (orgtrello-cbx/--org-delete-property 'orgtrello-id `(("orgtrello-id" . "123") (orgtrello-marker . "marker")))))

(expectations
  (expect '("  - [X] Peter " " {\"orgtrello-id\":\"456\"}") (orgtrello-cbx/--checkbox-split "  - [X] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}")))

(expectations
  (expect "{\"orgtrello-id\":\"456\"}" (orgtrello-cbx/--checkbox-metadata "  - [X] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}"))
  (expect ""                           (orgtrello-cbx/--checkbox-metadata "  - [X] Peter :PROPERTIES:"))
  (expect ""                           (orgtrello-cbx/--checkbox-metadata "  - [X] Peter :PROPERTIES: "))
  (expect nil                          (orgtrello-cbx/--checkbox-metadata "  - [X] Peter")))

(expectations
  (expect "  - [X] Peter" (orgtrello-cbx/--checkbox-data "  - [X] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}")))

(expectations
  (expect 'some-key (orgtrello-cbx/--key-to-search "some-key"))
  (expect 'some-key (orgtrello-cbx/--key-to-search 'some-key))
  (expect :some-key (orgtrello-cbx/--key-to-search :some-key)))

(expectations
  (expect nil (orgtrello-cbx/--list-is-checkbox-p '("1" "2" "3")))
  (expect nil (orgtrello-cbx/--list-is-checkbox-p '("1" "2" "3" "-")))
  (expect t   (orgtrello-cbx/--list-is-checkbox-p '("-" "1" "2" "3")))
  (expect t   (orgtrello-cbx/--list-is-checkbox-p '("" "" "-" "1" "2" "3"))))

(expectations
  (expect 1 (orgtrello-cbx/--get-level '(1 2 3)))
  (expect 2 (orgtrello-cbx/--get-level '(2 3))))

(expectations
  (expect "DEADLINE: <some-date>
" (orgtrello/--compute-due-date "some-date"))
  (expect "" (orgtrello/--compute-due-date nil)))

(expectations
  (expect "* name TODO
DEADLINE: <some-date>
" (orgtrello/--private-compute-card-to-org-entry "TODO" "name" "some-date"))
  (expect "* name TODO
" (orgtrello/--private-compute-card-to-org-entry "TODO" "name" nil)))

(expectations
  (expect "** name\n" (orgtrello/--compute-checklist-to-orgtrello-entry "name"))
  (expect "** name\n" (orgtrello/--compute-checklist-to-orgtrello-entry "name" nil))
  (expect "** name\n" (orgtrello/--compute-checklist-to-orgtrello-entry "name" 't))
  (expect "** name\n" (orgtrello/--compute-checklist-to-orgtrello-entry "name" nil 't))
  (expect "** name\n" (orgtrello/--compute-checklist-to-orgtrello-entry "name" 't nil))
  (expect "** name\n" (orgtrello/--compute-checklist-to-orgtrello-entry "name" nil nil))
  (expect "** name\n" (orgtrello/--compute-checklist-to-orgtrello-entry "name" 't 't)))

(expectations
  (expect ""      (orgtrello/--symbol " "  0))
  (expect "*"     (orgtrello/--symbol "*"  1))
  (expect "****"  (orgtrello/--symbol "**" 2))
  (expect "   "   (orgtrello/--symbol " "  3)) )

(expectations
  (expect ""    (orgtrello/--space 0))
  (expect " "   (orgtrello/--space 1))
  (expect "  "  (orgtrello/--space 2))
  (expect "   " (orgtrello/--space 3)) )

(expectations
  (expect ""    (orgtrello/--star 0))
  (expect "*"   (orgtrello/--star 1))
  (expect "**"  (orgtrello/--star 2))
  (expect "***" (orgtrello/--star 3)) )

(expectations
  (expect 0 (orgtrello/--compute-level-into-spaces 2))
  (expect 2 (orgtrello/--compute-level-into-spaces nil))
  (expect 2 (orgtrello/--compute-level-into-spaces 'any)))

(expectations
  (expect "- [X] name
" (orgtrello/--compute-checklist-to-org-checkbox "name" 2 "complete"))
  (expect "  - [X] name
" (orgtrello/--compute-checklist-to-org-checkbox "name" 3 "complete"))
  (expect "- [X] name
" (orgtrello/--compute-checklist-to-org-checkbox "name" 2 "complete"))
  (expect "  - [-] name
" (orgtrello/--compute-checklist-to-org-checkbox "name" 3 "incomplete")))

(expectations
  (expect "DONE" (orgrello/--compute-item-status "complete"))
  (expect "TODO" (orgrello/--compute-item-status "anything else will render incomplete")))

(expectations
  (expect "*** DONE name
" (orgtrello/--compute-item-to-orgtrello-entry "name" 3 "complete"))
  (expect "*** TODO name
" (orgtrello/--compute-item-to-orgtrello-entry "name" 3 "incomplete")))

(expectations
  (expect "- [-] name
" (orgtrello/--compute-checklist-to-org-entry `((name . "name")) t))
  (expect "- [-] name
" (orgtrello/--compute-checklist-to-org-entry `((name . "name")) t)))

(expectations
  (expect "** TODO name
" (orgtrello/--compute-checklist-to-org-entry `((name . "name")) nil))
  (expect "** TODO name
" (orgtrello/--compute-checklist-to-org-entry `((name . "name")) nil)))

(expectations
  (expect "  - [X] name
" (orgtrello/--compute-item-to-org-entry `((name . "name") (state . "complete")) t))
  (expect "  - [-] name
" (orgtrello/--compute-item-to-org-entry `((name . "name") (state . "incomplete")) t)))

(expectations
  (expect "*** DONE name
" (orgtrello/--compute-item-to-org-entry `((name . "name") (state . "complete")) nil))
  (expect "*** TODO name
" (orgtrello/--compute-item-to-org-entry `((name . "name") (state . "incomplete")) nil)))

(expectations
  (expect "test" (orgtrello-query/--retrieve-data 'marker `((marker . "test"))))
  (expect nil (orgtrello-query/--retrieve-data    'other  `((marker . "test"))))
  (expect "test" (orgtrello-query/--marker                `((marker . "test"))))
  (expect nil (orgtrello-query/--marker                   `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--buffername            `((buffername . "test"))))
  (expect nil (orgtrello-query/--buffername               `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--position              `((position . "test"))))
  (expect nil (orgtrello-query/--position                 `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--id                    `((id . "test"))))
  (expect nil (orgtrello-query/--id                       `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--name                  `((name . "test"))))
  (expect nil (orgtrello-query/--name                     `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--list-id               `((idList . "test"))))
  (expect nil (orgtrello-query/--list-id                  `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--checklist-ids         `((idChecklists . "test"))))
  (expect nil (orgtrello-query/--checklist-ids            `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--check-items           `((checkItems . "test"))))
  (expect nil (orgtrello-query/--check-items              `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--card-id               `((idCard . "test"))))
  (expect nil (orgtrello-query/--card-id                  `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--due                   `((due . "test"))))
  (expect nil (orgtrello-query/--due                      `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--state                 `((state . "test"))))
  (expect nil (orgtrello-query/--state                    `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--close-property        `((closed . "test"))))
  (expect nil (orgtrello-query/--close-property           `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--callback              `((callback . "test"))))
  (expect nil (orgtrello-query/--callback                 `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--sync-                 `((sync . "test"))))
  (expect nil (orgtrello-query/--sync-                    `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--level                 `((level . "test"))))
  (expect nil (orgtrello-query/--level                    `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--method-               `((method . "test"))))
  (expect nil (orgtrello-query/--method-                  `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--uri-                  `((uri . "test"))))
  (expect nil (orgtrello-query/--uri-                     `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--params-               `((params . "test"))))
  (expect nil (orgtrello-query/--params-                  `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--start                 `((start . "test"))))
  (expect nil (orgtrello-query/--start                    `((inexistant . "test"))))
  (expect "test" (orgtrello-query/--action                `((action . "test"))))
  (expect nil (orgtrello-query/--action                   `((inexistant . "test")))))

(expectations
  (expect :some-method (orgtrello-query/--method (orgtrello-hash/make-properties `((:method . :some-method )))))
  (expect nil (orgtrello-query/--method (orgtrello-hash/make-properties `((:inexistant . :some-method )))))
  (expect :some-uri (orgtrello-query/--method (orgtrello-hash/make-properties `((:method . :some-uri )))))
  (expect nil (orgtrello-query/--method (orgtrello-hash/make-properties `((:inexistant . :some-method )))))
  (expect :some-sync (orgtrello-query/--method (orgtrello-hash/make-properties `((:method . :some-sync )))))
  (expect nil (orgtrello-query/--method (orgtrello-hash/make-properties `((:inexistant . :some-method )))))
  (expect :some-params (orgtrello-query/--method (orgtrello-hash/make-properties `((:method . :some-params )))))
  (expect nil (orgtrello-query/--method (orgtrello-hash/make-properties `((:inexistant . :some-method ))))))

(expectations
  (expect "some-method" (orgtrello-query/gethash-data :method (orgtrello-hash/make-properties `((:method . "some-method")))))
  (expect nil           (orgtrello-query/gethash-data :method (orgtrello-hash/make-properties `((:inexistant . "some-method"))))))

(expectations
  (expect "DONE" (orgtrello/--compute-state-generic "complete" '("DONE" "TODO")))
  (expect "TODO" (orgtrello/--compute-state-generic "incomplete" '("DONE" "TODO")))

  (expect "[X]" (orgtrello/--compute-state-generic "complete" '("[X]" "[-]")))
  (expect "[-]" (orgtrello/--compute-state-generic "incomplete" '("[X]" "[-]"))))

(expectations
  (expect "[X]" (orgtrello/--compute-state-checkbox "complete"))
  (expect "[-]" (orgtrello/--compute-state-checkbox "incomplete")))

(expectations
 (expect "DONE" (orgtrello/--compute-state-item "complete"))
 (expect "TODO" (orgtrello/--compute-state-item "incomplete")))
(message "Tests done!")

(expectations
  (expect "this is a card" (orgtrello/--card-p `((idList . "this is a card"))))
  (expect nil (orgtrello/--card-p `((anything-else . "this is not a card")))))

(expectations
  (expect "this is a checklist" (orgtrello/--checklist-p `((idCard . "this is a checklist"))))
  (expect nil (orgtrello/--checklist-p `((anything-else . "this is not a checklist")))))

(expectations
  (expect "this is an item" (orgtrello/--item-p `((state . "this is an item"))))
  (expect nil (orgtrello/--checklist-p `((anything-else . "this is not a item")))))

(expectations
 (expect "- [X] call people [4/4]                                        :PROPERTIES: {\"orgtrello-id\":\"456\"}" (orgtrello-cbx/--justify-property-current-line "- [X] call people [4/4] :PROPERTIES: {\"orgtrello-id\":\"456\"}" 100))

 (expect "- [X] call people [4/4]                                         :PROPERTIES: {\"orgtrello-id\":\"456\"}" (orgtrello-cbx/--justify-property-current-line "- [X] call people [4/4]                                         :PROPERTIES: {\"orgtrello-id\":\"456\"}" 100)))

(expectations
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

(expectations
  (expect "- [X] some checkbox                                                                  :PROPERTIES: {\"orgtrello-id\":456}"
    (with-temp-buffer
      (insert "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}")
      (forward-line -1)
      (orgtrello-cbx/--write-properties-at-point (point) `(("orgtrello-id" . 456))))))

(expectations
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

(expectations
  (expect "- [X] some checkbox                                                                :PROPERTIES: {\"orgtrello-id\":\"abc\"}"
    (with-temp-buffer
      (insert "- [X] some checkbox")
      (forward-line -1)
      (orgtrello-cbx/org-set-property "orgtrello-id" "abc")
      (buffer-string)))
  (expect "- [X] some checkbox                                                                :PROPERTIES: {\"orgtrello-id\":\"def\"}"
    (with-temp-buffer
      (insert "- [X] some checkbox                                                                :PROPERTIES: {\"orgtrello-id\":\"123\"}")
      (forward-line -1)
      (orgtrello-cbx/org-set-property "orgtrello-id" "def")
      (buffer-string))))

(expectations
  (expect "- [X] some checkbox                                                                                    :PROPERTIES: {}"
    (with-temp-buffer
      (insert "- [X] some checkbox                                                                :PROPERTIES: {\"orgtrello-id\":\"123\"}")
      (forward-line -1)
      (orgtrello-cbx/org-delete-property "orgtrello-id")
      (buffer-string)))
  (expect "- [X] some checkbox                                                                :PROPERTIES: {\"orgtrello-id\":\"def\"}"
    (with-temp-buffer
      (insert "- [X] some checkbox                                                                :PROPERTIES: {\"orgtrello-id\":\"def\"}")
      (forward-line -1)
      (orgtrello-cbx/org-delete-property "inexistant")
      (buffer-string))))

(expectations
  (expect (format "%sorg-trello/3/test.org-123.el" elnode-webserver-docroot) (orgtrello-proxy/--compute-filename-from-entity '((level . 3) (buffername . "test.org") (position . "123")))))

(expectations
  (expect '(tr nil (td nil) (td nil "Action") (td nil "Entity") (td nil "Delete")) (orgtrello-admin/--header-table)))

(expectations
  (expect '(input ((type . "button") (onclick . "deleteEntities('/proxy/admin/entities/delete/id');") (value . "x"))) (orgtrello-admin/--delete-action '((id . "id"))))
  (expect ""                                          (orgtrello-admin/--delete-action '((name . "name")))))

(expectations
  (expect
      '(tr nil
           (td nil
               (i
                ((class . "icon-play"))))
           (td nil "test")
           (td nil "name")
           (td nil
               (input
                ((type . "button")
                 (onclick . "deleteEntities('/proxy/admin/entities/delete/id');")
                 (value . "x")))))
    (orgtrello-admin/--entity '((action . "test") (id . "id") (name . "name")) "icon-play"))

  (expect
      '(tr nil
           (td nil
               (i
                ((class . "icon-pause"))))
           (td nil "delete")
           (td nil "name")
           (td nil
               (input
                ((type . "button")
                 (onclick . "deleteEntities('/proxy/admin/entities/delete/id');")
                 (value . "x")))))
    (orgtrello-admin/--entity '((action . "delete") (id . "id") (name . "name")) "icon-pause"))

  (expect
      '(tr nil
           (td nil
               (i
                ((class . "icon-play"))))
           (td nil "test")
           (td nil "name 0")
           (td nil
               (input
                ((type . "button")
                 (onclick . "deleteEntities('/proxy/admin/entities/delete/id');")
                 (value . "x")))))
    (orgtrello-admin/--entity '((action . "test") (name . "name 0") (id . "id")) "icon-play"))

  (expect
      '(tr nil
           (td nil
               (i
                ((class . "icon-pause"))))
           (td nil "delete")
           (td nil "name 1")
           (td nil
               (input
                ((type . "button")
                 (onclick . "deleteEntities('/proxy/admin/entities/delete/id');")
                 (value . "x")))))
    (orgtrello-admin/--entity '((action . "delete") (name . "name 1") (id . "id")) "icon-pause")))

(expectations
  (expect
      '(input ((type . "button") (onclick . "deleteEntities('/proxy/admin/entities/delete/');") (value . "x")))
    (orgtrello-admin/--input-button-html "deleteEntities('/proxy/admin/entities/delete/');" "x")))

(expectations
  (expect
      '(div
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
                 ((type . "button")
                  (onclick . "deleteEntities('/proxy/admin/entities/delete/');")
                  (value . "Delete all")))))
         (span
          ((id . "next-actions")))))
    (orgtrello-admin/--main-body)))

(expectations
  (expect
      (esxml-to-xml `(div ((class . "hello")) "world"))
    (orgtrello-admin/--render-html `(div ((class . "hello")) "world"))))

(expectations
  (expect "None" (orgtrello-admin/--entities-as-html nil))
  (expect "None" (orgtrello-admin/--entities-as-html nil "icon-arrow-right"))
  (expect "None" (orgtrello-admin/--entities-as-html nil "icon-arrow-right" "icon-arrow-left"))
  (expect
      '(table
        ((class . "table table-striped table-bordered table-hover")
         (style . "font-size: 0.75em"))
        (tr nil
            (td nil)
            (td nil "Action")
            (td nil "Entity")
            (td nil "Delete"))
        (tr nil
            (td nil
                (i
                 ((class . "icon-arrow-right"))))
            (td nil "create")
            (td nil "name 0")
            (td nil
                (input
                 ((type . "button")
                  (onclick . "deleteEntities('/proxy/admin/entities/delete/id 0');")
                  (value . "x")))))
        (tr nil
            (td nil
                (i
                 ((class . "icon-arrow-up"))))
            (td nil "delete")
            (td nil "name 1")
            (td nil "")))
    (orgtrello-admin/--entities-as-html '(((action . "create") (name . "name 0") (id . "id 0")) ((action . "delete") (name . "name 1")))))

  (expect
      '(table
        ((class . "table table-striped table-bordered table-hover")
         (style . "font-size: 0.75em"))
        (tr nil
            (td nil)
            (td nil "Action")
            (td nil "Entity")
            (td nil "Delete"))
        (tr nil
            (td nil
                (i
                 ((class . "icon-arrow-right"))))
            (td nil "create")
            (td nil "name 0")
            (td nil ""))
        (tr nil
            (td nil
                (i
                 ((class . "icon-arrow-up"))))
            (td nil "delete")
            (td nil "name 1")
            (td nil "")))
    (orgtrello-admin/--entities-as-html '(((action . "create") (name . "name 0")) ((action . "delete") (name . "name 1"))) "icon-arrow-right"))

  (expect
      '(table
        ((class . "table table-striped table-bordered table-hover")
         (style . "font-size: 0.75em"))
        (tr nil
            (td nil)
            (td nil "Action")
            (td nil "Entity")
            (td nil "Delete"))
        (tr nil
            (td nil
                (i
                 ((class . "icon-arrow-right"))))
            (td nil "create")
            (td nil "name 0")
            (td nil ""))
        (tr nil
            (td nil
                (i
                 ((class . "icon-arrow-up"))))
            (td nil "delete")
            (td nil "name 1")
            (td nil "")))
    (orgtrello-admin/--entities-as-html '(((action . "create") (name . "name 0")) ((action . "delete") (name . "name 1"))) nil "icon-arrow-up"))

  (expect
      '(table
        ((class . "table table-striped table-bordered table-hover")
         (style . "font-size: 0.75em"))
        (tr nil
            (td nil)
            (td nil "Action")
            (td nil "Entity")
            (td nil "Delete"))
        (tr nil
            (td nil
                (i
                 ((class . "icon-play"))))
            (td nil "create")
            (td nil "name 0")
            (td nil ""))
        (tr nil
            (td nil
                (i
                 ((class . "icon-pause"))))
            (td nil "delete")
            (td nil "name 1")
            (td nil "")))
    (orgtrello-admin/--entities-as-html '(((action . "create") (name . "name 0")) ((action . "delete") (name . "name 1"))) "icon-play" "icon-pause")))

(expectations
  (expect
      '((tr nil
            (td nil
                (i
                 ((class . "next"))))
            (td nil "action")
            (td nil "nil")
            (td nil
                (input
                 ((type . "button")
                  (onclick . "deleteEntities('/proxy/admin/entities/delete/id');")
                  (value . "x")))))
        (tr nil
            (td nil
                (i
                 ((class . "next"))))
            (td nil "action")
            (td nil "nil")
            (td nil
                (input
                 ((type . "button")
                  (onclick . "deleteEntities('/proxy/admin/entities/delete/id2');")
                  (value . "x"))))))
    (orgtrello-admin/--list-entities-as-html '(((action . "action") (id . "id") (marker . "marker"))
                                               ((action . "action") (id . "id2") (marker . "marker2"))) "next")))

(provide 'org-trello-tests)
;;; org-trello-tests ends here
