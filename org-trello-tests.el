(require 'cl-lib)
(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(message "Launching tests!")

(load-file "org-trello.el")

;; ########################## util test function

(defun hash-equal (hash1 hash2) "Compare two hash tables to see whether they are equal."
  (and (= (hash-table-count hash1) (hash-table-count hash2))
       (catch 'flag (maphash (lambda (x y) (or (equal (gethash x hash2) y) (throw 'flag nil))) hash1)
              (throw 'flag t))))

(ert-deftest testing-hash-equal ()
  (should (hash-equal (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO")))
                      (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO")))))
  (should (not (hash-equal (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO")))
                           (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "DONE")))))))

;; ########################## orgtrello-hash

(expectations
;;  (desc "testing orgtrello-hash/make-hash-org")
  (expect "some name"       (gethash :name       (orgtrello-hash/make-hash-org 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org")))
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
  (expect "some name :orgtrello-id-identifier:"  (gethash :name     (orgtrello-data/--get-metadata '("buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
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
  (expect "GET"                 (gethash :method (orgtrello-api/get-boards)))
  (expect "/members/me/boards" (gethash :uri    (orgtrello-api/get-boards)))
  (expect nil                  (gethash :params (orgtrello-api/get-boards))))

(expectations
  (expect "GET"                                                         (gethash :method (orgtrello-api/get-board :id)))
  (expect "/boards/:id"                                                 (gethash :uri    (orgtrello-api/get-board :id)))
  (expect '(("memberships" . "active") ("memberships_member" . "true")) (gethash :params (orgtrello-api/get-board :id))))

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
  (expect "POST"                      (gethash :method (orgtrello-api/add-list "list-name" "board-id")))
  (expect "/lists/"                   (gethash :uri    (orgtrello-api/add-list "list-name" "board-id")))
  (expect '(("name" . "list-name")
            ("idBoard" . "board-id")) (gethash :params (orgtrello-api/add-list "list-name" "board-id"))))

(expectations
  (expect "PUT"                                             (gethash :method (orgtrello-api/move-card :id-card :id-list "name-card")))
  (expect "/cards/:id-card"                                 (gethash :uri    (orgtrello-api/move-card :id-card :id-list "name-card")))
  (expect '(("name"   . "name-card") ("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list "name-card"))))

(expectations
  (expect "PUT"                    (gethash :method (orgtrello-api/move-card :id-card :id-list)))
  (expect "/cards/:id-card"        (gethash :uri    (orgtrello-api/move-card :id-card :id-list)))
  (expect '(("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list))))

(expectations
  (expect "PUT"                                     (gethash :method (orgtrello-api/move-card :id-card :id-list :name)))
  (expect "/cards/:id-card"                         (gethash :uri    (orgtrello-api/move-card :id-card :id-list :name)))
  (expect '(("name" . :name) ("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list :name))))

(expectations
  (expect "PUT"                                        (gethash :method (orgtrello-api/move-card :id-card :id-list nil :due-date)))
  (expect "/cards/:id-card"                            (gethash :uri    (orgtrello-api/move-card :id-card :id-list nil :due-date)))
  (expect '(("due" . :due-date) ("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list nil :due-date))))

(expectations
  (expect "PUT"                                                         (gethash :method (orgtrello-api/move-card :id-card :id-list :name :due-date)))
  (expect "/cards/:id-card"                                             (gethash :uri    (orgtrello-api/move-card :id-card :id-list :name :due-date)))
  (expect '(("due" . :due-date) ("name" . :name) ("idList" . :id-list)) (gethash :params (orgtrello-api/move-card :id-card :id-list :name :due-date))))

(expectations
  (expect "GET"                    (gethash :method (orgtrello-api/get-cards-from-list :list-id)))
  (expect "/lists/:list-id/cards" (gethash :uri    (orgtrello-api/get-cards-from-list :list-id)))
  (expect nil                     (gethash :params (orgtrello-api/get-cards-from-list :list-id))))

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
  (expect "POST"                                     (gethash :method (orgtrello-api/add-items :checklist-id "item-name" t)))
  (expect "/checklists/:checklist-id/checkItems"     (gethash :uri    (orgtrello-api/add-items :checklist-id "item-name" t)))
  (expect '(("checked" . t) ("name"  . "item-name")) (gethash :params (orgtrello-api/add-items :checklist-id "item-name" t))))

(expectations
  (expect "POST"                                 (gethash :method (orgtrello-api/add-items :checklist-id "item-name")))
  (expect "/checklists/:checklist-id/checkItems" (gethash :uri    (orgtrello-api/add-items :checklist-id "item-name")))
  (expect '(("name"  . "item-name"))             (gethash :params (orgtrello-api/add-items :checklist-id "item-name"))))

(expectations
  (expect "POST"                                 (gethash :method (orgtrello-api/add-items :checklist-id "item-name" nil)))
  (expect "/checklists/:checklist-id/checkItems" (gethash :uri    (orgtrello-api/add-items :checklist-id "item-name" nil)))
  (expect '(("name"  . "item-name"))             (gethash :params (orgtrello-api/add-items :checklist-id "item-name" nil))))

(expectations
  (expect "PUT"                                                        (gethash :method (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name "incomplete")))
  (expect "/cards/:card-id/checklist/:checklist-id/checkItem/:item-id" (gethash :uri    (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name "incomplete")))
  (expect '(("state" ."incomplete") ("name"  . :item-name))            (gethash :params (orgtrello-api/update-item :card-id :checklist-id :item-id :item-name "incomplete"))))

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
  (expect "GET"          (gethash :method (orgtrello-api/get-member :id)))
  (expect "/members/:id" (gethash :uri (orgtrello-api/get-member :id))))

(expectations
  (expect "GET"         (gethash :method (orgtrello-api/get-me)))
  (expect "/members/me" (gethash :uri (orgtrello-api/get-me))))

;; ########################## orgtrello-query

(expectations
  (expect (format "%s%s" *TRELLO-URL* "/uri")            (orgtrello-query/--compute-url *TRELLO-URL* "/uri"))
  (expect (format "%s%s" *TRELLO-URL* "/uri/other")      (orgtrello-query/--compute-url *TRELLO-URL* "/uri/other"))
  (expect (format "some-server/uri/some/other")          (orgtrello-query/--compute-url "some-server" "/uri/some/other")))

(expectations
  (expect :some-get (orgtrello-data/method (orgtrello-hash/make-properties `((:method . :some-get) (:uri . :some-uri) (:sync . :some-sync) (:params . :some-params)))))
  (expect :some-uri (orgtrello-data/uri (orgtrello-hash/make-properties `((:method . :some-get) (:uri . :some-uri) (:sync . :some-sync) (:params . :some-params)))))
  (expect :some-sync (orgtrello-data/sync (orgtrello-hash/make-properties `((:method . :some-get) (:uri . :some-uri) (:sync . :some-sync) (:params . :some-params)))))
  (expect :some-params (orgtrello-data/params (orgtrello-hash/make-properties `((:method . :some-get) (:uri . :some-uri) (:sync . :some-sync) (:params . :some-params))))))

(expectations
  (expect "some-id" (orgtrello-data/id '((id . "some-id"))))
  (expect nil       (orgtrello-data/id '((noid . "some-id")))))

(expectations
  (expect :some-name (orgtrello-data/name '((name . :some-name))))
  (expect nil        (orgtrello-data/name '((noname . :some-name)))))

(expectations
  (expect :some-list-id (orgtrello-data/list-id '((idList . :some-list-id))))
  (expect nil           (orgtrello-data/list-id '((noIdList . :some-list-id)))))

(expectations
  (expect :some-clist-ids (orgtrello-data/checklist-ids '((idChecklists . :some-clist-ids))))
  (expect nil             (orgtrello-data/checklist-ids '((no . :some-clist-ids)))))

(expectations
  (expect :some-check-items (orgtrello-data/check-items '((checkItems . :some-check-items))))
  (expect nil               (orgtrello-data/check-items '((no . :some-check-items)))))

(expectations
  (expect :some-card-id (orgtrello-data/card-id '((idCard . :some-card-id))))
  (expect nil           (orgtrello-data/card-id '((no . :some-card-id)))))

(expectations
  (expect :some-due (orgtrello-data/due '((due . :some-due))))
  (expect nil       (orgtrello-data/due '((no . :some-due)))))

(expectations
  (expect :some-state (orgtrello-data/state '((state . :some-state))))
  (expect nil         (orgtrello-data/state '((no . :some-state)))))

(expectations
  (expect :closed (orgtrello-data/close-property '((closed . :closed))))
  (expect nil     (orgtrello-data/close-property '((no . :some-state)))))

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
       (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 4 :kwd :name nil :due :position :buffer-name))
       (lambda (entity s) (format "%S %s" entity s))
       "- hello"))

  (expect "#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:current #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:buffername :buffer-name :position :position :level 3 :keyword :kwd :name :name :id nil :due :due)) :parent nil :grandparent nil)) - hello"
    (org-action/--functional-controls-then-do
     '(orgtrello/--right-level-p)
     (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 3 :kwd :name nil :due :position :buffer-name))
     (lambda (entity s) (format "%S %s" entity s))
     "- hello")))

(expectations
  (expect "List of errors:
 - Entity must been synchronized with trello first!
"
    (org-action/--functional-controls-then-do
     '(orgtrello/--right-level-p orgtrello/--already-synced-p)
     (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :kwd :name nil :due :position :buffer-name))
     (lambda (entity s) (format "%S %s" entity s))
     "- hello"))
  (expect "#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:current #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:buffername :buffer-name :position :position :level 1 :keyword :kwd :name :name :id \"some-id\" :due :due)) :parent nil :grandparent nil)) - hello"

    (org-action/--functional-controls-then-do
     '(orgtrello/--right-level-p orgtrello/--already-synced-p)
     (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :kwd :name "some-id" :due :position :buffer-name))
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
  (expect "test" (orgtrello-data/retrieve-data 'marker `((marker . "test"))))
  (expect nil (orgtrello-data/retrieve-data    'other  `((marker . "test"))))
  (expect "test" (orgtrello-data/buffername            `((buffername . "test"))))
  (expect nil (orgtrello-data/buffername               `((inexistant . "test"))))
  (expect "test" (orgtrello-data/position              `((position . "test"))))
  (expect nil (orgtrello-data/position                 `((inexistant . "test"))))
  (expect "test" (orgtrello-data/id                    `((id . "test"))))
  (expect nil (orgtrello-data/id                       `((inexistant . "test"))))
  (expect "test" (orgtrello-data/name                  `((name . "test"))))
  (expect nil (orgtrello-data/name                     `((inexistant . "test"))))
  (expect "test" (orgtrello-data/list-id               `((idList . "test"))))
  (expect nil (orgtrello-data/list-id                  `((inexistant . "test"))))
  (expect "test" (orgtrello-data/checklist-ids         `((idChecklists . "test"))))
  (expect nil (orgtrello-data/checklist-ids            `((inexistant . "test"))))
  (expect "test" (orgtrello-data/check-items           `((checkItems . "test"))))
  (expect nil (orgtrello-data/check-items              `((inexistant . "test"))))
  (expect "test" (orgtrello-data/card-id               `((idCard . "test"))))
  (expect nil (orgtrello-data/card-id                  `((inexistant . "test"))))
  (expect "test" (orgtrello-data/due                   `((due . "test"))))
  (expect nil (orgtrello-data/due                      `((inexistant . "test"))))
  (expect "test" (orgtrello-data/state                 `((state . "test"))))
  (expect nil (orgtrello-data/state                    `((inexistant . "test"))))
  (expect "test" (orgtrello-data/close-property        `((closed . "test"))))
  (expect nil (orgtrello-data/close-property           `((inexistant . "test"))))
  (expect "test" (orgtrello-data/callback              `((callback . "test"))))
  (expect nil (orgtrello-data/callback                 `((inexistant . "test"))))
  (expect "test" (orgtrello-data/sync-                 `((sync . "test"))))
  (expect nil (orgtrello-data/sync-                    `((inexistant . "test"))))
  (expect "test" (orgtrello-data/level                 `((level . "test"))))
  (expect nil (orgtrello-data/level                    `((inexistant . "test"))))
  (expect "test" (orgtrello-data/method-               `((method . "test"))))
  (expect nil (orgtrello-data/method-                  `((inexistant . "test"))))
  (expect "test" (orgtrello-data/uri-                  `((uri . "test"))))
  (expect nil (orgtrello-data/uri-                     `((inexistant . "test"))))
  (expect "test" (orgtrello-data/params-               `((params . "test"))))
  (expect nil (orgtrello-data/params-                  `((inexistant . "test"))))
  (expect "test" (orgtrello-data/start                 `((start . "test"))))
  (expect nil (orgtrello-data/start                    `((inexistant . "test"))))
  (expect "test" (orgtrello-data/action                `((action . "test"))))
  (expect nil (orgtrello-data/action                   `((inexistant . "test")))))

(expectations
  (expect :some-method (orgtrello-data/method (orgtrello-hash/make-properties `((:method . :some-method )))))
  (expect nil (orgtrello-data/method (orgtrello-hash/make-properties `((:inexistant . :some-method )))))
  (expect :some-uri (orgtrello-data/method (orgtrello-hash/make-properties `((:method . :some-uri )))))
  (expect nil (orgtrello-data/method (orgtrello-hash/make-properties `((:inexistant . :some-method )))))
  (expect :some-sync (orgtrello-data/method (orgtrello-hash/make-properties `((:method . :some-sync )))))
  (expect nil (orgtrello-data/method (orgtrello-hash/make-properties `((:inexistant . :some-method )))))
  (expect :some-params (orgtrello-data/method (orgtrello-hash/make-properties `((:method . :some-params )))))
  (expect nil (orgtrello-data/method (orgtrello-hash/make-properties `((:inexistant . :some-method ))))))

(expectations
  (expect "some-method" (orgtrello-data/gethash-data :method (orgtrello-hash/make-properties `((:method . "some-method")))))
  (expect nil           (orgtrello-data/gethash-data :method (orgtrello-hash/make-properties `((:inexistant . "some-method"))))))

(expectations
  (expect "DONE" (orgtrello/--compute-state-generic "complete" '("DONE" "TODO")))
  (expect "TODO" (orgtrello/--compute-state-generic "incomplete" '("DONE" "TODO")))
  (expect "DONE" (orgtrello/--compute-state-generic "DONE" '("DONE" "TODO")))

  (expect "[X]" (orgtrello/--compute-state-generic "complete" '("[X]" "[-]")))
  (expect "[-]" (orgtrello/--compute-state-generic "incomplete" '("[X]" "[-]")))
  (expect "[X]" (orgtrello/--compute-state-generic "DONE" '("[X]" "[-]"))))

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
  (expect (format "%sorg-trello/3/test.org-123.el" elnode-webserver-docroot) (orgtrello-proxy/--compute-filename-from-entity '((level . 3) (buffername . "test.org") (position . "123")))))

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
  (expect "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":456}"
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

(expectations
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

(expectations
  (expect '((id . "id") (name . "some%20content%20to%20escape%20%26%20voila%21"))
    (orgtrello-query/--prepare-params-assoc! '((id . "id") (name . "some content to escape & voila!"))))
  (expect '((id . "id") (name . "some%20content%20to%20escape%20%26%20voila%21") (any . "content%20is%20escaped%20%26%20%3E%20this%20is%20fun%21"))
    (orgtrello-query/--prepare-params-assoc! '((id . "id") (name . "some content to escape & voila!") (any . "content is escaped & > this is fun!"))))
  (expect '((id) (name . "some%20content%20to%20escape%20%26%20voila%21") (any . "content%20is%20escaped%20%26%20%3E%20this%20is%20fun%21"))
    (orgtrello-query/--prepare-params-assoc! '((id) (name . "some content to escape & voila!") (any . "content is escaped & > this is fun!"))))
  (expect '((ok . t) (name . "some%20content%20to%20escape%20%26%20voila%21") (any . "content%20is%20escaped%20%26%20%3E%20this%20is%20fun%21"))
    (orgtrello-query/--prepare-params-assoc! '((ok . t) (name . "some content to escape & voila!") (any . "content is escaped & > this is fun!")))))

(expectations
  (expect '((name . "some%20content%20to%20escape%20%26%20voila%21") (id . "id"))
    (orgtrello-query/--prepare-query-params! '((id . "id") (name . "some content to escape & voila!"))))
  (expect '((any . "content%20is%20escaped%20%26%20%3E%20this%20is%20fun%21") (name . "some%20content%20to%20escape%20%26%20voila%21") (id . "id"))
    (orgtrello-query/--prepare-query-params! '((id . "id") (name . "some content to escape & voila!") (any . "content is escaped & > this is fun!"))))
  (expect '((any . "content%20is%20escaped%20%26%20%3E%20this%20is%20fun%21") (name . "some%20content%20to%20escape%20%26%20voila%21") (id))
    (orgtrello-query/--prepare-query-params! '((id) (name . "some content to escape & voila!") (any . "content is escaped & > this is fun!"))))
  (expect '((any . "content%20is%20escaped%20%26%20%3E%20this%20is%20fun%21") (name . "some%20content%20to%20escape%20%26%20voila%21") (ok . t))
    (orgtrello-query/--prepare-query-params! '((ok . t) (name . "some content to escape & voila!") (any . "content is escaped & > this is fun!")))))

(expectations
  (expect '((name . "some data with & keywords hexified") (id . "abc") (other-field . "hexified string"))
    (->> '((other-field . "hexified string") (id . "abc") (name . "some data with & keywords hexified"))
         orgtrello-query/--prepare-params-assoc!
         json-encode
         orgtrello-proxy/--json-read-from-string)))

(expectations
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

(expectations
  (expect 'orgtrello-query/--get         (orgtrello-query/--dispatch-http-query "GET"))
  (expect 'orgtrello-query/--post-or-put (orgtrello-query/--dispatch-http-query "POST"))
  (expect 'orgtrello-query/--post-or-put (orgtrello-query/--dispatch-http-query "PUT"))
  (expect 'orgtrello-query/--delete      (orgtrello-query/--dispatch-http-query "DELETE")))

(expectations
  (expect nil                    (orgtrello-api/--deal-with-optional-value nil nil nil))
  (expect nil                    (orgtrello-api/--deal-with-optional-value nil :a nil))
  (expect :existing-list         (orgtrello-api/--deal-with-optional-value nil :a :existing-list))
  (expect :existing-list         (orgtrello-api/--deal-with-optional-value nil nil :existing-list))
  (expect '(:value-a)            (orgtrello-api/--deal-with-optional-value :a :value-a nil))
  (expect '(:value-a :value-b)   (orgtrello-api/--deal-with-optional-value :a :value-a '(:value-b)))
  (expect '(nil :value-b) (orgtrello-api/--deal-with-optional-value :a nil '(:value-b))))

(expectations
  (expect nil                    (orgtrello-api/--deal-with-optional-values '((nil . nil)) nil))
  (expect nil                    (orgtrello-api/--deal-with-optional-values '((nil . :a)) nil))
  (expect :existing-list         (orgtrello-api/--deal-with-optional-values '((nil . :a)) :existing-list))
  (expect :existing-list         (orgtrello-api/--deal-with-optional-values '((nil . nil)) :existing-list))

  (expect '(:value-a)            (orgtrello-api/--deal-with-optional-values '((:a . :value-a)) nil))
  (expect '(:value-a :value-b)   (orgtrello-api/--deal-with-optional-values '((:a . :value-a)) '(:value-b)))
  (expect '(nil :value-b)        (orgtrello-api/--deal-with-optional-values '((:a . nil)) '(:value-b))))

(expectations
  (expect nil                           (orgtrello-api/--deal-with-optional-values '((nil . nil) (nil . nil)) nil))
  (expect nil                           (orgtrello-api/--deal-with-optional-values '((nil . :a)  (nil . :a)) nil))
  (expect :existing-list                (orgtrello-api/--deal-with-optional-values '((nil . :a) (nil . :a)) :existing-list))
  (expect :existing-list                (orgtrello-api/--deal-with-optional-values '((nil . nil) (nil . nil)) :existing-list))

  (expect '(:value-c :value-a)          (orgtrello-api/--deal-with-optional-values '((:a . :value-a) (:c . :value-c)) nil))
  (expect '(:value-c :value-a :value-b) (orgtrello-api/--deal-with-optional-values '((:a . :value-a) (:c . :value-c)) '(:value-b)))
  (expect '(nil nil :value-b)           (orgtrello-api/--deal-with-optional-values '((:a . nil) (:c . nil)) '(:value-b))))

(expectations
  (expect "POST"                      (gethash :method (orgtrello-api/add-board ":some-board")))
  (expect "/boards"                   (gethash :uri    (orgtrello-api/add-board ":some-board")))
  (expect '(("name" . ":some-board")) (gethash :params (orgtrello-api/add-board ":some-board"))))

(expectations
  (expect "POST"                           (gethash :method (orgtrello-api/add-board "some-board" "some-description")))
  (expect "/boards"                        (gethash :uri    (orgtrello-api/add-board "some-board" "some-description")))
  (expect '(("desc" . "some-description")
            ("name" . "some-board")) (gethash :params (orgtrello-api/add-board "some-board" "some-description"))))

(expectations
  (expect "POST"                                           (gethash :method (orgtrello-api/add-card "card-name" "list-id")))
  (expect "/cards/"                                        (gethash :uri    (orgtrello-api/add-card "card-name" "list-id")))
  (expect '(("name" . "card-name") ("idList" . "list-id")) (gethash :params (orgtrello-api/add-card "card-name" "list-id"))))

(expectations
  (expect "POST"                                                                (gethash :method (orgtrello-api/add-card "card-name" "list-id" "due-date")))
  (expect "/cards/"                                                             (gethash :uri    (orgtrello-api/add-card "card-name" "list-id" "due-date")))
  (expect '(("due" . "due-date") ("name" . "card-name") ("idList" . "list-id")) (gethash :params (orgtrello-api/add-card "card-name" "list-id" "due-date"))))

(expectations
  (expect 'orgtrello/--card      (gethash *CARD-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*))
  (expect 'orgtrello/--checklist (gethash *CHECKLIST-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*))
  (expect 'orgtrello/--item      (gethash *ITEM-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*)))

(expectations
  (expect 'orgtrello/--card-delete      (gethash *CARD-LEVEL* *MAP-DISPATCH-DELETE*))
  (expect 'orgtrello/--checklist-delete (gethash *CHECKLIST-LEVEL* *MAP-DISPATCH-DELETE*))
  (expect 'orgtrello/--item-delete      (gethash *ITEM-LEVEL* *MAP-DISPATCH-DELETE*)))

(expectations
 (expect (format "%sorg-trello/1/" elnode-webserver-docroot) (orgtrello-proxy/--compute-entity-level-dir *CARD-LEVEL*))
 (expect (format "%sorg-trello/2/" elnode-webserver-docroot) (orgtrello-proxy/--compute-entity-level-dir *CHECKLIST-LEVEL*))
 (expect (format "%sorg-trello/3/" elnode-webserver-docroot) (orgtrello-proxy/--compute-entity-level-dir *ITEM-LEVEL*)))

(expectations
  (expect 50
    (with-temp-buffer
      (insert "* heading\n")
      (insert "- [ ] some checklist\n")
      (insert "  - [ ] some item\n")
      (orgtrello/--compute-next-card-point)))
  (expect 70
    (with-temp-buffer
      (insert "#+TODO: TODO | DONE\n")
      (insert "* heading\n")
      (insert "- [ ] some checklist\n")
      (insert "  - [ ] some item\n")
      (forward-line -2)
      (orgtrello/--compute-next-card-point)))
  (expect 65
    (with-temp-buffer
      (insert "* heading\n")
      (insert "- [ ] some checklist\n")
      (insert "  - [ ] some item\n")
      (insert "* next heading\n")
      (forward-line -2)
      (orgtrello/--compute-next-card-point)))
  (expect 85
    (with-temp-buffer
      (insert "#+TODO: TODO | DONE\n")
      (insert "* heading\n")
      (insert "- [ ] some checklist\n")
      (insert "  - [ ] some item\n")
      (insert "* next heading\n")
      (forward-line -2)
      (orgtrello/--compute-next-card-point)))
  (expect 85
    (with-temp-buffer
      (insert "#+TODO: TODO | DONE\n")
      (insert "* heading\n")
      (insert "- [ ] some checklist\n")
      (insert "  - [ ] some item\n")
      (insert "* next heading\n")
      (forward-line -3)
      (orgtrello/--compute-next-card-point)))
  (expect 85
    (with-temp-buffer
      (insert "#+TODO: TODO | DONE\n")
      (insert "* heading\n")
      (insert "- [ ] some checklist\n")
      (insert "  - [ ] some item\n")
      (insert "* next heading\n")
      (forward-line -4)
      (orgtrello/--compute-next-card-point))))

(expectations
  (expect '(tr nil (td nil) (td nil "Action") (td nil "Entity") (td nil "Delete")) (orgtrello-admin/--header-table)))

(expectations
  (expect '(input ((class . "btn btn-danger btn-mini") (type . "button") (onclick . "deleteEntities('/proxy/admin/entities/delete/id');") (value . "x"))) (orgtrello-admin/--delete-action '((id . "id"))))
  (expect ""                                          (orgtrello-admin/--delete-action '((name . "name")))))

(expectations
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
    (orgtrello-admin/--entity '((action . "test") (id . "id") (name . "name")) "icon-play"))

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
    (orgtrello-admin/--entity '((action . "delete") (id . "id") (name . "name")) "icon-pause"))

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
    (orgtrello-admin/--entity '((action . "test") (name . "name 0") (id . "id")) "icon-play"))

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
    (orgtrello-admin/--entity '((action . "delete") (name . "name 1") (id . "id")) "icon-pause")))

(expectations
  (expect '(input
            ((class . "btn btn-danger btn-mini")
             (type . "button")
             (onclick . "deleteEntities('/proxy/admin/entities/delete/');")
             (value . "x")))
    (orgtrello-admin/--input-button-html "deleteEntities('/proxy/admin/entities/delete/');" "x")))

(expectations
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
    (orgtrello-admin/--main-body)))

(expectations
  (expect
      (esxml-to-xml `(div ((class . "hello")) "world"))
    (orgtrello-admin/--render-html `(div ((class . "hello")) "world"))))

(expectations
  (expect "None" (orgtrello-admin/--entities-as-html nil))
  (expect "None" (orgtrello-admin/--entities-as-html nil "icon-arrow-right"))
  (expect "None" (orgtrello-admin/--entities-as-html nil "icon-arrow-right" "icon-arrow-left"))
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
    (orgtrello-admin/--entities-as-html '(((action . "create") (name . "name 0") (id . "id 0")) ((action . "delete") (name . "name 1")))))

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
    (orgtrello-admin/--entities-as-html '(((action . "create") (name . "name 0")) ((action . "delete") (name . "name 1"))) "icon-arrow-right"))

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
    (orgtrello-admin/--entities-as-html '(((action . "create") (name . "name 0")) ((action . "delete") (name . "name 1"))) nil "icon-arrow-up"))

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
    (orgtrello-admin/--entities-as-html '(((action . "create") (name . "name 0")) ((action . "delete") (name . "name 1"))) "icon-play" "icon-pause")))

(expectations
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
    (orgtrello-admin/--list-entities-as-html '(((action . "action") (id . "id") (marker . "marker"))
                                               ((action . "action") (id . "id2") (marker . "marker2"))) "next")))

(expectations
  (expect '(class . "success") (orgtrello-admin/--compute-class "icon-play"))
  (expect '(class . "warning") (orgtrello-admin/--compute-class "icon-pause"))
  (expect '(class . "")        (orgtrello-admin/--compute-class nil))
  (expect '(class . "")        (orgtrello-admin/--compute-class "any")))

(expectations
 (expect :current (gethash :current (orgtrello-hash/make-hierarchy :current)))
 (expect nil (gethash :parent (orgtrello-hash/make-hierarchy :current)))
 (expect nil (gethash :grandparent (orgtrello-hash/make-hierarchy :current))))

(expectations
 (expect :current (gethash :current (orgtrello-hash/make-hierarchy :current :parent)))
 (expect :parent (gethash :parent (orgtrello-hash/make-hierarchy :current :parent)))
 (expect nil (gethash :grandparent (orgtrello-hash/make-hierarchy :current :parent))))

(expectations
 (expect :current (gethash :current (orgtrello-hash/make-hierarchy :current :parent :grandparent)))
 (expect :parent (gethash :parent (orgtrello-hash/make-hierarchy :current :parent :grandparent)))
 (expect :grandparent (gethash :grandparent (orgtrello-hash/make-hierarchy :current :parent :grandparent))))

(expectations
 (expect :current (gethash :current (orgtrello-hash/make-hierarchy :current nil :grandparent)))
 (expect nil (gethash :parent (orgtrello-hash/make-hierarchy :current nil :grandparent)))
 (expect :grandparent (gethash :grandparent (orgtrello-hash/make-hierarchy nil :parent :grandparent))))

;; (expectations
;;   (expect :ok                                      (-> (orgtrello-hash/make-hash-org 1 :keyword :name :id :due :position :buffer-name)
;;                                                        orgtrello-hash/make-hierarchy
;;                                                        orgtrello/--can-be-synced-p))
;;   (expect :ok                                      (-> (orgtrello-hash/make-hash-org 2 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name "some-id" :due :position :buffer-name))
;;                                                        orgtrello/--can-be-synced-p))
;;   (expect *ERROR-SYNC-CHECKLIST-SYNC-CARD-FIRST*   (-> (orgtrello-hash/make-hash-org 2 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name nil :due :position :buffer-name))
;;                                                        orgtrello/--can-be-synced-p))
;;   (expect *ERROR-SYNC-CHECKLIST-SYNC-CARD-FIRST*   (-> (orgtrello-hash/make-hash-org 2 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name "orgtrello-marker-bad-id-equiv-nil" :due :position :buffer-name))
;;                                                        orgtrello/--can-be-synced-p))
;;   (expect :ok                                      (-> (orgtrello-hash/make-hash-org 3 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name "some-id" :due :position :buffer-name)
;;                                                                                       (orgtrello-hash/make-hash-org 2 :keyword :name "some-id" :due :position :buffer-name))
;;                                                        orgtrello/--can-be-synced-p))
;;   (expect *ERROR-SYNC-ITEM-SYNC-UPPER-LAYER-FIRST* (-> (orgtrello-hash/make-hash-org 3 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name nil :due :position :buffer-name)
;;                                                                                       (orgtrello-hash/make-hash-org 2 :keyword :name "some-id" :due :position :buffer-name))
;;                                                        orgtrello/--can-be-synced-p))
;;   (expect *ERROR-SYNC-ITEM-SYNC-UPPER-LAYER-FIRST* (-> (orgtrello-hash/make-hash-org 3 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name "orgtrello-marker-nil" :due :position :buffer-name)
;;                                                                                       (orgtrello-hash/make-hash-org 2 :keyword :name "some-id" :due :position :buffer-name))
;;                                                        orgtrello/--can-be-synced-p))
;;   (expect *ERROR-SYNC-ITEM-SYNC-UPPER-LAYER-FIRST* (-> (orgtrello-hash/make-hash-org 3 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name "some-id" :due :position :buffer-name)
;;                                                                                       (orgtrello-hash/make-hash-org 2 :keyword :name nil :due :position :buffer-name))
;;                                                        orgtrello/--can-be-synced-p))
;;   (expect *ERROR-SYNC-ITEM-SYNC-UPPER-LAYER-FIRST* (-> (orgtrello-hash/make-hash-org 3 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name "some-id" :due :position :buffer-name)
;;                                                                                       (orgtrello-hash/make-hash-org 2 :keyword :name "orgtrello-marker-nil" :due :position :buffer-name))
;;                                                        orgtrello/--can-be-synced-p))
;;   (expect *ERROR-SYNC-ITEM-SYNC-UPPER-LAYER-FIRST* (-> (orgtrello-hash/make-hash-org 3 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name nil :due :position :buffer-name)
;;                                                                                       (orgtrello-hash/make-hash-org 2 :keyword :name nil :due :position :buffer-name))
;;                                                        orgtrello/--can-be-synced-p))
;;   (expect *ERROR-SYNC-ITEM-SYNC-UPPER-LAYER-FIRST* (-> (orgtrello-hash/make-hash-org 3 :keyword :name :id :due :position :buffer-name)
;;                                                        (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org 1 :keyword :name "orgtrello-marker-nil" :due :position :buffer-name)
;;                                                                                       (orgtrello-hash/make-hash-org 2 :keyword :name "orgtrello-marker-nil" :due :position :buffer-name))
;;                                                        orgtrello/--can-be-synced-p)))

(expectations
  (expect :ok                                 (-> (orgtrello-hash/make-hash-org 1 :keyword "some name" :id :due :position :buffer-name)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-CARD-MISSING-NAME*      (-> (orgtrello-hash/make-hash-org 1 :keyword "" :id :due :position :buffer-name)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-CARD-MISSING-NAME*      (-> (orgtrello-hash/make-hash-org 1 :keyword nil :id :due :position :buffer-name)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello/--mandatory-name-ok-p))
  (expect :ok                                 (-> (orgtrello-hash/make-hash-org 2 :keyword "some name" :id :due :position :buffer-name)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-CHECKLIST-MISSING-NAME* (-> (orgtrello-hash/make-hash-org 2 :keyword "" :id :due :position :buffer-name)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-CHECKLIST-MISSING-NAME* (-> (orgtrello-hash/make-hash-org 2 :keyword nil :id :due :position :buffer-name)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello/--mandatory-name-ok-p))
  (expect :ok                                 (-> (orgtrello-hash/make-hash-org 3 :keyword "some name" :id :due :position :buffer-name)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-ITEM-MISSING-NAME*      (-> (orgtrello-hash/make-hash-org 3 :keyword "" :id :due :position :buffer-name)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-ITEM-MISSING-NAME*      (-> (orgtrello-hash/make-hash-org 3 :keyword nil :id :due :position :buffer-name)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello/--mandatory-name-ok-p)))

(expectations
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
                      orgtrello/--name)))

(expectations
  (expect "card"      (->> (with-temp-buffer
                             (org-mode)
                             (insert "* card\n")
                             (insert "- [ ] checklist")
                             (orgtrello-data/entry-get-full-metadata))
                           (orgtrello-data/parent)
                           orgtrello/--name))
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
                           orgtrello/--name)))

(expectations
  (expect "checklist" (->> (with-temp-buffer
                             (org-mode)
                             (insert "* card\n")
                             (insert "- [ ] checklist\n")
                             (insert "  - [ ] item")
                             (orgtrello-data/entry-get-full-metadata))
                           (orgtrello-data/parent)
                           orgtrello/--name))
  (expect "card"      (->> (with-temp-buffer
                             (org-mode)
                             (insert "* card\n")
                             (insert "- [ ] checklist\n")
                             (insert "  - [ ] item")
                             (orgtrello-data/entry-get-full-metadata))
                           (orgtrello-data/grandparent)
                           orgtrello/--name))
  (expect "item"      (->> (with-temp-buffer
                             (org-mode)
                             (insert "* card\n")
                             (insert "- [ ] checklist\n")
                             (insert "  - [ ] item")
                             (orgtrello-data/entry-get-full-metadata))
                           (orgtrello-data/current)
                           orgtrello/--name)))

(expectations
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

(expectations
  (expect "tests.scanning" (orgtrello-proxy/--archived-scanning-dir "tests"))
  (expect "nil.scanning" (orgtrello-proxy/--archived-scanning-dir nil)))

(expectations
  (expect "test/folder/.scanning/filename" (orgtrello-proxy/--archived-scanning-file "test/folder/filename")))

(expectations
 (expect '(:a) (orgtrello-proxy/--update-buffer-to-save :a nil))
 (expect '(:a) (orgtrello-proxy/--update-buffer-to-save :a '(:a)))
 (expect '(:a :b) (orgtrello-proxy/--update-buffer-to-save :a '(:b))))

(expectations
 (setq *ORGTRELLO-LIST-BUFFERS-TO-SAVE* nil)
 (expect '(:a) (orgtrello-proxy/update-buffer-to-save! :a))
 (expect '(:a) (orgtrello-proxy/update-buffer-to-save! :a))
 (expect '(:b :a) (orgtrello-proxy/update-buffer-to-save! :b)))

(expectations
  (expect 'orgtrello/--put-card      (orgtrello/--dispatch-create-map (orgtrello-hash/make-hash-org *CARD-LEVEL* nil nil nil nil nil nil)))
  (expect 'orgtrello/--put-entities (orgtrello/--dispatch-create-map (orgtrello-hash/make-hash-org *CHECKLIST-LEVEL* nil nil nil nil nil nil)))
  (expect 'orgtrello/--put-entities (orgtrello/--dispatch-create-map (orgtrello-hash/make-hash-org *ITEM-LEVEL* nil nil nil nil nil nil))))

(ert-deftest testing-orgtrello/--init-map-from ()
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ()) (orgtrello/--init-map-from nil))))

(expectations
  (expect :data (orgtrello/--init-map-from :data)))

(ert-deftest testing-orgtrello/--merge-item ()
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id nil :level 3))
                      (orgtrello/--merge-item `((state . "anything") (name . "some name")) (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id nil :level 3))
                      (orgtrello/--merge-item `((state . "anything") (name . "some name")) (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "DONE" :id nil :level 3))
                      (orgtrello/--merge-item `((state . "complete") (name . "some name")) nil)))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id nil :level 3))
                      (orgtrello/--merge-item `((state . "anything") (name . "some name")) (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO") (:id 1))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id 1 :level 3))
                      (orgtrello/--merge-item `((state . "anything") (name . "some name") (id . 1)) (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id 1 :level 3))
                      (orgtrello/--merge-item `((state . "anything") (name . "some name") (id . 1)) (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO") (:id 2))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id 1 :level 3))
                      (orgtrello/--merge-item `((state . "anything") (name . "some name") (id . 1)) (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "DONE" :id 1 :level 3))
                      (orgtrello/--merge-item `((state . "complete") (name . "some name") (id . 1)) nil))))

(ert-deftest testing-orgtrello/--merge-checklist ()
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :id nil :level 2))
                      (orgtrello/--merge-checklist `((id . nil) (name . "some name")) (orgtrello-hash/make-properties `((:name . "some other name"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :id nil :level 2))
                      (orgtrello/--merge-checklist `((id . nil) (name . "some name")) (orgtrello-hash/make-properties `((:name . "some other name"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :id nil :level 2))
                      (orgtrello/--merge-checklist `((id . nil) (name . "some name")) nil)))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :id nil :level 2))
                      (orgtrello/--merge-checklist `((id . nil) (name . "some name")) (orgtrello-hash/make-properties `((:name . "some other name") (:id 1))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :id 1 :level 2))
                      (orgtrello/--merge-checklist `((id . 1) (name . "some name") (id . 1)) (orgtrello-hash/make-properties `((:name . "some other name"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :id 1 :level 2))
                      (orgtrello/--merge-checklist `((id . 1) (name . "some name") (id . 1)) (orgtrello-hash/make-properties `((:name . "some other name") (:id 2))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :id 1 :level 2))
                      (orgtrello/--merge-checklist `((id . 1) (name . "some name") (id . 1)) (orgtrello-hash/make-properties `((:name . "some other name"))))))
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :id 1 :level 2))
                      (orgtrello/--merge-checklist `((id . 1) (name . "some name")) nil))))

(ert-deftest testing-orgtrello/--merge-checklist ()
  (let ((*HMAP-ID-NAME* (orgtrello-hash/make-properties `((1 . "TODO") (2 . "DONE") (3 . "IN-PROGRESS")))))
    (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id nil :level 1))
                        (orgtrello/--merge-card `((id . nil) (name . "some name") (idList . 1)) (orgtrello-hash/make-properties `((:name . "some other name"))))))
    (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "DONE" :id nil :level 1))
                        (orgtrello/--merge-card `((id . nil) (name . "some name") (idList . 2)) (orgtrello-hash/make-properties `((:name . "some other name"))))))
    (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id nil :level 1))
                        (orgtrello/--merge-card `((id . nil) (name . "some name") (idList . 1)) nil)))
    (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id nil :level 1))
                        (orgtrello/--merge-card `((id . nil) (name . "some name") (idList . 1)) (orgtrello-hash/make-properties `((:name . "some other name") (:id 1))))))
    (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id 1 :level 1))
                        (orgtrello/--merge-card `((id . 1) (name . "some name") (id . 1) (idList . 1)) (orgtrello-hash/make-properties `((:name . "some other name"))))))
    (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id 1 :level 1))
                        (orgtrello/--merge-card `((id . 1) (name . "some name") (id . 1) (idList . 1)) (orgtrello-hash/make-properties `((:name . "some other name") (:id 2))))))
    (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id 1 :level 1))
                        (orgtrello/--merge-card `((id . 1) (name . "some name") (id . 1) (idList . 1)) (orgtrello-hash/make-properties `((:name . "some other name"))))))
    (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:name "some name" :keyword "TODO" :id 1 :level 1))
                        (orgtrello/--merge-card `((id . 1) (name . "some name") (idList . 1)) nil)))))

(expectations
 (expect '(1 2 3 4) (orgtrello/--add-to-last-pos 4 '(1 2 3))))

(expectations
  (expect '(1 2 3 4) (orgtrello-data/merge-2-lists-without-duplicates '(1 2 3) '(4 1 2)))
  (expect '(4 1 2)   (orgtrello-data/merge-2-lists-without-duplicates nil '(4 1 2)))
  (expect '(4 1 2)   (orgtrello-data/merge-2-lists-without-duplicates '(4 1 2) nil))
  (expect nil        (orgtrello-data/merge-2-lists-without-duplicates nil nil)))

(expectations
 (expect t (orgtrello/--hcard-p (orgtrello-hash/make-properties `((:level . ,*CARD-LEVEL*)))))
 (expect nil (orgtrello/--hcard-p (orgtrello-hash/make-properties `((:level . ,*CHECKLIST-LEVEL*))))))

(expectations
 (expect t (orgtrello/--entity-with-level-p (orgtrello-hash/make-properties `((:level . ,*CARD-LEVEL*))) *CARD-LEVEL*))
 (expect nil (orgtrello/--entity-with-level-p (orgtrello-hash/make-properties `((:level . ,*CHECKLIST-LEVEL*))) *CARD-LEVEL*)))

(expectations
 (expect t (orgtrello-data/entity-card-p (orgtrello-hash/make-properties `((:level . ,*CARD-LEVEL*)))))
 (expect nil (orgtrello-data/entity-card-p (orgtrello-hash/make-properties `((:level . ,*CHECKLIST-LEVEL*)))))
 (expect 1 (orgtrello-data/entity-card-p `((idList . 1))))
 (expect nil (orgtrello-data/entity-card-p `((id . 1)))))

(expectations
 (expect t (orgtrello-data/entity-checklist-p (orgtrello-hash/make-properties `((:level . ,*CHECKLIST-LEVEL*)))))
 (expect nil (orgtrello-data/entity-checklist-p (orgtrello-hash/make-properties `((:level . ,*ITEM-LEVEL*)))))
 (expect 1 (orgtrello-data/entity-checklist-p `((idCard . 1))))
 (expect nil (orgtrello-data/entity-checklist-p `((id . 1)))))

(expectations
 (expect t (orgtrello-data/entity-item-p (orgtrello-hash/make-properties `((:level . ,*ITEM-LEVEL*)))))
 (expect nil (orgtrello-data/entity-item-p (orgtrello-hash/make-properties `((:level . ,*CARD-LEVEL*)))))
 (expect 1 (orgtrello-data/entity-item-p `((state . 1))))
 (expect nil (orgtrello-data/entity-item-p `((id . 1)))))

(provide 'org-trello-tests)
;;; org-trello-tests ends here
