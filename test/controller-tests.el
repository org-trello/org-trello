(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(ert-deftest testing-orgtrello-controller/--compute-data-from-entity-meta ()
  (let* ((entry   (orgtrello-hash/make-hash-org :member-ids :some-level :some-keyword :some-name "some-id" :some-due :some-point :some-buffername :desc :comments)))
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

(expectations (desc "orgtrello-controller/--compute-marker-from-entry")
  (expect "id"                                                        (orgtrello-controller/--compute-marker-from-entry (orgtrello-hash/make-hash-org :users :level :kwd :name      "id"  :due :position :buffername :desc :comments)))
  (expect "orgtrello-marker-2a0b98e652ce6349a0659a7a8eeb3783ffe9a11a" (orgtrello-controller/--compute-marker-from-entry (orgtrello-hash/make-hash-org :users :level :kwd "some-name" nil :due 1234      "buffername" :desc :comments)))
  (expect "orgtrello-marker-6c59c5dcf6c83edaeb3f4923bfd929a091504bb3" (orgtrello-controller/--compute-marker-from-entry (orgtrello-hash/make-hash-org :users :level :kwd "some-name" nil :due 4321      "some-other-buffername" :desc :comments))))

(expectations (desc "orgtrello-marker-2a0b98e652ce6349a0659a7a8eeb3783ffe9a11a")
  (expect "orgtrello-marker-2a0b98e652ce6349a0659a7a8eeb3783ffe9a11a" (orgtrello-controller/compute-marker "buffername" "some-name" 1234))
  (expect "orgtrello-marker-6c59c5dcf6c83edaeb3f4923bfd929a091504bb3" (orgtrello-controller/compute-marker "some-other-buffername" "some-name" 4321)))

(expectations (desc "orgtrello-controller/id-p")
              (expect t   (orgtrello-controller/id-p "anything-that-does-not-start-with-orgtrello-marker"))
              (expect t   (orgtrello-controller/id-p "agfgdsfgbdfgbdfgbdfshouldbetrue"))
              (expect t   (orgtrello-controller/id-p "orgtrello-markeragfgdsfgbdfgbdfgbdfshouldbetrue"))
              (expect t   (orgtrello-controller/id-p "should-be-true-orgtrello-marker-agfgdsfgbdfgbdfgbdf"))
              (expect nil (orgtrello-controller/id-p "orgtrello-marker-shouldbenil"))
              (expect nil (orgtrello-controller/id-p nil)))

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

(expectations (desc "orgtrello-controller/--compute-checklist-to-org-entry")
  (expect "- [-] name
" (orgtrello-controller/--compute-checklist-to-org-entry (orgtrello-hash/make-properties `((:name . "name"))) t))
  (expect "- [-] name
" (orgtrello-controller/--compute-checklist-to-org-entry (orgtrello-hash/make-properties `((:name . "name"))) t)))

(expectations (desc "orgtrello-controller/--compute-item-to-org-entry - 1")
  (expect "  - [X] name
" (orgtrello-controller/--compute-item-to-org-entry (orgtrello-hash/make-properties `((:name . "name") (:keyword . "complete")))))
  (expect "  - [ ] name
" (orgtrello-controller/--compute-item-to-org-entry (orgtrello-hash/make-properties `((:name . "name") (:keyword . "incomplete"))))))

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

(expectations (desc "orgtrello-controller/--card")
              (expect 'orgtrello-controller/--card      (gethash *CARD-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*))
              (expect 'orgtrello-controller/--checklist (gethash *CHECKLIST-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*))
              (expect 'orgtrello-controller/--item      (gethash *ITEM-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*)))

(expectations (desc "orgtrello-controller/--card-delete")
              (expect 'orgtrello-controller/--card-delete      (gethash *CARD-LEVEL* *MAP-DISPATCH-DELETE*))
              (expect 'orgtrello-controller/--checklist-delete (gethash *CHECKLIST-LEVEL* *MAP-DISPATCH-DELETE*))
              (expect 'orgtrello-controller/--item-delete      (gethash *ITEM-LEVEL* *MAP-DISPATCH-DELETE*)))

(expectations (desc "orgtrello-controller/--dispatch-create-entities-map-with-adjacency")
  (expect 'orgtrello-controller/--put-card-with-adjacency     (orgtrello-controller/--dispatch-create-entities-map-with-adjacency (orgtrello-hash/make-hash-org :users *CARD-LEVEL* nil nil nil nil nil nil nil :comments)))
  (expect 'orgtrello-controller/--put-entities-with-adjacency (orgtrello-controller/--dispatch-create-entities-map-with-adjacency (orgtrello-hash/make-hash-org :users *CHECKLIST-LEVEL* nil nil nil nil nil nil nil :comments)))
  (expect 'orgtrello-controller/--put-entities-with-adjacency (orgtrello-controller/--dispatch-create-entities-map-with-adjacency (orgtrello-hash/make-hash-org :users *ITEM-LEVEL* nil nil nil nil nil nil nil :comments))))

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

(expectations
  (expect "complete" (orgtrello-controller/compute-state *DONE*))
  (expect "incomplete" (orgtrello-controller/compute-state "anything-else")))

(expectations
  (expect t   (orgtrello-controller/compute-check *DONE*))
  (expect nil (orgtrello-controller/compute-check "anything-else")))

(expectations
  (expect "me: some first comment###another-me: another comment"
    (orgtrello-controller/--comments-to-list (list (orgtrello-hash/make-properties '((:comment-user . "me") (:comment-text . "some first comment")))
                                                   (orgtrello-hash/make-properties '((:comment-user . "another-me") (:comment-text . "another comment")))))))

(expectations
  (expect "dude0: some comments\n\ndude1: some other comments"
    (orgtrello-controller/format-comments "dude0: some comments###dude1: some other comments")))

(expectations
 (expect "dude0: some comments###dude1: some other comments"
         (orgtrello-controller/unformat-comments "dude0: some comments\n\ndude1: some other comments")))
