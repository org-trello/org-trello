(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(ert-deftest testing-orgtrello-controller/--compute-data-from-entity-meta ()
  (let* ((entry   (orgtrello-hash/make-hash-org :member-ids :some-level :some-keyword :some-name "some-id" :some-due :some-point :some-buffername :desc :comments :tags)))
    (should (equal (orgtrello-data/entity-id entry)          "some-id"))
    (should (equal (orgtrello-data/entity-name entry)        :some-name))
    (should (equal (orgtrello-data/entity-keyword entry)     :some-keyword))
    (should (equal (orgtrello-data/entity-level entry)       :some-level))
    (should (equal (orgtrello-data/entity-due entry)         :some-due))
    (should (equal (orgtrello-data/entity-position entry)    :some-point))
    (should (equal (orgtrello-data/entity-buffername entry)  :some-buffername))
    (should (equal (orgtrello-data/entity-member-ids entry)  :member-ids))
    (should (equal (orgtrello-data/entity-tags entry)        :tags))
    (should (equal (orgtrello-data/entity-comments entry)    :comments))
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
  (expect 'none (orgtrello-controller/--compute-state-from-keyword *ORGTRELLO-TODO*))
  (expect 'done (orgtrello-controller/--compute-state-from-keyword *ORGTRELLO-DONE*))
  (expect 'none (orgtrello-controller/--compute-state-from-keyword "IN")))

(expectations (desc "orgtrello-marker-2a0b98e652ce6349a0659a7a8eeb3783ffe9a11a")
  (expect "orgtrello-marker-2a0b98e652ce6349a0659a7a8eeb3783ffe9a11a" (orgtrello-controller/compute-marker "buffername" "some-name" 1234))
  (expect "orgtrello-marker-6c59c5dcf6c83edaeb3f4923bfd929a091504bb3" (orgtrello-controller/compute-marker "some-other-buffername" "some-name" 4321)))

(expectations (desc "orgtrello-controller/--card")
              (expect 'orgtrello-controller/--card      (gethash *CARD-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*))
              (expect 'orgtrello-controller/--checklist (gethash *CHECKLIST-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*))
              (expect 'orgtrello-controller/--item      (gethash *ITEM-LEVEL* *MAP-DISPATCH-CREATE-UPDATE*)))

(expectations (desc "orgtrello-controller/--card-delete")
              (expect 'orgtrello-controller/--card-delete      (gethash *CARD-LEVEL* *MAP-DISPATCH-DELETE*))
              (expect 'orgtrello-controller/--checklist-delete (gethash *CHECKLIST-LEVEL* *MAP-DISPATCH-DELETE*))
              (expect 'orgtrello-controller/--item-delete      (gethash *ITEM-LEVEL* *MAP-DISPATCH-DELETE*)))

(expectations (desc "orgtrello-controller/--compute-user-properties")
  (expect t (hash-equal
             #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                           (:username "ardumont" :full-name "Antoine R. Dumont" :id "4f2baa2f72b7c1293501cad3"))
             (car (orgtrello-controller/--compute-user-properties '(#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
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
             (cadr (orgtrello-controller/--compute-user-properties '(#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
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

(expectations (desc "orgtrello-controller/--add-user")
  (expect '("a" "b" "c") (orgtrello-controller/--add-user "a" '("a" "b" "c")))
  (expect '("a" "b" "c") (orgtrello-controller/--add-user "a" '("b" "c"))))

(expectations (desc "orgtrello-controller/--remove-user")
  (expect '("b")     (orgtrello-controller/--remove-user "a" '("a" "b")))
  (expect '("a" "b") (orgtrello-controller/--remove-user "c" '("a" "b")))
  (expect nil        (orgtrello-controller/--remove-user "c" nil))
  (expect nil        (orgtrello-controller/--remove-user nil nil))
  (expect '("a")     (orgtrello-controller/--remove-user nil '("a"))))

(expectations (desc "orgtrello-controller/compute-property")
 (expect "#+property: test "      (orgtrello-controller/compute-property "test"))
 (expect "#+property: test value" (orgtrello-controller/compute-property "test" "value")))

(expectations
  (expect "complete" (orgtrello-controller/compute-state *ORGTRELLO-DONE*))
  (expect "incomplete" (orgtrello-controller/compute-state "anything-else")))

(expectations
  (expect t   (orgtrello-controller/compute-check *ORGTRELLO-DONE*))
  (expect nil (orgtrello-controller/compute-check "anything-else")))

(expectations
  (expect t
    (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ("0" :id-board0 "1" :id-board1))
                (orgtrello-controller/--index-board-map (orgtrello-hash/make-properties '((:id-board0 . "board0-name") (:id-board1 . "board1-name")))))))

(expectations
  (expect
      "0: board0-name\n1: board1-name\n"
    (orgtrello-controller/--display-boards-to-choose (orgtrello-hash/make-properties '((:id-board0 . "board0-name") (:id-board1 . "board1-name"))))))

(expectations
  (expect '(":PROPERTIES:"
            "#+property: board-name some-board-name"
            "#+property: board-id some-board-id"
            "#+PROPERTY: DONE done-id"
            "#+PROPERTY: TODO todo-id"
            ""
            "#+PROPERTY: orgtrello-user-some-other-user some-other-user-id"
            "#+PROPERTY: orgtrello-user-user user-id"
            "#+PROPERTY: :green green label"
            "#+PROPERTY: :red red label"
            "#+PROPERTY: orgtrello-user-me user"
            ":END:")
         (orgtrello-controller/--compute-metadata!
          "some-board-name"
          "some-board-id"
          (orgtrello-hash/make-properties '(("TODO" . "todo-id") ("DONE" . "done-id")))
          (orgtrello-hash/make-properties '(("user" . "user-id") ("some-other-user" . "some-other-user-id")))
          "user"
          (orgtrello-hash/make-properties '((:red . "red label") (:green . "green label"))))))

(expectations
  (expect
      '("#+PROPERTY: :green green label" "#+PROPERTY: :red red label")
    (orgtrello-controller/--properties-labels (orgtrello-hash/make-properties '((:red . "red label") (:green . "green label"))))))

;; cannot keep this test because the prod code does save the buffer
;; (expectations
;;   (expect
;;       ":PROPERTIES:
;; #+PROPERTY: board-name    some-board-name
;; #+PROPERTY: board-id      some-board-id
;; #+PROPERTY: DONE done-id
;; #+PROPERTY: TODO todo-id

;; #+PROPERTY: orgtrello-user-some-other-user some-other-user-id
;; #+PROPERTY: orgtrello-user-user user-id
;; #+PROPERTY: orgtrello-user-me user
;; :END:"
;;     (orgtrello-tests/with-temp-buffer ""
;;                                       (orgtrello-controller/--update-orgmode-file-with-properties!
;;                                        "some-board-name"
;;                                        "some-board-id"
;;                                        (orgtrello-hash/make-properties '(("TODO" . "todo-id") ("DONE" . "done-id")))
;;                                        (orgtrello-hash/make-properties '(("user" . "user-id") ("some-other-user" . "some-other-user-id")))
;;                                        "user"
;;                                        (orgtrello-hash/make-properties '((:red . "red label") (:green . "green label")))))))

(expectations
  (expect "a,b,c" (orgtrello-controller/--tags-to-labels ":a:b:c"))
  (expect "a,b,c" (orgtrello-controller/--tags-to-labels "a:b:c"))
  (expect "a," (orgtrello-controller/--tags-to-labels ":a:"))
  (expect "a," (orgtrello-controller/--tags-to-labels "a:"))
  (expect nil  (orgtrello-controller/--tags-to-labels nil)))
