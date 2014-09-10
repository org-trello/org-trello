(require 'org-trello-controller)
(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(ert-deftest testing-orgtrello-controller/--compute-data-from-entity-meta ()
  (let* ((entry   (orgtrello-data/make-hash-org :member-ids :some-level :some-keyword :some-name "some-id" :some-due :some-point :some-buffername :desc :comments :tags :unknown)))
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
    (should (equal (orgtrello-data/entity-description entry) :desc))
    (should (equal (orgtrello-data/entity-unknown-properties entry) :unknown))))

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
    (orgtrello-hash/puthash-data "id" "testing board" hashtable-expected)
    (orgtrello-hash/puthash-data "another-id" "testing board 2" hashtable-expected)
    (orgtrello-hash/puthash-data "yet-another-id" "testing board 3" hashtable-expected)
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
    (orgtrello-hash/puthash-data "testing board" "id" hashtable-expected)
    (orgtrello-hash/puthash-data "testing board 2" "another-id"  hashtable-expected)
    (orgtrello-hash/puthash-data "testing board 3" "yet-another-id"  hashtable-expected)
    (should (equal (gethash "testing board" hashtable-result) (gethash "testing board" hashtable-expected)))
    (should (equal (gethash "testing board 2" hashtable-result) (gethash "testing board 2" hashtable-expected)))
    (should (equal (gethash "testing board 3" hashtable-result) (gethash "testing board 3" hashtable-expected)))
    (should (equal (hash-table-count hashtable-result) (hash-table-count hashtable-expected)))))

(expectations (desc "orgtrello-controller/--compute-user-properties")
  (expect t (orgtrello-tests/hash-equal
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
  (expect t (orgtrello-tests/hash-equal
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
  (expect t (orgtrello-tests/hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ("ardumont" "4f2baa2f72b7c1293501cad3" "orgmode" "5203a0c833fc36360800177f"))
                        (orgtrello-controller/--compute-user-properties-hash '(#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                                                                  (:username "ardumont" :full-name "Antoine R. Dumont" :id "4f2baa2f72b7c1293501cad3"))
                                                                      #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                                                                                    (:username "orgmode" :full-name "org trello" :id "5203a0c833fc36360800177f")))))))

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
  (expect t
    (orgtrello-tests/hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ("0" :id-board0 "1" :id-board1))
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

(expectations
 (expect :ok
         (with-mock
          (mock (file-exists-p *ORGTRELLO/CONFIG-FILE*) => t)
          (mock (load *ORGTRELLO/CONFIG-FILE*)          => t)
          (orgtrello-controller/load-keys!)))
 (expect "Setup problem - Problem during credentials (consumer-key and the read/write access-token) loading - C-c o i or M-x org-trello/install-key-and-token"
   (with-mock
     (mock (file-exists-p *ORGTRELLO/CONFIG-FILE*) => nil)
     (orgtrello-controller/load-keys!)))
 (expect "Setup problem - Problem during credentials (consumer-key and the read/write access-token) loading - C-c o i or M-x org-trello/install-key-and-token"
   (with-mock
     (mock (file-exists-p *ORGTRELLO/CONFIG-FILE*) => t)
     (mock (load *ORGTRELLO/CONFIG-FILE*)          => nil)
     (orgtrello-controller/load-keys!))))

(expectations
  (expect :ok
    (let ((*consumer-key* "some-consumer-key")
          (*access-token* "some-access-token"))
      (orgtrello-controller/control-keys!)))
  (expect "Setup problem - You need to install the consumer-key and the read/write access-token - C-c o i or M-x org-trello/install-key-and-token"
    (let ((*consumer-key* "some-consumer-key")
          (*access-token* nil))
      (orgtrello-controller/control-keys!)))
  (expect "Setup problem - You need to install the consumer-key and the read/write access-token - C-c o i or M-x org-trello/install-key-and-token"
    (let ((*consumer-key* nil)
          (*access-token* "some-access-token"))
      (orgtrello-controller/control-keys!))))

(expectations
  (expect :id-board0
    (with-mock
      (mock (read-string *) => "0")
      (orgtrello-controller/choose-board! (orgtrello-hash/make-properties '((:id-board0 . "board0-name") (:id-board1 . "board1-name"))))))
  (expect :id-board1
    (with-mock
      (mock (read-string *) => "1")
      (orgtrello-controller/choose-board! (orgtrello-hash/make-properties '((:id-board0 . "board0-name") (:id-board1 . "board1-name")))))))

(expectations
  (expect t
    (orgtrello-tests/hash-equal
     #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:id "id0" :name "name0" :closed nil))
     (car (with-mock
            (mock (orgtrello-api/get-boards)                          => :query)
            (mock (orgtrello-query/http-trello :query 'sync) => `(,(orgtrello-hash/make-properties '((:id . "id0") (:name . "name0") (:closed)))
                                                                  ,(orgtrello-hash/make-properties '((:id . "id1") (:name . "name1") (:closed)))
                                                                  ,(orgtrello-hash/make-properties '((:id . "id1") (:name . "name1") (:closed . t)))))
            (orgtrello-controller/--list-boards!)))))
  (expect
      t
    (orgtrello-tests/hash-equal
     #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:id "id1" :name "name1" :closed nil))
     (cadr (with-mock
             (mock (orgtrello-api/get-boards)                          => :query)
             (mock (orgtrello-query/http-trello :query 'sync) => `(,(orgtrello-hash/make-properties '((:id . "id0") (:name . "name0") (:closed)))
                                                                            ,(orgtrello-hash/make-properties '((:id . "id1") (:name . "name1") (:closed)))
                                                                            ,(orgtrello-hash/make-properties '((:id . "id1") (:name . "name1") (:closed . t)))))
             (orgtrello-controller/--list-boards!))))))

(expectations
  (expect :some-result
    (with-mock
      (mock (orgtrello-api/get-lists :board-id)        => :query)
      (mock (orgtrello-query/http-trello :query 'sync) => :some-result)
      (orgtrello-controller/--list-board-lists! :board-id))))

(expectations
  (expect t
    (orgtrello-tests/hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data
                              ("786" "CANCELLED" "456" "FAILED" "ijk" "DONE" "abc" "TODO"))
                (orgtrello-controller/hmap-id-name '("CANCELLED" "FAILED" "DONE" "TODO")
                                                   '(("board-name" . "some board")
                                                     ("board-id" . "10223")
                                                     ("CANCELLED" . "786")
                                                     ("FAILED" . "456")
                                                     ("DELEGATED" . "123")
                                                     ("PENDING" . "efg")
                                                     ("DONE" . "ijk")
                                                     ("IN-PROGRESS" . "def")
                                                     ("TODO" . "abc")))))
  (expect t
    (orgtrello-tests/hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ())
                (orgtrello-controller/hmap-id-name '("CANCELLED" "FAILED" "DONE" "TODO")
                                                   '())))
  (expect t
    (orgtrello-tests/hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ())
                (orgtrello-controller/hmap-id-name '()
                                                   '(("board-name" . "some board")))))
  (expect t
    (orgtrello-tests/hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ())
                (orgtrello-controller/hmap-id-name '()
                                                   '()))))

(expectations
  (expect
      '("TODO" "IN-PROGRESS" "DONE" "PENDING" "DELEGATED" "FAILED" "CANCELLED")
    (orgtrello-tests/with-temp-buffer
     ":PROPERTIES:
#+property: board-name api test board
#+property: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-orgmode 888
#+PROPERTY: orgtrello-user-ardumont 999
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:"
     (orgtrello-buffer/filtered-kwds!))))

(expectations
  (expect
      '(("board-name" . "api test board")
        ("board-id" . "abc")
        ("CANCELLED" . "def")
        ("FAILED" . "ijk")
        ("DELEGATED" . "lmn")
        ("PENDING" . "opq")
        ("DONE" . "rst")
        ("IN-PROGRESS" . "uvw")
        ("TODO" . "xyz")
        ("orgtrello-user-orgmode" . "888")
        ("orgtrello-user-ardumont" . "999")
        (":yellow" . "yellow label")
        (":red" . "red label")
        (":purple" . "this is the purple label")
        (":orange" . "orange label")
        (":green" . "green label with & char")
        ("orgtrello-user-me" . "ardumont"))
    (orgtrello-tests/with-temp-buffer
     ":PROPERTIES:
#+property: board-name api test board
#+property: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-orgmode 888
#+PROPERTY: orgtrello-user-ardumont 999
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:"
     (orgtrello-buffer/org-file-properties!))))

(expectations
  (desc "orgtrello-controller/compute-and-overwrite-card! (buffer-name trello-card)")
  (expect
      ":PROPERTIES:
#+property: board-name api test board
#+property: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-dude 888
#+PROPERTY: orgtrello-user-ardumont 999
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:
* TODO updated card title                                               :orange:red:green:
  :PROPERTIES:
  :orgtrello-id: some-card-id
  :orgtrello-users: dude,ardumont
  :orgtrello-card-comments: ardumont: some comment###ardumont: some second comment
  :orgtrello-local-checksum: local-checkbox-checksum-678
  :END:
  updated description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\", \"orgtrello-local-checksum\":\"1bc8e829585b7857f3ebb1148a206df326f361bbec201cf77cebf3927112c954\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\", \"orgtrello-local-checksum\":\"852fa674016c5f13e302fb1fca513335632b3a7776da9cf87a89e88d58f23c87\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\", \"orgtrello-local-checksum\":\"b1f7d4b699796d9930747d53e55a9125f5c15731d96eeae0482878695895873e\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"edc031d664252bcbb12c2a123ec63feacac08cb8937e9680866b42d20eaac7d2\"}

* other card name
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content
     ":PROPERTIES:
#+property: board-name api test board
#+property: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-dude 888
#+PROPERTY: orgtrello-user-ardumont 999
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:
* TODO some card name                                                   :orange:
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-card-comments: ardumont: some comment
:END:
some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* other card name
"
     (with-mock
      (mock (orgtrello-buffer/card-checksum!) => "local-card-checksum-678")
      (mock (orgtrello-buffer/card-checksum!) => "local-checkbox-checksum-678")
      (let* ((trello-card (orgtrello-hash/make-properties `((:keyword . "TODO")
                                                            (:member-ids . "dude,ardumont")
                                                            (:comments . "ardumont: some comment###ardumont: some second comment")
                                                            (:tags . ":red:green:")
                                                            (:desc . "updated description")
                                                            (:level . 1)
                                                            (:name . "updated card title")
                                                            (:id . "some-card-id")))))
        (orgtrello-controller/compute-and-overwrite-card! (current-buffer) trello-card)))
     -2)))

(expectations
  (desc "orgtrello-controller/sync-buffer-with-trello-cards!")
  (expect
      ":PROPERTIES:
#+property: board-name api test board
#+property: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-dude 888
#+PROPERTY: orgtrello-user-ardumont 999
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:
* TODO updated card title                                               :orange:red:green:
  :PROPERTIES:
  :orgtrello-id: some-card-id
  :orgtrello-users: dude,ardumont
  :orgtrello-card-comments: ardumont: some comment###ardumont: some second comment
  :orgtrello-local-checksum: card-checksum-12
  :END:
  updated description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\", \"orgtrello-local-checksum\":\"checkbox-checksum-12\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\", \"orgtrello-local-checksum\":\"checkbox-checksum-12\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\", \"orgtrello-local-checksum\":\"checkbox-checksum-12\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"checkbox-checksum-12\"}
* TODO other card name
  :PROPERTIES:
  :orgtrello-id: some-new-marker
  :orgtrello-local-checksum: card-checksum-12
  :END:

"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content
     ":PROPERTIES:
#+property: board-name api test board
#+property: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-dude 888
#+PROPERTY: orgtrello-user-ardumont 999
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:
* TODO some card name                                                   :orange:
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-card-comments: ardumont: some comment
:END:
some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* other card name
"
     (with-mock
       (mock (orgtrello-buffer/--compute-marker-from-entry *) => "some-new-marker")
       (mock (orgtrello-buffer/card-checksum!) => "card-checksum-12")
       (mock (orgtrello-buffer/checkbox-checksum!) => "checkbox-checksum-12")
       (let* ((trello-card0 (orgtrello-hash/make-properties `((:keyword . "TODO")
                                                              (:member-ids . "orgtrello-user-dude,orgtrello-user-ardumont")
                                                              (:comments . "ardumont: some comment###ardumont: some second comment")
                                                              (:tags . ":red:green:")
                                                              (:desc . "updated description")
                                                              (:level . ,*ORGTRELLO/CARD-LEVEL*)
                                                              (:name . "updated card title")
                                                              (:id . "some-card-id")))))
         (orgtrello-controller/sync-buffer-with-trello-cards! (current-buffer) (list trello-card0)))))))

(expectations
  (desc "orgtrello-controller/sync-buffer-with-trello-cards!")
  (expect
      ":PROPERTIES:
#+property: board-name api test board
#+property: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-dude 888
#+PROPERTY: orgtrello-user-ardumont 999
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:
* TODO updated card title                                               :orange:red:green:
  :PROPERTIES:
  :orgtrello-id: some-card-id
  :orgtrello-users: dude,ardumont
  :orgtrello-card-comments: ardumont: some comment###ardumont: some second comment
  :orgtrello-local-checksum: card-checksum-1234
  :END:
  updated description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\", \"orgtrello-local-checksum\":\"checkbox-checksum-1234\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\", \"orgtrello-local-checksum\":\"checkbox-checksum-1234\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\", \"orgtrello-local-checksum\":\"checkbox-checksum-1234\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"checkbox-checksum-1234\"}
* TODO other card name                                                  :green:
  :PROPERTIES:
  :orgtrello-id: some-card-id2
  :orgtrello-users: dude
  :orgtrello-card-comments: ardumont: great
  :orgtrello-local-checksum: card-checksum-1234
  :END:
  this is a description
* TODO other card name
  :PROPERTIES:
  :orgtrello-id: some-new-marker
  :orgtrello-local-checksum: card-checksum-1234
  :END:

"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content
     ":PROPERTIES:
#+property: board-name api test board
#+property: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-dude 888
#+PROPERTY: orgtrello-user-ardumont 999
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:
* TODO some card name                                                   :orange:
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-card-comments: ardumont: some comment
:END:
some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* other card name
"
     (with-mock
       (mock (orgtrello-buffer/--compute-marker-from-entry *) => "some-new-marker")
       (mock (orgtrello-buffer/card-checksum!) => "card-checksum-1234")
       (mock (orgtrello-buffer/checkbox-checksum!) => "checkbox-checksum-1234")
       (let* ((trello-card0 (orgtrello-hash/make-properties `((:keyword . "TODO")
                                                              (:member-ids . "orgtrello-user-dude,orgtrello-user-ardumont")
                                                              (:comments . "ardumont: some comment###ardumont: some second comment")
                                                              (:tags . ":red:green:")
                                                              (:desc . "updated description")
                                                              (:level . ,*ORGTRELLO/CARD-LEVEL*)
                                                              (:name . "updated card title")
                                                              (:id . "some-card-id"))))
              (trello-card1 (orgtrello-hash/make-properties `((:keyword . "TODO")
                                                              (:member-ids . "orgtrello-user-dude")
                                                              (:comments . "ardumont: great")
                                                              (:tags . ":green:")
                                                              (:desc . "this is a description")
                                                              (:level . ,*ORGTRELLO/CARD-LEVEL*)
                                                              (:name . "other card name")
                                                              (:id . "some-card-id2")))))
         (orgtrello-controller/sync-buffer-with-trello-cards! (current-buffer) (list trello-card0 trello-card1)))))))

(expectations
  (desc "orgtrello-controller/sync-buffer-with-trello-cards!")
  (expect
      ":PROPERTIES:
#+property: board-name api test board
#+property: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-dude 888
#+PROPERTY: orgtrello-user-ardumont 999
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:
* TODO updated card title                                               :orange:red:green:
  :PROPERTIES:
  :orgtrello-id: some-card-id
  :orgtrello-users: dude,ardumont
  :orgtrello-card-comments: ardumont: some comment###ardumont: some second comment
  :orgtrello-local-checksum: card-checksum-123456
  :END:
  updated description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\", \"orgtrello-local-checksum\":\"checkbox-checksum-123456\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\", \"orgtrello-local-checksum\":\"checkbox-checksum-123456\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\", \"orgtrello-local-checksum\":\"checkbox-checksum-123456\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"checkbox-checksum-123456\"}
* DONE other card name                                                  :green:
  :PROPERTIES:
  :orgtrello-id: some-card-id2
  :orgtrello-users: dude
  :orgtrello-card-comments: ardumont: great
  :orgtrello-local-checksum: card-checksum-123456
  :END:
  this is a description
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content
     ":PROPERTIES:
#+property: board-name api test board
#+property: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-dude 888
#+PROPERTY: orgtrello-user-ardumont 999
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:
* TODO some card name                                                   :orange:
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-card-comments: ardumont: some comment
:END:
some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* other card name
:PROPERTIES:
:orgtrello-id: some-card-id2
:END:
"
     (with-mock
       (mock (orgtrello-buffer/card-checksum!) => "card-checksum-123456")
       (mock (orgtrello-buffer/checkbox-checksum!) => "checkbox-checksum-123456")
       (let* ((trello-card0 (orgtrello-hash/make-properties `((:keyword . "TODO")
                                                              (:member-ids . "orgtrello-user-dude,orgtrello-user-ardumont")
                                                              (:comments . "ardumont: some comment###ardumont: some second comment")
                                                              (:tags . ":red:green:")
                                                              (:desc . "updated description")
                                                              (:level . ,*ORGTRELLO/CARD-LEVEL*)
                                                              (:name . "updated card title")
                                                              (:id . "some-card-id"))))
              (trello-card1 (orgtrello-hash/make-properties `((:keyword . "DONE")
                                                              (:member-ids . "orgtrello-user-dude")
                                                              (:comments . "ardumont: great")
                                                              (:tags . ":green:")
                                                              (:desc . "this is a description")
                                                              (:level . ,*ORGTRELLO/CARD-LEVEL*)
                                                              (:name . "other card name")
                                                              (:id . "some-card-id2")))))
         (orgtrello-controller/sync-buffer-with-trello-cards! (current-buffer) (list trello-card0 trello-card1)))))))

(provide 'org-trello-controller-tests)
;;; org-trello-controller-tests.el ends here
