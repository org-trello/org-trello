(require 'org-trello-buffer)
(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(expectations
  (expect "llo there"
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
   DEADLINE: <2014-04-01T00:00:00.000Z>
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
hello there
"
                                      (orgtrello-buffer/extract-description-from-current-position!)))

  (expect "hello there"
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
  - [X] Common-Lisp :PROPERTIES: {\"orgtrello-id\":\"52c94518b2c5b28e37012ba4\"}"
                                      (orgtrello-buffer/extract-description-from-current-position!)))

  (expect "\nhello there\n"
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:

  hello there

- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
  - [X] Common-Lisp :PROPERTIES: {\"orgtrello-id\":\"52c94518b2c5b28e37012ba4\"}"
                                      (orgtrello-buffer/extract-description-from-current-position!)))

  (expect nil
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES" (orgtrello-buffer/extract-description-from-current-position!)))

  (expect nil
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}"
                                      (orgtrello-buffer/extract-description-from-current-position!)))
  (expect "One Paragraph\n\nAnother Paragraph"
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
  DEADLINE: <2014-04-01T00:00:00.000Z>
  :PROPERTIES:
  :orgtrello-id: 52c945143004d4617c012528
  :END:
  One Paragraph

  Another Paragraph
"
                                      (orgtrello-buffer/extract-description-from-current-position!))))

(expectations (desc "orgtrello-buffer/extract-description-from-current-position! - non standard org-trello properties with blanks before them.")
              (expect "hello there"
                (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
 :PROPERTIES:
 :orgtrello-id: 52c945143004d4617c012528
 :END:
  hello there
"
                                                  (orgtrello-buffer/extract-description-from-current-position!)))

              (expect "hello there"
                (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
 :PROPERTIES:
 :orgtrello-id: 52c945143004d4617c012528
    :END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
  - [X] Common-Lisp :PROPERTIES: {\"orgtrello-id\":\"52c94518b2c5b28e37012ba4\"}"
                                                  (orgtrello-buffer/extract-description-from-current-position!)))

              (expect "\nhello there\n"
                (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
  :PROPERTIES:
         :orgtrello-id: 52c945143004d4617c012528
  :END:

  hello there

- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
  - [X] Common-Lisp :PROPERTIES: {\"orgtrello-id\":\"52c94518b2c5b28e37012ba4\"}"
                                                  (orgtrello-buffer/extract-description-from-current-position!)))

              (expect nil
                (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
  :PROPERTIES:
 :orgtrello-id: 52c945143004d4617c012528
:END:
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}"
                                                  (orgtrello-buffer/extract-description-from-current-position!))))

(expectations
  (expect "some-comments###with-dummy-data"
    (orgtrello-tests/with-temp-buffer
     "* card
:PROPERTIES:
:orgtrello-card-comments: some-comments###with-dummy-data
:END:"
     (orgtrello-buffer/get-card-comments!))))

(expectations
  (expect
      "this-is-the-board-id"
    (orgtrello-tests/with-org-buffer
     (format ":PROPERTIES:\n#+PROPERTY: %s this-is-the-board-id\n:END:\n* card\n" *ORGTRELLO/BOARD-ID*)
     (orgtrello-buffer/board-id!)))
  (expect "this-is-the-board-name"
    (orgtrello-tests/with-org-buffer
     (format ":PROPERTIES:\n#+PROPERTY: %s this-is-the-board-name\n:END:\n* card\n" *ORGTRELLO/BOARD-NAME*)
     (orgtrello-buffer/board-name!)))
  (expect "this-is-the-user"
    (orgtrello-tests/with-org-buffer
     (format ":PROPERTIES:\n#+PROPERTY: %s this-is-the-user\n:END:\n* card\n" *ORGTRELLO/USER-ME*)
     (orgtrello-buffer/me!))))

(expectations
  (expect ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:
* TODO some card name
  :PROPERTIES:
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments: ardumont: some comment
  :END:
  some description"
      (orgtrello-tests/with-temp-buffer-and-return-buffer-content
       ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:
"

       (orgtrello-buffer/write-card-header! "some-card-id" (orgtrello-hash/make-properties `((:keyword . "TODO")
                                                                                             (:member-ids . "ardumont,dude")
                                                                                             (:comments . "ardumont: some comment")
                                                                                             (:desc . "some description")
                                                                                             (:level . ,*ORGTRELLO/CARD-LEVEL*)
                                                                                             (:name . "some card name"))))
       0)))

(expectations
  (expect ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:
* TODO some card name                                                   :red:green:
DEADLINE: <some-due-date>
  :PROPERTIES:
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments: ardumont: some comment
  :END:
  some description"
   (orgtrello-tests/with-temp-buffer-and-return-buffer-content
    ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:
"
    (orgtrello-buffer/write-card-header! "some-card-id" (orgtrello-hash/make-properties `((:keyword . "TODO")
                                                                                          (:member-ids . "ardumont,dude")
                                                                                          (:comments . "ardumont: some comment")
                                                                                          (:tags . ":red:green:")
                                                                                          (:desc . "some description")
                                                                                          (:level . ,*ORGTRELLO/CARD-LEVEL*)
                                                                                          (:name . "some card name")
                                                                                          (:due . "some-due-date"))))
    0)))

(expectations
  (expect ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:

* TODO some old card name
  :PROPERTIES:
  :orgtrello-id: some-id
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments:
  :END:
some old description
- [ ] some old checklist name
- [-] some checklist name
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content
     ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:

* TODO some old card name
  :PROPERTIES:
  :orgtrello-id: some-id
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments:
  :END:
some old description
- [ ] some old checklist name\n"
     (orgtrello-buffer/write-checklist-header! "some-id" (orgtrello-hash/make-properties `((:keyword . "DONE")
                                                                                           (:level . ,*ORGTRELLO/CHECKLIST-LEVEL*)
                                                                                           (:name . "some checklist name"))))
     0)))

(expectations
  (expect ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:
* TODO some card name                                                   :red:green:
  :PROPERTIES:
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments: ardumont: some comment
  :orgtrello-id: some-card-id
  :orgtrello-local-checksum: local-card-checksum-456
  :END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\", \"orgtrello-local-checksum\":\"local-checkbox-checksum-456\"}
  - [X] some item name :PROPERTIES: {\"orgtrello-id\":\"some-item-id\", \"orgtrello-local-checksum\":\"local-item-checksum-456\"}
  - [ ] some other item name :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\", \"orgtrello-local-checksum\":\"local-item-checksum-456\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"local-checkbox-checksum-456\"}
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content
     ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:
"
     (with-mock
       (mock (orgtrello-buffer/card-checksum!) => "local-card-checksum-456")
       (mock (orgtrello-buffer/checklist-checksum!) => "local-checkbox-checksum-456")
       (mock (orgtrello-buffer/item-checksum!) => "local-item-checksum-456")
       (orgtrello-buffer/write-card! "some-card-id"
                                     (orgtrello-hash/make-properties `((:keyword . "TODO")
                                                                       (:member-ids . "ardumont,dude")
                                                                       (:comments . "ardumont: some comment")
                                                                       (:tags . ":red:green:")
                                                                       (:desc . "some description")
                                                                       (:level . ,*ORGTRELLO/CARD-LEVEL*)
                                                                       (:name . "some card name")
                                                                       (:id . "some-card-id")))
                                     (orgtrello-hash/make-properties `(("some-checklist-id" . ,(orgtrello-hash/make-properties `((:id . "some-checklist-id")
                                                                                                                                 (:name . "some checklist name")
                                                                                                                                 (:level . ,*ORGTRELLO/CHECKLIST-LEVEL*))))
                                                                       ("some-other-checklist-id" . ,(orgtrello-hash/make-properties `((:id . "some-other-checklist-id")
                                                                                                                                       (:name . "some other checklist name")
                                                                                                                                       (:level . ,*ORGTRELLO/CHECKLIST-LEVEL*))))
                                                                       ("some-item-id"  . ,(orgtrello-hash/make-properties `((:id . "some-item-id")
                                                                                                                             (:name . "some item name")
                                                                                                                             (:level . ,*ORGTRELLO/ITEM-LEVEL*)
                                                                                                                             (:keyword . "DONE"))))
                                                                       ("some-other-item-id"  . ,(orgtrello-hash/make-properties `((:id . "some-other-item-id")
                                                                                                                                   (:name . "some other item name")
                                                                                                                                   (:level . ,*ORGTRELLO/ITEM-LEVEL*)
                                                                                                                                   (:keyword . "TODO"))))))
                                     (orgtrello-hash/make-properties `(("some-other-checklist-id" . ())
                                                                       ("some-checklist-id" . ("some-item-id" "some-other-item-id"))
                                                                       ("some-card-id" . ("some-checklist-id" "some-other-checklist-id"))))))
     0)))

(expectations
  (expect ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:
* TODO task A
  :PROPERTIES:
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments: ardumont: some comment
  :orgtrello-id: card-id-a
  :orgtrello-local-checksum: local-checksum-a
  :END:
  description A
* TODO task B
  :PROPERTIES:
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments: ardumont: some comment
  :orgtrello-id: card-id-b
  :orgtrello-local-checksum: local-checksum-b
  :END:
  description B
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content
     ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:
"
     (progn
       (with-mock
         (mock (orgtrello-buffer/card-checksum!) => "local-checksum-a")
         (orgtrello-buffer/write-card! "card-id-a"
                                       (orgtrello-hash/make-properties
                                        `((:keyword . "TODO")
                                          (:desc . "description A")
                                          (:level . ,*ORGTRELLO/CARD-LEVEL*)
                                          (:name . "task A")
                                          (:id . "card-id-a")
                                          (:member-ids . "ardumont,dude")
                                          (:comments . "ardumont: some comment")))
                                       (orgtrello-hash/make-properties `())
                                       (orgtrello-hash/make-properties `())))
       (with-mock
         (mock (orgtrello-buffer/card-checksum!) => "local-checksum-b")
         (orgtrello-buffer/write-card! "card-id-b"
                                       (orgtrello-hash/make-properties
                                        `((:keyword . "TODO")
                                          (:desc . "description B")
                                          (:level . ,*ORGTRELLO/CARD-LEVEL*)
                                          (:name . "task B")
                                          (:id . "card-id-b")
                                          (:member-ids . "ardumont,dude")
                                          (:comments . "ardumont: some comment")))
                                       (orgtrello-hash/make-properties `())
                                       (orgtrello-hash/make-properties `()))))
     0)))

(expectations
  (expect ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:

* TODO some card name
  :PROPERTIES:
  :orgtrello-id: some-card-id
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments: ardumont: some comment
  :END:
some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\", \"orgtrello-local-checksum\":\"checklist-checksum\"}
  - [X] some item name :PROPERTIES: {\"orgtrello-id\":\"some-item-id\", \"orgtrello-local-checksum\":\"item-checksum\"}
  - [ ] some other item name :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\", \"orgtrello-local-checksum\":\"item-checksum\"}
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content
     ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:

* TODO some card name
  :PROPERTIES:
  :orgtrello-id: some-card-id
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments: ardumont: some comment
  :END:
some description
"
     (with-mock
       (mock (orgtrello-buffer/item-checksum!) => "item-checksum")
       (mock (orgtrello-buffer/checklist-checksum!) => "checklist-checksum")
       (orgtrello-buffer/write-checklist! "some-checklist-id"
                                          (orgtrello-hash/make-properties `(("some-checklist-id" . ,(orgtrello-hash/make-properties `((:id . "some-checklist-id")
                                                                                                                                      (:name . "some checklist name")
                                                                                                                                      (:level . ,*ORGTRELLO/CHECKLIST-LEVEL*))))
                                                                            ("some-item-id"  . ,(orgtrello-hash/make-properties `((:id . "some-item-id")
                                                                                                                                  (:name . "some item name")
                                                                                                                                  (:level . ,*ORGTRELLO/ITEM-LEVEL*)
                                                                                                                                  (:keyword . "DONE"))))
                                                                            ("some-other-item-id"  . ,(orgtrello-hash/make-properties `((:id . "some-other-item-id")
                                                                                                                                        (:name . "some other item name")
                                                                                                                                        (:level . ,*ORGTRELLO/ITEM-LEVEL*)
                                                                                                                                        (:keyword . "TODO"))))))
                                          (orgtrello-hash/make-properties `(("some-checklist-id" . ("some-item-id" "some-other-item-id"))))))
     0)))

(expectations
  (expect ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:

* TODO some card name
  :PROPERTIES:
  :orgtrello-id: some-card-id
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments: ardumont: some comment
  :END:
some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\", \"orgtrello-local-checksum\":\"checklist-checksum\"}
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content
     ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:

* TODO some card name
  :PROPERTIES:
  :orgtrello-id: some-card-id
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments: ardumont: some comment
  :END:
some description
"
     (with-mock
       (mock (orgtrello-buffer/checklist-checksum!) => "checklist-checksum")
       (orgtrello-buffer/write-checklist! "some-checklist-id"
                                          (orgtrello-hash/make-properties `(("some-checklist-id" . ,(orgtrello-hash/make-properties `((:id . "some-checklist-id")
                                                                                                                                      (:name . "some checklist name")
                                                                                                                                      (:level . ,*ORGTRELLO/CHECKLIST-LEVEL*))))))
                                          (orgtrello-hash/make-properties `(("some-checklist-id" . nil)))))
     0)))

(expectations
  (expect ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:

* TODO some card name
  :PROPERTIES:
  :orgtrello-id: some-card-id
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments: ardumont: some comment
  :END:
some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item name :PROPERTIES: {\"orgtrello-id\":\"some-item-id\", \"orgtrello-local-checksum\":\"item-checksum\"}
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content
     ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:

* TODO some card name
  :PROPERTIES:
  :orgtrello-id: some-card-id
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments: ardumont: some comment
  :END:
some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
"
     (with-mock
       (mock (orgtrello-buffer/item-checksum!) => "item-checksum")
       (orgtrello-buffer/write-item! "some-item-id"
                                     (orgtrello-hash/make-properties `(("some-item-id" . ,(orgtrello-hash/make-properties `((:id . "some-item-id")
                                                                                                                            (:name . "some item name")
                                                                                                                            (:level . ,*ORGTRELLO/ITEM-LEVEL*)
                                                                                                                            (:keyword . "DONE"))))))))
     0)))

(expectations
  (desc "orgtrello-buffer/write-entity! - card")
  (expect "
* DONE some card name                                                   :red:green:
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content
     "\n"
     (orgtrello-buffer/write-entity! "some-card-id" (orgtrello-hash/make-properties `((:keyword . "DONE")
                                                                                      (:tags . ":red:green:")
                                                                                      (:desc . "some description")
                                                                                      (:level . ,*ORGTRELLO/CARD-LEVEL*)
                                                                                      (:name . "some card name"))))
     0)))

(expectations
  (desc "orgtrello-buffer/write-entity! - checklist")
  (expect "* some content
- [-] some checklist name
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content
     "* some content
"
     (orgtrello-buffer/write-entity! "some-checklist-id" (orgtrello-hash/make-properties `((:keyword . "DONE")
                                                                                           (:level . ,*ORGTRELLO/CHECKLIST-LEVEL*)
                                                                                           (:name . "some checklist name"))))
     0)))

(expectations
  (desc "orgtrello-buffer/write-entity! - item")
  (expect "* some content
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item name
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content
     "* some content
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
"
     (orgtrello-buffer/write-entity! "some-item-id" (orgtrello-hash/make-properties `((:keyword . "DONE")
                                                                                      (:level . ,*ORGTRELLO/ITEM-LEVEL*)
                                                                                      (:name . "some item name"))))
     0)))

(expectations
  (desc "orgtrello-buffer/--compute-checklist-to-org-entry")
  (expect "- [-] name\n" (orgtrello-buffer/--compute-checklist-to-org-entry (orgtrello-hash/make-properties `((:name . "name"))) t))
  (expect "- [-] name\n" (orgtrello-buffer/--compute-checklist-to-org-entry (orgtrello-hash/make-properties `((:name . "name"))) t)))

(expectations
  (desc "orgtrello-buffer/--compute-item-to-org-entry - 1")
  (expect "  - [X] name\n" (orgtrello-buffer/--compute-item-to-org-entry (orgtrello-hash/make-properties `((:name . "name") (:keyword . "complete")))))
  (expect "  - [ ] name\n" (orgtrello-buffer/--compute-item-to-org-entry (orgtrello-hash/make-properties `((:name . "name") (:keyword . "incomplete"))))))

(expectations
  (desc "orgtrello-buffer/--compute-item-to-org-checkbox")
  (expect "- [X] name\n" (orgtrello-buffer/--compute-item-to-org-checkbox "name" 2 "complete"))
  (expect "  - [X] name\n" (orgtrello-buffer/--compute-item-to-org-checkbox "name" 3 "complete"))
  (expect "- [X] name\n" (orgtrello-buffer/--compute-item-to-org-checkbox "name" 2 "complete"))
  (expect "  - [ ] name\n" (orgtrello-buffer/--compute-item-to-org-checkbox "name" 3 "incomplete")))

(expectations
  (desc "orgtrello-buffer/--private-compute-card-to-org-entry")
  (expect "* name TODO                                                             :some-tags:\nDEADLINE: <some-date>\n"
    (orgtrello-buffer/--private-compute-card-to-org-entry "TODO" "name" "some-date" ":some-tags:"))
  (expect "* name TODO\n"
    (orgtrello-buffer/--private-compute-card-to-org-entry "TODO" "name" nil nil))
  (expect "* name TODO                                                             :tag,tag2:\n"
    (orgtrello-buffer/--private-compute-card-to-org-entry "TODO" "name" nil ":tag,tag2:")))

(expectations
  (desc "orgtrello-buffer/--compute-due-date")
  (expect "DEADLINE: <some-date>\n" (orgtrello-buffer/--compute-due-date "some-date"))
  (expect "" (orgtrello-buffer/--compute-due-date nil)))

(expectations
  (expect "" (orgtrello-buffer/--serialize-tags "* card name" nil))
  (expect "" (orgtrello-buffer/--serialize-tags "* card name" ""))
  (expect "                                                             :red:green:" (orgtrello-buffer/--serialize-tags "* card name" ":red:green:"))
  (expect "                                                     :red:green:blue:" (orgtrello-buffer/--serialize-tags "* another card name" ":red:green:blue:"))
  (expect " :red:green:blue:" (orgtrello-buffer/--serialize-tags "* this is another card name with an extremely long label name, more than 72 chars" ":red:green:blue:")))

(expectations (desc "orgtrello-buffer/--compute-marker-from-entry")
              (expect "id"                                                        (orgtrello-buffer/--compute-marker-from-entry (orgtrello-data/make-hash-org :users :level :kwd :name      "id"  :due :position :buffername :desc :comments :tags :unknown)))
              (expect "orgtrello-marker-2a0b98e652ce6349a0659a7a8eeb3783ffe9a11a" (orgtrello-buffer/--compute-marker-from-entry (orgtrello-data/make-hash-org :users :level :kwd "some-name" nil :due 1234      "buffername" :desc :comments :tags :unknown)))
              (expect "orgtrello-marker-6c59c5dcf6c83edaeb3f4923bfd929a091504bb3" (orgtrello-buffer/--compute-marker-from-entry (orgtrello-data/make-hash-org :users :level :kwd "some-name" nil :due 4321      "some-other-buffername" :desc :comments :tags :unknown))))

(expectations (desc "orgtrello-buffer/--symbol")
              (expect ""      (orgtrello-buffer/--symbol " "  0))
              (expect "*"     (orgtrello-buffer/--symbol "*"  1))
              (expect "****"  (orgtrello-buffer/--symbol "**" 2))
              (expect "   "   (orgtrello-buffer/--symbol " "  3)) )

(expectations (desc "orgtrello-buffer/--space")
              (expect ""    (orgtrello-buffer/--space 0))
              (expect " "   (orgtrello-buffer/--space 1))
              (expect "  "  (orgtrello-buffer/--space 2))
              (expect "   " (orgtrello-buffer/--space 3)) )

(expectations (desc "orgtrello-buffer/--compute-level-into-spaces")
              (expect 0 (orgtrello-buffer/--compute-level-into-spaces 2))
              (expect 2 (orgtrello-buffer/--compute-level-into-spaces nil))
              (expect 2 (orgtrello-buffer/--compute-level-into-spaces 'any)))

(expectations (desc "orgtrello-buffer/--compute-checklist-to-org-checkbox")
              (expect "- [X] name\n" (orgtrello-buffer/--compute-checklist-to-org-checkbox "name" 2 "complete"))
              (expect "  - [X] name\n" (orgtrello-buffer/--compute-checklist-to-org-checkbox "name" 3 "complete"))
              (expect "- [X] name\n" (orgtrello-buffer/--compute-checklist-to-org-checkbox "name" 2 "complete"))
              (expect "  - [-] name\n" (orgtrello-buffer/--compute-checklist-to-org-checkbox "name" 3 "incomplete")))

(expectations (desc "orgtrello-buffer/--compute-state-checkbox")
              (expect "[X]" (orgtrello-buffer/--compute-state-checkbox "complete"))
              (expect "[-]" (orgtrello-buffer/--compute-state-checkbox "incomplete")))

(expectations
  (desc "orgtrello-buffer/--dispatch-create-entities-map-with-adjacency")
  (expect 'orgtrello-buffer/--put-card-with-adjacency     (orgtrello-buffer/--dispatch-create-entities-map-with-adjacency (orgtrello-data/make-hash-org :users *ORGTRELLO/CARD-LEVEL* nil nil nil nil nil nil nil :comments :tags :unknown)))
  (expect 'orgtrello-backend/--put-entities-with-adjacency (orgtrello-buffer/--dispatch-create-entities-map-with-adjacency (orgtrello-data/make-hash-org :users *ORGTRELLO/CHECKLIST-LEVEL* nil nil nil nil nil nil nil :comments :tags :unknown)))
  (expect 'orgtrello-backend/--put-entities-with-adjacency (orgtrello-buffer/--dispatch-create-entities-map-with-adjacency (orgtrello-data/make-hash-org :users *ORGTRELLO/ITEM-LEVEL* nil nil nil nil nil nil nil :comments :tags :unknown))))

(expectations
  (desc "testing orgtrello-buffer/--to-orgtrello-metadata")
  (expect "some name :orgtrello-id-identifier:"  (gethash :name       (orgtrello-buffer/--to-orgtrello-metadata '(:unknown :comments "" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect "IN PROGRESS"                          (gethash :keyword    (orgtrello-buffer/--to-orgtrello-metadata '(:unknown :comments "" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect 0                                      (gethash :level      (orgtrello-buffer/--to-orgtrello-metadata '(:unknown :comments "" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect :id                                    (gethash :id         (orgtrello-buffer/--to-orgtrello-metadata '(:unknown :comments "" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect :due                                   (gethash :due        (orgtrello-buffer/--to-orgtrello-metadata '(:unknown :comments "" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect :point                                 (gethash :position   (orgtrello-buffer/--to-orgtrello-metadata '(:unknown :comments "" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect "1,2,3"                                (gethash :member-ids (orgtrello-buffer/--to-orgtrello-metadata '(:unknown :comments "" "1,2,3" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect :desc                                  (gethash :desc       (orgtrello-buffer/--to-orgtrello-metadata '(:unknown :comments :desc "1,2,3" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect :comments                              (gethash :comments   (orgtrello-buffer/--to-orgtrello-metadata '(:unknown :comments :desc "1,2,3" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil))))
  (expect :unknown                               (gethash :unknown-properties (orgtrello-buffer/--to-orgtrello-metadata '(:unknown :comments :desc "1,2,3" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil)))))

(expectations
 (desc "orgtrello-buffer/--to-orgtrello-metadata - Default keyword if nil is the first element of *ORGTRELLO/ORG-KEYWORD-TRELLO-LIST-NAMES*")
 (expect :default (let ((*ORGTRELLO/ORG-KEYWORD-TRELLO-LIST-NAMES* '(:default :other-keywords-we-do-not-care)))
                     (gethash :keyword (orgtrello-buffer/--to-orgtrello-metadata '(:unknown :comments "" "" "buffer-name.org" :point :id :due 0 1 nil nil "some name :orgtrello-id-identifier:" nil))))))


(expectations (desc "testing orgtrello-buffer/--convert-orgmode-date-to-trello-date")
              (expect "2013-07-18T02:00:00.000Z" (orgtrello-buffer/--convert-orgmode-date-to-trello-date "2013-07-18T02:00:00.000Z"))
              (expect "2013-07-29T14:00:00.000Z" (orgtrello-buffer/--convert-orgmode-date-to-trello-date "2013-07-29 lun. 14:00"))
              (expect "2013-07-29T00:00:00.000Z" (orgtrello-buffer/--convert-orgmode-date-to-trello-date "2013-07-29"))
              (expect nil                        (orgtrello-buffer/--convert-orgmode-date-to-trello-date nil)))

(expectations (desc "orgtrello-buffer/entry-get-full-metadata!")
              (expect nil    (->> (orgtrello-tests/with-temp-buffer "* card" (orgtrello-buffer/entry-get-full-metadata!))
                               (orgtrello-data/parent)))
              (expect nil    (->> (orgtrello-tests/with-temp-buffer "* card" (orgtrello-buffer/entry-get-full-metadata!))
                               (orgtrello-data/grandparent)))
              (expect "card" (->> (orgtrello-tests/with-temp-buffer "* card" (orgtrello-buffer/entry-get-full-metadata!))
                               (orgtrello-data/current)
                               orgtrello-data/entity-name)))

(expectations (desc "orgtrello-buffer/entry-get-full-metadata!")
              (expect "card"      (->> (orgtrello-tests/with-temp-buffer "* card\n- [ ] checklist\n" (orgtrello-buffer/entry-get-full-metadata!))
                                    (orgtrello-data/parent)
                                    orgtrello-data/entity-name))
              (expect nil         (->> (orgtrello-tests/with-temp-buffer "* card\n- [ ] checklist\n" (orgtrello-buffer/entry-get-full-metadata!))
                                    (orgtrello-data/grandparent)))
              (expect "checklist" (->> (orgtrello-tests/with-temp-buffer "* card\n- [ ] checklist\n" (orgtrello-buffer/entry-get-full-metadata!))
                                    (orgtrello-data/current)
                                    orgtrello-data/entity-name)))

(expectations (desc "orgtrello-buffer/entry-get-full-metadata!")
              (expect "checklist" (->> (orgtrello-tests/with-temp-buffer "* card\n- [ ] checklist\n  - [ ] item\n" (orgtrello-buffer/entry-get-full-metadata!))
                                    (orgtrello-data/parent)
                                    orgtrello-data/entity-name))
              (expect "card"      (->> (orgtrello-tests/with-temp-buffer "* card\n- [ ] checklist\n  - [ ] item\n" (orgtrello-buffer/entry-get-full-metadata!))
                                    (orgtrello-data/grandparent)
                                    orgtrello-data/entity-name))
              (expect "item"      (->> (orgtrello-tests/with-temp-buffer "* card\n- [ ] checklist\n  - [ ] item\n" (orgtrello-buffer/entry-get-full-metadata!))
                                    (orgtrello-data/current)
                                    orgtrello-data/entity-name)))

(ert-deftest testing-orgtrello-buffer/metadata! ()
  (let ((h-values            (orgtrello-tests/with-temp-buffer ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont some-user-id
#+PROPERTY: orgtrello-user-dude some-user-id2
:END:

* TODO card title
:PROPERTIES:
:orgtrello-id: some-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: this is some comments###dude: some other comment
:END:
  some description\n"
                                                               (orgtrello-buffer/metadata!)
                                                               -2)))
    (should (equal 1                                                             (orgtrello-data/entity-level h-values)))
    (should (equal nil                                                           (orgtrello-data/entity-tags h-values)))
    (should (equal "card title"                                                  (orgtrello-data/entity-name h-values)))
    (should (equal "some-id"                                                     (orgtrello-data/entity-id h-values)))
    (should (equal nil                                                           (orgtrello-data/entity-due h-values)))
    (should (equal "some description"                                            (orgtrello-data/entity-description h-values)))
    (should (equal "ardumont: this is some comments###dude: some other comment"  (orgtrello-data/entity-comments h-values)))
    (should (equal "some-user-id,some-user-id2"                                  (orgtrello-data/entity-member-ids h-values)))
    (should (equal "TODO"                                                        (orgtrello-data/entity-keyword h-values)))
    (should (equal nil                                                           (orgtrello-data/entity-unknown-properties h-values)))))

(expectations (desc "orgtrello-marker-2a0b98e652ce6349a0659a7a8eeb3783ffe9a11a")
              (expect "orgtrello-marker-2a0b98e652ce6349a0659a7a8eeb3783ffe9a11a" (orgtrello-buffer/compute-marker "buffername" "some-name" 1234))
              (expect "orgtrello-marker-6c59c5dcf6c83edaeb3f4923bfd929a091504bb3" (orgtrello-buffer/compute-marker "some-other-buffername" "some-name" 4321)))

(expectations
  (desc "orgtrello-buffer/--filter-out-known-properties")
  (expect '(("some-unknown-thingy" . "some value"))
    (orgtrello-buffer/--filter-out-known-properties '(("orgtrello-id" . "orgtrello-marker-08677ec948991d1e5a25ab6b813d8eba03fac20f")
                                                      ("orgtrello-users" . "some user")
                                                      ("some-unknown-thingy" . "some value")
                                                      ("CATEGORY" . "TESTS-simple")))))

(expectations
  (desc "orgtrello-buffer/org-unknown-drawer-properties!")
  (expect
      '(("some-unknown-thingy" . "some value"))
    (orgtrello-tests/with-temp-buffer
     "* TODO Joy of FUN(ctional) LANGUAGES
DEADLINE: <2014-05-17 Sat>
:PROPERTIES:
:orgtrello-id: orgtrello-marker-08677ec948991d1e5a25ab6b813d8eba03fac20f
:some-unknown-thingy: some value
:orgtrello-users: ardumont
:orgtrello-unknown-key-prefixed-by-orgtrello: some unknown value that will be filtered
:END:
"
     (orgtrello-buffer/org-unknown-drawer-properties!))))

(expectations
  (expect
      "* TODO Joy of FUN(ctional) LANGUAGES
DEADLINE: <2014-05-17 Sat>
:PROPERTIES:
:orgtrello-id: orgtrello-marker-08677ec948991d1e5a25ab6b813d8eba03fac20f
:property0: value0
:property1: value1
:property2: value2
:END:
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content
     "* TODO Joy of FUN(ctional) LANGUAGES
DEADLINE: <2014-05-17 Sat>
:PROPERTIES:
:orgtrello-id: orgtrello-marker-08677ec948991d1e5a25ab6b813d8eba03fac20f
:END:
"
     (orgtrello-buffer/update-properties-unknown! '(("property0" . "value0")
                                                   ("property1" . "value1")
                                                   ("property2" . "value2"))))))

(expectations
  (desc "orgtrello-buffer/overwrite-card! - No previous card on buffer.")
  (expect "* TODO some card name                                                   :red:green:
  :PROPERTIES:
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments: ardumont: some comment
  :orgtrello-id: some-card-id
  :orgtrello-local-checksum: local-card-checksum-567
  :END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\", \"orgtrello-local-checksum\":\"local-checkbox-checksum-567\"}
  - [X] some item name :PROPERTIES: {\"orgtrello-id\":\"some-item-id\", \"orgtrello-local-checksum\":\"local-item-checksum-567\"}
  - [ ] some other item name :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\", \"orgtrello-local-checksum\":\"local-item-checksum-567\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"local-checkbox-checksum-567\"}
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content
     "" ;; no previous content on buffer
     (with-mock
       (mock (orgtrello-buffer/card-checksum!) => "local-card-checksum-567")
       (mock (orgtrello-buffer/checklist-checksum!) => "local-checkbox-checksum-567")
       (mock (orgtrello-buffer/item-checksum!) => "local-item-checksum-567")
       (let* ((card (orgtrello-hash/make-properties `((:keyword . "TODO")
                                                      (:member-ids . "ardumont,dude")
                                                      (:comments . "ardumont: some comment")
                                                      (:tags . ":red:green:")
                                                      (:desc . "some description")
                                                      (:level . ,*ORGTRELLO/CARD-LEVEL*)
                                                      (:name . "some card name")
                                                      (:id . "some-card-id"))))
              (entities (orgtrello-hash/make-properties `(("some-checklist-id" . ,(orgtrello-hash/make-properties `((:id . "some-checklist-id")
                                                                                                                    (:name . "some checklist name")
                                                                                                                    (:level . ,*ORGTRELLO/CHECKLIST-LEVEL*))))
                                                          ("some-other-checklist-id" . ,(orgtrello-hash/make-properties `((:id . "some-other-checklist-id")
                                                                                                                          (:name . "some other checklist name")
                                                                                                                          (:level . ,*ORGTRELLO/CHECKLIST-LEVEL*))))
                                                          ("some-item-id"  . ,(orgtrello-hash/make-properties `((:id . "some-item-id")
                                                                                                                (:name . "some item name")
                                                                                                                (:level . ,*ORGTRELLO/ITEM-LEVEL*)
                                                                                                                (:keyword . "DONE"))))
                                                          ("some-other-item-id"  . ,(orgtrello-hash/make-properties `((:id . "some-other-item-id")
                                                                                                                      (:name . "some other item name")
                                                                                                                      (:level . ,*ORGTRELLO/ITEM-LEVEL*)
                                                                                                                      (:keyword . "TODO")))))))
              (entities-adj (orgtrello-hash/make-properties `(("some-other-checklist-id" . ())
                                                              ("some-checklist-id" . ("some-item-id" "some-other-item-id"))
                                                              ("some-card-id" . ("some-checklist-id" "some-other-checklist-id"))))))
         (orgtrello-buffer/overwrite-card! '(1 2) card entities entities-adj))))))

(expectations
  (desc "orgtrello-buffer/overwrite-card! - Multiple cards present at point. Overwrite given previous region card with updated data.")
  (expect "* TODO some card name                                                   :red:green:
  :PROPERTIES:
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments: ardumont: some comment
  :orgtrello-id: some-card-id
  :orgtrello-local-checksum: local-card-checksum-567
  :END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\", \"orgtrello-local-checksum\":\"local-checklist-checksum-567\"}
  - [X] some item name :PROPERTIES: {\"orgtrello-id\":\"some-item-id\", \"orgtrello-local-checksum\":\"local-item-checksum-567\"}
  - [ ] some other item name :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\", \"orgtrello-local-checksum\":\"local-item-checksum-567\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"local-checklist-checksum-567\"}

* IN-PROGRESS another card
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content
     "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ,
:orgtrello-card-comments: ardumont: some comment
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* IN-PROGRESS another card
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
"
     (with-mock
       (mock (orgtrello-buffer/card-checksum!) => "local-card-checksum-567")
       (mock (orgtrello-buffer/checklist-checksum!) => "local-checklist-checksum-567")
       (mock (orgtrello-buffer/item-checksum!) => "local-item-checksum-567")
       (let* ((card (orgtrello-hash/make-properties `((:keyword . "TODO")
                                                      (:member-ids . "ardumont,dude")
                                                      (:comments . "ardumont: some comment")
                                                      (:tags . ":red:green:")
                                                      (:desc . "some description")
                                                      (:level . ,*ORGTRELLO/CARD-LEVEL*)
                                                      (:name . "some card name")
                                                      (:id . "some-card-id"))))
              (entities (orgtrello-hash/make-properties `(("some-checklist-id" . ,(orgtrello-hash/make-properties `((:id . "some-checklist-id")
                                                                                                                    (:name . "some checklist name")
                                                                                                                    (:level . ,*ORGTRELLO/CHECKLIST-LEVEL*))))
                                                          ("some-other-checklist-id" . ,(orgtrello-hash/make-properties `((:id . "some-other-checklist-id")
                                                                                                                          (:name . "some other checklist name")
                                                                                                                          (:level . ,*ORGTRELLO/CHECKLIST-LEVEL*))))
                                                          ("some-item-id"  . ,(orgtrello-hash/make-properties `((:id . "some-item-id")
                                                                                                                (:name . "some item name")
                                                                                                                (:level . ,*ORGTRELLO/ITEM-LEVEL*)
                                                                                                                (:keyword . "DONE"))))
                                                          ("some-other-item-id"  . ,(orgtrello-hash/make-properties `((:id . "some-other-item-id")
                                                                                                                      (:name . "some other item name")
                                                                                                                      (:level . ,*ORGTRELLO/ITEM-LEVEL*)
                                                                                                                      (:keyword . "TODO")))))))
              (entities-adj (orgtrello-hash/make-properties `(("some-other-checklist-id" . ())
                                                              ("some-checklist-id" . ("some-item-id" "some-other-item-id"))
                                                              ("some-card-id" . ("some-checklist-id" "some-other-checklist-id"))))))
         (orgtrello-buffer/overwrite-card! '(1 461) card entities entities-adj)))
     -5)))

(expectations
  (desc "orgtrello-buffer/get-card-local-checksum! - Retrieve the card's checksum.")
  (expect
      "123"
    (orgtrello-tests/with-temp-buffer "* card
:PROPERTIES:
:orgtrello-local-checksum: 123
:END:"
                                      (orgtrello-buffer/get-card-local-checksum!))))
(expectations
  (desc "orgtrello-buffer/get-card-local-checksum! - Retrieve card's nil checksum.")
  (expect
      nil
    (orgtrello-tests/with-temp-buffer "* card"
                                      (orgtrello-buffer/get-card-local-checksum!))))

(expectations
  (desc "orgtrello-buffer/write-local-checksum-at-point! - checklist - Write local checksum at the current position.")
  (expect
      "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\", \"orgtrello-local-checksum\":\"checklist-checksum-098\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* another card"

    (orgtrello-tests/with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* another card"
                                                                (with-mock
                                                                  (mock (orgtrello-buffer/checklist-checksum!) => "checklist-checksum-098")
                                                                  (orgtrello-buffer/write-local-checksum-at-point!))
                                                                -5)))

(expectations
  (desc "orgtrello-buffer/write-local-checksum-at-point! - checklist - Write local checksum at the current position.")
  (expect
      "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\", \"orgtrello-local-checksum\":\"item-checksum-098876\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* another card"

    (orgtrello-tests/with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* another card"
                                                                (with-mock
                                                                  (mock (orgtrello-buffer/item-checksum!) => "item-checksum-098876")
                                                                  (orgtrello-buffer/write-local-checksum-at-point!))
                                                                -4)))

(expectations
  (desc "orgtrello-buffer/write-local-checksum-at-point! - Write local card checksum at the current position.")
  (expect
      "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: blah
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* another card
  :PROPERTIES:
  :orgtrello-local-checksum: card-checksum-987
  :END:
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: blah
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* another card
"
                                                                (with-mock
                                                                  (mock (orgtrello-buffer/card-checksum!) => "card-checksum-987")
                                                                  (orgtrello-buffer/write-local-checksum-at-point!)))))

(expectations
  (desc "orgtrello-buffer/org-delete-property! - on a checklist")
  (expect "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {}
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                                                                (orgtrello-buffer/org-delete-property! "orgtrello-id"))))

(expectations
  (desc "orgtrello-buffer/org-delete-property! - on a checklist - no previous property.")
  (expect "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {}
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {}
"
                                                                (orgtrello-buffer/org-delete-property! "orgtrello-id"))))

(expectations
  (desc "orgtrello-buffer/org-delete-property! - on an item.")
  (expect "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                                                                (orgtrello-buffer/org-delete-property! "orgtrello-id")
                                                                -2)))

(expectations
  (desc "orgtrello-buffer/org-delete-property! - on an item - no previous property.")
  (expect "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                                                                (orgtrello-buffer/org-delete-property! "orgtrello-id")
                                                                -2)))

(expectations
  (desc "orgtrello-buffer/org-delete-property! - on a card.")
  (expect "* TODO some card name
:PROPERTIES:
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                                                                (orgtrello-buffer/org-delete-property! "orgtrello-id")
                                                                -5)))

(expectations
  (desc "orgtrello-buffer/org-delete-property! - on a card - no previous property.")
  (expect "* TODO some card name
:PROPERTIES:
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                                                                (orgtrello-buffer/org-delete-property! "orgtrello-id")
                                                                -5)))

(expectations
  (desc "orgtrello-buffer/write-local-checksum-at-point! - Write local checksum at the current position.")
  (expect
      "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: blah
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"checklist-checksum-876\"}
"
      (orgtrello-tests/with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: blah
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                                                                  (with-mock
                                                                    (mock (orgtrello-buffer/checklist-checksum!) => "checklist-checksum-876")
                                                                    (orgtrello-buffer/write-local-checksum-at-point!))
                                                                  -1)))

(expectations
  (desc "orgtrello-buffer/write-local-checksum-at-point! - The local checksum does not change if no modification.")
  (expect
      "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: blah
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"checklist-checksum-543\"}
"
      (orgtrello-tests/with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: blah
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855\"}
"
                                                                  (with-mock
                                                                    (mock (orgtrello-buffer/checklist-checksum!) => "checklist-checksum-543")
                                                                    (orgtrello-buffer/write-local-checksum-at-point!))
                                                                  -1)))

(expectations
  (desc "orgtrello-buffer/write-local-checksum-at-point! - The local checksum changes if modifications.")
  (expect
      "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: blah
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist names :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"checklist-checksum-432\"}
"
      (orgtrello-tests/with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: blah
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist names :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                                                                  (with-mock
                                                                    (mock (orgtrello-buffer/checklist-checksum!) => "checklist-checksum-432")
                                                                    (orgtrello-buffer/write-local-checksum-at-point!))
                                                                  -1)))

(expectations
  (desc "orgtrello-buffer/write-local-checksum-at-point! - The local checksum does not change if no modification.")
  (expect
      nil
      (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: blah
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist names :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                                                                  (orgtrello-buffer/get-checkbox-local-checksum!)
                                                                  -1)))

(expectations
  (desc "orgtrello-buffer/get-checkbox-local-checksum! - Retrieve the local checksum from item.")
  (expect
      "foo"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: blah
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"foo\"}
"
                                      (orgtrello-buffer/get-checkbox-local-checksum!)
                                      -1)))

(expectations
  (desc "orgtrello-buffer/get-checkbox-local-checksum! - Retrieve the local checksum from item.")
  (expect
      "bar"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: blah
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\", \"orgtrello-local-checksum\":\"bar\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"foo\"}
"
                                      (orgtrello-buffer/get-checkbox-local-checksum!)
                                      -2)))

(expectations
  (desc "orgtrello-buffer/get-checkbox-local-checksum! - Works also on card but it is not intended to!")
  (expect
      "foobar"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: foobar
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\", \"orgtrello-local-checksum\":\"bar\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"foo\"}
"
                                      (orgtrello-buffer/get-checkbox-local-checksum!)
                                      -3)))

(expectations
  (desc "orgtrello-buffer/get-checkbox-local-checksum! - Compute the checksum of the item.")
  (expect
      "item-checksum"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: foobar
:END:
  some description
- [-] some checklist name
  - [ ] some other item
"
                                      (with-mock
                                        (mock (orgtrello-buffer/item-checksum!) => "item-checksum")
                                        (orgtrello-buffer/compute-checksum!))
                                      -1)))

(expectations
  (desc "orgtrello-buffer/get-checkbox-local-checksum! - Compute the checksum of the checklist.")
  (expect
      "checklist-checksum"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: foobar
:END:
  some description
- [-] some checklist name
  - [ ] some other item"
                                      (with-mock
                                        (mock (orgtrello-buffer/checklist-checksum!) => "checklist-checksum")
                                        (orgtrello-buffer/compute-checksum!))
                                      -1)))


(expectations
  (desc "orgtrello-buffer/compute-checksum! - Compute the checksum of the card.")
  (expect
      "card-checksum"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: foobar
:END:
  some description
- [-] some checklist name
  - [ ] some other item"
                                      (with-mock
                                        (mock (orgtrello-buffer/card-checksum!) => "card-checksum")
                                        (orgtrello-buffer/compute-checksum!))
                                      -10)))

(expectations
  (desc "orgtrello-buffer/get-local-checksum! - The local checksum does not change if no modification.")
  (expect
      nil
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: blah
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist names :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                                      (orgtrello-buffer/get-local-checksum!)
                                      -1)))

(expectations
  (desc "orgtrello-buffer/get-local-checksum! - Retrieve the local checksum from item.")
  (expect
      "foo"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: blah
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"foo\"}
"
                                      (orgtrello-buffer/get-local-checksum!)
                                      -1)))

(expectations
  (desc "orgtrello-buffer/get-local-checksum! - Retrieve the local checksum from item.")
  (expect
      "bar"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: blah
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\", \"orgtrello-local-checksum\":\"bar\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"foo\"}
"
                                      (orgtrello-buffer/get-local-checksum!)
                                      -2)))

(expectations
  (desc "orgtrello-buffer/get-local-checksum! - Works also on card but it is not intended to!")
  (expect
      "foobar"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: foobar
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\", \"orgtrello-local-checksum\":\"bar\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"foo\"}
"
                                      (orgtrello-buffer/get-local-checksum!)
                                      -3)))

(expectations
  (desc "orgtrello-buffer/update-properties-at-pt! - Update card property's id + card checksum computation and update.")
  (expect
      "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: card-checksum-321
:END:
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: foobar
:END:
"
                                                                (with-mock
                                                                  (mock (orgtrello-buffer/card-checksum!) => "card-checksum-321")
                                                                  (orgtrello-buffer/update-properties-at-pt! "some-id" nil)))))

(expectations
  (desc "orgtrello-buffer/update-properties-at-pt! - Update checkbox property's id + compute checksum at point and set it.")
  (expect
      "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: foobar
:END:
- [ ] new checkbox :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\", \"orgtrello-local-checksum\":\"checklist-checksum-321\"}
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: foobar
:END:
- [ ] new checkbox
"
                                                                (with-mock
                                                                  (mock (orgtrello-buffer/checklist-checksum!) => "checklist-checksum-321")
                                                                  (orgtrello-buffer/update-properties-at-pt! "some-checklist-id" 'checkbox-p))
                                                                -1)))

(expectations
  (desc "orgtrello-buffer/update-properties-at-pt! - Update checkbox property's id + compute checksum at point and set it.")
  (expect
      "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: foobar
:END:
- [ ] new checkbox
  - [ ] new item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\", \"orgtrello-local-checksum\":\"item-checksum-321\"}
"
    (orgtrello-tests/with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: foobar
:END:
- [ ] new checkbox
  - [ ] new item
"
                                                                (with-mock
                                                                  (mock (orgtrello-buffer/item-checksum!) => "item-checksum-321")
                                                                  (orgtrello-buffer/update-properties-at-pt! "some-item-id" 'checkbox-p))
                                                                -1)))

(expectations
  (desc "orgtrello-buffer/card-checksum! - Compute the checksum of a card.")
  (expect
      "a058272445d320995bd4c677dd35c0924ff65ce7640cbe7cae21d6ea39ff32c6"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* another card"
                                      (orgtrello-buffer/card-checksum!)
                                      -5)))

(expectations
  (desc "orgtrello-buffer/card-checksum! - A card with a checksum should give the same checksum if nothing has changed.")
  (expect
      "a058272445d320995bd4c677dd35c0924ff65ce7640cbe7cae21d6ea39ff32c6"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: a058272445d320995bd4c677dd35c0924ff65ce7640cbe7cae21d6ea39ff32c6
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* another card"
                                      (orgtrello-buffer/card-checksum!)
                                      -5)))
(expectations
  (desc "orgtrello-buffer/card-checksum! - A modified card with a checksum should give another checksum.")
  (expect
      "4c3c5e097d3d6d906a967cf7237d1bb173c019440239cc0bdc96caa296628df1"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: a058272445d320995bd4c677dd35c0924ff65ce7640cbe7cae21d6ea39ff32c6
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [X] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* another card"
                                      (orgtrello-buffer/card-checksum!)
                                      -5)))

(expectations
  (desc "orgtrello-buffer/checklist-checksum! - A checklist gives a checksum when asked politely.")
  (expect
      "0bf5cb599c6f0e40f4b163fc0feb25e67f3da9c275d576b830deff19043502eb"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: a058272445d320995bd4c677dd35c0924ff65ce7640cbe7cae21d6ea39ff32c6
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [X] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                                      (orgtrello-buffer/checklist-checksum!))))

(expectations
  (desc "orgtrello-buffer/checklist-checksum! - A checklist gives a checksum when asked politely - does not take `'orgtrello-local-checksum`' property into account.")
  (expect
      "0bf5cb599c6f0e40f4b163fc0feb25e67f3da9c275d576b830deff19043502eb"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: a058272445d320995bd4c677dd35c0924ff65ce7640cbe7cae21d6ea39ff32c6
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
  - [X] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
- [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"0bf5cb599c6f0e40f4b163fc0feb25e67f3da9c275d576b830deff19043502eb\"}
"
                                      (orgtrello-buffer/checklist-checksum!))))

(expectations
  (desc "orgtrello-buffer/checklist-checksum! - A checklist checksum takes into account its items. If items change then the checkbox's checksum is updated.")
  (expect
      "bb52a07b4359d5a00aa5e313456f14e69820c2850c75fc1e55ea59bbbe28f6d5"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: a058272445d320995bd4c677dd35c0924ff65ce7640cbe7cae21d6ea39ff32c6
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
"
                                      (orgtrello-buffer/checklist-checksum!)
                                      -2))
  (expect
      "06789a52f5d3a212877afdef343a145fb595b7f220204983bf5177d3a192795a"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: a058272445d320995bd4c677dd35c0924ff65ce7640cbe7cae21d6ea39ff32c6
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [ ] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
"
                                      (orgtrello-buffer/checklist-checksum!)
                                      -2)))

(expectations
  (desc "orgtrello-buffer/item-checksum! - An item's checksum")
  (expect
      "d5503e92e8880ddb839c42e31e8ead17a70f09d39f069fbfd9956424984047fc"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: a058272445d320995bd4c677dd35c0924ff65ce7640cbe7cae21d6ea39ff32c6
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
"
                                      (orgtrello-buffer/item-checksum!)
                                      -1)))

(expectations
  (desc "orgtrello-buffer/item-checksum! - An item's checksum does not change even if there is already a checksum computed.")
  (expect
      "d5503e92e8880ddb839c42e31e8ead17a70f09d39f069fbfd9956424984047fc"
    (orgtrello-tests/with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: a058272445d320995bd4c677dd35c0924ff65ce7640cbe7cae21d6ea39ff32c6
:END:
  some description
- [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
  - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\", \"orgtrello-local-checksum\":\"d5503e92e8880ddb839c42e31e8ead17a70f09d39f069fbfd9956424984047fc\"}
"
                                      (orgtrello-buffer/item-checksum!)
                                      -1)))

(provide 'org-trello-buffer-tests)
;;; org-trello-buffer-tests.el ends here
