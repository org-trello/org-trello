(require 'org-trello-buffer)
(require 'ert)
(require 'el-mock)

(ert-deftest test-orgtrello-buffer-extract-description-from-current-position ()
  (should (string= "llo there"
                   (orgtrello-tests-with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
   DEADLINE: <2014-04-01T00:00:00.000Z>
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
hello there
"
                                                     (orgtrello-buffer-extract-description-from-current-position))))

  (should (string= "hello there"
                   (orgtrello-tests-with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
  - [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
    - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
    - [X] Common-Lisp :PROPERTIES: {\"orgtrello-id\":\"52c94518b2c5b28e37012ba4\"}"
                                                     (orgtrello-buffer-extract-description-from-current-position))))

  (should (string= "\nhello there\n"
                   (orgtrello-tests-with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:

  hello there

  - [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
    - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
    - [X] Common-Lisp :PROPERTIES: {\"orgtrello-id\":\"52c94518b2c5b28e37012ba4\"}"
                                                     (orgtrello-buffer-extract-description-from-current-position))))

  (should (string= ""
                   (orgtrello-tests-with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES" (orgtrello-buffer-extract-description-from-current-position))))

  (should (string= ""
                   (orgtrello-tests-with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}"
                                                     (orgtrello-buffer-extract-description-from-current-position))))
  (should (string= "One Paragraph\n\nAnother Paragraph"
                   (orgtrello-tests-with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
  DEADLINE: <2014-04-01T00:00:00.000Z>
  :PROPERTIES:
  :orgtrello-id: 52c945143004d4617c012528
  :END:
  One Paragraph

  Another Paragraph
"
                                                     (orgtrello-buffer-extract-description-from-current-position))))
  (should (string= "hello there"
                   (orgtrello-tests-with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
 :PROPERTIES:
 :orgtrello-id: 52c945143004d4617c012528
 :END:
  hello there
"
                                                     (orgtrello-buffer-extract-description-from-current-position))))

  (should (string= "hello there"
                   (orgtrello-tests-with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
 :PROPERTIES:
 :orgtrello-id: 52c945143004d4617c012528
    :END:
  hello there
  - [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
    - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
    - [X] Common-Lisp :PROPERTIES: {\"orgtrello-id\":\"52c94518b2c5b28e37012ba4\"}"
                                                     (orgtrello-buffer-extract-description-from-current-position))))

  (should (string= "\nhello there\n"
                   (orgtrello-tests-with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
  :PROPERTIES:
         :orgtrello-id: 52c945143004d4617c012528
  :END:

  hello there

  - [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
    - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
    - [X] Common-Lisp :PROPERTIES: {\"orgtrello-id\":\"52c94518b2c5b28e37012ba4\"}"
                                                     (orgtrello-buffer-extract-description-from-current-position))))

  (should (string= ""
                   (orgtrello-tests-with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
  :PROPERTIES:
 :orgtrello-id: 52c945143004d4617c012528
:END:
  - [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}"
                                                     (orgtrello-buffer-extract-description-from-current-position)))))

(ert-deftest test-orgtrello-buffer-board-id ()
  (should (equal
           "this-is-the-board-id"
           (orgtrello-tests-with-org-buffer
            (format ":PROPERTIES:\n#+PROPERTY: %s this-is-the-board-id\n:END:\n* card\n" org-trello--property-board-id)
            (orgtrello-buffer-board-id))))
  (should (equal "this-is-the-board-name"
                 (orgtrello-tests-with-org-buffer
                  (format ":PROPERTIES:\n#+PROPERTY: %s this-is-the-board-name\n:END:\n* card\n" org-trello--property-board-name)
                  (orgtrello-buffer-board-name))))
  (should (equal "this-is-the-user"
                 (orgtrello-tests-with-org-buffer
                  (format ":PROPERTIES:\n#+PROPERTY: %s this-is-the-user\n:END:\n* card\n" org-trello--property-user-me)
                  (orgtrello-buffer-me)))))

(ert-deftest test-orgtrello-buffer-write-card-header ()
  (should (equal ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:
* TODO some card name
  :PROPERTIES:
  :orgtrello-users: ardumont,dude
  :END:
  some description"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content
                  ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:
"

                  (orgtrello-buffer-write-card-header "some-card-id" (orgtrello-hash-make-properties `((:keyword . "TODO")
                                                                                                        (:member-ids . "ardumont,dude")
                                                                                                        (:comments . 'no-longer-exploited-here-comments)
                                                                                                        (:desc . "some description")
                                                                                                        (:level . ,org-trello--card-level)
                                                                                                        (:name . "some card name"))))
                  0)))
  (should (equal ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:
* TODO some card name                                                   :red:green:
DEADLINE: <some-due-date>
  :PROPERTIES:
  :orgtrello-users: ardumont,dude
  :END:
  some description"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content
                  ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:
"
                  (orgtrello-buffer-write-card-header "some-card-id" (orgtrello-hash-make-properties `((:keyword . "TODO")
                                                                                                        (:member-ids . "ardumont,dude")
                                                                                                        (:comments . 'no-longer-exploited-here-comments)
                                                                                                        (:tags . ":red:green:")
                                                                                                        (:desc . "some description")
                                                                                                        (:level . ,org-trello--card-level)
                                                                                                        (:name . "some card name")
                                                                                                        (:due . "some-due-date"))))
                  0))))

(ert-deftest test-orgtrello-buffer-write-checklist-header ()
  (should (equal ":PROPERTIES:
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
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content
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
                  (orgtrello-buffer-write-checklist-header "some-id" (orgtrello-hash-make-properties `((:keyword . "DONE")
                                                                                                        (:level . ,org-trello--checklist-level)
                                                                                                        (:name . "some checklist name"))))
                  0))))

(ert-deftest test-orgtrello-buffer-write-card-no-previous-card-on-buffer ()
  (should (equal ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:
* TODO some card name                                                   :red:green:
  :PROPERTIES:
  :orgtrello-users: ardumont,dude
  :orgtrello-local-checksum: local-card-checksum-456
  :orgtrello-id: some-card-id
  :END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\",\"orgtrello-local-checksum\":\"local-checkbox-checksum-456\"}
    - [X] some item name :PROPERTIES: {\"orgtrello-id\":\"some-item-id\",\"orgtrello-local-checksum\":\"local-item-checksum-456\"}
    - [ ] some other item name :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\",\"orgtrello-local-checksum\":\"local-item-checksum-456\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\",\"orgtrello-local-checksum\":\"local-checkbox-checksum-456\"}

** COMMENT ardumont, some-date
:PROPERTIES:
:orgtrello-id: some-comment-id
:orgtrello-local-checksum: local-comment-checksum-456
:END:
  some comment

"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content
                  ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:
"
                  (with-mock
                    (mock (orgtrello-buffer-card-checksum) => "local-card-checksum-456")
                    (mock (orgtrello-buffer-checklist-checksum) => "local-checkbox-checksum-456")
                    (mock (orgtrello-buffer-item-checksum) => "local-item-checksum-456")
                    (mock (orgtrello-buffer-comment-checksum) => "local-comment-checksum-456")
                    (orgtrello-buffer-write-card "some-card-id"
                                                  (orgtrello-hash-make-properties `((:keyword . "TODO")
                                                                                    (:member-ids . "ardumont,dude")
                                                                                    (:comments . ,(list (orgtrello-hash-make-properties '((:comment-user . "ardumont")
                                                                                                                                          (:comment-date . "some-date")
                                                                                                                                          (:comment-id   . "some-comment-id")
                                                                                                                                          (:comment-text . "some comment")))))
                                                                                    (:tags . ":red:green:")
                                                                                    (:desc . "some description")
                                                                                    (:level . ,org-trello--card-level)
                                                                                    (:name . "some card name")
                                                                                    (:id . "some-card-id")))
                                                  (orgtrello-hash-make-properties `(("some-checklist-id" . ,(orgtrello-hash-make-properties `((:id . "some-checklist-id")
                                                                                                                                              (:name . "some checklist name")
                                                                                                                                              (:level . ,org-trello--checklist-level))))
                                                                                    ("some-other-checklist-id" . ,(orgtrello-hash-make-properties `((:id . "some-other-checklist-id")
                                                                                                                                                    (:name . "some other checklist name")
                                                                                                                                                    (:level . ,org-trello--checklist-level))))
                                                                                    ("some-item-id"  . ,(orgtrello-hash-make-properties `((:id . "some-item-id")
                                                                                                                                          (:name . "some item name")
                                                                                                                                          (:level . ,org-trello--item-level)
                                                                                                                                          (:keyword . "DONE"))))
                                                                                    ("some-other-item-id"  . ,(orgtrello-hash-make-properties `((:id . "some-other-item-id")
                                                                                                                                                (:name . "some other item name")
                                                                                                                                                (:level . ,org-trello--item-level)
                                                                                                                                                (:keyword . "TODO"))))))

                                                  (orgtrello-hash-make-properties `(("some-other-checklist-id" . ())
                                                                                    ("some-checklist-id" . ("some-item-id" "some-other-item-id"))
                                                                                    ("some-card-id" . ("some-checklist-id" "some-other-checklist-id"))))))
                  0))))

(ert-deftest test-orgtrello-buffer-write-card-successive-writes-on-buffer ()
  (should (equal ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:
* TODO task A
  :PROPERTIES:
  :orgtrello-users: ardumont,dude
  :orgtrello-id: card-id-a
  :orgtrello-local-checksum: local-card-checksum-a
  :END:
  description A

** COMMENT ardumont, some-date
:PROPERTIES:
:orgtrello-id: some-comment-id
:orgtrello-local-checksum: local-comment-checksum-a
:END:
  some comment

** COMMENT ben, 10/01/2202
:PROPERTIES:
:orgtrello-id: some-id
:orgtrello-local-checksum: local-comment-checksum-a
:END:
  comment text

* TODO task B
  :PROPERTIES:
  :orgtrello-users: ardumont,dude
  :orgtrello-id: card-id-b
  :orgtrello-local-checksum: local-card-checksum-b
  :END:
  description B

** COMMENT tony, 10/10/2014
:PROPERTIES:
:orgtrello-id: some-com-id
:orgtrello-local-checksum: local-comment-checksum-b
:END:
  some text

"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content
                  ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:
"
                  (progn
                    (with-mock
                      (mock (orgtrello-buffer-card-checksum) => "local-card-checksum-a")
                      (mock (orgtrello-buffer-comment-checksum) => "local-comment-checksum-a")
                      (orgtrello-buffer-write-card "card-id-a"
                                                    (orgtrello-hash-make-properties
                                                     `((:keyword . "TODO")
                                                       (:desc . "description A")
                                                       (:level . ,org-trello--card-level)
                                                       (:name . "task A")
                                                       (:id . "card-id-a")
                                                       (:member-ids . "ardumont,dude")
                                                       (:comments . ,(list (orgtrello-hash-make-properties '((:comment-user . "ardumont")
                                                                                                             (:comment-date . "some-date")
                                                                                                             (:comment-id   . "some-comment-id")
                                                                                                             (:comment-text . "some comment")))
                                                                           (orgtrello-hash-make-properties '((:comment-user . "ben")
                                                                                                             (:comment-date . "10/01/2202")
                                                                                                             (:comment-id   . "some-id")
                                                                                                             (:comment-text . "comment text")))))))
                                                    (orgtrello-hash-make-properties `())
                                                    (orgtrello-hash-make-properties `())))
                    (with-mock
                      (mock (orgtrello-buffer-card-checksum) => "local-card-checksum-b")
                      (mock (orgtrello-buffer-comment-checksum) => "local-comment-checksum-b")
                      (orgtrello-buffer-write-card "card-id-b"
                                                    (orgtrello-hash-make-properties
                                                     `((:keyword . "TODO")
                                                       (:desc . "description B")
                                                       (:level . ,org-trello--card-level)
                                                       (:name . "task B")
                                                       (:id . "card-id-b")
                                                       (:member-ids . "ardumont,dude")
                                                       (:comments . ,(list (orgtrello-hash-make-properties '((:comment-user . "tony")
                                                                                                             (:comment-date . "10/10/2014")
                                                                                                             (:comment-id   . "some-com-id")
                                                                                                             (:comment-text . "some text")))))))
                                                    (orgtrello-hash-make-properties `())
                                                    (orgtrello-hash-make-properties `()))))
                  0))))

(ert-deftest test-orgtrello-buffer-write-card-successive-writes-on-buffer-with-indentation ()
  (should (equal ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:

* TODO task A
  :PROPERTIES:
  :orgtrello-users: ardumont,dude
  :orgtrello-id: card-id-a
  :orgtrello-local-checksum: local-card-checksum
  :END:


** COMMENT ardumont, some-date
:PROPERTIES:
:orgtrello-id: some-comment-id
:orgtrello-local-checksum: local-comment-checksum
:END:
  some comment

** COMMENT ben, 10/01/2202
:PROPERTIES:
:orgtrello-id: some-id
:orgtrello-local-checksum: local-comment-checksum
:END:
  comment text

* TODO task B
  :PROPERTIES:
  :orgtrello-users: ardumont,dude
  :orgtrello-id: card-id-b
  :orgtrello-local-checksum: local-card-checksum
  :END:


** COMMENT tony, 10/10/2014
:PROPERTIES:
:orgtrello-id: some-com-id
:orgtrello-local-checksum: local-comment-checksum
:END:
  some text

"
                 (orgtrello-tests-with-temp-buffer-indented-and-return-buffer-content
                  ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:

"
                  (progn
                    (with-mock
                      (mock (orgtrello-buffer-card-checksum) => "local-card-checksum")
                      (mock (orgtrello-buffer-comment-checksum) => "local-comment-checksum")
                      (orgtrello-controller-sync-buffer-with-trello-cards (buffer-name)
                                                                           `(,(orgtrello-hash-make-properties
                                                                               `((:keyword . "TODO")
                                                                                 (:desc . "")
                                                                                 (:level . ,org-trello--card-level)
                                                                                 (:name . "task A")
                                                                                 (:id . "card-id-a")
                                                                                 (:member-ids . "ardumont-id,dude-id")
                                                                                 (:comments . ,(list (orgtrello-hash-make-properties '((:comment-user . "ardumont")
                                                                                                                                       (:comment-date . "some-date")
                                                                                                                                       (:comment-id   . "some-comment-id")
                                                                                                                                       (:comment-text . "some comment")))
                                                                                                     (orgtrello-hash-make-properties '((:comment-user . "ben")
                                                                                                                                       (:comment-date . "10/01/2202")
                                                                                                                                       (:comment-id   . "some-id")
                                                                                                                                       (:comment-text . "comment text")))))))
                                                                             ,(orgtrello-hash-make-properties
                                                                               `((:keyword . "TODO")
                                                                                 (:desc . "")
                                                                                 (:level . ,org-trello--card-level)
                                                                                 (:name . "task B")
                                                                                 (:id . "card-id-b")
                                                                                 (:member-ids . "ardumont-id,dude-id")
                                                                                 (:comments . ,(list (orgtrello-hash-make-properties '((:comment-user . "tony")
                                                                                                                                       (:comment-date . "10/10/2014")
                                                                                                                                       (:comment-id   . "some-com-id")
                                                                                                                                       (:comment-text . "some text")))))))))))
                  0))))

(ert-deftest test-orgtrello-buffer-write-checklist ()
  ;; Simple case
  (should (equal ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:

* TODO some card name
  :PROPERTIES:
  :orgtrello-id: some-card-id
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments: ardumont: some comment
  :orgtrello-local-checksum: 1234-card-checksum
  :END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\",\"orgtrello-local-checksum\":\"1234-checklist-checksum\"}
"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content
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
                    (mock (orgtrello-buffer-checklist-checksum) => "1234-checklist-checksum")
                    (mock (orgtrello-buffer-card-checksum) => "1234-card-checksum")
                    (orgtrello-buffer-write-checklist "some-checklist-id"
                                                       (orgtrello-hash-make-properties `(("some-checklist-id" . ,(orgtrello-hash-make-properties `((:id . "some-checklist-id")
                                                                                                                                                   (:name . "some checklist name")
                                                                                                                                                   (:level . ,org-trello--checklist-level))))))
                                                       (orgtrello-hash-make-properties `(("some-checklist-id" . nil)))))
                  0)))
  ;; a little more complicated case
  (should (equal ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:

* TODO some card name
  :PROPERTIES:
  :orgtrello-id: some-card-id
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments: ardumont: some comment
  :orgtrello-local-checksum: card-checksum-654321
  :END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\",\"orgtrello-local-checksum\":\"checklist-checksum-654321\"}
    - [X] some item name :PROPERTIES: {\"orgtrello-id\":\"some-item-id\",\"orgtrello-local-checksum\":\"item-checksum-654321\"}
    - [ ] some other item name :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\",\"orgtrello-local-checksum\":\"item-checksum-654321\"}
"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content
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
                    (mock (orgtrello-buffer-item-checksum) => "item-checksum-654321")
                    (mock (orgtrello-buffer-checklist-checksum) => "checklist-checksum-654321")
                    (mock (orgtrello-buffer-card-checksum) => "card-checksum-654321")
                    (orgtrello-buffer-write-checklist "some-checklist-id"
                                                       (orgtrello-hash-make-properties `(("some-checklist-id" . ,(orgtrello-hash-make-properties `((:id . "some-checklist-id")
                                                                                                                                                   (:name . "some checklist name")
                                                                                                                                                   (:level . ,org-trello--checklist-level))))
                                                                                         ("some-item-id"  . ,(orgtrello-hash-make-properties `((:id . "some-item-id")
                                                                                                                                               (:name . "some item name")
                                                                                                                                               (:level . ,org-trello--item-level)
                                                                                                                                               (:keyword . "DONE"))))
                                                                                         ("some-other-item-id"  . ,(orgtrello-hash-make-properties `((:id . "some-other-item-id")
                                                                                                                                                     (:name . "some other item name")
                                                                                                                                                     (:level . ,org-trello--item-level)
                                                                                                                                                     (:keyword . "TODO"))))))
                                                       (orgtrello-hash-make-properties `(("some-checklist-id" . ("some-item-id" "some-other-item-id"))))))
                  0))))

(ert-deftest test-orgtrello-buffer-write-item ()
  (should (equal ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont ardumont-id
#+PROPERTY: orgtrello-user-dude dude-id
:END:

* TODO some card name
  :PROPERTIES:
  :orgtrello-id: some-card-id
  :orgtrello-users: ardumont,dude
  :orgtrello-card-comments: ardumont: some comment
  :orgtrello-local-checksum: card-checksum-135
  :END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\",\"orgtrello-local-checksum\":\"checklist-checksum-135\"}
    - [X] some item name :PROPERTIES: {\"orgtrello-id\":\"some-item-id\",\"orgtrello-local-checksum\":\"item-checksum-135\"}
"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content
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
                    (mock (orgtrello-buffer-item-checksum) => "item-checksum-135")
                    (mock (orgtrello-buffer-checklist-checksum) => "checklist-checksum-135")
                    (mock (orgtrello-buffer-card-checksum) => "card-checksum-135")
                    (orgtrello-buffer-write-item "some-item-id"
                                                  (orgtrello-hash-make-properties `(("some-item-id" . ,(orgtrello-hash-make-properties `((:id . "some-item-id")
                                                                                                                                         (:name . "some item name")
                                                                                                                                         (:level . ,org-trello--item-level)
                                                                                                                                         (:keyword . "DONE"))))))))
                  0))))

(ert-deftest test-orgtrello-buffer-write-entity-card ()
  (should (equal "
* DONE some card name                                                   :red:green:
"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content
                  "\n"
                  (orgtrello-buffer-write-entity "some-card-id" (orgtrello-hash-make-properties `((:keyword . "DONE")
                                                                                                   (:tags . ":red:green:")
                                                                                                   (:desc . "some description")
                                                                                                   (:level . ,org-trello--card-level)
                                                                                                   (:name . "some card name"))))
                  0))))

(ert-deftest test-orgtrello-buffer-write-entity-checklist ()
  (should (equal "* some content
  - [-] some checklist name
"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content
                  "* some content
"
                  (orgtrello-buffer-write-entity "some-checklist-id" (orgtrello-hash-make-properties `((:keyword . "DONE")
                                                                                                        (:level . ,org-trello--checklist-level)
                                                                                                        (:name . "some checklist name"))))
                  0))))

(ert-deftest test-orgtrello-buffer-write-entity-item ()
  (should (equal "* some content
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item name
"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content
                  "* some content
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
"
                  (orgtrello-buffer-write-entity "some-item-id" (orgtrello-hash-make-properties `((:keyword . "DONE")
                                                                                                   (:level . ,org-trello--item-level)
                                                                                                   (:name . "some item name"))))
                  0))))

(ert-deftest test-orgtrello-buffer--compute-checklist-to-org-entry ()
  (should (equal "  - [-] name\n" (orgtrello-buffer--compute-checklist-to-org-entry (orgtrello-hash-make-properties `((:name . "name")))))))

(ert-deftest test-orgtrello-buffer--compute-item-to-org-entry ()
  (should (equal "    - [X] name\n" (orgtrello-buffer--compute-item-to-org-entry (orgtrello-hash-make-properties `((:name . "name") (:keyword . "complete"))))))
  (should (equal "    - [ ] name\n" (orgtrello-buffer--compute-item-to-org-entry (orgtrello-hash-make-properties `((:name . "name") (:keyword . "incomplete")))))))

(ert-deftest test-orgtrello-buffer--compute-item-to-org-checkbox ()
  (should (equal "  - [X] name\n" (orgtrello-buffer--compute-item-to-org-checkbox "name" 2 "complete")))
  (should (equal "    - [X] name\n" (orgtrello-buffer--compute-item-to-org-checkbox "name" 3 "complete")))
  (should (equal "  - [X] name\n" (orgtrello-buffer--compute-item-to-org-checkbox "name" 2 "complete")))
  (should (equal "    - [ ] name\n" (orgtrello-buffer--compute-item-to-org-checkbox "name" 3 "incomplete"))))

(ert-deftest test-orgtrello-buffer--private-compute-card-to-org-entry ()
  (should (equal "* name TODO                                                             :some-tags:\nDEADLINE: <some-date>\n"
                 (orgtrello-buffer--private-compute-card-to-org-entry "TODO" "name" "some-date" ":some-tags:")))
  (should (equal "* name TODO\n"
                 (orgtrello-buffer--private-compute-card-to-org-entry "TODO" "name" nil nil)))
  (should (equal "* name TODO                                                             :tag,tag2:\n"
                 (orgtrello-buffer--private-compute-card-to-org-entry "TODO" "name" nil ":tag,tag2:"))))

(ert-deftest test-orgtrello-buffer--compute-due-date ()
  (should (equal "DEADLINE: <some-date>\n" (orgtrello-buffer--compute-due-date "some-date")))
  (should (equal "" (orgtrello-buffer--compute-due-date nil))))

(ert-deftest test-orgtrello-buffer--serialize-tags ()
  (should (equal "" (orgtrello-buffer--serialize-tags "* card name" nil)))
  (should (equal "" (orgtrello-buffer--serialize-tags "* card name" "")))
  (should (equal "                                                             :red:green:" (orgtrello-buffer--serialize-tags "* card name" ":red:green:")))
  (should (equal "                                                     :red:green:blue:" (orgtrello-buffer--serialize-tags "* another card name" ":red:green:blue:")))
  (should (equal " :red:green:blue:" (orgtrello-buffer--serialize-tags "* this is another card name with an extremely long label name, more than 72 chars" ":red:green:blue:"))))

(ert-deftest test-orgtrello-buffer--compute-marker-from-entry ()
  (should (equal "id"                                                        (orgtrello-buffer--compute-marker-from-entry (orgtrello-data-make-hash-org :users :level :kwd :name      "id"  :due :position :buffername :desc :tags :unknown))))
  (should (equal "orgtrello-marker-2a0b98e652ce6349a0659a7a8eeb3783ffe9a11a" (orgtrello-buffer--compute-marker-from-entry (orgtrello-data-make-hash-org :users :level :kwd "some-name" nil :due 1234      "buffername" :desc :tags :unknown))))
  (should (equal "orgtrello-marker-6c59c5dcf6c83edaeb3f4923bfd929a091504bb3" (orgtrello-buffer--compute-marker-from-entry (orgtrello-data-make-hash-org :users :level :kwd "some-name" nil :due 4321      "some-other-buffername" :desc :tags :unknown)))))

(ert-deftest test-orgtrello-buffer--compute-level-into-spaces ()
  (should (equal 2 (orgtrello-buffer--compute-level-into-spaces 2)))
  (should (equal 4 (orgtrello-buffer--compute-level-into-spaces nil)))
  (should (equal 4 (orgtrello-buffer--compute-level-into-spaces 'any))))

(ert-deftest test-orgtrello-buffer--compute-checklist-to-org-checkbox ()
  (should (equal "  - [X] name\n" (orgtrello-buffer--compute-checklist-to-org-checkbox "name" 2 "complete")))
  (should (equal "    - [X] name\n" (orgtrello-buffer--compute-checklist-to-org-checkbox "name" 3 "complete")))
  (should (equal "  - [X] name\n" (orgtrello-buffer--compute-checklist-to-org-checkbox "name" 2 "complete")))
  (should (equal "    - [-] name\n" (orgtrello-buffer--compute-checklist-to-org-checkbox "name" 3 "incomplete"))))

(ert-deftest test-orgtrello-buffer--compute-state-checkbox ()
  (should (equal "[X]" (orgtrello-buffer--compute-state-checkbox "complete")))
  (should (equal "[-]" (orgtrello-buffer--compute-state-checkbox "incomplete"))))

(ert-deftest test-orgtrello-buffer--dispatch-create-entities-map-with-adjacency ()
  (should (equal 'orgtrello-buffer--put-card-with-adjacency      (orgtrello-buffer--dispatch-create-entities-map-with-adjacency (orgtrello-data-make-hash-org :users org-trello--card-level nil nil nil nil nil nil nil :tags :unknown))))
  (should (equal 'orgtrello-backend--put-entities-with-adjacency (orgtrello-buffer--dispatch-create-entities-map-with-adjacency (orgtrello-data-make-hash-org :users org-trello--checklist-level nil nil nil nil nil nil nil :tags :unknown))))
  (should (equal 'orgtrello-backend--put-entities-with-adjacency (orgtrello-buffer--dispatch-create-entities-map-with-adjacency (orgtrello-data-make-hash-org :users org-trello--item-level nil nil nil nil nil nil nil :tags :unknown)))))

(ert-deftest test-orgtrello-buffer--to-orgtrello-metadata ()
  (should (equal "some name :orgtrello-id-identifier:"  (gethash :name       (orgtrello-buffer--to-orgtrello-metadata '(:unknown "" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil)))))
  (should (equal "IN PROGRESS"                          (gethash :keyword    (orgtrello-buffer--to-orgtrello-metadata '(:unknown "" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil)))))
  (should (equal 0                                      (gethash :level      (orgtrello-buffer--to-orgtrello-metadata '(:unknown "" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil)))))
  (should (equal :id                                    (gethash :id         (orgtrello-buffer--to-orgtrello-metadata '(:unknown "" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil)))))
  (should (equal :due                                   (gethash :due        (orgtrello-buffer--to-orgtrello-metadata '(:unknown "" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil)))))
  (should (equal :point                                 (gethash :position   (orgtrello-buffer--to-orgtrello-metadata '(:unknown "" "" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil)))))
  (should (equal "1,2,3"                                (gethash :member-ids (orgtrello-buffer--to-orgtrello-metadata '(:unknown "" "1,2,3" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil)))))
  (should (equal :desc                                  (gethash :desc       (orgtrello-buffer--to-orgtrello-metadata '(:unknown :desc "1,2,3" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil)))))
  (should (equal :unknown                               (gethash :unknown-properties (orgtrello-buffer--to-orgtrello-metadata '(:unknown :desc "1,2,3" "buffer-name.org" :point :id :due 0 1 "IN PROGRESS" nil "some name :orgtrello-id-identifier:" nil)))))
  (should (equal :default (let ((org-trello--org-keyword-trello-list-names '(:default :other-keywords-we-do-not-care)))
                            (gethash :keyword (orgtrello-buffer--to-orgtrello-metadata '(:unknown "" "" "buffer-name.org" :point :id :due 0 1 nil nil "some name :orgtrello-id-identifier:" nil)))))))

(ert-deftest test-orgtrello-buffer--convert-orgmode-date-to-trello-date ()
  (should (equal "2013-07-18T02:00:00.000Z" (orgtrello-buffer--convert-orgmode-date-to-trello-date "2013-07-18T02:00:00.000Z")))
  (should (equal "2013-07-29T14:00:00.000Z" (orgtrello-buffer--convert-orgmode-date-to-trello-date "2013-07-29 lun. 14:00")))
  (should (equal "2013-07-29T00:00:00.000Z" (orgtrello-buffer--convert-orgmode-date-to-trello-date "2013-07-29")))
  (should (equal nil                        (orgtrello-buffer--convert-orgmode-date-to-trello-date nil))))

(ert-deftest test-orgtrello-buffer-entry-get-full-metadata ()
  ;; on card
  (should (equal nil    (->> (orgtrello-tests-with-temp-buffer "* card" (orgtrello-buffer-entry-get-full-metadata))
                             (orgtrello-data-parent))))
  (should (equal nil    (->> (orgtrello-tests-with-temp-buffer "* card" (orgtrello-buffer-entry-get-full-metadata))
                             (orgtrello-data-grandparent))))
  (should (equal "card" (->> (orgtrello-tests-with-temp-buffer "* card" (orgtrello-buffer-entry-get-full-metadata))
                             (orgtrello-data-current)
                             orgtrello-data-entity-name)))
  ;; on checklist
  (should (equal "card"      (->> (orgtrello-tests-with-temp-buffer "* card\n  - [ ] checklist\n" (orgtrello-buffer-entry-get-full-metadata))
                                  (orgtrello-data-parent)
                                  orgtrello-data-entity-name)))
  (should (equal nil         (->> (orgtrello-tests-with-temp-buffer "* card\n  - [ ] checklist\n" (orgtrello-buffer-entry-get-full-metadata))
                                  (orgtrello-data-grandparent))))
  (should (equal "checklist" (->> (orgtrello-tests-with-temp-buffer "* card\n  - [ ] checklist\n" (orgtrello-buffer-entry-get-full-metadata))
                                  (orgtrello-data-current)
                                  orgtrello-data-entity-name)))
  ;; on item
  (should (equal "checklist" (->> (orgtrello-tests-with-temp-buffer "* card\n  - [ ] checklist\n    - [ ] item\n" (orgtrello-buffer-entry-get-full-metadata))
                                  (orgtrello-data-parent)
                                  orgtrello-data-entity-name)))
  (should (equal "card"      (->> (orgtrello-tests-with-temp-buffer "* card\n  - [ ] checklist\n    - [ ] item\n" (orgtrello-buffer-entry-get-full-metadata))
                                  (orgtrello-data-grandparent)
                                  orgtrello-data-entity-name)))
  (should (equal "item"      (->> (orgtrello-tests-with-temp-buffer "* card\n  - [ ] checklist\n    - [ ] item\n" (orgtrello-buffer-entry-get-full-metadata))
                                  (orgtrello-data-current)
                                  orgtrello-data-entity-name))))

(ert-deftest test-orgtrello-buffer-entity-metadata-card ()
  (let ((h-values (orgtrello-tests-with-temp-buffer ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont some-user-id
#+PROPERTY: orgtrello-user-dude some-user-id2
:END:

* TODO card title
:PROPERTIES:
:orgtrello-id: some-id
:orgtrello-users: ardumont,dude
:END:
  some description\n"
                                                    (progn (org-back-to-heading)
                                                           (orgtrello-buffer-entity-metadata)))))
    (should (equal 1                                                             (orgtrello-data-entity-level h-values)))
    (should (equal nil                                                           (orgtrello-data-entity-tags h-values)))
    (should (equal "card title"                                                  (orgtrello-data-entity-name h-values)))
    (should (equal "some-id"                                                     (orgtrello-data-entity-id h-values)))
    (should (equal nil                                                           (orgtrello-data-entity-due h-values)))
    (should (equal "some description"                                            (orgtrello-data-entity-description h-values)))
    (should (equal "some-user-id,some-user-id2"                                  (orgtrello-data-entity-member-ids h-values)))
    (should (equal "TODO"                                                        (orgtrello-data-entity-keyword h-values)))
    (should (equal nil                                                           (orgtrello-data-entity-unknown-properties h-values)))))

(ert-deftest test-orgtrello-buffer-entity-metadata-comment ()
  (let ((h-values (orgtrello-tests-with-temp-buffer ":PROPERTIES:
#+PROPERTY: orgtrello-user-ardumont some-user-id
#+PROPERTY: orgtrello-user-dude some-user-id2
:END:

* TODO card title
:PROPERTIES:
:orgtrello-id: some-id
:orgtrello-users: ardumont,dude
:END:
  some description\n
** COMMENT, user date
:PROPERTIES:
:orgtrello-id: some-comment-id
:END:
the comment is here, the other are trello's metadata
this comment can be multiline
and contains text
nothing enforces the content of the description
"
                                                    (progn (org-back-to-heading)
                                                           (orgtrello-buffer-entity-metadata)))))
    (should (equal "some-comment-id"                                             (orgtrello-data-entity-id h-values)))
    (should (equal "the comment is here, the other are trello's metadata
this comment can be multiline
and contains text
nothing enforces the content of the description
"                                 (orgtrello-data-entity-description h-values)))))

(ert-deftest test-orgtrello-buffer-compute-marker ()
  (should (equal "orgtrello-marker-2a0b98e652ce6349a0659a7a8eeb3783ffe9a11a" (orgtrello-buffer-compute-marker "buffername" "some-name" 1234)))
  (should (equal "orgtrello-marker-6c59c5dcf6c83edaeb3f4923bfd929a091504bb3" (orgtrello-buffer-compute-marker "some-other-buffername" "some-name" 4321))))

(ert-deftest test-orgtrello-buffer--filter-out-known-properties ()
  (should (equal '(("some-unknown-thingy" . "some value"))
                 (orgtrello-buffer--filter-out-known-properties '(("orgtrello-id" . "orgtrello-marker-08677ec948991d1e5a25ab6b813d8eba03fac20f")
                                                                   ("orgtrello-users" . "some user")
                                                                   ("some-unknown-thingy" . "some value")
                                                                   ("CATEGORY" . "TESTS-simple"))))))

(ert-deftest test-orgtrello-buffer-org-unknown-drawer-properties ()
  (should (equal
           '(("some-unknown-thingy" . "some value"))
           (orgtrello-tests-with-temp-buffer
            "* TODO Joy of FUN(ctional) LANGUAGES
DEADLINE: <2014-05-17 Sat>
:PROPERTIES:
:orgtrello-id: orgtrello-marker-08677ec948991d1e5a25ab6b813d8eba03fac20f
:some-unknown-thingy: some value
:orgtrello-users: ardumont
:orgtrello-unknown-key-prefixed-by-orgtrello: some unknown value that will be filtered
:END:
"
            (orgtrello-buffer-org-unknown-drawer-properties)))))

(ert-deftest test-orgtrello-buffer-update-properties-unknown ()
  (should (equal
           "* TODO Joy of FUN(ctional) LANGUAGES
DEADLINE: <2014-05-17 Sat>
:PROPERTIES:
:orgtrello-id: orgtrello-marker-08677ec948991d1e5a25ab6b813d8eba03fac20f
:property0: value0
:property1: value1
:property2: value2
:END:
"
           (orgtrello-tests-with-temp-buffer-and-return-buffer-content
            "* TODO Joy of FUN(ctional) LANGUAGES
DEADLINE: <2014-05-17 Sat>
:PROPERTIES:
:orgtrello-id: orgtrello-marker-08677ec948991d1e5a25ab6b813d8eba03fac20f
:END:
"
            (orgtrello-buffer-update-properties-unknown '(("property0" . "value0")
                                                           ("property1" . "value1")
                                                           ("property2" . "value2")))))))

(ert-deftest test-orgtrello-buffer-overwrite-card-with-no-previous-buffer-content ()
  "No buffer"
  (should (equal "* TODO some card name                                                   :red:green:
  :PROPERTIES:
  :orgtrello-users: ardumont,dude
  :orgtrello-local-checksum: local-card-checksum-567
  :orgtrello-id: some-card-id
  :END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\",\"orgtrello-local-checksum\":\"local-checkbox-checksum-567\"}
    - [X] some item name :PROPERTIES: {\"orgtrello-id\":\"some-item-id\",\"orgtrello-local-checksum\":\"local-item-checksum-567\"}
    - [ ] some other item name :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\",\"orgtrello-local-checksum\":\"local-item-checksum-567\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\",\"orgtrello-local-checksum\":\"local-checkbox-checksum-567\"}

** COMMENT ardumont, some-date
:PROPERTIES:
:orgtrello-id: some-comment-id
:orgtrello-local-checksum: local-comment-checksum-567
:END:
  some comment

"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content
                  "" ;; no previous content on buffer
                  (with-mock
                    (mock (orgtrello-buffer-card-checksum) => "local-card-checksum-567")
                    (mock (orgtrello-buffer-checklist-checksum) => "local-checkbox-checksum-567")
                    (mock (orgtrello-buffer-item-checksum) => "local-item-checksum-567")
                    (mock (orgtrello-buffer-comment-checksum) => "local-comment-checksum-567")
                    (let* ((card (orgtrello-hash-make-properties `((:keyword . "TODO")
                                                                   (:member-ids . "ardumont,dude")
                                                                   (:comments . ,(list (orgtrello-hash-make-properties '((:comment-user . "ardumont")
                                                                                                                         (:comment-date . "some-date")
                                                                                                                         (:comment-id   . "some-comment-id")
                                                                                                                         (:comment-text . "some comment")))))
                                                                   (:tags . ":red:green:")
                                                                   (:desc . "some description")
                                                                   (:level . ,org-trello--card-level)
                                                                   (:name . "some card name")
                                                                   (:id . "some-card-id"))))
                           (entities (orgtrello-hash-make-properties `(("some-checklist-id" . ,(orgtrello-hash-make-properties `((:id . "some-checklist-id")
                                                                                                                                 (:name . "some checklist name")
                                                                                                                                 (:level . ,org-trello--checklist-level))))
                                                                       ("some-other-checklist-id" . ,(orgtrello-hash-make-properties `((:id . "some-other-checklist-id")
                                                                                                                                       (:name . "some other checklist name")
                                                                                                                                       (:level . ,org-trello--checklist-level))))
                                                                       ("some-item-id"  . ,(orgtrello-hash-make-properties `((:id . "some-item-id")
                                                                                                                             (:name . "some item name")
                                                                                                                             (:level . ,org-trello--item-level)
                                                                                                                             (:keyword . "DONE"))))
                                                                       ("some-other-item-id"  . ,(orgtrello-hash-make-properties `((:id . "some-other-item-id")
                                                                                                                                   (:name . "some other item name")
                                                                                                                                   (:level . ,org-trello--item-level)
                                                                                                                                   (:keyword . "TODO")))))))
                           (entities-adj (orgtrello-hash-make-properties `(("some-other-checklist-id" . ())
                                                                           ("some-checklist-id" . ("some-item-id" "some-other-item-id"))
                                                                           ("some-card-id" . ("some-checklist-id" "some-other-checklist-id"))))))
                      (orgtrello-buffer-overwrite-card '(1 2) card entities entities-adj)))))))

(ert-deftest test-orgtrello-buffer-overwrite-card-buffer-with-previous-content ()
  "Multiple cards present at point. Overwrite given previous region card with updated data."
  (should (equal "* TODO some card name                                                   :red:green:
  :PROPERTIES:
  :orgtrello-users: ardumont,dude
  :orgtrello-local-checksum: local-card-checksum-567
  :orgtrello-id: some-card-id
  :END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\",\"orgtrello-local-checksum\":\"local-checklist-checksum-567\"}
    - [X] some item name :PROPERTIES: {\"orgtrello-id\":\"some-item-id\",\"orgtrello-local-checksum\":\"local-item-checksum-567\"}
    - [ ] some other item name :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\",\"orgtrello-local-checksum\":\"local-item-checksum-567\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\",\"orgtrello-local-checksum\":\"local-checklist-checksum-567\"}

** COMMENT ardumont, some-date
:PROPERTIES:
:orgtrello-id: some-comment-id
:orgtrello-local-checksum: local-comment-checksum-567
:END:
  some comment


* IN-PROGRESS another card
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content
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
                    (mock (orgtrello-buffer-card-checksum) => "local-card-checksum-567")
                    (mock (orgtrello-buffer-checklist-checksum) => "local-checklist-checksum-567")
                    (mock (orgtrello-buffer-item-checksum) => "local-item-checksum-567")
                    (mock (orgtrello-buffer-comment-checksum) => "local-comment-checksum-567")
                    (let* ((card (orgtrello-hash-make-properties `((:keyword . "TODO")
                                                                   (:member-ids . "ardumont,dude")
                                                                   (:comments . ,(list (orgtrello-hash-make-properties '((:comment-user . "ardumont")
                                                                                                                         (:comment-date . "some-date")
                                                                                                                         (:comment-id   . "some-comment-id")
                                                                                                                         (:comment-text . "some comment")))))
                                                                   (:tags . ":red:green:")
                                                                   (:desc . "some description")
                                                                   (:level . ,org-trello--card-level)
                                                                   (:name . "some card name")
                                                                   (:id . "some-card-id"))))
                           (entities (orgtrello-hash-make-properties `(("some-checklist-id" . ,(orgtrello-hash-make-properties `((:id . "some-checklist-id")
                                                                                                                                 (:name . "some checklist name")
                                                                                                                                 (:level . ,org-trello--checklist-level))))
                                                                       ("some-other-checklist-id" . ,(orgtrello-hash-make-properties `((:id . "some-other-checklist-id")
                                                                                                                                       (:name . "some other checklist name")
                                                                                                                                       (:level . ,org-trello--checklist-level))))
                                                                       ("some-item-id"  . ,(orgtrello-hash-make-properties `((:id . "some-item-id")
                                                                                                                             (:name . "some item name")
                                                                                                                             (:level . ,org-trello--item-level)
                                                                                                                             (:keyword . "DONE"))))
                                                                       ("some-other-item-id"  . ,(orgtrello-hash-make-properties `((:id . "some-other-item-id")
                                                                                                                                   (:name . "some other item name")
                                                                                                                                   (:level . ,org-trello--item-level)
                                                                                                                                   (:keyword . "TODO")))))))
                           (entities-adj (orgtrello-hash-make-properties `(("some-other-checklist-id" . ())
                                                                           ("some-checklist-id" . ("some-item-id" "some-other-item-id"))
                                                                           ("some-card-id" . ("some-checklist-id" "some-other-checklist-id"))))))
                      (orgtrello-buffer-overwrite-card '(1 469) card entities entities-adj)))
                  -5))))

(ert-deftest test-orgtrello-buffer-get-card-local-checksum ()
  "Retrieve the card's checksum."
  (should (equal
           "123"
           (orgtrello-tests-with-temp-buffer "* card
:PROPERTIES:
:orgtrello-local-checksum: 123
:END:"
                                             (orgtrello-buffer-get-card-local-checksum))))
  (should (equal
           nil
           (orgtrello-tests-with-temp-buffer "* card"
                                             (orgtrello-buffer-get-card-local-checksum)))))

(ert-deftest test-orgtrello-buffer-write-local-checksum-at-pt-for-checklist ()
  "Write local checksum at the current position."
  (should (equal
           "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: card-checksum-098
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\",\"orgtrello-local-checksum\":\"checklist-checksum-098\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* another card"

           (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
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
                                                                         (mock (orgtrello-buffer-checklist-checksum) => "checklist-checksum-098")
                                                                         (mock (orgtrello-buffer-card-checksum) => "card-checksum-098")
                                                                         (orgtrello-buffer-write-local-checksum-at-pt))
                                                                       -5))))

(ert-deftest test-orgtrello-buffer-write-local-checksum-at-pt-for-item ()
  "checklist - Write local checksum at the current position."
  (should (equal
           "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: card-checksum-098876
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\",\"orgtrello-local-checksum\":\"checklist-checksum-098876\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\",\"orgtrello-local-checksum\":\"item-checksum-098876\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* another card"

           (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
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
                                                                         (mock (orgtrello-buffer-item-checksum) => "item-checksum-098876")
                                                                         (mock (orgtrello-buffer-checklist-checksum) => "checklist-checksum-098876")
                                                                         (mock (orgtrello-buffer-card-checksum) => "card-checksum-098876")
                                                                         (orgtrello-buffer-write-local-checksum-at-pt))
                                                                       -4))))

(ert-deftest test-orgtrello-buffer-write-local-checksum-at-pt-for-card ()
  "Write local card checksum at the current position."
  (should (equal
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
           (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
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
                                                                         (mock (orgtrello-buffer-card-checksum) => "card-checksum-987")
                                                                         (orgtrello-buffer-write-local-checksum-at-pt))))))

(ert-deftest test-orgtrello-buffer-write-local-checksum-at-pt-for-comment ()
  "checklist - Write local checksum at the current position for card's comment."
  (should (equal
           "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: card-checksum-123
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
** COMMENT ardumont, date
   :PROPERTIES:
   :orgtrello-local-checksum: comment-checksum-123
   :END:
  some comment
* another card"

           (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
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
** COMMENT ardumont, date
  some comment
* another card"
                                                                       (with-mock
                                                                         (mock (orgtrello-buffer-comment-checksum) => "comment-checksum-123")
                                                                         (mock (orgtrello-buffer-card-checksum) => "card-checksum-123")
                                                                         (orgtrello-buffer-write-local-checksum-at-pt))
                                                                       -2))))

(ert-deftest test-orgtrello-buffer-org-delete-property-checklist ()
  ;; On a checklist
  (should (equal "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {}
"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                                                                             (orgtrello-buffer-org-delete-property "orgtrello-id"))))
  ;; on checklist without property
  (should (equal "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {}
"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {}
"
                                                                             (orgtrello-buffer-org-delete-property "orgtrello-id")))))

(ert-deftest test-orgtrello-buffer-org-delete-property-item ()
  ;; On an item with property to delete
  (should (equal "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                                                                             (orgtrello-buffer-org-delete-property "orgtrello-id")
                                                                             -2)))

  ;; On an item with no previous property to delete
  (should (equal "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                                                                             (orgtrello-buffer-org-delete-property "orgtrello-id")
                                                                             -2))))

(ert-deftest test-orgtrello-buffer-org-delete-property-card ()
  ;; On a card.
  (should (equal "* TODO some card name
:PROPERTIES:
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                                                                             (orgtrello-buffer-org-delete-property "orgtrello-id")
                                                                             -5)))

  ;; On a card - no previous property.
  (should (equal "* TODO some card name
:PROPERTIES:
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}
"
                                                                             (orgtrello-buffer-org-delete-property "orgtrello-id")
                                                                             -5))))

(ert-deftest test-orgtrello-buffer-write-local-checksum-at-pt-checklist ()
  ;; on a checklist without checksum yet
  (should (equal
           "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: card-checksum-876
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\",\"orgtrello-local-checksum\":\"checklist-checksum-876\"}
"
           (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
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
                                                                         (mock (orgtrello-buffer-checklist-checksum) => "checklist-checksum-876")
                                                                         (mock (orgtrello-buffer-card-checksum) => "card-checksum-876")
                                                                         (orgtrello-buffer-write-local-checksum-at-pt))
                                                                       -1)))
  ;; already with a checksum
  (should (equal
           "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: card-checksum-543
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\",\"orgtrello-local-checksum\":\"checklist-checksum-543\"}
"
           (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
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
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\",\"orgtrello-local-checksum\":\"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855\"}
"
                                                                       (with-mock
                                                                         (mock (orgtrello-buffer-checklist-checksum) => "checklist-checksum-543")
                                                                         (mock (orgtrello-buffer-card-checksum) => "card-checksum-543")
                                                                         (orgtrello-buffer-write-local-checksum-at-pt))
                                                                       -1))))

(ert-deftest test-orgtrello-buffer-write-local-checksum-at-pt-card ()
  ;; card + current checklist checksum updated
  (should (equal
           "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: card-checksum-432
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist names :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\",\"orgtrello-local-checksum\":\"checklist-checksum-432\"}
"
           (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
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
                                                                         (mock (orgtrello-buffer-checklist-checksum) => "checklist-checksum-432")
                                                                         (mock (orgtrello-buffer-card-checksum) => "card-checksum-432")
                                                                         (orgtrello-buffer-write-local-checksum-at-pt))
                                                                       -1))))

(ert-deftest test-orgtrello-buffer-write-local-item-checksum-at-point ()
  "The local checksum changes if modifications."
  (should (equal
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
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\",\"orgtrello-local-checksum\":\"item-checksum-43210\"}
"
           (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
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
"
                                                                       (with-mock
                                                                         (mock (orgtrello-buffer-item-checksum) => "item-checksum-43210")
                                                                         (orgtrello-buffer-write-local-item-checksum-at-point))
                                                                       -1))))

(ert-deftest test-orgtrello-buffer-write-local-comment-checksum-at-point ()
  "The local checksum changes if modifications."
  (should (equal
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
** COMMENT user, date
:PROPERTIES:
:orgtrello-local-checksum: comment-checksum-10324
:END:
  some comment
"
           (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
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
** COMMENT user, date
:PROPERTIES:
:END:
  some comment
"
                                                                       (with-mock
                                                                         (mock (orgtrello-buffer-comment-checksum) => "comment-checksum-10324")
                                                                         (orgtrello-buffer-write-local-comment-checksum-at-point))
                                                                       -1))))

(ert-deftest test-orgtrello-buffer-get-checkbox-local-checksum-checklist ()
  "The local checksum does not change if no modification."
  (should (equal
           nil
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (orgtrello-buffer-get-checkbox-local-checksum)
                                             -1)))
  (should (equal
           "bar"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (orgtrello-buffer-get-checkbox-local-checksum)
                                             -2))))

(ert-deftest test-orgtrello-buffer-get-checkbox-local-checksum-item ()
  "Retrieve the local checksum from item."
  (should (equal
           "foo"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (orgtrello-buffer-get-checkbox-local-checksum)
                                             -1))))

(ert-deftest test-orgtrello-buffer-get-checkbox-local-checksum-card ()
  "Works also on card but it is not intended to!"
  (should (equal
           "foobar"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (orgtrello-buffer-get-checkbox-local-checksum)
                                             -3))))

(ert-deftest test-orgtrello-buffer-compute-checksum-item ()
  "Compute the checksum of the item."
  (should (equal
           "item-checksum"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                               (mock (orgtrello-buffer-item-checksum) => "item-checksum")
                                               (orgtrello-buffer-compute-checksum))
                                             -1))))

(ert-deftest test-orgtrello-buffer-compute-checksum-comment ()
  "Compute the checksum of the item."
  (should (equal
           "comment-checksum"
           (orgtrello-tests-with-temp-buffer "* card
** COMMENT ardumont, date
:PROPERTIES:
:orgtrello-id: some-comment-id
:orgtrello-local-checksum: foobar
:END:
  some comment
"
                                             (with-mock
                                               (mock (orgtrello-buffer-comment-checksum) => "comment-checksum")
                                               (orgtrello-buffer-compute-checksum))))))

(ert-deftest test-orgtrello-buffer-compute-checksum-checklist ()
  "Compute the checksum of the checklist."
  (should (equal
           "checklist-checksum"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                               (mock (orgtrello-buffer-checklist-checksum) => "checklist-checksum")
                                               (orgtrello-buffer-compute-checksum))
                                             -1))))


(ert-deftest test-orgtrello-buffer-compute-checksum-card ()
  "Compute the checksum of the card."
  (should (equal
           "card-checksum"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                               (mock (orgtrello-buffer-card-checksum) => "card-checksum")
                                               (orgtrello-buffer-compute-checksum))
                                             -10))))

(ert-deftest test-orgtrello-buffer-get-local-checksum-checklist ()
  "The local checksum does not change if no modification."
  (should (equal
           nil
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (orgtrello-buffer-get-local-checksum)
                                             -1)))
  (should (equal
           "bar"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (orgtrello-buffer-get-local-checksum)
                                             -2))))

(ert-deftest test-orgtrello-buffer-get-local-checksum-item ()
  "Retrieve the local checksum from item."
  (should (equal
           "foo"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (orgtrello-buffer-get-local-checksum)
                                             -1))))

(ert-deftest test-orgtrello-buffer-get-local-checksum-card ()
  "Works also on card but it is not intended to!"
  (should (equal
           "foobar"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (orgtrello-buffer-get-local-checksum)
                                             -3))))

(ert-deftest test-orgtrello-buffer-write-properties-at-pt-card ()
  "Update card property's id + card checksum computation and update."
  (should (equal
           "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: card-checksum-321
:END:
"
           (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: foobar
:END:
"
                                                                       (with-mock
                                                                         (mock (orgtrello-buffer-card-checksum) => "card-checksum-321")
                                                                         (orgtrello-buffer-write-properties-at-pt "some-id"))))))

(ert-deftest test-orgtrello-buffer-write-properties-at-pt-checklist ()
  "Update checkbox property's id + compute checksum at point and set it."
  (should (equal
           "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: card-checksum-321
:END:
  - [ ] new checkbox :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\",\"orgtrello-local-checksum\":\"checklist-checksum-321\"}
"
           (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: foobar
:END:
  - [ ] new checkbox
"
                                                                       (with-mock
                                                                         (mock (orgtrello-buffer-checklist-checksum) => "checklist-checksum-321")
                                                                         (mock (orgtrello-buffer-card-checksum) => "card-checksum-321")
                                                                         (orgtrello-buffer-write-properties-at-pt "some-checklist-id"))
                                                                       -1))))

(ert-deftest test-orgtrello-buffer-write-properties-at-pt-item ()
  "Update checkbox property's id + compute checksum at point and set it."
  (should (equal
           "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: card-checksum-321
:END:
  - [ ] new checkbox :PROPERTIES: {\"orgtrello-local-checksum\":\"checklist-checksum-321\"}
    - [ ] new item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\",\"orgtrello-local-checksum\":\"item-checksum-321\"}
"
           (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
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
                                                                         (mock (orgtrello-buffer-item-checksum) => "item-checksum-321")
                                                                         (mock (orgtrello-buffer-checklist-checksum) => "checklist-checksum-321")
                                                                         (mock (orgtrello-buffer-card-checksum) => "card-checksum-321")
                                                                         (orgtrello-buffer-write-properties-at-pt "some-item-id"))
                                                                       -1))))

(ert-deftest test-orgtrello-buffer-checksum ()
  (should (string= "cabc552bfc3fb1fe64933c9b6a5eb41c8f81cd969e0c8add55870c0afb87c63c"
                   (orgtrello-buffer-checksum "some simple text")))
  (should (string= "b6132893c37102ca9d2f2323e057aa9e0573bdd9fb1a48141a469b8407552ac7"
                   (orgtrello-buffer-checksum "some simple text\nwith-multiple\nlines"))))

(ert-deftest test-orgtrello-buffer-delete-property-from-entry ()
  ;; remove only the property
  (should (string= "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\",\"orgtrello-local-checksum\":\"aabbcc\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\",\"orgtrello-local-checksum\":\"ddeeff\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\",\"orgtrello-local-checksum\":\"gghhii\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\",\"orgtrello-local-checksum\":\"jjkkll\"}
* another card"
                   (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: some-local-checksum
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\",\"orgtrello-local-checksum\":\"aabbcc\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\",\"orgtrello-local-checksum\":\"ddeeff\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\",\"orgtrello-local-checksum\":\"gghhii\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\",\"orgtrello-local-checksum\":\"jjkkll\"}
* another card"
                                                                               (orgtrello-buffer-delete-property-from-entry "orgtrello-local-checksum")))))

(ert-deftest test-orgtrello-buffer-delete-property ()
  (should (string= "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:END:
  some description
  - [-] some checklist name
    - [X] some item
    - [ ] some other item
  - [-] some other checklist name
* another card"
                   (orgtrello-tests-with-temp-buffer-and-return-buffer-content "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: some-local-checksum
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\",\"orgtrello-local-checksum\":\"aabbcc\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\",\"orgtrello-local-checksum\":\"ddeeff\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\",\"orgtrello-local-checksum\":\"gghhii\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\",\"orgtrello-local-checksum\":\"jjkkll\"}
* another card"
                                                                               (orgtrello-buffer-delete-property "orgtrello-local-checksum")
                                                                               -1))))

(ert-deftest test-orgtrello-buffer--compute-string-to-checksum ()
  "Compute the region to checksum on an entity."
  (should (equal
           "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:END:
  some description
  - [-] some checklist name
    - [X] some item
    - [ ] some other item
  - [-] some other checklist name


1"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (let ((orgtrello-setup-use-position-in-checksum-computation 'please-do-use-position-in-checksumt))
                                               (orgtrello-buffer--compute-string-to-checksum (orgtrello-entity-card-region)))
                                             -5)))

  (should (equal
           "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:END:
  some description
  - [-] some checklist name
    - [X] some item
    - [ ] some other item
  - [-] some other checklist name

"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (let ((orgtrello-setup-use-position-in-checksum-computation nil))
                                               (orgtrello-buffer--compute-string-to-checksum (orgtrello-entity-card-region)))
                                             -5)))

  ;; checklist
  (should (equal
           "  - [-] some other checklist name
482"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (orgtrello-buffer--compute-string-to-checksum (orgtrello-entity-compute-checklist-region))
                                             -1)))
  (should (equal
           "    - [X] some other item
405"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (orgtrello-buffer--compute-string-to-checksum (orgtrello-entity-compute-item-region))
                                             -2))))

(ert-deftest test-orgtrello-buffer-card-checksum ()
  "Compute the checksum of a card."
  (should (equal
           "2a71e11a34c8778629d2e1c36f9efdec3e81a0013bd56c649e88e4d91fd91d3a"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (orgtrello-buffer-card-checksum)
                                             -5))))

(ert-deftest test-orgtrello-buffer-card-checksum-no-change-gives-same-checksum ()
  "A card with a checksum should give the same checksum if nothing has changed."
  (should (equal
           "2a71e11a34c8778629d2e1c36f9efdec3e81a0013bd56c649e88e4d91fd91d3a"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: 2a71e11a34c8778629d2e1c36f9efdec3e81a0013bd56c649e88e4d91fd91d3a
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* another card"
                                             (orgtrello-buffer-card-checksum)
                                             -5))))

(ert-deftest test-orgtrello-buffer-card-checksum-modified-then-new-checksum ()
  "A modified card with a checksum should give another checksum."
  (should (equal
           "c3875e3e92a0aa7df37b97e58b2c30ade2a84235e7c8303a29e22bcde93d3847"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: 2a71e11a34c8778629d2e1c36f9efdec3e81a0013bd56c649e88e4d91fd91d3a
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [X] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* another card"
                                             (orgtrello-buffer-card-checksum)
                                             -5))))

(ert-deftest test-orgtrello-buffer-checklist-checksum ()
  "A checklist gives a checksum when asked politely."
  (should (equal
           "a9a2d45c6d406ef5bd9f8654f663ed8df222b030893d8a00cfdc37a6b3431378"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (orgtrello-buffer-checklist-checksum)
                                             -1))))

(ert-deftest test-orgtrello-buffer-checklist-checksum-not-modified-gives-same-checksum ()
  "A checklist gives a checksum when asked politely - does not take `'orgtrello-local-checksum`' property into account."
  (should (equal
           "a9a2d45c6d406ef5bd9f8654f663ed8df222b030893d8a00cfdc37a6b3431378"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\", \"orgtrello-local-checksum\":\"a9a2d45c6d406ef5bd9f8654f663ed8df222b030893d8a00cfdc37a6b3431378\"}
"
                                             (orgtrello-buffer-checklist-checksum)
                                             -1))))

(ert-deftest test-orgtrello-buffer-checklist-checksum-updates-so-new-checksum ()
  "A checklist checksum takes into account its items. If items change then the checkbox's checksum is updated."
  (should (equal
           "4abd37301df93d3de1c4cd66a6cd0fe0e2a2115968511d554dd422477ab085f4"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: dcae7ad96da00965d3f63eb9104fa676d9ee0d2cedc69b1fd865d0f8c2a0b3f5
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
"
                                             (orgtrello-buffer-checklist-checksum)
                                             -2)))
  (should (equal
           "38797826a0d514e63129ad8d7563d59a5f8c6008c9335cc9213eee0124dd254d"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (orgtrello-buffer-checklist-checksum)
                                             -2))))

(ert-deftest test-orgtrello-buffer-item-checksum ()
  "An item's checksum"
  (should (equal
           "66c919bd5d8d258feed09d6af2ada061e6abb0aed934534ad77b2f858d156bb0"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (orgtrello-buffer-item-checksum)
                                             -1))))

(ert-deftest test-orgtrello-buffer-item-checksum-not-modified-so-same-checksum ()
  "An item's checksum does not change even if there is already a checksum computed."
  (should (equal
           "66c919bd5d8d258feed09d6af2ada061e6abb0aed934534ad77b2f858d156bb0"
           (orgtrello-tests-with-temp-buffer "* TODO some card name
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-users: ardumont,dude
:orgtrello-card-comments: ardumont: some comment
:orgtrello-local-checksum: e7dc2fd6842e823786868235b6b33cb4cad4b75f73fed32bdb3df1dc54ef0418
:END:
  some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\", \"orgtrello-local-checksum\":\"66c919bd5d8d258feed09d6af2ada061e6abb0aed934534ad77b2f858d156bb0\"}
"
                                             (orgtrello-buffer-item-checksum)
                                             -1))))

(ert-deftest test-orgtrello-buffer-comment-checksum ()
  "A comment's checksum"
  (should (equal
           "e4d969d8287880e52f02d216131a70cffd91cee0eba5c6ed866fdfcf0a8afc95"
           (orgtrello-tests-with-temp-buffer "** COMMENT ardumont,date
:PROPERTIES:
:orgtrello-id: some-comment-id
:END:
  some comment
"
                                             (orgtrello-buffer-comment-checksum)
                                             -1))))

(ert-deftest test-orgtrello-buffer-comment-checksum-not-modified-so-same-checksum ()
  "A comment's checksum"
  (should (equal
           "e4d969d8287880e52f02d216131a70cffd91cee0eba5c6ed866fdfcf0a8afc95"
           (orgtrello-tests-with-temp-buffer "** COMMENT ardumont,date
:PROPERTIES:
:orgtrello-id: some-comment-id
:orgtrello-local-checksum: e4d969d8287880e52f02d216131a70cffd91cee0eba5c6ed866fdfcf0a8afc95
:END:
  some comment
"
                                             (orgtrello-buffer-comment-checksum)
                                             -1))))

(ert-deftest test-orgtrello-buffer-comment-checksum-modified-checksum-is-different ()
  "A comment's checksum"
  (should (equal
           "41d89d3b85c121ee47d2e1e8ee7c070767c542e17ad9899ecd7c995543c7366c"
           (orgtrello-tests-with-temp-buffer "** COMMENT ardumont,date
:PROPERTIES:
:orgtrello-id: some-comment-id
:orgtrello-local-checksum: e4d969d8287880e52f02d216131a70cffd91cee0eba5c6ed866fdfcf0a8afc95
:END:
  some slightly modified comment
generates another checksum
"
                                             (orgtrello-buffer-comment-checksum)))))

(ert-deftest test-orgtrello-buffer-checklist-beginning-pt ()
  "Determine the beginning of the checklist."
  (should (equal
           262
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (orgtrello-buffer-checklist-beginning-pt)
                                             -1)))

  (should (equal
           262
           (orgtrello-tests-with-temp-buffer "* TODO some card name
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
                                             (orgtrello-buffer-checklist-beginning-pt)
                                             -2))))

(ert-deftest test-orgtrello-buffer-filtered-kwds ()
  (should (equal
           '("TODO" "IN-PROGRESS" "DONE" "PENDING" "DELEGATED" "FAILED" "CANCELLED")
           (orgtrello-tests-with-temp-buffer
            ":PROPERTIES:
#+PROPERTY: board-name api test board
#+PROPERTY: board-id abc
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
            (orgtrello-buffer-filtered-kwds)))))

(ert-deftest test-orgtrello-buffer-org-file-properties ()
  (should (equal
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
           (orgtrello-tests-with-temp-buffer
            ":PROPERTIES:
#+PROPERTY: board-name api test board
#+PROPERTY: board-id abc
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
            (orgtrello-buffer-org-file-properties)))))

(ert-deftest test-orgtrello-buffer--serialize-comment ()
  (should (equal "\n** COMMENT tony, 10/10/2013\n:PROPERTIES:\n:orgtrello-id: comment-id\n:END:\n  hello, this is a comment!\n"
                 (orgtrello-buffer--serialize-comment (orgtrello-hash-make-properties '((:comment-user . "tony")
                                                                                         (:comment-date . "10/10/2013")
                                                                                         (:comment-id   . "comment-id")
                                                                                         (:comment-text . "hello, this is a comment!")))))))

(ert-deftest test-orgtrello-buffer-trim-input-comment ()
  (should (string= "text as is"
                   (orgtrello-buffer-trim-input-comment "# hello, some comment\n# ignore this also\ntext as is")))
  (should (string= "text as is\nwith lines"
                   (orgtrello-buffer-trim-input-comment "# hello, some comment\n# ignore this also\ntext as is\nwith lines\n")))
  (should (string= "line 1     \nline 2"
                   (orgtrello-buffer-trim-input-comment "# comment line\n# another comment line\nline 1     \nline 2\n\n\n"))))

(ert-deftest test-orgtrello-buffer--prepare-comment ()
  (should (string= "  a\n  b\n  c"
                   (orgtrello-buffer--prepare-comment "a\nb\nc")))
  (should (string= "  \n  a\n  b\n  c\n  "
                   (orgtrello-buffer--prepare-comment "\na\nb\nc\n"))))

(provide 'org-trello-buffer-tests)
;;; org-trello-buffer-tests.el ends here
