(require 'org-trello-entity)
(require 'ert)
(require 'el-mock)

(ert-deftest test-orgtrello-entity-level ()
  ;; ok
  (should (equal 1  (orgtrello-tests/with-temp-buffer "* some card" (orgtrello-entity-level))))
  (should (equal 2  (orgtrello-tests/with-temp-buffer "  - [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-entity-level))))
  (should (equal 3  (orgtrello-tests/with-temp-buffer "    - [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-entity-level))))
  ;; ko
  (should (equal -1 (orgtrello-tests/with-temp-buffer "* card\n - [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-entity-level) 0)))
  (should (equal -1 (orgtrello-tests/with-temp-buffer "* card\n     - [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-entity-level) 0))))

(ert-deftest test-orgtrello-entity-card-at-pt ()
  (should (equal t
                 (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
   DEADLINE: <2014-04-01T00:00:00.000Z>
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
hello there
"
                                                   (orgtrello-entity-card-at-pt))))

  (should (equal nil
                 (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}"
                                                   (orgtrello-entity-card-at-pt)
                                                   0)))
  (should (equal nil
                 (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}"
                                                   (orgtrello-entity-card-at-pt)
                                                   0))))

(ert-deftest test-orgtrello-entity-checklist-at-pt ()
  (should (equal nil
                 (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
   DEADLINE: <2014-04-01T00:00:00.000Z>
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
hello there
"
                                                   (orgtrello-entity-checklist-at-pt))))

  (should (equal t
                 (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
  - [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}"
                                                   (orgtrello-entity-checklist-at-pt)
                                                   0)))
  (should (equal nil
                 (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
  - [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
    - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}"
                                                   (orgtrello-entity-checklist-at-pt)
                                                   0))))

(ert-deftest test-orgtrello-entity-item-at-pt ()
  (should (equal nil
                 (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
   DEADLINE: <2014-04-01T00:00:00.000Z>
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
hello there
"
                                                   (orgtrello-entity-item-at-pt))))

  (should (equal nil
                 (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
  - [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}"
                                                   (orgtrello-entity-item-at-pt)
                                                   0)))
  (should (equal t
                 (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
  - [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
    - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}"
                                                   (orgtrello-entity-item-at-pt)
                                                   0))))

(ert-deftest test-orgtrello-entity-compute-checklist-header-region ()
  (should (equal '(8 24)
                 (orgtrello-tests/with-temp-buffer
                  "* card
- [ ] checklist
- [ ] another"
                  (orgtrello-entity-compute-checklist-header-region)))))

(ert-deftest test-orgtrello-entity-compute-checklist-region ()
  (should (equal '(8 25)
                 (orgtrello-tests/with-temp-buffer
                  "* card
  - [ ] checklist
  - [ ] another checklist"
                  (orgtrello-entity-compute-checklist-region))))
  (should (equal "  - [ ] checklist"
                 (orgtrello-tests/with-temp-buffer
                  "* card
  - [ ] checklist
  - [ ] another checklist"
                  (apply 'buffer-substring-no-properties (orgtrello-entity-compute-checklist-region)))))
  (should (equal '(8 40)
                 (orgtrello-tests/with-temp-buffer
                  "* card
  - [ ] checklist
    - [ ] item
"
                  (orgtrello-entity-compute-checklist-region))))
  (should (equal '(8 53)
                 (orgtrello-tests/with-temp-buffer
                  "* card
  - [ ] checklist
    - [ ] item
    - item 2
"
                  (orgtrello-entity-compute-checklist-region)
                  -3)))
  (should (equal '(8 53)
                 (orgtrello-tests/with-temp-buffer
                  "* card
  - [ ] checklist
    - [ ] item
    - item 2
* another card"
                  (orgtrello-entity-compute-checklist-region)
                  -3))))

(ert-deftest test-orgtrello-entity-compute-item-region ()
  (should (equal '(17 32)
                 (orgtrello-tests/with-temp-buffer "- [ ] checklist\n  - [ ] another" (orgtrello-entity-compute-item-region) 0))))

(ert-deftest test-orgtrello-entity-next-checklist-point ()
  (should (equal 24 (orgtrello-tests/with-temp-buffer "* card\n- [ ] checkbox 0\n- [ ] checkbox 1\n" (orgtrello-entity-next-checklist-point) -2)))
  (should (equal 55 (orgtrello-tests/with-temp-buffer "* card\n- [ ] checkbox 0\n  - [ ] item0\n- [ ] checkbox 1\n" (orgtrello-entity-next-checklist-point) -1))))

(ert-deftest test-orgtrello-entity-card-end-point ()
  (should (equal 50 (orgtrello-tests/with-temp-buffer "* heading\n- [ ] some checklist\n  - [ ] some item\n"                                      (orgtrello-entity-card-end-point)))) ;; return the max point
  (should (equal 70 (orgtrello-tests/with-temp-buffer "#+TODO: TODO | DONE\n* heading\n- [ ] some checklist\n  - [ ] some item\n"                 (orgtrello-entity-card-end-point)))) ;; return the max point
  (should (equal 65 (orgtrello-tests/with-temp-buffer "* heading\n- [ ] some checklist\n  - [ ] some item\n* next heading\n"                      (orgtrello-entity-card-end-point))))
  (should (equal 85 (orgtrello-tests/with-temp-buffer "#+TODO: TODO | DONE\n* heading\n- [ ] some checklist\n  - [ ] some item\n* next heading\n" (orgtrello-entity-card-end-point))))
  (should (equal 70 (orgtrello-tests/with-temp-buffer "#+TODO: TODO | DONE\n* heading\n- [ ] some checklist\n  - [ ] some item\n* next heading\n" (orgtrello-entity-card-end-point) -3)))
  (should (equal 70 (orgtrello-tests/with-temp-buffer "#+TODO: TODO | DONE\n* heading\n- [ ] some checklist\n  - [ ] some item\n* next heading\n" (orgtrello-entity-card-end-point) -4))))

(ert-deftest test-orgtrello-entity-card-region ()
  "Compute the region of the card."
  (should (equal
           '(1 265)
           (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
* another card"
                                             (orgtrello-entity-card-region)
                                             -2)))
  (should (equal
           '(265 279)
           (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
* another card"
                                             (orgtrello-entity-card-region)
                                             0)))
  (should (equal
           "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
"
           (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
* another card"
                                             (apply 'buffer-substring-no-properties (orgtrello-entity-card-region))
                                             -2)))
  (should (equal
           "* another card"
           (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
* another card"
                                             (apply 'buffer-substring-no-properties (orgtrello-entity-card-region))
                                             0))))


(ert-deftest test-orgtrello-entity-card-metadata-region ()
  (should (equal
           "  hello there"
           (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
* another card"
                                             (apply 'buffer-substring-no-properties (orgtrello-entity-card-metadata-region))
                                             -2))))


(ert-deftest test-orgtrello-entity-card-data-region ()
  (should (equal
           "- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}"
           (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
* another card"
                                             (apply 'buffer-substring-no-properties (orgtrello-entity-card-data-region))
                                             -2))))

(ert-deftest test-orgtrello-entity-comment-region ()
  (should (equal
           "** COMMENT ardumont, 2014-12-09T17:29:42.073Z
:PROPERTIES:
:orgtrello-id: 548731866513c90940aa7746
:END:
ardumont comment
"
           (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
* another card
** COMMENT ardumont, 2014-12-09T17:29:42.073Z
:PROPERTIES:
:orgtrello-id: 548731866513c90940aa7746
:END:
ardumont comment
"
                                             (apply 'buffer-substring-no-properties (orgtrello-entity-comment-region))
                                             -2))))

(ert-deftest test-orgtrello-entity/comment-at-pt! ()
  (should-not (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
   DEADLINE: <2014-04-01T00:00:00.000Z>
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
hello there
"
                                                (orgtrello-entity-org-comment-p)))
  (should (equal t
                 (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
  - [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
    - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
** COMMENT ardumont, some-date
hello"
                                                   (orgtrello-entity-org-comment-p)
                                                   0))))


(provide 'org-trello-entity-tests)
;;; org-trello-cbx-tests.el ends here
