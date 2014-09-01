(require 'org-trello-entity)
(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(expectations
 (desc "orgtrello-entity/level!")
 (expect 1  (orgtrello-tests/with-temp-buffer "* some card" (orgtrello-entity/level!)))
 (expect 2  (orgtrello-tests/with-temp-buffer "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-entity/level!)))
 (expect 3  (orgtrello-tests/with-temp-buffer "  - [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-entity/level!)))
 (expect 3  (orgtrello-tests/with-temp-buffer " - [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-entity/level!)))
 (expect 3  (orgtrello-tests/with-temp-buffer "     - [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-entity/level!)))
 (expect -1 (orgtrello-tests/with-temp-buffer "something else" (orgtrello-entity/level!))))

(expectations
  (desc "orgtrello-entity/card-at-pt!")
  (expect t
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
   DEADLINE: <2014-04-01T00:00:00.000Z>
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
hello there
"
                                      (orgtrello-entity/card-at-pt!)))

  (expect nil
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}"
                                      (orgtrello-entity/card-at-pt!)
                                      0))
  (expect nil
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}"
                                      (orgtrello-entity/card-at-pt!)
                                      0)))

(expectations
  (desc "orgtrello-entity/checklist-at-pt!")
  (expect nil
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
   DEADLINE: <2014-04-01T00:00:00.000Z>
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
hello there
"
                                      (orgtrello-entity/checklist-at-pt!)))

  (expect t
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}"
                                      (orgtrello-entity/checklist-at-pt!)
                                      0))
  (expect nil
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}"
                                      (orgtrello-entity/checklist-at-pt!)
                                      0)))

(expectations
  (desc "orgtrello-entity/item-at-pt!")
  (expect nil
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
   DEADLINE: <2014-04-01T00:00:00.000Z>
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
hello there
"
                                      (orgtrello-entity/item-at-pt!)))

  (expect nil
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}"
                                      (orgtrello-entity/item-at-pt!)
                                      0))
  (expect t
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}"
                                      (orgtrello-entity/item-at-pt!)
                                      0)))

(expectations
 (expect '(8 24)
         (orgtrello-tests/with-temp-buffer "* card\n- [ ] checklist\n- [ ] another" (orgtrello-entity/compute-checklist-header-region!))))

(expectations
 (expect '(8 24)
         (orgtrello-tests/with-temp-buffer "* card\n- [ ] checklist\n- [ ] item" (orgtrello-entity/compute-checklist-region!)))
 (expect '(8 36)
         (orgtrello-tests/with-temp-buffer "* card\n- [ ] checklist\n  - [ ] item" (orgtrello-entity/compute-checklist-region!)))
 (expect '(8 48)
         (orgtrello-tests/with-temp-buffer "* card\n- [ ] checklist\n  - [ ] item\n  - item 2\n" (orgtrello-entity/compute-checklist-region!) -3))
 (expect '(8 48)
         (orgtrello-tests/with-temp-buffer "* card\n- [ ] checklist\n  - [ ] item\n  - item 2\n* another card" (orgtrello-entity/compute-checklist-region!) -3)))

(expectations
 (expect '(17 33)
         (orgtrello-tests/with-temp-buffer "- [ ] checklist\n  - [ ] another" (orgtrello-entity/compute-item-region!) 0)))

(expectations
 (expect 25 (orgtrello-tests/with-temp-buffer "* card\n- [ ] checkbox 0\n- [ ] checkbox 1\n" (orgtrello-entity/next-checklist-point!) -2))
 (expect 56 (orgtrello-tests/with-temp-buffer "* card\n- [ ] checkbox 0\n  - [ ] item0\n- [ ] checkbox 1\n" (orgtrello-entity/next-checklist-point!) -1)))

(expectations (desc "orgtrello-entity/compute-next-card-point!")
  (expect 50 (orgtrello-tests/with-temp-buffer "* heading\n- [ ] some checklist\n  - [ ] some item\n"                                      (orgtrello-entity/compute-next-card-point!))) ;; return the max point
  (expect 70 (orgtrello-tests/with-temp-buffer "#+TODO: TODO | DONE\n* heading\n- [ ] some checklist\n  - [ ] some item\n"                 (orgtrello-entity/compute-next-card-point!))) ;; return the max point
  (expect 65 (orgtrello-tests/with-temp-buffer "* heading\n- [ ] some checklist\n  - [ ] some item\n* next heading\n"                      (orgtrello-entity/compute-next-card-point!)))
  (expect 85 (orgtrello-tests/with-temp-buffer "#+TODO: TODO | DONE\n* heading\n- [ ] some checklist\n  - [ ] some item\n* next heading\n" (orgtrello-entity/compute-next-card-point!)))
  (expect 70 (orgtrello-tests/with-temp-buffer "#+TODO: TODO | DONE\n* heading\n- [ ] some checklist\n  - [ ] some item\n* next heading\n" (orgtrello-entity/compute-next-card-point!) -3))
  (expect 70 (orgtrello-tests/with-temp-buffer "#+TODO: TODO | DONE\n* heading\n- [ ] some checklist\n  - [ ] some item\n* next heading\n" (orgtrello-entity/compute-next-card-point!) -4)))

(expectations
  (desc "orgtrello-entity/compute-card-region! - compute the region of the card.")
  (expect
      '(1 265)
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
* another card"
                                      (orgtrello-entity/compute-card-region!)
                                      -2))
  (expect
      '(265 279)
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
* another card"
                                      (orgtrello-entity/compute-card-region!)
                                      0))
  (expect
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
                                      (apply 'buffer-substring-no-properties (orgtrello-entity/compute-card-region!))
                                      -2))
  (expect
      "* another card"
    (orgtrello-tests/with-temp-buffer "* TODO Joy of FUN(ctional) LANGUAGES
:PROPERTIES:
:orgtrello-id: 52c945143004d4617c012528
:END:
  hello there
- [-] LISP family   :PROPERTIES: {\"orgtrello-id\":\"52c945140a364c5226007314\"}
  - [X] Emacs-Lisp  :PROPERTIES: {\"orgtrello-id\":\"52c9451784251e1b260127f8\"}
* another card"
                                      (apply 'buffer-substring-no-properties (orgtrello-entity/compute-card-region!))
                                      0)))


(provide 'org-trello-entity-tests)
;;; org-trello-cbx-tests.el ends here
