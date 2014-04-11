(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(expectations (desc "orgtrello-cbx/--status")
  (expect "DONE" (orgtrello-cbx/--status"[X]"))
  (expect "TODO" (orgtrello-cbx/--status"[ ]"))
  (expect "TODO" (orgtrello-cbx/--status"[]"))
  (expect "TODO" (orgtrello-cbx/--status"[-]"))
  (expect "TODO" (orgtrello-cbx/--status"")))

(expectations (desc "orgtrello-cbx/--org-split-data")
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

(expectations (desc "orgtrello-cbx/--retrieve-status")
  (expect "[X]" (orgtrello-cbx/--retrieve-status '("" "" "-" "[X]" "Peter")))
  (expect "[]" (orgtrello-cbx/--retrieve-status '("" "" "-" "[]" "Peter")))
  (expect "[-]" (orgtrello-cbx/--retrieve-status '("" "" "-" "[-]" "Peter")))
  (expect "[ ]" (orgtrello-cbx/--retrieve-status '("" "" "-" "[ ]" "Peter"))))

(expectations (desc "orgtrello-cbx/--name")
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

(expectations (desc "orgtrello-cbx/--to-properties\\\":\\\"123\\\"}")
  (expect "{\"orgtrello-id\":\"123\"}"                              (orgtrello-cbx/--to-properties `((,*ORGTRELLO-ID* . "123"))))
  (expect "{\"orgtrello-id\":\"456\"}"                              (orgtrello-cbx/--to-properties `((,*ORGTRELLO-ID* . "123") (,*ORGTRELLO-ID* . "456"))))
  (expect "{\"orgtrello-id\":\"def\",\"orgtrello-marker\":\"456\",\"orgtrello-id\":\"abc\"}"
    (replace-regexp-in-string ", " "," (orgtrello-cbx/--to-properties `(("orgtrello-id" . "abc") (orgtrello-marker . "456") (orgtrello-id . "def")))))
  (expect "{\"orgtrello-marker\":\"456\",\"orgtrello-id\":\"def\"}"
    (replace-regexp-in-string ", " "," (orgtrello-cbx/--to-properties `(("orgtrello-id" . "abc") (orgtrello-marker . "456") ("orgtrello-id" . "def")))))
  (expect "{\"orgtrello-marker\":\"456\",\"orgtrello-id\":\"def\"}"
    (replace-regexp-in-string ", " "," (orgtrello-cbx/--to-properties `((orgtrello-id . "abc") (orgtrello-marker . "456") (orgtrello-id . "def"))))))

(expectations (desc "orgtrello-cbx/--from-properties")
  (expect '((orgtrello-id . "123")) (orgtrello-cbx/--from-properties "{\"orgtrello-id\":\"123\"}"))
  (expect '((orgtrello-marker . "456") (orgtrello-id . "123")) (orgtrello-cbx/--from-properties "{\"orgtrello-id\":\"123\",\"orgtrello-marker\":\"456\"}"))
  (expect '((orgtrello-marker . "456") (orgtrello-id . "123")) (orgtrello-cbx/--from-properties "{\"orgtrello-id\":\"123\", \"orgtrello-marker\":\"456\"}")))

(expectations (desc "orgtrello-cbx/--read-properties")
  (expect '((orgtrello-id . "123")) (orgtrello-cbx/--read-properties "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}")))

(expectations (desc "orgtrello-cbx/--org-get-property")
  (expect "123"    (orgtrello-cbx/--org-get-property "orgtrello-id" `((orgtrello-id . "123"))))
  (expect nil      (orgtrello-cbx/--org-get-property "orgtrello-id" `(("orgtrello-id" . "123"))))
  (expect nil      (orgtrello-cbx/--org-get-property 'orgtrello-id `(("orgtrello-id" . "123"))))
  (expect "123"    (orgtrello-cbx/--org-get-property 'orgtrello-id `((orgtrello-id . "123"))))
  (expect "marker" (orgtrello-cbx/--org-get-property "orgtrello-marker" `(("orgtrello-id" . "123") (orgtrello-marker . "marker")))))

(expectations (desc "orgtrello-cbx/--org-update-property")
  (expect `((orgtrello-id . "10") (orgtrello-marker . "123"))    (orgtrello-cbx/--org-update-property "orgtrello-id" "10" `((orgtrello-marker . "123"))))
  (expect `((orgtrello-toto . "abc") (orgtrello-marker . "456")) (orgtrello-cbx/--org-update-property "orgtrello-toto" "abc" `((orgtrello-marker . "456"))))
  (expect `((orgtrello-id . "abc") (orgtrello-marker . "456"))   (orgtrello-cbx/--org-update-property "orgtrello-id" "abc" `((orgtrello-marker . "456") (orgtrello-id . "def")))))

(expectations (desc "orgtrello-cbx/--org-delete-property")
  (expect `(("orgtrello-id" . "123") (orgtrello-marker . "marker")) (orgtrello-cbx/--org-delete-property "orgtrello-id" `(("orgtrello-id" . "123") (orgtrello-marker . "marker"))))
  (expect `((orgtrello-marker . "marker")) (orgtrello-cbx/--org-delete-property "orgtrello-id" `((orgtrello-id . "123") (orgtrello-marker . "marker"))))
  (expect `((orgtrello-marker . "marker")) (orgtrello-cbx/--org-delete-property 'orgtrello-id `((orgtrello-id . "123") (orgtrello-marker . "marker"))))
  (expect `(("orgtrello-id" . "123") (orgtrello-marker . "marker")) (orgtrello-cbx/--org-delete-property 'orgtrello-id `(("orgtrello-id" . "123") (orgtrello-marker . "marker")))))

(expectations (desc "orgtrello-cbx/--checkbox-split")
  (expect '("  - [X] Peter " " {\"orgtrello-id\":\"456\"}") (orgtrello-cbx/--checkbox-split "  - [X] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}")))

(expectations (desc "orgtrello-id\\\":\\\"456\\\"}")
  (expect "{\"orgtrello-id\":\"456\"}" (orgtrello-cbx/--checkbox-metadata "  - [X] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}"))
  (expect ""                           (orgtrello-cbx/--checkbox-metadata "  - [X] Peter :PROPERTIES:"))
  (expect ""                           (orgtrello-cbx/--checkbox-metadata "  - [X] Peter :PROPERTIES: "))
  (expect nil                          (orgtrello-cbx/--checkbox-metadata "  - [X] Peter")))

(expectations (desc "orgtrello-cbx/--checkbox-data")
  (expect "  - [X] Peter" (orgtrello-cbx/--checkbox-data "  - [X] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}")))

(expectations (desc "orgtrello-cbx/--key-to-search")
  (expect 'some-key (orgtrello-cbx/--key-to-search "some-key"))
  (expect 'some-key (orgtrello-cbx/--key-to-search 'some-key))
  (expect :some-key (orgtrello-cbx/--key-to-search :some-key)))

(expectations (desc "orgtrello-cbx/--get-level")
  (expect 1 (orgtrello-cbx/--get-level '(1 2 3)))
  (expect 2 (orgtrello-cbx/--get-level '(2 3))))

(expectations (desc "orgtrello-cbx/--read-properties-from-point")
  (expect '((orgtrello-id . "123"))
          (orgtrello-tests/with-temp-buffer "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/--read-properties-from-point (point))))
  (expect nil
          (orgtrello-tests/with-temp-buffer "- [X] some checkbox :PROPERTIES: {}" (orgtrello-cbx/--read-properties-from-point (point))))
  (expect nil (orgtrello-tests/with-temp-buffer "- [X] some checkbox" (orgtrello-cbx/--read-properties-from-point (point)))))

(expectations (desc "orgtrello-cbx/--write-properties-at-point")
  (expect "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":456}"
          (orgtrello-tests/with-temp-buffer "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/--write-properties-at-point (point) `(("orgtrello-id" . 456))))))

(expectations (desc "orgtrello-cbx/org-get-property")
  (expect "abc"
          (orgtrello-tests/with-temp-buffer "- [X] some checkbox                                                                :PROPERTIES: {\"orgtrello-id\":\"abc\"}" (orgtrello-cbx/org-get-property (point) "orgtrello-id")))
  (expect nil
          (orgtrello-tests/with-temp-buffer "- [X] some checkbox                                                                :PROPERTIES: {\"orgtrello-id\":\"abc\"}" (orgtrello-cbx/org-get-property (point) "inexistant-id"))))

(expectations (desc "orgtrello-cbx/org-set-property")
  (expect "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"abc\"}"
          (orgtrello-tests/with-temp-buffer-and-return-buffer-content "- [X] some checkbox"  (orgtrello-cbx/org-set-property "orgtrello-id" "abc")))
  (expect "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"abc\"}"
          (orgtrello-tests/with-temp-buffer-and-return-buffer-content "- [X] some checkbox :PROPERTIES: {}" (orgtrello-cbx/org-set-property "orgtrello-id" "abc")))
  (expect "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"def\"}"
          (orgtrello-tests/with-temp-buffer-and-return-buffer-content "- [X] some checkbox                                                                                :PROPERTIES: {\"orgtrello-id\":\"abc\"}" (orgtrello-cbx/org-set-property "orgtrello-id" "def"))))

(expectations (desc "orgtrello-cbx/org-delete-property")
  (expect "- [X] some checkbox :PROPERTIES: {}"
          (orgtrello-tests/with-temp-buffer-and-return-buffer-content "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/org-delete-property "orgtrello-id")))
  (expect "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"def\"}"
          (orgtrello-tests/with-temp-buffer-and-return-buffer-content  "- [X] some checkbox                                                    :PROPERTIES: {\"orgtrello-id\":\"def\"}" (orgtrello-cbx/org-delete-property "inexistant")))
  (expect "- [X] some checkbox :PROPERTIES: {}"
          (orgtrello-tests/with-temp-buffer-and-return-buffer-content "- [X] some checkbox" (orgtrello-cbx/org-delete-property "inexistant"))))

(expectations (desc "orgtrello-cbx/compute-next-card-point!")
  (expect 50 (orgtrello-tests/with-temp-buffer "* heading\n- [ ] some checklist\n  - [ ] some item\n"                                      (orgtrello-cbx/compute-next-card-point!))) ;; return the max point
  (expect 70 (orgtrello-tests/with-temp-buffer "#+TODO: TODO | DONE\n* heading\n- [ ] some checklist\n  - [ ] some item\n"                 (orgtrello-cbx/compute-next-card-point!))) ;; return the max point
  (expect 65 (orgtrello-tests/with-temp-buffer "* heading\n- [ ] some checklist\n  - [ ] some item\n* next heading\n"                      (orgtrello-cbx/compute-next-card-point!)))
  (expect 85 (orgtrello-tests/with-temp-buffer "#+TODO: TODO | DONE\n* heading\n- [ ] some checklist\n  - [ ] some item\n* next heading\n" (orgtrello-cbx/compute-next-card-point!)))
  (expect 70 (orgtrello-tests/with-temp-buffer "#+TODO: TODO | DONE\n* heading\n- [ ] some checklist\n  - [ ] some item\n* next heading\n" (orgtrello-cbx/compute-next-card-point!) -3))
  (expect 70 (orgtrello-tests/with-temp-buffer "#+TODO: TODO | DONE\n* heading\n- [ ] some checklist\n  - [ ] some item\n* next heading\n" (orgtrello-cbx/compute-next-card-point!) -4)))


(expectations (desc "orgtrello-cbx/--read-properties-from-point")
  (expect '((orgtrello-id . "orgtrello-marker-123")) (with-temp-buffer
                                                       (org-mode)
                                                       (insert "* card\n")
                                                       (insert "- [X] hello :PROPERTIES: {\"orgtrello-id\":\"orgtrello-marker-123\"}")
                                                       (org-trello-mode)
                                                       (orgtrello-cbx/--read-properties-from-point (point))))

  (expect nil (orgtrello-tests/with-temp-buffer "* card\n- [X] hello :PROPERTIES: {\"orgtrello-id\":\"orgtrello-marker-123\"}\n" (progn
                                                                                                                                   (orgtrello-proxy/--cleanup-meta (orgtrello-buffer/entry-get-full-metadata!))
                                                                                                                                   (orgtrello-cbx/--read-properties-from-point (point)))))

  (expect nil (orgtrello-tests/with-temp-buffer "* card\n- [X] hello :PROPERTIES: {\"orgtrello-id\":\"orgtrello-marker-123\"}\n" (progn (orgtrello-proxy/--cleanup-meta (orgtrello-buffer/entry-get-full-metadata!))
                                                                                                                                        (orgtrello-cbx/--read-properties-from-point (point)))))

  (expect nil (orgtrello-tests/with-temp-buffer "* card\n- [X] cl :PROPERTIES: {\"orgtrello-id\":\"abc\"}\n  - [X] item :PROPERTIES: {\"orgtrello-id\":\"orgtrello-marker-123\"}\n" (progn (orgtrello-proxy/--cleanup-meta (orgtrello-buffer/entry-get-full-metadata!))
                                                                                                                                                                                           (orgtrello-cbx/--read-properties-from-point (point))))))

(expectations (desc "orgtrello-cbx/--metadata-from-checklist")
  (expect '(nil "DONE" nil "some checkbox" nil)
    (orgtrello-tests/with-temp-buffer "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/--metadata-from-checklist (orgtrello-cbx/--read-checkbox!))))
  (expect '(nil "TODO" nil "some other checkbox" nil)
    (orgtrello-tests/with-temp-buffer " - [] some other checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/--metadata-from-checklist (orgtrello-cbx/--read-checkbox!)))))

(expectations (desc "orgtrello-cbx/--read-checkbox!")
  (expect " - [] some other checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}"
    (orgtrello-tests/with-temp-buffer " - [] some other checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/--read-checkbox!)))
  (expect "- [X] some checkbox"
    (orgtrello-tests/with-temp-buffer "- [X] some checkbox" (orgtrello-cbx/--read-checkbox!))))

(expectations (desc "orgtrello-cbx/--level!")
  (expect 2 (orgtrello-tests/with-temp-buffer "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/--level!)))
  (expect 3 (orgtrello-tests/with-temp-buffer " - [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/--level!))))

(expectations (desc "orgtrello-cbx/org-checkbox-metadata!")
  (expect '(2 nil "DONE" nil "some checkbox" nil)
    (orgtrello-tests/with-temp-buffer "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/org-checkbox-metadata!)))
  (expect '(3 nil "TODO" nil "some other checkbox" nil)
    (orgtrello-tests/with-temp-buffer " - [ ] some other checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/org-checkbox-metadata!))))

(expectations
  (expect 25 (orgtrello-tests/with-temp-buffer "* card\n- [ ] checkbox 0\n- [ ] checkbox 1\n" (orgtrello-cbx/next-checklist-point!) -2))
  (expect 56 (orgtrello-tests/with-temp-buffer "* card\n- [ ] checkbox 0\n  - [ ] item0\n- [ ] checkbox 1\n" (orgtrello-cbx/next-checklist-point!) -1)))
