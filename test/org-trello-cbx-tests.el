(require 'org-trello-cbx)
(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(ert-deftest test-orgtrello-cbx/--status ()
  (should (equal "DONE" (orgtrello-cbx/--status"[X]")))
  (should (equal "TODO" (orgtrello-cbx/--status"[ ]")))
  (should (equal "TODO" (orgtrello-cbx/--status"[]")))
  (should (equal "TODO" (orgtrello-cbx/--status"[-]")))
  (should (equal "TODO" (orgtrello-cbx/--status""))))

(ert-deftest test-orgtrello-cbx/--org-split-data ()
  (should (equal '("-" "[X]" "call" "people" "[4/4]")                                             (orgtrello-cbx/--org-split-data "- [X] call people [4/4]")))
  (should (equal '("-" "[X]" "call" "people" "[4/4]" ":PROPERTIES:" "{\"orgtrello-id\":\"456\"}") (orgtrello-cbx/--org-split-data "- [X] call people [4/4] :PROPERTIES: {\"orgtrello-id\":\"456\"}")))
  (should (equal '("" "" "-" "[X]" "Peter")                                                       (orgtrello-cbx/--org-split-data "  - [X] Peter")))
  (should (equal '("" "" "-" "[X]" "Peter" ":PROPERTIES:" "{\"orgtrello-id\":\"456\"}")           (orgtrello-cbx/--org-split-data "  - [X] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}")))

  (should (equal '("-" "[]" "call" "people" "[4/4]")                                             (orgtrello-cbx/--org-split-data "- [] call people [4/4]")))
  (should (equal '("-" "[]" "call" "people" "[4/4]" ":PROPERTIES:" "{\"orgtrello-id\":\"456\"}") (orgtrello-cbx/--org-split-data "- [] call people [4/4] :PROPERTIES: {\"orgtrello-id\":\"456\"}")))
  (should (equal '("" "" "-" "[]" "Peter")                                                       (orgtrello-cbx/--org-split-data "  - [] Peter")))
  (should (equal '("" "" "-" "[]" "Peter" ":PROPERTIES:" "{\"orgtrello-id\":\"456\"}")           (orgtrello-cbx/--org-split-data "  - [] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}")))

  (should (equal '("-" "[]" "call" "people" "[4/4]")                                             (orgtrello-cbx/--org-split-data "- [ ] call people [4/4]")))
  (should (equal '("-" "[]" "call" "people" "[4/4]" ":PROPERTIES:" "{\"orgtrello-id\":\"456\"}") (orgtrello-cbx/--org-split-data "- [ ] call people [4/4] :PROPERTIES: {\"orgtrello-id\":\"456\"}")))
  (should (equal '("" "" "-" "[]" "Peter")                                                       (orgtrello-cbx/--org-split-data "  - [ ] Peter")))
  (should (equal '("" "" "-" "[]" "Peter" ":PROPERTIES:" "{\"orgtrello-id\":\"456\"}")           (orgtrello-cbx/--org-split-data "  - [ ] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}"))))

(ert-deftest test-orgtrello-cbx/--retrieve-status ()
  (should (equal "[X]" (orgtrello-cbx/--retrieve-status '("" "" "-" "[X]" "Peter"))))
  (should (equal "[]" (orgtrello-cbx/--retrieve-status '("" "" "-" "[]" "Peter"))))
  (should (equal "[-]" (orgtrello-cbx/--retrieve-status '("" "" "-" "[-]" "Peter"))))
  (should (equal "[ ]" (orgtrello-cbx/--retrieve-status '("" "" "-" "[ ]" "Peter")))))

(ert-deftest test-orgtrello-cbx/--name ()
  (should (equal "call people [4/4]" (orgtrello-cbx/--name "- [X] call people [4/4]"   "[X]")))
  (should (equal "call people [4/4]" (orgtrello-cbx/--name "- [] call people [4/4]"    "[]")))
  (should (equal "call people [4/4]" (orgtrello-cbx/--name "- [-] call people [4/4]"   "[-]")))
  (should (equal "call people [4/4]" (orgtrello-cbx/--name "-[X] call people [4/4]"    "[X]")))
  (should (equal "call people [4/4]" (orgtrello-cbx/--name "-[] call people [4/4]"     "[]")))
  (should (equal "call people [4/4]" (orgtrello-cbx/--name "-[-] call people [4/4]"    "[-]")))
  (should (equal "call people [4/4]" (orgtrello-cbx/--name "  - [X] call people [4/4]" "[X]")))
  (should (equal "call people [4/4]" (orgtrello-cbx/--name "  - [] call people [4/4]"  "[]")))
  (should (equal "call people [4/4]" (orgtrello-cbx/--name "  - [-] call people [4/4]" "[-]")))
  (should (equal "call people [4/4]" (orgtrello-cbx/--name "  -[X] call people [4/4]"  "[X]")))
  (should (equal "call people [4/4]" (orgtrello-cbx/--name "  -[] call people [4/4]"   "[]")))
  (should (equal "call people [4/4]" (orgtrello-cbx/--name "  -[-] call people [4/4]"  "[-]"))))

(ert-deftest test-orgtrello-cbx/--to-properties ()
  (should (equal "{\"orgtrello-id\":\"123\"}"                              (orgtrello-cbx/--to-properties `((,*ORGTRELLO/ID* . "123")))))
  (should (equal "{\"orgtrello-id\":\"456\"}"                              (orgtrello-cbx/--to-properties `((,*ORGTRELLO/ID* . "123") (,*ORGTRELLO/ID* . "456")))))
  (should (equal "{\"orgtrello-id\":\"def\",\"orgtrello-marker\":\"456\",\"orgtrello-id\":\"abc\"}"
                 (replace-regexp-in-string ", " "," (orgtrello-cbx/--to-properties `(("orgtrello-id" . "abc") (orgtrello-marker . "456") (orgtrello-id . "def"))))))
  (should (equal "{\"orgtrello-marker\":\"456\",\"orgtrello-id\":\"def\"}"
                 (replace-regexp-in-string ", " "," (orgtrello-cbx/--to-properties `(("orgtrello-id" . "abc") (orgtrello-marker . "456") ("orgtrello-id" . "def"))))))
  (should (equal "{\"orgtrello-marker\":\"456\",\"orgtrello-id\":\"def\"}"
                 (replace-regexp-in-string ", " "," (orgtrello-cbx/--to-properties `((orgtrello-id . "abc") (orgtrello-marker . "456") (orgtrello-id . "def")))))))

(ert-deftest test-orgtrello-cbx/--from-properties ()
  (should (equal '((orgtrello-id . "123")) (orgtrello-cbx/--from-properties "{\"orgtrello-id\":\"123\"}")))
  (should (equal '((orgtrello-marker . "456") (orgtrello-id . "123")) (orgtrello-cbx/--from-properties "{\"orgtrello-id\":\"123\",\"orgtrello-marker\":\"456\"}")))
  (should (equal '((orgtrello-marker . "456") (orgtrello-id . "123")) (orgtrello-cbx/--from-properties "{\"orgtrello-id\":\"123\", \"orgtrello-marker\":\"456\"}"))))

(ert-deftest test-orgtrello-cbx/--read-properties ()
  (should (equal '((orgtrello-id . "123")) (orgtrello-cbx/--read-properties "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}"))))

(ert-deftest test-orgtrello-cbx/--org-get-property ()
  (should (equal "123"    (orgtrello-cbx/--org-get-property "orgtrello-id" `((orgtrello-id . "123")))))
  (should (equal nil      (orgtrello-cbx/--org-get-property "orgtrello-id" `(("orgtrello-id" . "123")))))
  (should (equal nil      (orgtrello-cbx/--org-get-property 'orgtrello-id `(("orgtrello-id" . "123")))))
  (should (equal "123"    (orgtrello-cbx/--org-get-property 'orgtrello-id `((orgtrello-id . "123")))))
  (should (equal "marker" (orgtrello-cbx/--org-get-property "orgtrello-marker" `(("orgtrello-id" . "123") (orgtrello-marker . "marker"))))))

(ert-deftest test-orgtrello-cbx/--org-update-property ()
  (should (equal `((orgtrello-id . "10") (orgtrello-marker . "123"))    (orgtrello-cbx/--org-update-property "orgtrello-id" "10" `((orgtrello-marker . "123")))))
  (should (equal `((orgtrello-toto . "abc") (orgtrello-marker . "456")) (orgtrello-cbx/--org-update-property "orgtrello-toto" "abc" `((orgtrello-marker . "456")))))
  (should (equal `((orgtrello-id . "abc") (orgtrello-marker . "456"))   (orgtrello-cbx/--org-update-property "orgtrello-id" "abc" `((orgtrello-marker . "456") (orgtrello-id . "def"))))))

(ert-deftest test-orgtrello-cbx/--org-delete-property ()
  (should (equal `(("orgtrello-id" . "123") (orgtrello-marker . "marker")) (orgtrello-cbx/--org-delete-property "orgtrello-id" `(("orgtrello-id" . "123") (orgtrello-marker . "marker")))))
  (should (equal `((orgtrello-marker . "marker")) (orgtrello-cbx/--org-delete-property "orgtrello-id" `((orgtrello-id . "123") (orgtrello-marker . "marker")))))
  (should (equal `((orgtrello-marker . "marker")) (orgtrello-cbx/--org-delete-property 'orgtrello-id `((orgtrello-id . "123") (orgtrello-marker . "marker")))))
  (should (equal `(("orgtrello-id" . "123") (orgtrello-marker . "marker")) (orgtrello-cbx/--org-delete-property 'orgtrello-id `(("orgtrello-id" . "123") (orgtrello-marker . "marker"))))))

(ert-deftest test-orgtrello-cbx/--checkbox-split ()
  (should (equal '("  - [X] Peter " " {\"orgtrello-id\":\"456\"}") (orgtrello-cbx/--checkbox-split "  - [X] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}"))))

(ert-deftest test-orgtrello-cbx/--checkbox-metadata ()
  (should (equal "{\"orgtrello-id\":\"456\"}" (orgtrello-cbx/--checkbox-metadata "  - [X] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}")))
  (should (equal ""                           (orgtrello-cbx/--checkbox-metadata "  - [X] Peter :PROPERTIES:")))
  (should (equal ""                           (orgtrello-cbx/--checkbox-metadata "  - [X] Peter :PROPERTIES: ")))
  (should (equal nil                          (orgtrello-cbx/--checkbox-metadata "  - [X] Peter"))))

(ert-deftest test-orgtrello-cbx/--checkbox-data ()
  (should (equal "  - [X] Peter" (orgtrello-cbx/--checkbox-data "  - [X] Peter :PROPERTIES: {\"orgtrello-id\":\"456\"}"))))

(ert-deftest test-orgtrello-cbx/--key-to-search ()
  (should (equal 'some-key (orgtrello-cbx/--key-to-search "some-key")))
  (should (equal 'some-key (orgtrello-cbx/--key-to-search 'some-key)))
  (should (equal :some-key (orgtrello-cbx/--key-to-search :some-key))))

(ert-deftest test-orgtrello-cbx/--get-level ()
  (should (equal 1 (orgtrello-cbx/--get-level '(1 2 3))))
  (should (equal 2 (orgtrello-cbx/--get-level '(2 3)))))

(ert-deftest test-orgtrello-cbx/--read-properties-from-point ()
  (should (equal '((orgtrello-id . "123"))
                 (orgtrello-tests/with-temp-buffer "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/--read-properties-from-point (point)))))
  (should (equal nil
                 (orgtrello-tests/with-temp-buffer "- [X] some checkbox :PROPERTIES: {}" (orgtrello-cbx/--read-properties-from-point (point)))))
  (should (equal nil (orgtrello-tests/with-temp-buffer "- [X] some checkbox" (orgtrello-cbx/--read-properties-from-point (point))))))

(ert-deftest test-orgtrello-cbx/--write-properties-at-point ()
  (should (equal "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"456\"}"
                 (orgtrello-tests/with-temp-buffer "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/--write-properties-at-point (point) `(("orgtrello-id" . "456")))))))

(ert-deftest test-orgtrello-cbx/org-get-property ()
  (should (equal "abc"
                 (orgtrello-tests/with-temp-buffer "- [X] some checkbox                                                                :PROPERTIES: {\"orgtrello-id\":\"abc\"}" (orgtrello-cbx/org-get-property (point) "orgtrello-id"))))
  (should (equal nil
                 (orgtrello-tests/with-temp-buffer "- [X] some checkbox                                                                :PROPERTIES: {\"orgtrello-id\":\"abc\"}" (orgtrello-cbx/org-get-property (point) "inexistant-id")))))

(ert-deftest test-orgtrello-cbx/org-set-property ()
  (should (equal "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"abc\"}"
                 (orgtrello-tests/with-temp-buffer-and-return-buffer-content "- [X] some checkbox"  (orgtrello-cbx/org-set-property "orgtrello-id" "abc"))))
  (should (equal "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"abc\"}"
                 (orgtrello-tests/with-temp-buffer-and-return-buffer-content "- [X] some checkbox :PROPERTIES: {}" (orgtrello-cbx/org-set-property "orgtrello-id" "abc"))))
  (should (equal "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"def\"}"
                 (orgtrello-tests/with-temp-buffer-and-return-buffer-content "- [X] some checkbox                                                                                :PROPERTIES: {\"orgtrello-id\":\"abc\"}" (orgtrello-cbx/org-set-property "orgtrello-id" "def")))))

(ert-deftest test-orgtrello-cbx/org-delete-property ()
  (should (equal "- [X] some checkbox :PROPERTIES: {}"
                 (orgtrello-tests/with-temp-buffer-and-return-buffer-content "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/org-delete-property "orgtrello-id"))))
  (should (equal "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"def\"}"
                 (orgtrello-tests/with-temp-buffer-and-return-buffer-content  "- [X] some checkbox                                                    :PROPERTIES: {\"orgtrello-id\":\"def\"}" (orgtrello-cbx/org-delete-property "inexistant"))))
  (should (equal "- [X] some checkbox :PROPERTIES: {}"
                 (orgtrello-tests/with-temp-buffer-and-return-buffer-content "- [X] some checkbox" (orgtrello-cbx/org-delete-property "inexistant")))))

(ert-deftest test-orgtrello-cbx/--read-properties-from-point ()
  (should (equal '((orgtrello-id . "orgtrello-marker-123")) (with-temp-buffer
                                                              (org-mode)
                                                              (insert "* card\n")
                                                              (insert "- [X] hello :PROPERTIES: {\"orgtrello-id\":\"orgtrello-marker-123\"}")
                                                              (org-trello-mode)
                                                              (orgtrello-cbx/--read-properties-from-point (point)))))

  (should (equal nil (orgtrello-tests/with-temp-buffer "* card\n- [X] hello :PROPERTIES: {\"orgtrello-id\":\"orgtrello-marker-123\"}\n" (progn
                                                                                                                                          (orgtrello-proxy/--cleanup-meta (orgtrello-buffer/entry-get-full-metadata!))
                                                                                                                                          (orgtrello-cbx/--read-properties-from-point (point))))))

  (should (equal nil (orgtrello-tests/with-temp-buffer "* card\n- [X] hello :PROPERTIES: {\"orgtrello-id\":\"orgtrello-marker-123\"}\n" (progn (orgtrello-proxy/--cleanup-meta (orgtrello-buffer/entry-get-full-metadata!))
                                                                                                                                               (orgtrello-cbx/--read-properties-from-point (point))))))

  (should (equal nil (orgtrello-tests/with-temp-buffer "* card\n- [X] cl :PROPERTIES: {\"orgtrello-id\":\"abc\"}\n  - [X] item :PROPERTIES: {\"orgtrello-id\":\"orgtrello-marker-123\"}\n" (progn (orgtrello-proxy/--cleanup-meta (orgtrello-buffer/entry-get-full-metadata!))
                                                                                                                                                                                                  (orgtrello-cbx/--read-properties-from-point (point)))))))

(ert-deftest test-orgtrello-cbx/--metadata-from-checklist ()
  (should (equal '(nil "DONE" nil "some checkbox" nil)
                 (orgtrello-tests/with-temp-buffer "- [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/--metadata-from-checklist (orgtrello-cbx/--read-checkbox!)))))
  (should (equal '(nil "TODO" nil "some other checkbox" nil)
                 (orgtrello-tests/with-temp-buffer " - [] some other checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/--metadata-from-checklist (orgtrello-cbx/--read-checkbox!))))))

(ert-deftest test-orgtrello-cbx/--read-checkbox! ()
  (should (equal " - [] some other checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}"
                 (orgtrello-tests/with-temp-buffer " - [] some other checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/--read-checkbox!))))
  (should (equal "- [X] some checkbox"
                 (orgtrello-tests/with-temp-buffer "- [X] some checkbox" (orgtrello-cbx/--read-checkbox!)))))

(ert-deftest test-orgtrello-cbx/org-checkbox-metadata! ()
  (should (equal '(2 nil "DONE" nil "some checkbox" nil)
                 (orgtrello-tests/with-temp-buffer "  - [X] some checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/org-checkbox-metadata!))))
  (should (equal '(3 nil "TODO" nil "some other checkbox" nil)
                 (orgtrello-tests/with-temp-buffer "    - [ ] some other checkbox :PROPERTIES: {\"orgtrello-id\":\"123\"}" (orgtrello-cbx/org-checkbox-metadata!)))))

(provide 'org-trello-cbx-tests)
;;; org-trello-cbx-tests.el ends here
