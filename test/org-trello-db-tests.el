(require 'org-trello-db)
(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(ert-deftest testing-orgtrello-db/behavior ()
  "Test put/pop/get/copy/pop-last/move-key-values behaviour"
  (let ((db (db-make '(db-hash))))
    ;; put puts value to the end
    (should (equal '("some-value")                  (orgtrello-db/put "some-key" "some-value" db)))
    (should (equal '("some-value" "some-value2")    (orgtrello-db/put "some-key" "some-value2" db)))
    (should (equal '("some-value" "some-value2" 3)  (orgtrello-db/put "some-key" 3 db)))
    ;; pop extracts value at the front...
    (should (equal "some-value"                     (orgtrello-db/pop "some-key" db)))
    ;; and is destructive.
    (should (equal '("some-value2" 3)               (orgtrello-db/get "some-key" db)))
    ;; get is read-only
    (should (equal '("some-value2" 3)               (orgtrello-db/get "some-key" db)))
    ;; pop-last returns the last element
    (should (equal 3                                (orgtrello-db/pop-last "some-key" db)))
    ;; pop-last is destructive
    (should (equal '("some-value2")                 (orgtrello-db/get "some-key" db)))
    ;; if key is not found, nil is returned
    (should (equal nil                              (orgtrello-db/get "some-inexistant-key" db)))
    ;; add an entry to the list
    (should (equal '("some-value2" "some-value3")   (orgtrello-db/put "some-key" "some-value3" db)))
    ;; move the key's values to another key
    (should (equal '("some-value2" "some-value3")   (orgtrello-db/move-key-values "some-key" "some-new-key" db)))
    ;; the new key should have the same values as the old key
    (should (equal '("some-value2" "some-value3")   (orgtrello-db/get "some-new-key" db)))
    ;; the old key should be empty
    (should (equal nil                              (orgtrello-db/get "some-key" db)))))

(provide 'org-trello-db-tests)
;;; org-trello-db-tests.el ends here
