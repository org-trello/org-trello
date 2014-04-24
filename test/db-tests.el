(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(ert-deftest testing-orgtrello-db/put-pop-get-copy ()
  "Test put/pop/get/copy behaviour"
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
    ;; if key is not found, nil is returned
    (should (equal nil                              (orgtrello-db/get "some-inexistant-key" db)))
    ;; we can copy key to new key
    (orgtrello-db/copy "some-key" "some-new-key" db)
    (should (equal '("some-value2" 3)               (orgtrello-db/get "some-key" db)))
    (should (equal (orgtrello-db/get "some-key" db) (orgtrello-db/get "some-new-key" db)))))
