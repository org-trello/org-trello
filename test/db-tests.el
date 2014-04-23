(ert-deftest testing-orgtrello-db/put-get ()
  "Testing put"
  (let ((db (db-make '(db-hash))))
    (should (equal '("some-value") (orgtrello-db/put "some-key" "some-value" db)))
    (should (equal '("some-value2" "some-value") (orgtrello-db/put "some-key" "some-value2" db)))
    (should (equal "some-value2" (orgtrello-db/get "some-key" db)))
    (should (equal '("some-value") (db-get "some-key" db)))
    (should (equal nil (db-get "some-inexistant-key" db)))))
