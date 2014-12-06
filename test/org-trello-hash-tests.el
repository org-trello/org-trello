(require 'org-trello-hash)
(require 'ert)
(require 'el-mock)

(ert-deftest test-orgtrello-hash/init-map-from ()
  (should (orgtrello-tests/hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ()) (orgtrello-hash/init-map-from nil))))

(ert-deftest test-orgtrello-hash/init-map-from ()
  (should (equal :data (orgtrello-hash/init-map-from :data))))

(ert-deftest test-orgtrello-hash/gethash-data ()
  (should (equal "some-method" (orgtrello-hash/gethash-data :method (orgtrello-hash/make-properties `((:method . "some-method"))))))
  (should (equal nil           (orgtrello-hash/gethash-data :method (orgtrello-hash/make-properties `((:inexistant . "some-method"))))))
  (should (equal nil           (orgtrello-hash/gethash-data :key nil)))
  (should (equal :value        (orgtrello-hash/gethash-data :key (orgtrello-hash/make-properties `((:key . :value))))))
  (should (equal nil           (orgtrello-hash/gethash-data :key (orgtrello-hash/make-properties `((:other-key . :value))))))
  (should (equal nil           (orgtrello-hash/gethash-data :key (orgtrello-hash/make-properties `((:key . nil)))))))

(ert-deftest test-orgtrello-hash/puthash-data ()
  (should (equal nil
                 (orgtrello-hash/puthash-data :key :value nil)))
  (should (orgtrello-tests/hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:key :value))
                                      (orgtrello-hash/puthash-data :key :value (orgtrello-hash/empty-hash))))
  (should (orgtrello-tests/hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:1 :2 :key :value))
                                      (orgtrello-hash/puthash-data :key :value (orgtrello-hash/make-properties '((:1 . :2) (:key . :other-value)))))))

(provide 'org-trello-hash-tests)
;;; org-trello-hash-tests.el ends here
