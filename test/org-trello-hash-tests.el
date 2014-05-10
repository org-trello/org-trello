(require 'org-trello-hash)
(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(ert-deftest testing-orgtrello-hash/init-map-from ()
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ()) (orgtrello-hash/init-map-from nil))))

(expectations (desc "orgtrello-hash/init-map-from")
              (expect :data (orgtrello-hash/init-map-from :data)))

(expectations
  (expect "some-method" (orgtrello-hash/gethash-data :method (orgtrello-hash/make-properties `((:method . "some-method")))))
  (expect nil           (orgtrello-hash/gethash-data :method (orgtrello-hash/make-properties `((:inexistant . "some-method")))))
  (expect nil           (orgtrello-hash/gethash-data :key nil))
  (expect :value        (orgtrello-hash/gethash-data :key (orgtrello-hash/make-properties `((:key . :value)))))
  (expect nil           (orgtrello-hash/gethash-data :key (orgtrello-hash/make-properties `((:other-key . :value)))))
  (expect nil           (orgtrello-hash/gethash-data :key (orgtrello-hash/make-properties `((:key . nil))))))

(expectations
  (expect nil
    (orgtrello-hash/puthash-data :key :value nil))
  (expect t
    (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:key :value))
                (orgtrello-hash/puthash-data :key :value (orgtrello-hash/empty-hash))))
  (expect t
    (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:1 :2 :key :value))
                (orgtrello-hash/puthash-data :key :value (orgtrello-hash/make-properties '((:1 . :2) (:key . :other-value)))))))

(provide 'org-trello-hash-tests)
;;; org-trello-hash-tests.el ends here
