(require 'org-trello-hash)
(require 'ert)
(require 'el-mock)
(require 'dash)

(ert-deftest test-orgtrello-hash-init-map-from ()
  (should (eq 0
              (hash-table-count (orgtrello-hash-init-map-from nil))))

  (should (equal :data (orgtrello-hash-init-map-from :data))))

(ert-deftest test-orgtrello-hash-gethash-data ()
  (should (equal "some-method" (orgtrello-hash-gethash-data :method (orgtrello-hash-make-properties '((:method . "some-method"))))))
  (should (equal nil           (orgtrello-hash-gethash-data :method (orgtrello-hash-make-properties '((:inexistant . "some-method"))))))
  (should (equal nil           (orgtrello-hash-gethash-data :key nil)))
  (should (equal :value        (orgtrello-hash-gethash-data :key (orgtrello-hash-make-properties '((:key . :value))))))
  (should (equal nil           (orgtrello-hash-gethash-data :key (orgtrello-hash-make-properties '((:other-key . :value))))))
  (should (equal nil           (orgtrello-hash-gethash-data :key (orgtrello-hash-make-properties '((:key . nil)))))))

(ert-deftest test-orgtrello-hash-puthash-data ()
  (should (equal nil
                 (orgtrello-hash-puthash-data :key :value nil)))
  (should (eq :value-1 (->> (orgtrello-hash-empty-hash)
                            (orgtrello-hash-puthash-data :key :value-1)
                            (orgtrello-hash-gethash-data :key))))
  (should (eq :value-2 (->> (orgtrello-hash-make-properties '((:1 . :2) (:key . :other-value)))
                            (orgtrello-hash-puthash-data :key :value-2)
                            (orgtrello-hash-gethash-data :key)))))

(ert-deftest test-orgtrello-hash-keys ()
  (should (equal '("key0" "key1" "key2")
                 (orgtrello-hash-keys (orgtrello-hash-make-properties `(("key0" . "val0") ("key1" . "val1") ("key2" . "val2"))))))
  (should-not (orgtrello-hash-keys (orgtrello-hash-empty-hash))))

(ert-deftest test-orgtrello-hash-values ()
  (should (equal '("val0" "val1" "val2")
                 (orgtrello-hash-values (orgtrello-hash-make-properties `(("key0" . "val0") ("key1" . "val1") ("key2" . "val2"))))))
  (should-not (orgtrello-hash-keys (orgtrello-hash-empty-hash))))

(provide 'org-trello-hash-test)
;;; org-trello-hash-test.el ends here
