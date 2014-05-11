(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(defun hash-equal (hash1 hash2) "Compare two hash tables to see whether they are equal."
  (and (= (hash-table-count hash1) (hash-table-count hash2))
       (catch 'flag (maphash (lambda (x y) (or (equal (gethash x hash2) y) (throw 'flag nil))) hash1)
              (throw 'flag t))))

(expectations (desc "hash-equal")
 (expect t (hash-equal (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO")))
                       (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO")))))
 (expect nil (hash-equal (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO")))
                         (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "DONE"))))))

(expectations (desc "orgtrello-hash/make-transpose-properties")
  (expect t (hash-equal (orgtrello-hash/make-properties `(("some other name" . :name) ("TODO" . :keyword)))
                        (orgtrello-hash/make-transpose-properties `((:name . "some other name") (:keyword . "TODO"))))))

(expectations (desc "orgtrello-hash/empty-hash")
 (expect t (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ())
                       (orgtrello-hash/empty-hash))))

(defun org-trello-mode-test () "Trigger org-trello-mode but shaped for the tests."
  (remove-hook 'org-trello-mode-on-hook 'orgtrello-controller/mode-on-hook-fn)  (add-hook 'org-trello-mode-on-hook (lambda () (orgtrello-controller/mode-on-hook-fn t)))
  (remove-hook 'org-trello-mode-off-hook 'orgtrello-controller/mode-off-hook-fn) (add-hook 'org-trello-mode-off-hook (lambda () (orgtrello-controller/mode-off-hook-fn t)) )
  (org-trello-mode))

(defmacro orgtrello-tests/with-temp-buffer (text body-test &optional nb-lines-forward)
  `(with-temp-buffer
     (org-mode)
     (insert ,text)
     (forward-line (if ,nb-lines-forward ,nb-lines-forward -1))
     (org-trello-mode-test)
     (orgtrello-controller/setup-properties)
     ,body-test))

(defmacro orgtrello-tests/with-temp-buffer-and-return-buffer-content (text body-test &optional nb-line-forwards)
  `(with-temp-buffer
     (org-mode)
     (insert ,text)
     (forward-line (if ,nb-line-forwards ,nb-line-forwards -1))
     (org-trello-mode-test)
     (orgtrello-controller/setup-properties)
     ,body-test
     (buffer-substring-no-properties (point-min) (point-max))))

(defmacro orgtrello-tests/with-org-buffer (text body-test)
  `(with-temp-buffer
     (insert ,text)
     (org-mode)
     ,body-test))

(provide 'utilities-tests)
;;; utilities-tests.el ends here
