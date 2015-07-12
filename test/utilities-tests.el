(require 'ert)
(require 'el-mock)

(defun orgtrello-tests/hash-equal (hash1 hash2) "Compare two hash tables to see whether they are equal."
  (and (= (hash-table-count hash1) (hash-table-count hash2))
       (catch 'flag (maphash (lambda (x y) (or (equal (gethash x hash2) y) (throw 'flag nil))) hash1)
              (throw 'flag t))))

(ert-deftest test-orgtrello-tests/hash-equal ()
  (should (orgtrello-tests/hash-equal (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO")))
                                      (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO")))))
  (should-not (orgtrello-tests/hash-equal (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "TODO")))
                                          (orgtrello-hash/make-properties `((:name . "some other name") (:keyword "DONE"))))))

(ert-deftest test-orgtrello-hash/make-transpose-properties ()
  (should (orgtrello-tests/hash-equal (orgtrello-hash/make-properties `(("some other name" . :name) ("TODO" . :keyword)))
                                      (orgtrello-hash/make-transpose-properties `((:name . "some other name") (:keyword . "TODO"))))))

(ert-deftest test-orgtrello-hash/empty-hash ()
  (should (orgtrello-tests/hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ())
                                      (orgtrello-hash/empty-hash))))

(defun org-trello-mode-test ()
  "Trigger org-trello-mode but shaped for the tests (without hooks)."
  (remove-hook 'org-trello-mode-on-hook 'orgtrello-controller/mode-on-hook-fn)
  (remove-hook 'org-trello-mode-off-hook 'orgtrello-controller/mode-off-hook-fn)
  (setq org-trello-mode-on-hook)
  (setq org-trello-mode-off-hook)
  (setq orgtrello-setup-use-position-in-checksum-computation 'please-do-use-position-in-checksum-computation)
  (call-interactively 'org-trello-mode))

(defun orgtrello-tests/prepare-buffer! ()
  "orgtrello-tests - Prepare the buffer to receive org-trello data."
  (orgtrello-buffer/indent-card-descriptions!)
  (orgtrello-buffer/indent-card-data!))

(defmacro orgtrello-tests/with-temp-buffer (text body-test &optional nb-lines-forward)
  `(with-temp-buffer
     (org-mode)
     (insert ,text)
     (forward-line (if ,nb-lines-forward ,nb-lines-forward -1))
     (org-trello-mode-test)
     (orgtrello-controller/setup-properties!)
     ,body-test))

(defmacro orgtrello-tests/with-temp-buffer-and-return-buffer-content (text body-test &optional nb-line-forwards)
  `(with-temp-buffer
     (org-mode)
     (insert ,text)
     (forward-line (if ,nb-line-forwards ,nb-line-forwards -1))
     (org-trello-mode-test)
     (orgtrello-controller/setup-properties!)
     ,body-test
     (buffer-substring-no-properties (point-min) (point-max))))

(defmacro orgtrello-tests/with-temp-buffer-indented-and-return-buffer-content (text body-test &optional nb-line-forwards)
  `(with-temp-buffer
     (org-mode)
     (insert ,text)
     (forward-line (if ,nb-line-forwards ,nb-line-forwards -1))
     (org-trello-mode-test)
     (orgtrello-controller/setup-properties!)
     ,body-test
     (orgtrello-tests/prepare-buffer!) ;; force the indentation without hook (show how it's done using hook at runtime)
     (buffer-substring-no-properties (point-min) (point-max))))

(defmacro orgtrello-tests/with-org-buffer (text body-test)
  `(with-temp-buffer
     (insert ,text)
     (org-mode)
     ,body-test))

(provide 'utilities-tests)
;;; utilities-tests.el ends here
