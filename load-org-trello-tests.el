;;; load-org-trello-tests.el --- Load the namespaces of org-trello and org-trello-tests in a test context
;;; Commentary:
;;; Code:

(require 'ert)
(require 'el-mock)

(message "Launching tests!")

;; load code prod
(load-file "load-org-trello.el")

;; Add test folder to the load path
(add-to-list 'load-path (expand-file-name "./test"))

(message "Loading tests done!")

;; force loading

(require 'load-org-trello)

(defvar orgtrello-tests--namespaces '() "Org-trello test namespaces for development purposes.")
(setq orgtrello-tests--namespaces '("test/utilities-test.el"
                                    "test/org-trello-setup-test.el"
                                    "test/org-trello-action-test.el"
                                    "test/org-trello-api-test.el"
                                    "test/org-trello-backend-test.el"
                                    "test/org-trello-entity-test.el"
                                    "test/org-trello-entity-test.el"
                                    "test/org-trello-cbx-test.el"
                                    "test/org-trello-date-test.el"
                                    "test/org-trello-buffer-test.el"
                                    "test/org-trello-controller-test.el"
                                    "test/org-trello-data-test.el"
                                    "test/org-trello-hash-test.el"
                                    "test/org-trello-proxy-test.el"
                                    "test/org-trello-query-test.el"
                                    "test/org-trello-utils-test.el"))

(defun orgtrello-tests-number-of (regexp buf)
  "Given a REGEXP, count all occurences on BUFFER."
  (save-excursion
    (with-temp-buffer
      (insert-file buf)
      (goto-char (point-min))
      (let ((c 0))
        (while (re-search-forward regexp nil t)
          (setq c (1+ c)))
        c))))

(defun orgtrello-tests-interactive-number-of (regexp &optional ask-buffer)
  "Given a REGEXP, compute a number of occurrences.
If region is active, will use the region highlight as buffer.
Otherwise, if ASK-BUFFER is not nil, will ask the user.
Otherwise, default to current buffer."
  (let ((buf (if (region-active-p)
                 (buffer-substring (region-beginning) (region-end))
               (if ask-buffer
                   (read-string "Buffer name: ")
                 (current-buffer)))))
    (message "buf: %s" buf)
    (-when-let (c (orgtrello-tests-number-of regexp buf))
      (insert (int-to-string c)))))

(defun orgtrello-tests-count-functions (&optional ask-buffer)
  "Count the number of `def-un' or `def-alias'.
If region is active, will use the region highlight as buffer.
Otherwise, if ASK-BUFFER is not nil, will ask the user.
Otherwise, default to current buffer."
  (interactive "P")
  (orgtrello-tests-interactive-number-of "\\(defun\\).*" ask-buffer))

(defun orgtrello-tests-count-number-tests (&optional ask-buffer)
  "Count the number of `ert-def-test'.
If region is active, will use the region highlight as buffer.
Otherwise, if ASK-BUFFER is not nil, will ask the user.
Otherwise, default to current buffer."
  (interactive "P")
  (orgtrello-tests-interactive-number-of "\\(ert-deftest\\).*" ask-buffer))

(defun orgtrello-tests-load-namespaces ()
  "Load the org-trello namespaces."
  (interactive)
  (mapc #'load-file orgtrello-tests--namespaces)
  (require 'org-trello)
  (orgtrello-log-msg orgtrello-log-info "Tests loaded!"))

(defun orgtrello-tests-find-next-error ()
  "Find the next test error"
  (interactive)
  (with-current-buffer "*compilation*"
    (goto-char (point-min))
    (if (search-forward "(ert-test-failed" nil 'noerror)
        (progn
          (switch-to-buffer "*compilation*" nil 'same-window)
          (search-forward "(ert-test-failed" nil 'noerror)
          (forward-line 10))
      (message "All is good!"))))

(orgtrello-tests-load-namespaces)

(define-key emacs-lisp-mode-map (kbd "C-c o d") 'orgtrello-tests-load-namespaces)
(define-key emacs-lisp-mode-map (kbd "C-c o D") 'orgtrello-tests-find-next-error)

(require 'org-trello-action-test)
(require 'org-trello-api-test)
(require 'org-trello-backend-test)
(require 'org-trello-buffer-test)
(require 'org-trello-cbx-test)
(require 'org-trello-controller-test)
(require 'org-trello-data-test)
(require 'org-trello-hash-test)
(require 'org-trello-setup-test)
(require 'org-trello-proxy-test)

(provide 'load-org-trello-tests)
;;; load-org-trello-tests ends here
