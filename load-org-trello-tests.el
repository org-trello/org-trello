;;; load-org-trello-tests.el --- Load the namespaces of org-trello and org-trello-tests in a test context
;;; Commentary:
;;; Code:

(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(message "Launching tests!")

;; load code prod
(load-file "load-org-trello.el")

;; Add test folder to the load path
(add-to-list 'load-path (expand-file-name "./test"))

(message "Loading tests done!")

;; behaviour of expectations changed
(setq expectations-execute-at-once 'all)       ;; only execute the current sexp at point
;; (setq expectations-execute-at-once 'all) ;; all tests are executed once hit C-M-x after one sexp

;; force loading

(require 'load-org-trello)

(defvar orgtrello-test--namespaces '() "Org-trello test namespaces for development purposes.")
(setq orgtrello-test--namespaces '("test/utilities-test.el"
                                   "test/org-trello-setup-test.el"
                                   "test/org-trello-action-test.el"
                                   "test/org-trello-api-test.el"
                                   "test/org-trello-backend-test.el"
                                   "test/org-trello-entity-test.el"
                                   "test/org-trello-entity-test.el"
                                   "test/org-trello-cbx-test.el"
                                   "test/org-trello-buffer-test.el"
                                   "test/org-trello-controller-test.el"
                                   "test/org-trello-data-test.el"
                                   "test/org-trello-hash-test.el"
                                   "test/org-trello-proxy-test.el"
                                   "test/org-trello-query-test.el"
                                   "test/org-trello-utils-test.el"))

(defun orgtrello-test-load-namespaces ()
  "Load the org-trello namespaces."
  (interactive)
  (mapc #'load-file orgtrello-test--namespaces)
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

(orgtrello-test-load-namespaces)

(define-key emacs-lisp-mode-map (kbd "C-c o d") 'orgtrello-test-load-namespaces)
(define-key emacs-lisp-mode-map (kbd "C-c o D") 'orgtrello-tests-find-next-error)

(fset 'convert-expectations-to-ert-deftest
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("expectations\210deert-deftestdesc\210\363\363 (\206\206\202" 0 "%d")) arg)))

(fset 'convert-expect-to-should-equal
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 101 120 112 101 99 116 13 134217736 115 104 111 117 108 100 32 40 101 113 117 97 108 company-dummy-event 67108905 67108905 134217730 134217730 134217734 134217734] 0 "%d")) arg)))

(define-key emacs-lisp-mode-map (kbd "C-c o c t") 'convert-expectations-to-ert-deftest)
(define-key emacs-lisp-mode-map (kbd "C-c o c e") 'convert-expect-to-should-equal)

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
