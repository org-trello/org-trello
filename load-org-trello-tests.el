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

(defvar *ORGTRELLO-TEST-NAMESPACES* '() "Org-trello test namespaces for development purposes.")
(setq *ORGTRELLO-TEST-NAMESPACES* '("test/utilities-tests.el"
                                    "test/org-trello-tests.el"
                                    "test/org-trello-setup-tests.el"
                                    "test/org-trello-action-tests.el"
                                    "test/org-trello-api-tests.el"
                                    "test/org-trello-backend-tests.el"
                                    "test/org-trello-entity-tests.el"
                                    "test/org-trello-entity-tests.el"
                                    "test/org-trello-cbx-tests.el"
                                    "test/org-trello-buffer-tests.el"
                                    "test/org-trello-controller-tests.el"
                                    "test/org-trello-data-tests.el"
                                    "test/org-trello-hash-tests.el"
                                    "test/org-trello-proxy-tests.el"
                                    "test/org-trello-query-tests.el"
                                    "test/org-trello-utils-tests.el"))

(defun orgtrello-test/load-namespaces! ()
  "Load the org-trello namespaces."
  (interactive)
  (mapc #'load-file *ORGTRELLO-TEST-NAMESPACES*)
  (require 'org-trello)
  (orgtrello-log/msg *OT/INFO* "Tests loaded!"))

(defun orgtrello-tests/find-next-error! ()
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

(orgtrello-test/load-namespaces!)

(define-key emacs-lisp-mode-map (kbd "C-c o d") 'orgtrello-test/load-namespaces!)
(define-key emacs-lisp-mode-map (kbd "C-c o D") 'orgtrello-tests/find-next-error!)

(require 'org-trello-action-tests)
(require 'org-trello-api-tests)
(require 'org-trello-backend-tests)
(require 'org-trello-buffer-tests)
(require 'org-trello-cbx-tests)
(require 'org-trello-controller-tests)
(require 'org-trello-data-tests)
(require 'org-trello-hash-tests)
(require 'org-trello-tests)
(require 'org-trello-proxy-tests)

(provide 'load-org-trello-tests)
;;; load-org-trello-tests ends here
