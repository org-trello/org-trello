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
(setq expectations-execute-at-once t)       ;; only execute the current sexp at point
;; (setq expectations-execute-at-once 'all) ;; all tests are executed once hit C-M-x after one sexp

;; force loading

(require 'load-org-trello)

(defun org-trello/test-load-namespaces! ()
  "Load the org-trello namespaces."
  (interactive)
  (mapc #'load-file '("test/utilities-tests.el"
                      "test/org-trello-tests.el"
                      "test/org-trello-action-tests.el"
                      "test/org-trello-api-tests.el"
                      "test/org-trello-backend-tests.el"
                      "test/org-trello-buffer-tests.el"
                      "test/org-trello-cbx-tests.el"
                      "test/org-trello-controller-tests.el"
                      "test/org-trello-data-tests.el"
                      "test/org-trello-db-tests.el"
                      "test/org-trello-elnode-tests.el"
                      "test/org-trello-hash-tests.el"
                      "test/org-trello-proxy-tests.el"
                      "test/org-trello-query-tests.el"
                      "test/org-trello-server-tests.el"
                      "test/org-trello-utils-tests.el"
                      "test/org-trello-webadmin-tests.el")))

(org-trello/test-load-namespaces!)

(require 'org-trello-action-tests)
(require 'org-trello-api-tests)
(require 'org-trello-backend-tests)
(require 'org-trello-buffer-tests)
(require 'org-trello-cbx-tests)
(require 'org-trello-controller-tests)
(require 'org-trello-data-tests)
(require 'org-trello-db-tests)
(require 'org-trello-elnode-tests)
(require 'org-trello-hash-tests)
(require 'org-trello-tests)
(require 'org-trello-proxy-tests)

(provide 'load-org-trello-tests)
;;; load-org-trello-tests ends here
