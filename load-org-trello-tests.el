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
(setq expectations-execute-at-once t)

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
(require 'org-trello-query-tests)
(require 'org-trello-server-tests)
(require 'utilities-tests)
(require 'org-trello-utils-tests)
(require 'org-trello-webadmin-tests)

(provide 'load-org-trello-tests)
;;; load-org-trello-tests ends here
