(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(message "Launching tests!")

;; load code prod
(load-file "load-namespaces.el")
;; load test code
(org-trello/load-namespaces *ORG-TRELLO-TEST-FILES*)

;; behaviour of expectations changed
(setq expectations-execute-at-once t)

(provide 'org-trello-tests)
;;; org-trello-tests ends here
