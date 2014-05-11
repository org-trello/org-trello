;;; load-org-trello.el --- Load the namespaces of org-trello in a dev or test context
;;; Commentary:
;; Not designed to be used from shell - helper to load the splitted namespaces from src/ and still be able to browse source code from emacs
;;; Code:

(add-to-list 'load-path (expand-file-name "."))

(mapc #'load-file '("org-trello-action.el"
                    "org-trello-api.el"
                    "org-trello-backend.el"
                    "org-trello-buffer.el"
                    "org-trello-cbx.el"
                    "org-trello-controller.el"
                    "org-trello-data.el"
                    "org-trello-db.el"
                    "org-trello-elnode.el"
                    "org-trello-hash.el"
                    "org-trello-input.el"
                    "org-trello-log.el"
                    "org-trello-proxy.el"
                    "org-trello-query.el"
                    "org-trello-server.el"
                    "org-trello-setup.el"
                    "org-trello-utils.el"
                    "org-trello-webadmin.el"
                    "org-trello.el"))

(require 'org-trello)

(provide 'load-org-trello)
;;; load-org-trello.el ends here
