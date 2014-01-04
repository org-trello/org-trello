;; Just to reference the org-trello `namespaces`

(add-to-list 'load-path (expand-file-name "."))

(defvar *ORG-TRELLO-FILES* '("src/header.el"
                             "src/log.el"
                             "src/setup.el"
                             "src/hash.el"
                             "src/action.el"
                             "src/data.el"
                             "src/cbx.el"
                             "src/api.el"
                             "src/query.el"
                             "src/elnode.el"
                             "src/webadmin.el"
                             "src/proxy.el"
                             "src/controller.el"
                             "src/org-trello.el"
                             "src/footer.el") "Splitted org-trello files.")

(provide 'org-trello-namespaces)
