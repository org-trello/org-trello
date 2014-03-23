;; Just to reference the org-trello `namespaces`

(add-to-list 'load-path (expand-file-name "."))

(defconst *ORG-TRELLO-FILES* '("src/header.el"
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
                               "src/buffer.el"
                               "src/controller.el"
                               "src/org-trello.el"
                               "src/footer.el") "Splitted org-trello files.")

(defconst *ORG-TRELLO-TEST-FILES* '("test/utils-tests.el"         ;; utils function for the following tests buffer
                                    "test/hash-tests.el"
                                    "test/action-tests.el"
                                    "test/data-tests.el"
                                    "test/cbx-tests.el"
                                    "test/api-tests.el"
                                    "test/query-tests.el"
                                    "test/elnode-tests.el"
                                    "test/webadmin-tests.el"
                                    "test/proxy-tests.el"
                                    "test/buffer-tests.el"
                                    "test/controller-tests.el"
                                    "test/org-trello-tests.el") "Splitted org-trello tests files.")

(provide 'org-trello-namespaces)
