;; Just to reference the org-trello `namespaces`

(add-to-list 'load-path (expand-file-name "."))

(defconst *ORG-TRELLO-FILES* '("src/header.el"
                               "src/log.el"
                               "src/utils.el"
                               "src/setup.el"
                               "src/hash.el"
                               "src/action.el"
                               "src/db.el"
                               "src/data.el"
                               "src/cbx.el"
                               "src/api.el"
                               "src/query.el"
                               "src/backend.el"
                               "src/elnode.el"
                               "src/proxy.el"
                               "src/webadmin.el"
                               "src/server.el"
                               "src/buffer.el"
                               "src/input.el"
                               "src/controller.el"
                               "src/org-trello.el"
                               "src/footer.el") "Splitted org-trello files.")

(defconst *ORG-TRELLO-TEST-FILES* '("test/utilities-tests.el"         ;; utils function for the following tests buffer
                                    "test/utils-tests.el"
                                    "test/hash-tests.el"
                                    "test/action-tests.el"
                                    "test/db-tests.el"
                                    "test/data-tests.el"
                                    "test/cbx-tests.el"
                                    "test/api-tests.el"
                                    "test/query-tests.el"
                                    "test/backend-tests.el"
                                    "test/elnode-tests.el"
                                    "test/webadmin-tests.el"
                                    "test/server-tests.el"
                                    "test/proxy-tests.el"
                                    "test/buffer-tests.el"
                                    "test/controller-tests.el"
                                    "test/org-trello-tests.el") "Splitted org-trello tests files.")

(provide 'org-trello-namespaces)
