;; will create org-trello.el file before the packaging
(with-temp-file "org-trello.el"
    (dolist (current-file (nreverse '("src/header.el"
                                      "src/setup.el"
                                      "src/log.el"
                                      "src/hash.el"
                                      "src/cbx.el"
                                      "src/data.el"
                                      "src/api.el"
                                      "src/query.el"
                                      "src/action.el"
                                      "src/proxy.el"
                                      "src/webadmin.el"
                                      "src/proxy-install.el"
                                      "src/org-trello.el"
                                      "src/footer.el")))
      (insert-file current-file)))
