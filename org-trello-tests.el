(require 'ert)

(message "Launching tests!")

(load-file "lib/orgtrello-hash.el")
(load-file "lib/orgtrello-data.el")
(load-file "lib/orgtrello-api.el")
(load-file "lib/orgtrello-query.el")
(load-file "lib/orgtrello.el")
(load-file "org-trello.el")

(load-file "tests/orgtrello-hash-tests.el")
(load-file "tests/orgtrello-data-tests.el")
(load-file "tests/orgtrello-api-tests.el")
(load-file "tests/orgtrello-query-tests.el")
(load-file "tests/orgtrello-tests.el")

(require 'orgtrello-hash-tests)
(require 'orgtrello-query-tests)
(require 'orgtrello-api-tests)
(require 'orgtrello-data-tests)
(require 'orgtrello-tests)

(message "Tests done!")
