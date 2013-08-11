;; from shell: emacs -Q --batch -l ./launch-tests.el

(load-file (concat (expand-file-name ".") "/org-trello-tests.el"))
(require 'org-trello)
(require 'org-trello-tests)
(ert-run-tests-batch-and-exit)
