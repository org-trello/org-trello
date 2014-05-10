;; from shell: emacs -Q --batch -l ./launch-tests.el

(load-file (expand-file-name "./load-org-trello-tests.el"))

(require 'load-org-trello-tests)

(ert-run-tests-batch-and-exit)
