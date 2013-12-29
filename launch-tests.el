;; from shell: emacs -Q --batch -l ./launch-tests.el

(let ((current-directory (expand-file-name ".")))
  (load-file (concat current-directory "/" "org-trello-tests.el")))

(require 'org-trello)
(require 'org-trello-tests)
(ert-run-tests-batch-and-exit)
