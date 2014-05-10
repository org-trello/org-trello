;; Not designed to be used from shell - helper to load the splitted namespaces from src/ and still be able to browse source code from emacs

(add-to-list 'load-path (expand-file-name "."))

(require 'org-trello)

;;; load-namespaces.el ends here
