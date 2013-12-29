;; Not designed to be used from shell - helper to load the splitted namespaces from src/ and still be able to browse source code from emacs

(defun org-trello/load-ns (current-ns-file) "Load the current namespace file."
  (message "org-trello file: %s loading..." current-ns-file)
  (with-temp-file current-ns-file
    (insert-file-contents current-ns-file)
    (eval-buffer nil t current-ns-file t t)))

(defun org-trello/load-namespaces (splitted-files) "Load the src files."
  (message "org-trello files: %s" splitted-files)
  (mapcar (lambda (current-ns-file) (org-trello/load-ns current-ns-file)) splitted-files))

(load-file "./namespaces.el")
(require 'org-trello-namespaces)

(org-trello/load-namespaces *ORG-TRELLO-FILES*)
