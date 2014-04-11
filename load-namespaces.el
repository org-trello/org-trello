;; Not designed to be used from shell - helper to load the splitted namespaces from src/ and still be able to browse source code from emacs

(defvar *org-trello-dir* (expand-file-name "."))

(defun org-trello/full-file (filename)
  "Compute the full file path"
  (format "%s/%s" *org-trello-dir* filename))

(defun org-trello/load-ns (current-ns-file) "Load the current namespace file."
  (message "org-trello file: '%s' loading..." current-ns-file)
  (with-temp-file current-ns-file
    (insert-file-contents current-ns-file)
    (eval-buffer nil nil current-ns-file t t)
    (message "org-trello file: '%s' loaded!" current-ns-file)))

(defun org-trello/load-namespaces (splitted-files) "Load the src files."
  (message "org-trello files: %s" splitted-files)
  (mapc 'org-trello/load-ns splitted-files))

(defun org-trello/dev-load-namespaces ()
  "Load the namespace with interactive command"
  (interactive)
  ;; load the namespaces.el files references the namespaces from which generate the org-trello.el + load the src/dev.el namespace for some dev tools
  (mapc 'load-file `(,(org-trello/full-file "namespaces.el") ,(org-trello/full-file "src/dev.el")))
  (org-trello/load-namespaces *ORG-TRELLO-FILES*))

(org-trello/dev-load-namespaces)

(require 'org-trello-namespaces)

(org-trello/load-namespaces *ORG-TRELLO-FILES*)
