;; Not designed to be used from shell - helper to load the splitted namespace from src/

(defun org-trello/load-files (splitted-files) "Load the src files."
  (dolist (current-file splitted-files)
      (load-file current-file)))

(load-file "./namespaces.el");; this will load *ORG-TRELLO-FILES*

(org-trello/load-files *ORG-TRELLO-FILES*)
