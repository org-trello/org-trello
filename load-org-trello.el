;;; load-org-trello.el --- Load the namespaces of org-trello in a dev or test context
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "."))

(defvar *ORGTRELLO-NAMESPACES* '("org-trello-action.el"
                                 "org-trello-api.el"
                                 "org-trello-backend.el"
                                 "org-trello-buffer.el"
                                 "org-trello-cbx.el"
                                 "org-trello-controller.el"
                                 "org-trello-data.el"
                                 "org-trello-hash.el"
                                 "org-trello-input.el"
                                 "org-trello-log.el"
                                 "org-trello-proxy.el"
                                 "org-trello-query.el"
                                 "org-trello-setup.el"
                                 "org-trello-utils.el"
                                 "org-trello.el")
  "Org-trello namespaces for development purposes.")

(defun org-trello/dev-load-namespaces! ()
  "Load the org-trello namespaces."
  (interactive)
  (mapc (lambda (it) (load-with-code-conversion it it)) *ORGTRELLO-NAMESPACES*))

(defun org-trello/dev-find-unused-definitions! ()
  "Find unused definitions."
  (interactive)
  (with-temp-file "/tmp/org-trello-find-unused-definitions.el"
    (erase-buffer)
    (mapc (lambda (it)
            (insert-file-contents it)
            (goto-char (point-max))) *ORGTRELLO-NAMESPACES*)
    (write-file "/tmp/org-trello-find-unused-definitions.el")
    (emacs-lisp-mode)
    (call-interactively 'emr-el-find-unused-definitions)))

(global-set-key (kbd "C-c o n") 'org-trello/dev-load-namespaces!)
(global-set-key (kbd "C-c o f") 'org-trello/dev-find-unused-definitions!)

(org-trello/dev-load-namespaces!)
(message "org-trello loaded!")

(require 'org-trello)

;; dev utils functions

(defun trace-functions (fns)
  "Trace functions FNS."
  (mapc 'trace-function fns))

(defun untrace-functions (fns)
  "Trace functions FNS."
  (mapc 'untrace-function fns))

(provide 'load-org-trello)
;;; load-org-trello.el ends here
