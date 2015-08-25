;;; org-trello-tools.el --- Load the namespaces of org-trello in a dev or test context
;;; Commentary:
;;; Code:

(defvar org-trello-home (or (getenv "ORGTRELLO_HOME") (expand-file-name "."))
  "Org-trello home.")

(add-to-list 'load-path org-trello-home)

(defvar org-trello--namespaces '() "Org-trello namespaces for development purposes.")
(setq org-trello--namespaces '("org-trello-log.el"
                               "org-trello-setup.el"
                               "org-trello-action.el"
                               "org-trello-api.el"
                               "org-trello-backend.el"
                               "org-trello-entity.el"
                               "org-trello-cbx.el"
                               "org-trello-date.el"
                               "org-trello-buffer.el"
                               "org-trello-controller.el"
                               "org-trello-hash.el"
                               "org-trello-data.el"
                               "org-trello-input.el"
                               "org-trello-proxy.el"
                               "org-trello-query.el"
                               "org-trello-utils.el"
                               "org-trello-deferred.el"
                               "org-trello.el"))

(defun org-trello-tools-load-namespaces ()
  "Load the org-trello namespaces."
  (interactive)
  ;; recompile code
  (mapc (lambda (it) (load-with-code-conversion (concat org-trello-home "/" it) it)) org-trello--namespaces)
  (require 'org-trello)
  ;; reload bindings
  (custom-set-variables
   '(org-trello-current-prefix-keybinding "C-c o")
   '(orgtrello-log-level orgtrello-log-info))
  (orgtrello-log-msg orgtrello-log-info "Code loaded!"))

(defun org-trello-tools-remove-bindings ()
  "Remove bindings."
  (interactive)
  ;; Remove old bindings
  (mapc 'orgtrello-setup-remove-local-prefix-mode-keybinding '("C-c o"
                                                               "C-c a"
                                                               "C-c x"
                                                               "C-c z"))
  ;; install the default one
  (orgtrello-setup-install-local-prefix-mode-keybinding "C-c o"))

(defun org-trello-tools-find-unused-definitions ()
  "Find unused definitions."
  (interactive)
  (let ((filename "/tmp/org-trello-find-unused-definitions.el"))
    (with-temp-file filename
      (erase-buffer)
      (mapc (lambda (file)
              (insert-file-contents file)
              (goto-char (point-max))) org-trello--namespaces)
      (emacs-lisp-mode)
      (write-file filename)
      (call-interactively 'emr-el-find-unused-definitions))))

(defun org-trello-tools-load-tests ()
  "Load the load-org-trello-tests.el file."
  (interactive)
  (load-file "load-org-trello-tests.el"))

(require 'org-trello)

(define-key emacs-lisp-mode-map (kbd "C-c o r") 'org-trello-tools-load-namespaces)
(define-key emacs-lisp-mode-map (kbd "C-c o t") 'org-trello-tools-load-tests)
(define-key emacs-lisp-mode-map (kbd "C-c o f") 'org-trello-tools-find-unused-definitions)

(define-key org-trello-mode-map (kbd "C-c o r") 'org-trello-tools-load-namespaces)
(define-key org-trello-mode-map (kbd "C-c o t") 'org-trello-tools-load-tests)
(define-key org-trello-mode-map (kbd "C-c o f") 'org-trello-tools-find-unused-definitions)

(orgtrello-log-msg orgtrello-log-info "org-trello loaded!")

(provide 'org-trello-tools)
;;; org-trello-tools.el ends here
