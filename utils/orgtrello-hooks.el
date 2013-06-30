(defun org-trello--extends-org-trigger-hook (list)
  (let* ((type (plist-get list :type))
         (from (plist-get list :from))
         (to   (plist-get list :to)))
    (message "type: %s\nfrom: %s\nto: %s" type from to)))

(defun org-trello--extends-org-after-todo-state-change-hook ()
  (message "%S" org-state))

;; (add-hook <the-hook> <the-function-to-add-to-the-hook>)
;; means to remove hook
;; (remove-hook <the-hook> <the-function-to-remove-from-the-hook>)

;; (add-hook 'org-trigger-hook 'org-trello--extends-org-trigger-hook)
;; (remove-hook 'org-trigger-hook 'org-trello--extends-org-trigger-hook)

(add-hook 'org-after-todo-state-change-hook 'org-trello--extends-org-after-todo-state-change-hook)
;; (remove-hook 'org-trigger-hook 'org-trello--extends-org-after-todo-state-change-hook)

(defun org-trello--org-insert-heading-hook ()
  (message "new creation"))

(add-hook 'org-insert-heading-hook 'org-trello--org-insert-heading-hook)

(provide 'orgtrello)

(defun org-trello--org-after-promote-entry-hook ()
  (message "promotion"))

(add-hook 'org-after-promote-entry-hook 'org-trello--org-after-promote-entry-hook)

(defun org-trello--org-after-demote-entry-hook ()
  (message "demotion"))

(add-hook 'org-after-demote-entry-hook 'org-trello--org-after-demote-entry-hook)

(provide 'orgtrello-hooks)

;;; orgtrello-hooks.el ends here
