;;; orgtrello-hooks.el

(require 'orgtrello-data)

;; '(2 2 "IN PROGRESS" nil $HEADING_LABEL nil)
;;   "Return the components of the current heading.
;; This is a list with the following elements:
;; - the level as an integer
;; - the reduced level, different if `org-odd-levels-only' is set.
;; - the TODO keyword, or nil
;; - the priority character, like ?A, or nil if no priority is given
;; - the headline text itself, or the tags string if no headline text
;; - the tags string, or nil."

(defun org-trello--org-trigger-hook (list)
  (let* ((type (plist-get list :type))
         (from (plist-get list :from))
         (to   (plist-get list :to)))
    (message "type: %s\nfrom: %s\nto: %s" type from to)))

(defun orgtrello-hooks--org-after-todo-state-change-hook ()
  (let* ((org-metadata (org-heading-components))
         (metadata (orgtrello-data--get-metadata org-metadata)))
    (message "new creation\nstate:%s\norg-metadata:%S\nmetadata:%S" org-state org-metadata metadata)))

;; (add-hook <the-hook> <the-function-to-add-to-the-hook>)
;; means to remove hook
;; (remove-hook <the-hook> <the-function-to-remove-from-the-hook>)

;; (add-hook 'org-trigger-hook 'orgtrello-hooks--org-trigger-hook)
;; (remove-hook 'org-trigger-hook 'orgtrello-hooks--org-trigger-hook)

(add-hook 'org-after-todo-state-change-hook 'orgtrello-hooks--org-after-todo-state-change-hook)
;; (remove-hook 'org-trigger-hook 'orgtrello-hooks--org-after-todo-state-change-hook)

(defun orgtrello-hooks--org-insert-heading-hook ()
  (let* ((org-metadata (org-heading-components))
         (metadata (orgtrello-data--get-metadata org-metadata)))
    (message "new creation: %S\n%S" org-metadata metadata)))

(add-hook 'org-insert-heading-hook 'orgtrello-hooks--org-insert-heading-hook)

(defun orgtrello-hooks--org-after-promote-entry-hook ()
  (let* ((org-metadata (org-heading-components))
         (metadata (orgtrello-data--get-metadata org-metadata)))
    (message "promotion: %S\n%S" org-metadata metadata)))

(add-hook 'org-after-promote-entry-hook 'orgtrello-hooks--org-after-promote-entry-hook)

(defun orgtrello-hooks--org-after-demote-entry-hook ()
  (let* ((org-metadata (org-heading-components))
         (metadata (orgtrello-data--get-metadata org-metadata)))
    (message "demotion: %S\n%S" org-metadata metadata)))

(add-hook 'org-after-demote-entry-hook 'orgtrello-hooks--org-after-demote-entry-hook)

(provide 'orgtrello-hooks)

;;; orgtrello-hooks.el ends here
