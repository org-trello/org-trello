;;; org-trello-utils.el --- Utilities namespace
;;; Commentary:
;;; Code:

(defun orgtrello-utils/replace-in-string (expression-to-replace replacement-expression string-input)
  "Given an EXPRESSION-TO-REPLACE and a REPLACEMENT-EXPRESSION, replace such in STRING-INPUT."
  (replace-regexp-in-string expression-to-replace replacement-expression string-input 'fixed-case))

(provide 'org-trello-utils)
;;; org-trello-utils.el ends here
