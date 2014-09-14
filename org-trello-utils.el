;;; org-trello-utils.el --- Utilities namespace
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(defun orgtrello-utils/replace-in-string (expression-to-replace replacement-expression string-input)
  "Given an EXPRESSION-TO-REPLACE and a REPLACEMENT-EXPRESSION, replace such in STRING-INPUT."
  (replace-regexp-in-string expression-to-replace replacement-expression string-input 'fixed-case))

(defun orgtrello-utils/symbol (sym n)
  "Compute the repetition of a symbol SYM N times as a string."
  (--> n
    (-repeat it sym)
    (s-join "" it)))

(defun orgtrello-utils/space (n)
  "Given a level, compute N times the number of spaces for an org checkbox entry."
  (orgtrello-utils/symbol " "  n))

(provide 'org-trello-utils)
;;; org-trello-utils.el ends here
