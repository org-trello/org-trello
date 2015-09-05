;;; org-trello-input.el --- User input related functions.
;;; Commentary:
;;; Code:

(require 'org-trello-log)
(require 'ido)

(defun orgtrello-input-read-not-empty (prompt)
  "Read input as long as input is empty.
PROMPT is the prefix string displayed for input.
:: () -> String"
  (let ((value nil))
    (while (or (null value) (string= "" value))
      (setq value (read-string prompt)))
    value))

(defalias 'orgtrello-input-read-string 'read-string
  "Read input from user which can be null.
:: () -> String")

(defun orgtrello-input-read-string-completion (prompt choices)
  "Read input from user with completing mechanism.
PROMPT is the prompt for user to see.
CHOICES is the list of possibilities with completing properties.
:: String -> [a] -> a"
  (if (eq 'default org-trello-input-completion-mechanism)
      (ido-completing-read prompt choices nil 'do-match)
    (helm-comp-read prompt choices)))

(provide 'org-trello-input)
;;; org-trello-input.el ends here
