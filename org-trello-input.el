;;; org-trello-input.el --- User input related functions.
;;; Commentary:
;;; Code:

(require 'org-trello-log)

(defun orgtrello-input-read-not-empty (prompt)
  "Read input as long as input is empty.
PROMPT is the prefix string displayed for input."
  (let ((value nil))
    (while (or (null value) (string= "" value))
      (setq value (read-string prompt)))
    value))

(defalias 'orgtrello-input-read-string 'read-string)

(orgtrello-log-msg orgtrello-log-debug "orgtrello-input loaded!")

(provide 'org-trello-input)
;;; org-trello-input.el ends here
