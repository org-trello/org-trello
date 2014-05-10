(defun orgtrello-input/read-not-empty! (input-message)
  "Function dedicated to continue asking for input while the input typed is incorrect."
  (let ((value nil))
    (while (or (null value) (string= "" value))
      (setq value (read-string input-message)))
    value))

;; (orgtrello-input/read-not-empty! "some-value? ")

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-input loaded!")

(provide 'org-trello-input)
;;; org-trello-input.el ends here
