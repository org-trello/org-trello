(defun trace (label e)
  "Decorator for some inaccessible code to easily 'message'."
  (message "TRACE: %s: %S" label e)
  e)

(defun -trace (e &optional label)
  "Decorator for some inaccessible code to easily 'message'."
  (progn
    (if label
        (trace label e)
      (message "TRACE: %S" e))
    e))

(provide 'org-trello-dev)
;;; org-trello-dev.el ends here
