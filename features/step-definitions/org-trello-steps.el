(When "^I go to the word \"\\(.+\\)\"$"
      (lambda (word)
        (goto-char (point-min))
        (let ((search (re-search-forward word nil t)))
          (assert search nil
              (message "Cannot go to character '%s' since it does not exist in the current buffer: %s" search word)))))

(Then "^I should have a property \"\\(.+\\)\"$"
      (lambda (property)
        (let ((p (org-entry-get (point) property)))
          (message "Property %s should not be nil: %s" property p)
          (assert (and p (not (string= "" p))) nil
            (message "Expected to have the property '%s' present with a non nil value." property)))))

(Then "^I should not have a property \"\\(.+\\)\"$"
      (lambda (property)
        (let ((p (org-entry-get (point) property)))
          (message "Property %s should be nil: %s" property p)
          (assert (or (not p) (string= "" p)) nil
                  (message "Expected to have the property '%s' inexistant or without value." property)))))

(And "^I save the buffer$"
       (lambda ()
         (save-current-buffer)))

(Then "^the buffer should be saved$"
       (lambda ()
         (assert (not (buffer-modified-p)) nil
                 "The buffer should be saved, but was modified.")))
