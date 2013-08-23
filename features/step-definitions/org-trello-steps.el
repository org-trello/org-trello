(require 'org-trello)

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

(And "^I start the timer$" (lambda () (orgtrello-timer/start)))

(And "^I stop the timer$" (lambda () (orgtrello-timer/stop)))

(Given "^I am in buffer \"\\(.+\\)\"$"
  (lambda (buffer-name)
    (set-buffer buffer-name)))

(And "^I go to position \"\\(.+\\)\"$"
      (lambda (position)
        (goto-char (read position))))

(Then "^I should have a file \"\\(.+\\)\" with \"\\(.+\\)\"$"
  (lambda (file content)
    (let* ((dir-level-file (file-name-directory file))
           (full-file (format "%s/%s" (orgtrello-proxy/--compute-entity-level-dir dir-level-file) file)))
      (assert (and (file-exists-p full-file)
                   (message "file content: %S" (orgtrello-admin/--content-file full-file))
                   (equal (orgtrello-admin/--content-file full-file) (read content))) nil
              (message "Expected the file to exist and that the content match!")))))

;;(equal '((buffername . "org-trello-itest.org") (position . 100) (level . 1) (keyword . nil) (title . "card") (id) (due)) (read "((buffername . \"org-trello-itest.org\") (position . 100) (level . 1) (keyword . nil) (title . \"card\") (id) (due))"))

;; (And "^I save the buffer$"
;;        (lambda ()
;;          (save-current-buffer)))

;; (Then "^the buffer should be saved$"
;;        (lambda ()
;;          (assert (not (buffer-modified-p)) nil
;;                  "The buffer should be saved, but was modified.")))
