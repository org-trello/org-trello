(require 'org-trello-cbx)

;; #################### orgtrello-buffer

(defun orgtrello-buffer/back-to-card! () "Given the current position, goes on the card's heading"
  (org-back-to-heading))

(defun orgtrello-buffer/--card-data-start-point () "Compute the first character of the card's text content."
  (save-excursion
    (orgtrello-buffer/back-to-card!)
    (end-of-line)
    (1+ (point))))

(defun orgtrello-buffer/--first-checkbox-point () "Compute the first position of the card's next checkbox."
  (save-excursion
    (orgtrello-buffer/back-to-card!)
    (orgtrello-cbx/--goto-next-checkbox)
    (1- (point))))

(defun orgtrello-buffer/extract-description-from-current-position () "Given the current position, extract the text content of current card."
  (let ((start (orgtrello-buffer/--card-data-start-point))
        (end   (orgtrello-buffer/--first-checkbox-point)))
    (when (< start end)
          (orgtrello-buffer/filter-out-properties
           (buffer-substring-no-properties start end)))))

(defun orgtrello-buffer/filter-out-properties (text-content) "Given a string, remove any org properties if any"
  (->> text-content
       (replace-regexp-in-string "^:.*" "")
       (s-trim-left)))

(provide 'org-trello-buffer)


