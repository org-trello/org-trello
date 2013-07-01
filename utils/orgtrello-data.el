;;; orgtrello-data.el

(require 'orgtrello-hash)

(defun orgtrello-data-metadata ()
  (let* ((org-metadata (org-heading-components)))
    (orgtrello-data--get-metadata org-metadata)))

(defun orgtrello-data--get-level (heading-metadata)
  "Given the heading-metadata returned by the function 'org-heading-components, extract the level."
  (car heading-metadata))

(defun orgtrello-data--get-keyword (heading-metadata)
  "Given the heading-metadata returned by the function 'org-heading-components, extract the keyword."
  (third heading-metadata))

(defun orgtrello-data--get-title (heading-metadata)
  "Given the heading-metadata returned by the function 'org-heading-components, extract the title."
  (fifth heading-metadata))

(defun orgtrello-data--extract-id-from-title (heading-title)
  "Given a title, return the id prefixed by :orgtrello-id- and suffixed by :, or nil otherwise."
  (if heading-title
      (let ((heading-id (first (cdr (org-split-string heading-title ":orgtrello-id-")))))
        (if heading-id
            (first (org-split-string heading-id ":"))))))

(defun orgtrello-data--get-id (heading-metadata)
  "Get the id if present in the title"
  (let ((title (orgtrello-data--get-title heading-metadata)))
          (orgtrello-data--extract-id-from-title title)))

(defun orgtrello-data--get-metadata (heading-metadata)
  "Given the heading-metadata returned by the function 'org-heading-components, make it a hashmap with key :level, :keyword, :title. and their respective value"
  (let* ((level   (orgtrello-data--get-level   heading-metadata))
         (title   (orgtrello-data--get-title   heading-metadata))
         (keyword (orgtrello-data--get-keyword heading-metadata))
         (id      (orgtrello-data--get-id      heading-metadata)))
    (orgtrello-hash--make-hash-org level keyword title id)))

(provide 'orgtrello-data)

;;; orgtrello-data.el ends here
