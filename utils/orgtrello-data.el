;;; orgtrello-data.el

(require 'orgtrello-hash)

(defun orgtrello-data--metadata ()
  "Compute the metadata from the org-heading-components entry, add the identifier and extract the metadata needed."
  (let* ((id           (org-entry-get (point) "orgtrello-id"))
         (org-metadata (org-heading-components)))
    (->> org-metadata
         (cons id)
         orgtrello-data--get-metadata)))

(defun orgtrello-data--parent-metadata ()
  "Extract the metadata from the current heading's parent."
  (save-excursion
    (org-up-heading-safe)
    (orgtrello-data-metadata)))

(defun orgtrello-data--grandparent-metadata ()
  "Extract the metadata from the current heading's grandparent."
  (save-excursion
    (org-up-heading-safe)
    (org-up-heading-safe)
    (orgtrello-data-metadata)))

(defun orgtrello-data-entry-get-full-metadata ()
  "Compute the metadata needed for one entry into a map with keys :current, :parent, :grandparent.
   Returns nil if the level is superior to 4."
  (let* ((heading (orgtrello-data--metadata))
         (level   (gethash :level heading)))
    (if (< level 4)
        (let* ((parent-heading      (orgtrello-data--parent-metadata))
               (grandparent-heading (orgtrello-data--grandparent-metadata))
               (mapdata (make-hash-table :test 'equal)))
          ;; build the metadata with every important pieces
          (puthash :current     heading             mapdata)
          (puthash :parent      parent-heading      mapdata)
          (puthash :grandparent grandparent-heading mapdata)
          mapdata))))

(defun orgtrello-data-compute-full-metadata ()
  "Compute the metadata from the org-heading-components entry - full card up to level 3 (rest is dismissed)."
  (let* ((dispatch-map-list (make-hash-table :test 'equal)))
    ;; build the dispatch map into which add the data depending on the level
    (puthash 1 nil dispatch-map-list)
    (puthash 2 nil dispatch-map-list)
    (puthash 3 nil dispatch-map-list)
    (save-excursion
      ;; up to the highest level
      (while (org-up-heading-safe))
      ;; retrieve all metadata
      (org-map-tree (lambda () (push (orgtrello-data-entry-get-full-metadata) (gethash level dispatch-map-list)))))
    ;; first the card
    ;; then the checklists
    ;; then the tasks
    ;; reverse the result list to keep the right order in one time (I have no control over org-map-tree + push
    (reverse (append (gethash 3 dispatch-map-list)
                     (gethash 2 dispatch-map-list)
                     (gethash 1 dispatch-map-list)))))

(defun orgtrello-data--get-level (heading-metadata)
  "Given the heading-metadata, extract the level"
  (second heading-metadata))

(defun orgtrello-data--get-keyword (heading-metadata)
  "Given the heading-metadata, extract the keyword."
  (fourth heading-metadata))

(defun orgtrello-data--get-title (heading-metadata)
  "Given the heading-metadata, extract the title."
  (sixth heading-metadata))

(defun orgtrello-data--get-id (heading-metadata)
  "Given the heading-metadata, extract the id."
  (car heading-metadata))

(defun orgtrello-data--get-metadata (heading-metadata)
  "Given the heading-metadata returned by the function 'org-heading-components, make it a hashmap with key :level, :keyword, :title. and their respective value"
  (let* ((level   (orgtrello-data--get-level   heading-metadata))
         (title   (orgtrello-data--get-title   heading-metadata))
         (keyword (orgtrello-data--get-keyword heading-metadata))
         (id      (orgtrello-data--get-id      heading-metadata)))
    (orgtrello-hash--make-hash-org level keyword title id)))

(provide 'orgtrello-data)

;;; orgtrello-data.el ends here
