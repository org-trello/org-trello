;;; org-trello-cbx.el --- Manipulation functions of checkbox to add some behavior to org's checkbox
;;; Commentary:
;;; Code:

(require 'org-trello-setup)
(require 'org-trello-log)
(require 'org-trello-hash)

(defun orgtrello-cbx/checkbox-p ()
  "Is there a checkbox at point?"
  (org-at-item-checkbox-p))

(defun orgtrello-cbx/--to-properties (alist)
  "Serialize an association list to json."
  (json-encode-hash-table (orgtrello-hash/make-properties alist)))

(defun orgtrello-cbx/--from-properties (string)
  "Deserialize from json to list."
  (when string (json-read-from-string string)))

(defun orgtrello-cbx/--checkbox-split (s)
  "Split the checkbox into the checkbox data and the checkbox metadata."
  (s-split ":PROPERTIES:" s))

(defun orgtrello-cbx/--checkbox-metadata (s)
  "Retrieve the checkbox's metadata."
  (-when-let (res (-> s
                    orgtrello-cbx/--checkbox-split
                    cadr))
    (s-trim-left res)))

(defun orgtrello-cbx/--checkbox-data (s)
  "Retrieve the checkbox's data."
  (-> s
    orgtrello-cbx/--checkbox-split
    car
    s-trim-right))

(defun orgtrello-cbx/--read-properties (s)
  "Read the properties from the current string."
  (->> s
    orgtrello-cbx/--checkbox-metadata
    orgtrello-cbx/--from-properties))

(defun orgtrello-cbx/--read-checkbox! ()
  "Read the full checkbox's content"
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun orgtrello-cbx/--read-properties-from-point (pt)
  "Read the properties from the current point."
  (save-excursion
    (goto-char pt)
    (orgtrello-cbx/--read-properties (orgtrello-cbx/--read-checkbox!))))

(defun orgtrello-cbx/--make-properties-as-string (properties)
  (format ":PROPERTIES: %s" (orgtrello-cbx/--to-properties properties)))

(defun orgtrello-cbx/remove-overlays! (start end)
  "Remove the overlays presents between start and end in the current buffer"
  (remove-overlays start end 'invisible 'org-trello-cbx-property))

(defun orgtrello-cbx/install-overlays! (start-position)
  "Install org-trello overlays (first remove the current overlay on line)."
  ;; remove overlay present on current position
  (orgtrello-cbx/remove-overlays! (point-at-bol) (point-at-eol))
  ;; build an overlay to hide the cbx properties
  (overlay-put (make-overlay start-position (point-at-eol) (current-buffer) t nil)
               'invisible 'org-trello-cbx-property))

(defun orgtrello-cbx/--write-properties-at-point (pt properties)
  "Given the new properties, update the current entry."
  (save-excursion
    (goto-char pt)
    (let* ((checkbox-title   (-> (orgtrello-cbx/--read-checkbox!) orgtrello-cbx/--checkbox-data))
           (updated-property (orgtrello-cbx/--make-properties-as-string properties))
           (text-to-insert   (format "%s %s" checkbox-title updated-property)))
      (beginning-of-line)
      (kill-line)
      (insert text-to-insert)
      text-to-insert)))

(defun orgtrello-cbx/--key-to-search (key)
  "Search the key key as a symbol"
  (if (stringp key) (intern key) key))

(defun orgtrello-cbx/--org-get-property (key properties)
  "Internal accessor to the key property."
  (-> key
    orgtrello-cbx/--key-to-search
    (assoc-default properties)))

(defun orgtrello-cbx/--org-update-property (key value properties)
  "Internal accessor to the key property."
  (->> properties
    (orgtrello-cbx/--org-delete-property key)
    (cons `(,(orgtrello-cbx/--key-to-search key) . ,value))))

(defun orgtrello-cbx/--org-delete-property (key properties)
  "Delete the key from the properties."
  (-> key
    orgtrello-cbx/--key-to-search
    (assq-delete-all properties)))

(defun orgtrello-cbx/org-set-property (key value)
  "Read the properties. Add the new property key with the value value. Write the new properties."
  (let ((current-point (point)))
    (->> current-point
      orgtrello-cbx/--read-properties-from-point
      (orgtrello-cbx/--org-update-property key value)
      (orgtrello-cbx/--write-properties-at-point current-point))))

(defun orgtrello-cbx/org-get-property (point key)
  "Retrieve the value for the key key."
  (->> point
    orgtrello-cbx/--read-properties-from-point
    (orgtrello-cbx/--org-get-property key)))

(defun orgtrello-cbx/org-delete-property (key)
  "Delete the property key from the properties."
  (let ((current-point (point)))
    (->> current-point
      orgtrello-cbx/--read-properties-from-point
      (orgtrello-cbx/--org-delete-property key)
      (orgtrello-cbx/--write-properties-at-point current-point))))

(defun orgtrello-cbx/--org-split-data (s)
  "Split the string into meta data with -."
  (->> s
    (s-replace "[ ]" "[]")
    (s-split " ")))

(defun orgtrello-cbx/--retrieve-status (l)
  "Given a list of metadata, return the status"
  (car (--drop-while (not (or (string= "[]" it)
                              (string= "[X]" it)
                              (string= "[-]" it)
                              (string= "[ ]" it))) l)))

(defun orgtrello-cbx/--status (s)
  "Given a checklist status, return the TODO/DONE for org-trello to work."
  (if (string= "[X]" s) *ORGTRELLO/DONE* *ORGTRELLO/TODO*))

(defun orgtrello-cbx/--name (s status)
  "Retrieve the name of the checklist"
  (->> s
    (s-replace "[ ]" "[]")
    s-trim-left
    (s-chop-prefix "-")
    s-trim-left
    (s-chop-prefix status)
    s-trim))

(defun orgtrello-cbx/--metadata-from-checklist (full-checklist)
  "Given a checklist string, extract the list of metadata"
  (let* ((checklist-data   (orgtrello-cbx/--checkbox-data full-checklist))
         (meta             (orgtrello-cbx/--org-split-data checklist-data))
         (status-retrieved (orgtrello-cbx/--retrieve-status meta)))
    (list nil (orgtrello-cbx/--status status-retrieved) nil (orgtrello-cbx/--name checklist-data status-retrieved) nil)))

(defun orgtrello-cbx/--level! ()
  "Compute the levels from the current position (which is `bol`)"
  (if (org-at-item-bullet-p) *ORGTRELLO/CHECKLIST-LEVEL* *ORGTRELLO/ITEM-LEVEL*))

(defun orgtrello-cbx/org-checkbox-metadata! ()
  "Extract the metadata about the checklist - this is the symmetrical with `org-heading-components` but for the checklist.
Return the components of the current heading.
This is a list with the following elements:
- the level as an integer                                          - (begins at 2)
- the reduced level                                                - always nil
- the TODO keyword, or nil                                         - [], [-] map to TODO, [X] map to DONE
- the priority character, like ?A, or nil if no priority is given  - nil
- the headline text itself, or the tags string if no headline text - the name of the checkbox
- the tags string, or nil.                                         - nil"
  (save-excursion
    (beginning-of-line)
    (cons (orgtrello-cbx/--level!)
          (orgtrello-cbx/--metadata-from-checklist (orgtrello-cbx/--read-checkbox!)))))

(defun orgtrello-cbx/--get-level (meta)
  "Retrieve the level from the meta describing the checklist"
  (car meta))

(defun orgtrello-cbx/--org-up! (destination-level)
  "An internal function to get back to the current entry's parent - return the level found or nil if the level found is a card."
  (let ((current-level (orgtrello-cbx/--get-level (orgtrello-cbx/org-checkbox-metadata!))))
    (cond ((= *ORGTRELLO/CARD-LEVEL*      current-level) nil)
          ((= destination-level current-level) destination-level)
          ((= *ORGTRELLO/CHECKLIST-LEVEL* current-level) (org-up-heading-safe))
          (t                                   (progn
                                                 (forward-line -1)
                                                 (orgtrello-cbx/--org-up! destination-level))))))

(defun orgtrello-cbx/current-level! ()
  "Give the current level of the checkbox."
  (orgtrello-cbx/--get-level (orgtrello-cbx/org-checkbox-metadata!)))

(defun orgtrello-cbx/org-up! ()
  "A function to get back to the current entry's parent."
  (-> (orgtrello-cbx/current-level!)
    1-
    orgtrello-cbx/--org-up!))

(defun orgtrello-cbx/compute-next-card-point! ()
  "Compute the next card's position. Does preserve position. If a sibling is found, return the point-at-bol, otherwise return the max point in buffer."
  (save-excursion
    (org-back-to-heading)
    (if (org-goto-sibling) (point-at-bol) (point-max))))

(defun orgtrello-cbx/--goto-next-checkbox ()
  "Compute the next checkbox's beginning of line. Does not preserve the current position. If hitting a heading or the end of the file, return nil."
  (forward-line)
  (when (and (not (org-at-heading-p)) (< (point) (point-max)) (not (orgtrello-cbx/checkbox-p)))
    (orgtrello-cbx/--goto-next-checkbox)))

(defun orgtrello-cbx/--goto-next-checkbox-with-same-level! (level)
  "Compute the next checkbox's beginning of line (with the same level). Does not preserve the current position. If hitting a heading or the end of the file, return nil. Otherwise, return the current position."
  (forward-line)
  (if (= level (orgtrello-cbx/current-level!))
      (point)
    (if (or (org-at-heading-p) (<= (point-max) (point)))
        nil
      (orgtrello-cbx/--goto-next-checkbox-with-same-level! level))))

(defun orgtrello-cbx/--map-checkboxes (level fn-to-execute)
  "Map over the checkboxes and execute fn when in checkbox. Does not preserve the cursor position. Do not exceed the point-max."
  (orgtrello-cbx/--goto-next-checkbox)
  (when (< level (orgtrello-cbx/current-level!))
    (funcall fn-to-execute)
    (orgtrello-cbx/--map-checkboxes level fn-to-execute)))

(defun orgtrello-cbx/map-checkboxes (fn-to-execute)
  "Map over the current checkbox and sync them."
  (let ((level (orgtrello-cbx/current-level!)))
    (when (= level *ORGTRELLO/CHECKLIST-LEVEL*) (funcall fn-to-execute))
    (save-excursion (orgtrello-cbx/--map-checkboxes level fn-to-execute)))) ;; then map over the next checkboxes and sync them

(defun orgtrello-cbx/next-checklist-point! ()
  "Compute the next checklist position"
  (-if-let (next-checklist-point (save-excursion (orgtrello-cbx/--goto-next-checkbox-with-same-level! *ORGTRELLO/CHECKLIST-LEVEL*) (point)))
      next-checklist-point
    (orgtrello-cbx/compute-next-card-point!)))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-cbx loaded!")

(provide 'org-trello-cbx)
;;; org-trello-cbx.el ends here
