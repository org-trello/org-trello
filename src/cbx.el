(require 'org-trello-log)
(require 'org-trello-setup)
(require 'org-trello-hash)
(require 'org-trello-data)

;; #################### orgtrello-cbx

(defun orgtrello-cbx/checkbox-p () "Is there a checkbox at point?"
  (and *ORGTRELLO-NATURAL-ORG-CHECKLIST* (org-at-item-checkbox-p)))

(defun orgtrello-cbx/--to-properties (alist) "Serialize an association list to json."
  (json-encode-hash-table (orgtrello-hash/make-properties alist)))

(defun orgtrello-cbx/--from-properties (string) "Deserialize from json to list."
  (when string (json-read-from-string string)))

(defun orgtrello-cbx/--checkbox-split (s) "Split the checkbox into the checkbox data and the checkbox metadata."
  (s-split ":PROPERTIES:" s))

(defun orgtrello-cbx/--checkbox-metadata (s) "Retrieve the checkbox's metadata."
  (-when-let (res (-> s
                      orgtrello-cbx/--checkbox-split
                      second))
             (s-trim-left res)))

(defun orgtrello-cbx/--checkbox-data (s) "Retrieve the checkbox's data."
    (-> s
      orgtrello-cbx/--checkbox-split
      first
      s-trim-right))

(defun orgtrello-cbx/--read-properties (s) "Read the properties from the current string."
  (->> s
       orgtrello-cbx/--checkbox-metadata
       orgtrello-cbx/--from-properties))

(defun orgtrello-cbx/--read-checkbox! () "Read the full checkbox's content"
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun orgtrello-cbx/--read-properties-from-point (pt) "Read the properties from the current point."
  (save-excursion
    (goto-char pt)
    (orgtrello-cbx/--read-properties (orgtrello-cbx/--read-checkbox!))))

(defun orgtrello-cbx/--make-properties-as-string (properties)
  (format " :PROPERTIES: %s" (orgtrello-cbx/--to-properties properties)))

(defun orgtrello-cbx/--write-properties-at-point (pt properties) "Given the new properties, update the current entry."
  (save-excursion
    (defvar orgtrello-cbx/--tmp-point)
    (goto-char pt)
    (let* ((checkbox-title   (-> (orgtrello-cbx/--read-checkbox!) orgtrello-cbx/--checkbox-data))
           (updated-property (orgtrello-cbx/--make-properties-as-string properties)))
      (beginning-of-line)
      (kill-line)
      (insert checkbox-title)
      (setq orgtrello-cbx/--tmp-point (point))
      (insert updated-property)
      ;; remove overlay present on current position
      (remove-overlays (point-at-bol) (point-at-eol))
      ;; build an overlay to hide the cbx properties
      (overlay-put (make-overlay orgtrello-cbx/--tmp-point (point-at-eol) (current-buffer) t nil)
                   'invisible 'org-trello-cbx-property) ;; outline to use the default one but beware with outline, there is an ellipsis (...)
      (format "%s%s" checkbox-title updated-property))))

(defun orgtrello-cbx/--key-to-search (key) "Search the key key as a symbol"
  (if (stringp key) (intern key) key))

(defun orgtrello-cbx/--org-get-property (key properties) "Internal accessor to the key property."
  (-> key
      orgtrello-cbx/--key-to-search
      (assoc-default properties)))

(defun orgtrello-cbx/--org-update-property (key value properties) "Internal accessor to the key property."
  (->> properties
       (orgtrello-cbx/--org-delete-property key)
       (cons `(,(orgtrello-cbx/--key-to-search key) . ,value))))

(defun orgtrello-cbx/--org-delete-property (key properties) "Delete the key from the properties."
  (-> key
      orgtrello-cbx/--key-to-search
      (assq-delete-all properties)))

(defun orgtrello-cbx/org-set-property (key value) "Read the properties. Add the new property key with the value value. Write the new properties."
  (let ((current-point (point)))
    (->> current-point
         orgtrello-cbx/--read-properties-from-point
         (orgtrello-cbx/--org-update-property key value)
         (orgtrello-cbx/--write-properties-at-point current-point))))

(defun orgtrello-cbx/org-get-property (point key) "Retrieve the value for the key key."
  (->> point
       orgtrello-cbx/--read-properties-from-point
       (orgtrello-cbx/--org-get-property key)))

(defun orgtrello-cbx/org-delete-property (key) "Delete the property key from the properties."
  (let ((current-point (point)))
    (->> current-point
         orgtrello-cbx/--read-properties-from-point
         (orgtrello-cbx/--org-delete-property key)
         (orgtrello-cbx/--write-properties-at-point current-point))))

(defun orgtrello-cbx/--org-split-data (s) "Split the string into meta data with -."
  (->> s
       (s-replace "[ ]" "[]")
       (s-split " ")))

(defun orgtrello-cbx/--list-is-checkbox-p (l) "Is this a checkbox?"
  (string= "-" (first (--drop-while (string= "" it) l))))

(defun orgtrello-cbx/--level (l)
  "Given a list of strings, compute the level (starts at 2).
String look like:
- ('- '[X] 'call 'people '[4/4])
- (' '  '- '[X] 'call 'people '[4/4]).
To ease the computation, we consider level 4 if no - to start with, and to avoid missed typing, we consider level 2 if there is no space before the - and level 3 otherwise."
  (if (orgtrello-cbx/--list-is-checkbox-p l)
      (if (string= "-" (car l)) *CHECKLIST-LEVEL* *ITEM-LEVEL*)
      *OUTOFBOUNDS-LEVEL*))

(defun orgtrello-cbx/--retrieve-status (l) "Given a list of metadata, return the status"
  (car (--drop-while (not (or (string= "[]" it)
                              (string= "[X]" it)
                              (string= "[-]" it)
                              (string= "[ ]" it))) l)))

(defun orgtrello-cbx/--status (s) "Given a checklist status, return the TODO/DONE for org-trello to work."
  (if (string= "[X]" s) "DONE" "TODO"))

(defun orgtrello-cbx/--name (s status) "Retrieve the name of the checklist"
  (->> s
       (s-replace "[ ]" "[]")
       s-trim-left
       (s-chop-prefix "-")
       s-trim-left
       (s-chop-prefix status)
       s-trim))

(defun orgtrello-cbx/--metadata-from-checklist (full-checklist) "Given a checklist string, extract the list of metadata"
  (let* ((oc/--checklist-data   (orgtrello-cbx/--checkbox-data full-checklist))
         (oc/--meta             (orgtrello-cbx/--org-split-data oc/--checklist-data))
         (oc/--status-retrieved (orgtrello-cbx/--retrieve-status oc/--meta)))
      (list (orgtrello-cbx/--level oc/--meta) nil (orgtrello-cbx/--status oc/--status-retrieved) nil (orgtrello-cbx/--name oc/--checklist-data oc/--status-retrieved) nil)))

(defun orgtrello-cbx/org-checkbox-metadata ()
  "Extract the metadata about the checklist - this is the symmetrical as org-heading-components but for the checklist.
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
    (orgtrello-cbx/--metadata-from-checklist (orgtrello-cbx/--read-checkbox!))))

(defun orgtrello-cbx/--get-level (meta) "Retreve the level from the meta describing the checklist"
  (car meta))

(defun orgtrello-cbx/--org-up! (destination-level) "An internal function to get back to the current entry's parent - return the level found or nil if the level found is a card."
  (let ((current-level (orgtrello-cbx/--get-level (orgtrello-cbx/org-checkbox-metadata))))
    (cond ((= *CARD-LEVEL*      current-level) nil)
          ((= destination-level current-level) destination-level)
          ((= *CHECKLIST-LEVEL* current-level) (org-up-heading-safe))
          (t                                   (progn
                                                 (forward-line -1)
                                                 (orgtrello-cbx/--org-up! destination-level))))))

(defun orgtrello-cbx/org-up! () "A function to get back to the current entry's parent."
  (-> (orgtrello-cbx/org-checkbox-metadata)
      orgtrello-cbx/--get-level
      1-
      orgtrello-cbx/--org-up!))

(defun orgtrello-cbx/--compute-next-card-point () "Compute the next card's position."
  (save-excursion
    (org-back-to-heading)
    (if (org-goto-sibling) (point-at-bol) (point-max))))

(defun orgtrello-cbx/--goto-next-checkbox () "Compute the next checkbox's beginning of line. Does preserve the current position. If hitting a heading or the end of the file, return nil."
  (forward-line)
  (when (and (not (org-at-heading-p)) (< (point) (point-max)) (not (orgtrello-cbx/checkbox-p)))
        (orgtrello-cbx/--goto-next-checkbox)))

(defun orgtrello-cbx/--map-checkboxes (level fn-to-execute) "Map over the checkboxes and execute fn when in checkbox. Does not preserve the cursor position. Do not exceed the point-max."
  (orgtrello-cbx/--goto-next-checkbox)
  (when (< level (orgtrello-data/current-level))
        (funcall fn-to-execute)
        (orgtrello-cbx/--map-checkboxes level fn-to-execute)))

(defun orgtrello-cbx/map-checkboxes (fn-to-execute) "Map over the current checkbox and sync them."
  (let ((level (orgtrello-data/current-level)))
    (when (= level *CHECKLIST-LEVEL*) (funcall fn-to-execute))
    (save-excursion (orgtrello-cbx/--map-checkboxes level fn-to-execute)))) ;; then map over the next checkboxes and sync them

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-cbx loaded!")

(provide 'org-trello-cbx)


