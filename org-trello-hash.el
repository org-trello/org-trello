;;; org-trello-hash.el --- Hash manipulation functions (the base data structure)
;;; Commentary:
;;; Code:

(require 'org-trello-log)

(defun orgtrello-hash/empty-hash ()
  "Empty hash table with test 'equal."
  (make-hash-table :test 'equal))

(defun orgtrello-hash/gethash-data (key map &optional default-value)
  "Retrieve the value at KEY in MAP.
If the value is not found, return DEFAULT-VALUE."
  (when map (gethash key map default-value)))

(defun orgtrello-hash/puthash-data (key value entity)
  "Update at KEY the VALUE in the ENTITY map.
Return the entity updated or nil if the entity is nil."
  (when entity
    (puthash key value entity)
    entity))

(defun orgtrello-hash/make-properties (properties)
  "Return a hash-table from PROPERTIES key/values."
  (--reduce-from (orgtrello-hash/puthash-data (car it) (cdr it) acc)
                 (orgtrello-hash/empty-hash)
                 properties))

(defun orgtrello-hash/make-transpose-properties (properties)
  "Return a hash-table with transposed key/value from PROPERTIES key/values."
  (--reduce-from (orgtrello-hash/puthash-data (cdr it) (car it) acc)
                 (orgtrello-hash/empty-hash)
                 properties))

(defun orgtrello-hash/init-map-from (data)
  "Init a map from a given DATA.
If data is nil, return an empty hash table."
  (if data data (orgtrello-hash/empty-hash)))

(orgtrello-log/msg *OT/DEBUG* "orgtrello-hash loaded!")

(provide 'org-trello-hash)
;;; org-trello-hash.el ends here
