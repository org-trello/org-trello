;;; org-trello-hash.el --- Hash manipulation functions (base data structure used by org-trello)
;;; Commentary:
;;; Code:

(require 'org-trello-log)

(defun orgtrello-hash/empty-hash ()
  "Empty hash table with test 'equal"
  (make-hash-table :test 'equal))

(defun orgtrello-hash/gethash-data (key map &optional default-value) "Retrieve the map from some query-map" (when map (gethash key map default-value)))

(defun orgtrello-hash/puthash-data (key value entity)
  "Update the map at key with value. Return the entity updated. Nil if the entity is nil."
  (when entity
    (puthash key value entity)
    entity))

(defun orgtrello-hash/make-properties (properties)
  "Given a list of key value pair, return a hash table."
  (--reduce-from (orgtrello-hash/puthash-data (car it) (cdr it) acc)
                 (orgtrello-hash/empty-hash)
                 properties))

(defun orgtrello-hash/make-transpose-properties (properties)
  "Given a list of key value pair, return a hash table with key/value transposed."
  (--reduce-from (orgtrello-hash/puthash-data (cdr it) (car it) acc)
                 (orgtrello-hash/empty-hash)
                 properties))

(defun orgtrello-hash/init-map-from (data)
  "Init a map from a given data. If data is nil, return an empty hash table."
  (if data data (orgtrello-hash/empty-hash)))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-hash loaded!")

(provide 'org-trello-hash)
;;; org-trello-hash.el ends here
