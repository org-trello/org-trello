(defun orgtrello-hash/empty-hash ()
  "Empty hash table with test 'equal"
  (make-hash-table :test 'equal))

(defun orgtrello-hash/make-hash-org (member-ids level keyword name id due position buffer-name desc comments tags)
  "Utility function to ease the orgtrello-metadata creation"
  (->> (orgtrello-hash/empty-hash)
    (orgtrello-data/put-entity-buffername  buffer-name)
    (orgtrello-data/put-entity-position    position)
    (orgtrello-data/put-entity-level       level)
    (orgtrello-data/put-entity-keyword     keyword)
    (orgtrello-data/put-entity-name        name)
    (orgtrello-data/put-entity-id          id)
    (orgtrello-data/put-entity-due         due)
    (orgtrello-data/put-entity-member-ids  member-ids)
    (orgtrello-data/put-entity-description desc)
    (orgtrello-data/put-entity-comments    comments)
    (orgtrello-data/put-entity-tags        tags)))

(defun orgtrello-hash/make-properties (properties)
  "Given a list of key value pair, return a hash table."
  (--reduce-from (orgtrello-data/puthash-data (car it) (cdr it) acc)
                 (orgtrello-hash/empty-hash)
                 properties))

(defun orgtrello-hash/make-transpose-properties (properties)
  "Given a list of key value pair, return a hash table with key/value transposed."
  (--reduce-from (orgtrello-data/puthash-data (cdr it) (car it) acc)
                 (orgtrello-hash/empty-hash)
                 properties))

(defun orgtrello-hash/make-hierarchy (current &optional parent grandparent)
  "Helper constructor for the hashmap holding the full metadata about the current-entry."
  (->> (orgtrello-hash/empty-hash)
    (orgtrello-data/put-current current)
    (orgtrello-data/put-parent parent)
    (orgtrello-data/put-grandparent grandparent)))

(defun orgtrello-hash/init-map-from (data)
  "Init a map from a given data. If data is nil, return an empty hash table."
  (if data data (orgtrello-hash/empty-hash)))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-hash loaded!")


