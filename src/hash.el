(require 'org-trello-log)
(require 'org-trello-setup)

;; #################### orgtrello-hash

(defun orgtrello-hash/empty-hash () "Empty hash table with test 'equal"
  (make-hash-table :test 'equal))

(defun orgtrello-hash/make-hash-org (member-ids level keyword name id due position buffer-name desc) "Utility function to ease the orgtrello-metadata creation"
  (let ((h (orgtrello-hash/empty-hash)))
    (puthash :buffername     buffer-name h)
    (puthash :position       position    h)
    (puthash :level          level       h)
    (puthash :keyword        keyword     h)
    (puthash :name           name        h)
    (puthash :id             id          h)
    (puthash :due            due         h)
    (puthash :member-ids     member-ids  h)
    (puthash :desc           desc        h)
    h))

(defun orgtrello-hash/make-hash (method uri &optional params) "Utility function to ease the creation of the map - wait, where are my clojure data again!?"
  (let ((h (orgtrello-hash/empty-hash)))
    (puthash :method method h)
    (puthash :uri    uri    h)
    (if params (puthash :params params h))
    h))

(defun orgtrello-hash/make-properties (properties) "Given a list of key value pair, return a hash table."
  (--reduce-from (progn
                   (puthash (car it) (cdr it) acc)
                   acc)
                 (orgtrello-hash/empty-hash)
                 properties))

(defun orgtrello-hash/make-transpose-properties (properties) "Given a list of key value pair, return a hash table with key/value transposed."
  (--reduce-from (progn
                  (puthash (cdr it) (car it) acc)
                  acc)
                 (orgtrello-hash/empty-hash)
                 properties))

(defun orgtrello-hash/make-hierarchy (current &optional parent grandparent) "Helper constructor for the hashmap holding the full metadata about the current-entry."
  (orgtrello-hash/make-properties `((:current . ,current)
                                    (:parent . ,parent)
                                    (:grandparent . ,grandparent))))

(defun orgtrello-hash/key (s) "Given a string, compute its key format."
  (format ":%s:" s))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-hash loaded!")

(provide 'org-trello-hash)


