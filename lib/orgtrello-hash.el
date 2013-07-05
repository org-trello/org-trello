;;; orgtrello-hash.el --- Utility function to help in initializing orgtrello's hashmap

(defun orgtrello-hash--make-hash-org (level keyword title id)
  "Utility function to ease the creation of the orgtrello-metadata"
  (setq h (make-hash-table :test 'equal))
  (puthash :level   level   h)
  (puthash :keyword keyword h)
  (puthash :title   title h)
  (puthash :id      id h)
  h)

(defun orgtrello-hash--make-hash (method uri &optional params)
  "Utility function to ease the creation of the map - wait, where are my clojure data again!?"
  (setq h (make-hash-table :test 'equal))
  (puthash :method method h)
  (puthash :uri    uri    h)
  (if params (puthash :params params h))
  h)

(provide 'orgtrello-hash)

;;; orgtrello-hash.el ends here
