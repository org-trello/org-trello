;;; orgtrello-hash.el --- Build hash map easily

(defun make-hash (method uri &optional params)
  "Utility function to ease the creation of the map - wait, where are my clojure data again!?"
  (setq h (make-hash-table :test 'equal))
  (puthash :method method h)
  (puthash :uri    uri    h)
  (if params (puthash :params params h))
  h)

(provide 'orgtrello-hash)
