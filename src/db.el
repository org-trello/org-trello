(defun orgtrello-db/init ()
  "Initialize an empty database"
  `(db-hash
    :filename ,(format "%s/%s" *ORGTRELLO/CONFIG-DIR* "orgtrello.db.elisp")))

(defun orgtrello-db/put (key value db)
  "Put the value at key in db. If value is already present at key, cons the value to the old value."
  (let* ((old-value (db-get key db))
         (new-value (cons value old-value)))
    (db-put key new-value db)))

(defun orgtrello-db/get (key db)
  "Get the value from value at key. This is destructive."
  (-when-let (oldvl (db-get key db))
    (let ((value-to-return (pop oldvl)))
      (db-put key oldvl db)
      value-to-return)))
