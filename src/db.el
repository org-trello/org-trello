(defun orgtrello-db/init ()
  "Initialize an empty database"
  (db-make
   `(db-hash
     :filename ,(format "%s/%s" *ORGTRELLO/CONFIG-DIR* "orgtrello.db"))))

(defun orgtrello-db/put (key value db)
  "Put the value at key in db. If value is already present at key, cons the value to the old value."
  (let* ((old-value (db-get key db))
         (new-value (cons value old-value)))
    (db-put key new-value db)))

(defun orgtrello-db/get (key db)
  "Get the value from value at key in db. This is readonly"
  (db-get key db))

(defun orgtrello-db/pop (key db)
  "Get the value from value at key. This is destructive."
  (-when-let (oldvl (db-get key db))
    (let ((value-to-return (pop oldvl)))
      (db-put key oldvl db)
      value-to-return)))

(defun orgtrello-db/clear-key (key db)
  "Given a key, squash to value to nil."
  (db-put key nil db))

(defun orgtrello-db/clear-keys (keys db)
  "Given a list of keys, squash the different values for those keys."
  (--map (orgtrello-db/clear-key it db) keys))

(defun orgtrello-db/copy (old-key new-key db)
  "Copy the old-key as new-key in the db"
  (db-put new-key (db-get old-key db) db))
