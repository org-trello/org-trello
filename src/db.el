(defun orgtrello-db/init ()
  "Initialize an empty RAM database (if ~/.trello/orgtrello.db.elc exists, it will load from it."
  (db-make
   `(db-hash :from-filename ,(format "%s/%s" *ORGTRELLO/CONFIG-DIR* "orgtrello.db"))))

(defun orgtrello-db/save! (db)
  "Flush the database to disk."
  (plist-put db :filename (format "%s/%s" *ORGTRELLO/CONFIG-DIR* "orgtrello.db")) ;; add the :filename property
  (db-hash/save db)                                                               ;; flush to disk
  (plist-put db :filename nil))                                                   ;; remove the :filename property

(defun orgtrello-db/put (key value db)
  "Put the value at key in db. If value is already present at key, put the value to the end of old value."
  (let* ((old-value (db-get key db))
         (new-value (-snoc old-value value)))
    (db-put key new-value db)))

(defun orgtrello-db/get (key db)
  "Get the value from value at key in db. This is readonly"
  (db-get key db))

(defun orgtrello-db/pop (key db)
  "Get the value from value at key. This is destructive."
  (-when-let (oldvl (db-get key db))
    (let* ((value-to-return (pop oldvl)))
      (db-put key oldvl db)
      value-to-return)))

(defun orgtrello-db/clear-key (key db)
  "Given a key, squash to value to nil."
  (db-put key nil db))

(defun orgtrello-db/clear-keys (keys db)
  "Given a list of keys, squash the different values for those keys."
  (--map (orgtrello-db/clear-key it db) keys))

(defun orgtrello-db/clear-entity-with-id (keys id db)
  "Clear some entities"
  (--map (-if-let (entities (db-get it db))
             (db-put key (-remove (lambda (entity) (string= id (orgtrello-data/entity-id entity)))  entities)))
         keys))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-db loaded!")


