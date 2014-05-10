(defun orgtrello-db/init ()
  "Initialize a default RAM database (if ~/.trello/orgtrello.db.elc exists, it will load from it."
  (db-make `(db-hash :from-filename ,(format "%s/%s" *ORGTRELLO/CONFIG-DIR* "orgtrello.db"))))

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

(defun orgtrello-db/pop-last (key db)
  "Get the value from last value at key. This is destructive."
  (-when-let (oldvl (nreverse (db-get key db)))
    (let* ((value-to-return (pop oldvl)))
      (db-put key (nreverse oldvl) db)
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

(defun orgtrello-db/nb-buffers (db)
  "Return the number of buffers."
  (-if-let (v (db-get *ORGTRELLO/BUFFER-NUMBER* db)) v 0))

(defun orgtrello-db/set-nb-buffers (v db)
  "Set the number of buffers to a given value"
  (db-put *ORGTRELLO/BUFFER-NUMBER* v db))

(defun orgtrello-db/increment-buffer-size (db)
  "Increment the number of current buffer."
  (--> (orgtrello-db/nb-buffers db)
    (+ 1 it)
    (orgtrello-db/set-nb-buffers it db)))

(defun orgtrello-db/decrement-buffer-size (db)
  "Decrement the number of current buffer."
  (--> (orgtrello-db/nb-buffers db)
    (- it 1)
    (orgtrello-db/set-nb-buffers it db)))

(defun orgtrello-db/move-key-values (from-key to-key db)
  "Copy the content of the from-key to to-key in db. Return the content of the to-key."
  (-when-let (from-values (db-get from-key db))
    (--map (orgtrello-db/put to-key it db) from-values)
    (db-put from-key nil db))
  (db-get to-key db))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-db loaded!")

(provide 'org-trello-db)
;;; org-trello-db.el ends here
