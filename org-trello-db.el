;;; org-trello-db.el --- Database namespace
;;; Commentary:
;;; Code:

(require 'dash)
(require 'db)
(require 'org-trello-setup)
(require 'org-trello-log)
(require 'org-trello-hash)

(defun orgtrello-db/init ()
  "Initialize a default RAM database (if ~/.trello/orgtrello.db.elc exists, it will load from it."
  (db-make `(db-hash :from-filename ,(format "%s/%s" *ORGTRELLO/CONFIG-DIR* "orgtrello.db"))))

(defun orgtrello-db/save! (db)
  "Flush the database to disk."
  (plist-put db :filename (format "%s/%s" *ORGTRELLO/CONFIG-DIR* "orgtrello.db")) ;; add the :filename property
  (db-hash/save db)                                                               ;; flush to disk
  (plist-put db :filename nil))                                                   ;; remove the :filename property

(defun orgtrello-db/put (key value db)
  "Update the KEY by appending VALUE to its old-value in DB.
If an old-value is already present at KEY, put the VALUE to the end of old value."
  (--> (db-get key db)
    (-snoc it value)
    (db-put key it db)))

(defun orgtrello-db/get (key db)
  "Read the value at KEY in DB.
This is read-only."
  (db-get key db))

(defun orgtrello-db/pop (key db)
  "Pop the value at KEY in DB.
This is destructive."
  (-when-let (oldvl (db-get key db))
    (let ((value-to-return (pop oldvl)))
      (db-put key oldvl db)
      value-to-return)))

(defun orgtrello-db/pop-last (key db)
  "Pop the last value at KEY in DB.
This is destructive."
  (-when-let (oldvl (nreverse (db-get key db)))
    (let ((value-to-return (pop oldvl)))
      (db-put key (nreverse oldvl) db)
      value-to-return)))

(defun orgtrello-db/clear-key (key db)
  "Clear the KEY (put nil at that key) in DB."
  (db-put key nil db))

(defun orgtrello-db/clear-keys (keys db)
  "Clear the content's KEYS in DB."
  (--map (orgtrello-db/clear-key it db) keys))

(defun orgtrello-db/clear-entity-with-id (keys id db)
  "From the keys KEYS, Remove entity with id ID inside the database DB."
  (mapc (lambda (key)
          (-if-let (entities (db-get key db))
              (db-put key (-remove (lambda (entity) (string= id (orgtrello-data/entity-id entity))) entities) db)))
         keys))

(defun orgtrello-db/nb-buffers (db)
  "Return the number of buffers present in DB."
  (-if-let (v (db-get *ORGTRELLO/BUFFER-NUMBER* db)) v 0))

(defun orgtrello-db/set-nb-buffers (nb db)
  "Set the number of buffers NB in DB."
  (db-put *ORGTRELLO/BUFFER-NUMBER* nb db))

(defun orgtrello-db/increment-buffer-size (db)
  "Increment the number of current buffers in DB."
  (--> (orgtrello-db/nb-buffers db)
    (+ 1 it)
    (orgtrello-db/set-nb-buffers it db)))

(defun orgtrello-db/decrement-buffer-size (db)
  "Decrement the number of current buffers in DB."
  (--> (orgtrello-db/nb-buffers db)
    (- it 1)
    (orgtrello-db/set-nb-buffers it db)))

(defun orgtrello-db/move-key-values (from-key to-key db)
  "Copy the content of the FROM-KEY to TO-KEY in DB.
Return the content of the TO-KEY."
  (-when-let (from-values (db-get from-key db))
    (mapc (lambda (it) (orgtrello-db/put to-key it db)) from-values)
    (db-put from-key nil db))
  (db-get to-key db))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-db loaded!")

(provide 'org-trello-db)
;;; org-trello-db.el ends here
