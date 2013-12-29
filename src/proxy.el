

(require 'org-trello-log)
(require 'org-trello-setup)

;; #################### orgtrello-proxy

(defvar *ORGTRELLO-PROXY-HOST* "localhost" "proxy host")
(defvar *ORGTRELLO-PROXY-PORT* nil         "proxy port")
(defvar *ORGTRELLO-PROXY-URL*  nil         "proxy url")

(defvar *ORGTRELLO-PROXY-DEFAULT-PORT* 9876 "Default proxy port") (setq *ORGTRELLO-PROXY-PORT* *ORGTRELLO-PROXY-DEFAULT-PORT*)

(defun orgtrello-proxy/http (query-map &optional sync success-callback error-callback) "Query the proxy for the trello api."
  (--> query-map
       (orgtrello-hash/make-hash "POST" "/trello/" it)
       (orgtrello-query/--http *ORGTRELLO-PROXY-URL* it sync success-callback error-callback)))

(defun orgtrello-proxy/http-producer (query-map &optional sync) "Query the proxy producer"
  (--> query-map
       (orgtrello-query/--prepare-query-params! it)
       (orgtrello-hash/make-hash "POST" "/producer/" it)
       (orgtrello-query/--http *ORGTRELLO-PROXY-URL* it sync)))

(defun orgtrello-proxy/http-consumer (start) "Query the http-consumer process once to make it trigger a timer"
  (--> `((start . ,start))
       (orgtrello-hash/make-hash "POST" "/timer/" it)
       (orgtrello-query/--http *ORGTRELLO-PROXY-URL* it t)))

(defun orgtrello-proxy/--json-read-from-string (data) "Read the json data and unhexify them."
  (-> data json-read-from-string orgtrello-query/--read-data))

(defun orgtrello-proxy/--unhexify-data (params &optional unhexify-flag) "Given a params object, unhexify the content if need be."
  (funcall (if unhexify-flag 'orgtrello-proxy/--json-read-from-string 'json-read-from-string) params))

(defun orgtrello-proxy/--extract-trello-query (http-con &optional unhexify-flag) "Given an httpcon object, extract the params entry which corresponds to the real trello query."
  (-> http-con elnode-http-params caar (orgtrello-proxy/--unhexify-data unhexify-flag)))

(defun orgtrello-proxy/--compute-trello-query (query-map-wrapped) "Build a trello query from the control of query-map-wrapped."
  (orgtrello-hash/make-hash (orgtrello-data/entity-method query-map-wrapped) (orgtrello-data/entity-uri query-map-wrapped) (orgtrello-data/entity-params query-map-wrapped)))

(defun orgtrello-proxy/--response (http-con data) "A response wrapper"
  (elnode-http-start http-con 201 '("Content-type" . "application/json"))
  (elnode-http-return http-con (json-encode data)))

(defun orgtrello-proxy/--compute-entity-level-dir (level) "Given a level, compute the folder onto which the file will be serialized."
  (format "%s%s/%s/" elnode-webserver-docroot "org-trello" level))

(defun orgtrello-proxy/response-ok (http-con) "OK response from the proxy to the client."
  ;; all is good
  (orgtrello-proxy/--response http-con '((status . "ok"))))

(defun orgtrello-proxy/--elnode-proxy (http-con) "Deal with request to trello (for creation/sync request, use orgtrello-proxy/--elnode-proxy-producer)."
  (orgtrello-log/msg *OT/TRACE* "Proxy - Request received. Transmitting...")
  (let* ((query-map-wrapped    (orgtrello-proxy/--extract-trello-query http-con))                     ;; wrapped query is mandatory
         (position             (orgtrello-data/position query-map-wrapped))                        ;; position is mandatory
         (buffer-name          (orgtrello-data/buffername query-map-wrapped))                      ;; buffer-name is mandatory
         (standard-callback    (orgtrello-data/callback query-map-wrapped))                        ;; there is the possibility to transmit the callback from the client to the proxy
         (standard-callback-fn (when standard-callback (symbol-function (intern standard-callback)))) ;; the callback is passed as a string, we want it as a function when defined
         (sync                 (orgtrello-data/sync- query-map-wrapped))                           ;; there is a possibility to enforce the sync between proxy and client
         (query-map            (orgtrello-proxy/--compute-trello-query query-map-wrapped))            ;; extracting the query
         (name                 (orgtrello-data/name query-map-wrapped)))                           ;; extracting the name of the entity (optional)
    (orgtrello-query/http-trello query-map sync (when standard-callback-fn (funcall standard-callback-fn buffer-name position name)))
    (orgtrello-proxy/response-ok http-con)))

(defun orgtrello-proxy/--compute-metadata-filename (root-dir buffer-name position) "Compute the metadata entity filename"
  (format "%s%s-%s.el" root-dir buffer-name position))

(defun orgtrello-proxy/--elnode-proxy-producer (http-con) "A handler which is an entity informations producer on files under the docroot/level-entities/"
  (orgtrello-log/msg *OT/TRACE* "Proxy-producer - Request received. Generating entity file...")
  (let* ((query-map-wrapped    (orgtrello-proxy/--extract-trello-query http-con 'unhexify)) ;; wrapped query is mandatory
         (position             (orgtrello-data/position query-map-wrapped))              ;; position is mandatory
         (buffer-name          (orgtrello-data/buffername query-map-wrapped))            ;; buffer-name is mandatory
         (level                (orgtrello-data/level query-map-wrapped))
         (root-dir             (orgtrello-proxy/--compute-entity-level-dir level)))
    ;; generate a file with the entity information
    (with-temp-file (orgtrello-proxy/--compute-metadata-filename root-dir buffer-name position)
      (insert (format "%S\n" query-map-wrapped)))
    (orgtrello-proxy/response-ok http-con)))

(defun orgtrello-proxy/--read-lines (fPath) "Return a list of lines of a file at FPATH."
  (with-temp-buffer
    (insert-file-contents fPath)
    (split-string (buffer-string) "\n" t)))

(defun orgtrello/compute-marker (buffer-name name position) "Compute the orgtrello marker which is composed of buffer-name, name and position"
  (->> (list *ORGTRELLO-MARKER* buffer-name name (if (stringp position) position (int-to-string position)))
       (-interpose "-")
       (apply 'concat)
       sha1
       (concat *ORGTRELLO-MARKER* "-")))

(defun orgtrello-proxy/--remove-file (file-to-remove) "Remove metadata file."
  (when (file-exists-p file-to-remove) (delete-file file-to-remove)))

(defun orgtrello-proxy/--update-buffer-to-save (buffer-name buffers-to-save) "Add the buffer-name to the list if not already present"
  (if (member buffer-name buffers-to-save)
      buffers-to-save
      (cons buffer-name buffers-to-save)))

(defvar *ORGTRELLO-LIST-BUFFERS-TO-SAVE* nil "A simple flag to order the saving of buffer when needed.")

(defun orgtrello-proxy/update-buffer-to-save! (buffer-name) "Side-effect - Mutate the *ORGTRELLO-LIST-BUFFERS-TO-SAVE* by adding buffer-name to it if not already present."
  (setq *ORGTRELLO-LIST-BUFFERS-TO-SAVE* (orgtrello-proxy/--update-buffer-to-save buffer-name *ORGTRELLO-LIST-BUFFERS-TO-SAVE*)))

(defun orgtrello-proxy/--cleanup-and-save-buffer-metadata (archive-file buffer-name) "To cleanup metadata after the all actions are done!"
  (orgtrello-proxy/--remove-file archive-file) ;; cleanup archive file
  (orgtrello-proxy/update-buffer-to-save! buffer-name)) ;; register the buffer for later saving

(defun orgtrello-proxy/batch-save (buffers) "Save sequentially a list of buffers."
  (-each buffers 'save-buffer))

(defun orgtrello-proxy/batch-save! () "Save sequentially the org-trello list of modified buffers."
  (setq *ORGTRELLO-LIST-BUFFERS-TO-SAVE* (orgtrello-proxy/batch-save *ORGTRELLO-LIST-BUFFERS-TO-SAVE*)))

(defmacro orgtrello-proxy/--safe-wrap-or-throw-error (fn) "A specific macro to deal with interception of uncaught error when executing the fn call. If error is thrown, send the 'org-trello-timer-go-to-sleep flag."
  `(condition-case ex
       (progn ,fn)
     ('error
      (orgtrello-log/msg *OT/ERROR* (concat "### org-trello - consumer ### Caught exception: [" ex "]"))
      (throw 'org-trello-timer-go-to-sleep t))))

(defun orgtrello-proxy/--getting-back-to-headline (data) "Trying another approach to getting back to header computing the normal form of an entry in the buffer."
  (orgtrello-proxy/--getting-back-to-marker (orgtrello/--compute-entity-to-org-entry data)))

(defun orgtrello-proxy/--compute-pattern-search-from-marker (marker) "Given a marker, compute the pattern to look for in the file."
  marker)

(defun orgtrello-proxy/--getting-back-to-marker (marker) "Given a marker, getting back to marker function. Move the cursor position."
  (goto-char (point-min))
  (re-search-forward (orgtrello-proxy/--compute-pattern-search-from-marker marker) nil t))

(defun orgtrello-proxy/--get-back-to-marker (marker data) "Getting back to the marker. Move the cursor position."
  (-if-let (goto-ok (orgtrello-proxy/--getting-back-to-marker marker))
           goto-ok
           (orgtrello-proxy/--getting-back-to-headline data)))

(defun orgtrello/id-p (id) "Is the string a trello identifier?"
  (and id (not (string-match-p (format "^%s-" *ORGTRELLO-MARKER*) id))))

(defun orgtrello-proxy/--standard-post-or-put-success-callback (entity-to-sync file-to-cleanup) "Return a callback function able to deal with the update of the buffer at a given position."
  (lexical-let ((orgtrello-proxy/--entry-position    (orgtrello-data/position entity-to-sync))
                (orgtrello-proxy/--entry-buffer-name (orgtrello-data/buffername entity-to-sync))
                (orgtrello-proxy/--entry-file        file-to-cleanup)
                (orgtrello-proxy/--marker-id         (orgtrello-data/id entity-to-sync))
                (orgtrello-proxy/--entity-name       (orgtrello-data/name entity-to-sync)))
    (function* (lambda (&key data &allow-other-keys)
                 (orgtrello-action/safe-wrap
                  (let* ((orgtrello-proxy/--entry-new-id (orgtrello-data/id data)))
                    (set-buffer orgtrello-proxy/--entry-buffer-name) ;; switch to the right buffer
                    ;; will update via tag the trello id of the new persisted data (if needed)
                    (save-excursion
                      ;; get back to the buffer and update the id if need be
                      (let ((str-msg (when (orgtrello-proxy/--get-back-to-marker orgtrello-proxy/--marker-id data)
                                           ;; now we extract the data
                                           (let ((orgtrello-proxy/--entry-id (when (orgtrello/id-p orgtrello-proxy/--marker-id) orgtrello-proxy/--marker-id)))
                                             (if orgtrello-proxy/--entry-id ;; id already present in the org-mode file
                                                 ;; no need to add another
                                                 (concat "Entity '" orgtrello-proxy/--entity-name "' with id '" orgtrello-proxy/--entry-id "' synced!")
                                                 (let ((orgtrello-proxy/--entry-name (orgtrello-data/name data)))
                                                   ;; not present, this was just created, we add a simple property
                                                   (orgtrello-action/set-property *ORGTRELLO-ID* orgtrello-proxy/--entry-new-id)
                                                   (concat "Newly entity '" orgtrello-proxy/--entry-name "' with id '" orgtrello-proxy/--entry-new-id "' synced!")))))))
                        (when str-msg (orgtrello-log/msg *OT/INFO* str-msg)))))
                  (orgtrello-proxy/--cleanup-and-save-buffer-metadata orgtrello-proxy/--entry-file orgtrello-proxy/--entry-buffer-name))))))

(defun orgtrello-proxy/--archived-scanning-dir (dir-name) "Given a filename, return the archived scanning directory"
  (format "%s.scanning" dir-name))

(defun orgtrello-proxy/--archived-scanning-file (file) "Given a filename, return its archived filename if we were to move such file."
  (format "%s/%s" (orgtrello-proxy/--archived-scanning-dir (file-name-directory file)) (file-name-nondirectory file)))

(defun orgtrello-proxy/--archive-entity-file-when-scanning (file-to-archive file-archive-name) "Move the file to the running folder to specify a sync is running."
  (rename-file file file-archive-name t))

(defun orgtrello-proxy/--dispatch-action (action) "Dispatch action function depending on the flag action"
  (cond ((string= *ORGTRELLO-ACTION-DELETE* action) 'orgtrello-proxy/--delete)
        ((string= *ORGTRELLO-ACTION-SYNC*   action) 'orgtrello-proxy/--sync-entity)))

(defun orgtrello-proxy/--cleanup-meta (entity-full-metadata)
  (unless (-> entity-full-metadata orgtrello-data/current orgtrello/--id)
          (orgtrello-cbx/org-delete-property *ORGTRELLO-ID*)))

(defun orgtrello-proxy/--sync-entity (entity-data entity-full-metadata entry-file-archived) "Execute the entity synchronization."
  (lexical-let ((orgtrello-query/--query-map (orgtrello/--dispatch-create entity-full-metadata))
                (oq/--entity-full-meta       entity-full-metadata)
                (oq/--entry-file-archived    entry-file-archived))
    (if (hash-table-p orgtrello-query/--query-map)
        ;; execute the request
        (orgtrello-query/http-trello orgtrello-query/--query-map *do-sync-query*
                                     (orgtrello-proxy/--standard-post-or-put-success-callback entity-data entry-file-archived)
                                     (function* (lambda (&key error-thrown &allow-other-keys)
                                                  (orgtrello-log/msg *OT/ERROR* "client - Problem during the sync request to the proxy- error-thrown: %s" error-thrown)
                                                  (orgtrello-proxy/--cleanup-meta oq/--entity-full-meta)
                                                  (orgtrello-proxy/--remove-file oq/--entry-file-archived)
                                                  (throw 'org-trello-timer-go-to-sleep t))))
        ;; cannot execute the request
        (progn
          (orgtrello-log/msg *OT/INFO* orgtrello-query/--query-map)
          (orgtrello-proxy/--cleanup-meta entity-full-metadata)
          (throw 'org-trello-timer-go-to-sleep t)))))

(defun orgtrello-proxy/--deal-with-entity-action (entity-data file-to-archive) "Compute the synchronization of an entity (retrieving latest information from buffer)"
  (let* ((op/--position            (orgtrello-data/position entity-data))                       ;; position is mandatory
         (op/--buffer-name         (orgtrello-data/buffername entity-data))                     ;; buffer-name too
         (op/--entry-file-archived (orgtrello-proxy/--archived-scanning-file file-to-archive))
         (op/--marker              (orgtrello-data/id entity-data)))                            ;; retrieve the id (which serves as a marker too)
    (orgtrello-log/msg *OT/TRACE* "Proxy-consumer - Searching entity metadata from buffer '%s' at point '%s' to sync..." op/--buffer-name op/--position)
    (set-buffer op/--buffer-name)                                                                  ;; switch to the right buffer
    (orgtrello-proxy/--safe-wrap-or-throw-error                                                    ;; will update via tag the trello id of the new persisted data (if needed)
     (save-excursion
       (when (orgtrello-proxy/--get-back-to-marker op/--marker entity-data)
             (orgtrello-proxy/--archive-entity-file-when-scanning file-to-archive op/--entry-file-archived) ;; archive the scanned file
             (-> entity-data
                 orgtrello-data/action
                 orgtrello-proxy/--dispatch-action
                 (funcall entity-data (orgtrello-data/entry-get-full-metadata) op/--entry-file-archived)))))))

(defun orgtrello-action/org-delete-property (key) "Delete a property depending on the nature of the current entry (org heading or checkbox)."
  (funcall (if (orgtrello-cbx/checkbox-p) 'orgtrello-cbx/org-delete-property 'org-delete-property) key))

(defun orgtrello-proxy/--standard-delete-success-callback (entity-to-del file-to-cleanup) "Return a callback function able to deal with the position."
  (lexical-let ((op/--entry-position    (orgtrello-data/position entity-to-del))
                (op/--entry-buffer-name (orgtrello-data/buffername entity-to-del))
                (op/--entry-level       (orgtrello-data/level entity-to-del))
                (op/--entry-file        file-to-cleanup)
                (op/--marker            (orgtrello-data/id entity-to-del)))
    (lambda (&rest response)
      (orgtrello-action/safe-wrap
       (progn
         (set-buffer op/--entry-buffer-name)
         (save-excursion
           (when (orgtrello-proxy/--getting-back-to-marker op/--marker)
                 (unless (orgtrello-cbx/checkbox-p) (org-back-to-heading t)) ;; get back to the top level if on heading
                 (orgtrello-action/org-delete-property *ORGTRELLO-ID*)       ;; delete the property
                 (if (org-at-heading-p)
                     (hide-subtree)
                     (when (orgtrello-cbx/checkbox-p) (org-cycle 'fold)))
                 (beginning-of-line)
                 (kill-line)
                 (kill-line))))
       (orgtrello-proxy/--cleanup-and-save-buffer-metadata op/--entry-file op/--entry-buffer-name)))))

(defun orgtrello-proxy/--delete (entity-data entity-full-metadata entry-file-archived) "Execute the entity deletion."
  (lexical-let ((orgtrello-query/--query-map (orgtrello/--dispatch-delete (orgtrello-data/current entity-full-metadata) (orgtrello-data/parent entity-full-metadata)))
                (oq/--entity-full-meta       entity-full-metadata)
                (oq/--entry-file-archived    entry-file-archived))
    (if (hash-table-p orgtrello-query/--query-map)
        (orgtrello-query/http-trello orgtrello-query/--query-map *do-sync-query*
         (orgtrello-proxy/--standard-delete-success-callback entity-data entry-file-archived)
         (function* (lambda (&key error-thrown &allow-other-keys)
                      (orgtrello-log/msg *OT/ERROR* "client - Problem during the deletion request to the proxy- error-thrown: %s" error-thrown)
                      (orgtrello-proxy/--cleanup-meta oq/--entity-full-meta)
                      (orgtrello-proxy/--remove-file oq/--entry-file-archived)
                      (throw 'org-trello-timer-go-to-sleep t))))
        (progn
          (orgtrello-log/msg *OT/INFO* orgtrello-query/--query-map)
          (throw 'org-trello-timer-go-to-sleep t)))))

(defun orgtrello-proxy/--deal-with-entity-file-action (file) "Given an entity file, load it and run a query action through trello"
  (when (file-exists-p file)
        ;; extract the entity data
        (orgtrello-proxy/--deal-with-entity-action (-> file orgtrello-proxy/--read-lines read) file)))

(defun dictionary-lessp (str1 str2) "return t if STR1 is < STR2 when doing a dictionary compare (splitting the string at numbers and doing numeric compare with them)"
  (dict-lessp (dict-split str1) (dict-split str2)))

(defun dict-lessp (slist1 slist2) "compare the two lists of strings & numbers"
  (cond ((null slist1)                                       (not (null slist2)))
        ((null slist2)                                       nil)
        ((and (numberp (car slist1)) (stringp (car slist2))) t)
        ((and (numberp (car slist2)) (stringp (car slist1))) nil)
        ((and (numberp (car slist1)) (numberp (car slist2))) (or (< (car slist1) (car slist2))
                                                                 (and (= (car slist1) (car slist2))
                                                                      (dict-lessp (cdr slist1) (cdr slist2)))))
        (t                                                   (or (string-lessp (car slist1) (car slist2))
                                                                 (and (string-equal (car slist1) (car slist2))
                                                                      (dict-lessp (cdr slist1) (cdr slist2)))))))

(defun dict-split (str) "split a string into a list of number and non-number components"
  (save-match-data
    (let ((res nil))
      (while (and str (not (string-equal "" str)))
        (let ((p (string-match "[0-9]*\\.?[0-9]+" str)))
          (cond ((null p) (setq res (cons str res))
                          (setq str nil))
                ((= p 0)  (setq res (cons (string-to-number (match-string 0 str)) res))
                          (setq str (substring str (match-end 0))))
                (t        (setq res (cons (substring str 0 (match-beginning 0)) res))
                          (setq str (substring str (match-beginning 0)))))))
      (reverse res))))

(defun orgtrello-proxy/--list-files (directory &optional sort-lexicographically) "Compute list of regular files (no directory . and ..). List is sorted lexicographically if sort-flag-lexicographically is set, naturally otherwise."
  (let ((orgtrello-proxy/--list-files-result (--filter (file-regular-p it) (directory-files directory t))))
    (unless sort-lexicographically
        orgtrello-proxy/--list-files-result
        (sort orgtrello-proxy/--list-files-result 'dictionary-lessp))))

(defun orgtrello-proxy/--deal-with-directory-action (level directory) "Given a directory, list the files and take the first one (entity) and do some action on it with trello. Call again if it remains other entities."
  (-when-let (orgtrello-proxy/--files (orgtrello-proxy/--list-files directory))
             (orgtrello-proxy/--deal-with-entity-file-action (car orgtrello-proxy/--files))
             ;; if it potentially remains files, recall recursively this function
             (when (< 1 (length orgtrello-proxy/--files)) (orgtrello-proxy/--deal-with-level level directory))))

(defun orgtrello-proxy/--level-done-p (level) "Does all the entities for the level are their actions done?"
  (-> level
      orgtrello-proxy/--compute-entity-level-dir
      orgtrello-proxy/--list-files
      null))

(defun orgtrello-proxy/--level-inf-done-p (level) "Ensure the actions of the lower level is done (except for level 1 which has no deps)!"
  (cond ((= *CARD-LEVEL*      level) t)
        ((= *CHECKLIST-LEVEL* level) (orgtrello-proxy/--level-done-p *CARD-LEVEL*))
        ((= *ITEM-LEVEL*      level) (and (orgtrello-proxy/--level-done-p *CARD-LEVEL*) (orgtrello-proxy/--level-done-p *CHECKLIST-LEVEL*)))))

(defun orgtrello-proxy/--deal-with-level (level directory)"Given a level, retrieve one file (which represents an entity) for this level and sync it, then remove such file. Then recall the function recursively."
  (if (orgtrello-proxy/--level-inf-done-p level)
     (orgtrello-proxy/--deal-with-directory-action level directory)
     (throw 'org-trello-timer-go-to-sleep t)))

(defun orgtrello-proxy/--deal-with-archived-files (level) "Given a level, move all the remaining archived files into the scan folder from the same level."
  (let ((level-dir (orgtrello-proxy/--compute-entity-level-dir level)))
    (mapc (lambda (file) (rename-file file (format "%s%s" level-dir (file-name-nondirectory file)) t)) (-> level-dir
                                                                                                           orgtrello-proxy/--archived-scanning-dir
                                                                                                           orgtrello-proxy/--list-files))))

(defun orgtrello-proxy/--consumer-entity-files-hierarchically-and-do () "A handler to extract the entity informations from files (in order card, checklist, items)."
  (with-local-quit
    (dolist (l *ORGTRELLO-LEVELS*) (orgtrello-proxy/--deal-with-archived-files l))  ;; if archived file exists, get them back in the queue before anything else
    (catch 'org-trello-timer-go-to-sleep     ;; if some check regarding order fails, we catch and let the timer sleep. The next time, the trigger will get back normally to the upper level in order
      (dolist (l *ORGTRELLO-LEVELS*) (orgtrello-proxy/--deal-with-level l (orgtrello-proxy/--compute-entity-level-dir l))))
    (orgtrello-proxy/batch-save!))) ;; we need to save the modified buffers

(defun orgtrello-proxy/--compute-lock-filename () "Compute the name of a lock file"
  (format "%s%s/%s" elnode-webserver-docroot "org-trello" "org-trello-already-scanning.lock"))

(defvar *ORGTRELLO-LOCK* (orgtrello-proxy/--compute-lock-filename) "Lock file to ensure one timer is running at a time.")

(defun orgtrello-proxy/--timer-put-lock (lock-file) "Start triggering the timer."
  (with-temp-file lock-file
    (insert "Timer - Scanning entities...")))

(defun orgtrello-proxy/--timer-delete-lock (lock-file) "Cleanup after the timer has been triggered."
  (orgtrello-proxy/--remove-file lock-file))

(defun orgtrello-proxy/--consumer-lock-and-scan-entity-files-hierarchically-and-do () "A handler to extract the entity informations from files (in order card, checklist, items)."
  (undo-boundary)
  ;; only one timer at a time
  (orgtrello-action/safe-wrap
   (progn
     (orgtrello-proxy/--timer-put-lock *ORGTRELLO-LOCK*)
     (orgtrello-proxy/--consumer-entity-files-hierarchically-and-do))
   (orgtrello-proxy/--timer-delete-lock *ORGTRELLO-LOCK*))
  ;; undo boundary, to make a unit of undo
  (undo-boundary))

(defun orgtrello-proxy/--windows-system-considered-always-with-network () "function 'network-interface-list is not defined on windows system, so we avoid checking this and always return ok." :ok)

(defun orgtrello-proxy/--check-network-ok () "Ensure network exists!" (if (< 1 (length (network-interface-list))) :ok "No network!"))

(defun orgtrello-proxy/--check-network-connection (&optional args) "Ensure there is some network running (simply check that there is more than the lo interface)."
  (funcall (if (string-equal system-type "windows-nt") 'orgtrello-proxy/--windows-system-considered-always-with-network 'orgtrello-proxy/--check-network-ok)))

(defun orgtrello-proxy/--check-no-running-timer (&optional args) "Ensure there is not another running timer already."
  (if (file-exists-p (orgtrello-proxy/--compute-lock-filename)) "Timer already running!" :ok))

(defun orgtrello-proxy/--controls-and-scan-if-ok () "Execution of the timer which consumes the entities and execute the sync to trello."
  (org-action/--msg-controls-or-actions-then-do
   "Scanning entities to sync"
   '(orgtrello-proxy/--check-network-connection orgtrello-proxy/--check-no-running-timer)
   'orgtrello-proxy/--consumer-lock-and-scan-entity-files-hierarchically-and-do
   nil ;; cannot save the buffer
   nil ;; do not need to reload the org-trello setup
   *do-not-display-log*));; do no want to log

(defun orgtrello-proxy/--prepare-filesystem () "Prepare the filesystem for every level."
  (dolist (l *ORGTRELLO-LEVELS*)
    (-> l
        orgtrello-proxy/--compute-entity-level-dir
        orgtrello-proxy/--archived-scanning-dir
        (mkdir t))))

(defvar *ORGTRELLO-TIMER* nil "A timer run by elnode")

(defun orgtrello-proxy/--elnode-timer (http-con) "A process on elnode to trigger even regularly."
  (let* ((query-map     (orgtrello-proxy/--extract-trello-query http-con))
         (start-or-stop (orgtrello-data/start query-map)))
    (if start-or-stop
        ;; cleanup before starting anew
        (progn
          (orgtrello-log/msg *OT/DEBUG* "Proxy-timer - Request received. Start timer.")
          ;; cleanup anything that the timer possibly left behind
          (orgtrello-proxy/--timer-delete-lock *ORGTRELLO-LOCK*)
          ;; Prepare the filesystem with the right folders
          (orgtrello-proxy/--prepare-filesystem)
          ;; start the timer
          (setq *ORGTRELLO-TIMER* (run-with-timer 0 5 'orgtrello-proxy/--controls-and-scan-if-ok)))
        ;; otherwise, stop it
        (when *ORGTRELLO-TIMER*
              (orgtrello-log/msg *OT/DEBUG* "Proxy-timer - Request received. Stop timer.")
              ;; stop the timer
              (cancel-timer *ORGTRELLO-TIMER*)
              ;; nil the orgtrello reference
              (setq *ORGTRELLO-TIMER* nil)))
    ;; ok in any case
    (orgtrello-proxy/response-ok http-con)))

(defun orgtrello-timer/start () "Start the orgtrello-timer." (orgtrello-proxy/http-consumer t))

(defun orgtrello-timer/stop () "Stop the orgtrello-timer." (orgtrello-proxy/http-consumer nil))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-proxy loaded!")

(provide 'org-trello-proxy)
