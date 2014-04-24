(defun orgtrello-proxy/http (query-map &optional sync success-callback error-callback)
  "Query the proxy for the trello api."
  (--> query-map
    (orgtrello-query/--prepare-query-params! it)
    (orgtrello-api/make-query "POST" "/trello/" it)
    (orgtrello-query/http *ORGTRELLO/SERVER-URL* it sync success-callback error-callback)))

(defun orgtrello-proxy/http-producer (query-map &optional sync)
  "Query the proxy producer"
  (--> query-map
    (orgtrello-query/--prepare-query-params! it)
    (orgtrello-api/make-query "POST" "/producer/" it)
    (orgtrello-query/http *ORGTRELLO/SERVER-URL* it sync)))

(defun orgtrello-proxy/http-consumer (start)
  "Query the http-consumer process once to make it trigger a timer"
  (--> `((start . ,start))
    (orgtrello-api/make-query "POST" "/timer/" it)
    (orgtrello-query/http *ORGTRELLO/SERVER-URL* it 'synchronous-query)))

(defun orgtrello-proxy/--json-read-from-string (data)
  "Read the json data and unhexify them."
  (-> data json-read-from-string orgtrello-query/read-data))

(defun orgtrello-proxy/--unhexify-data (params &optional unhexify-flag)
  "Given a params object, unhexify the content if need be."
  (funcall (if unhexify-flag 'orgtrello-proxy/--json-read-from-string 'json-read-from-string) params))

(defun orgtrello-proxy/--extract-trello-query (http-con &optional unhexify-flag)
  "Given an httpcon object, extract the params entry which corresponds to the real trello query."
  (-> http-con
    elnode-http-params
    caar
    (orgtrello-proxy/--unhexify-data unhexify-flag)))

(defun orgtrello-proxy/--compute-trello-query (query-map-wrapped)
  "Build a trello query from the control of query-map-wrapped."
  (orgtrello-api/make-query (orgtrello-data/entity-method query-map-wrapped) (orgtrello-data/entity-uri query-map-wrapped) (orgtrello-data/entity-params query-map-wrapped)))

(defun orgtrello-proxy/--response (http-con data)
  "A response wrapper"
  (elnode-http-start http-con 201 '("Content-type" . "application/json"))
  (elnode-http-return http-con (json-encode data)))

(defun orgtrello-proxy/response-ok (http-con)
  "OK response from the proxy to the client." ;; all is good
  (orgtrello-proxy/--response http-con '((status . "ok"))))

(defconst *ORGTRELLO/PROXY-QUERY-KEY* (orgtrello-hash/make-properties '((params . :params)
                                                                        (method . :method)
                                                                        (uri    . :uri)
                                                                        (callback . :callback)
                                                                        (buffername . :buffername)
                                                                        (position . :position)
                                                                        (sync . :sync)
                                                                        (name . :name)
                                                                        (id . :id)
                                                                        (action . :action)
                                                                        (level . :level)
                                                                        (start . :start)))
  "Constant to map from assoc list to org-trello query map.")

(defun orgtrello-proxy/--transcode-key (key)
  "Retrieve the corresponding key."
  (gethash key *ORGTRELLO/PROXY-QUERY-KEY*))

(defun orgtrello-proxy/parse-query (entities)
  "Given a query wrapped, convert into org-trello entity. 'params key stayed as is."
  (cond ((eq :params entities)  entities) ;; return params as is
        (t                      (--reduce-from (let ((key (car it))
                                                     (val (cdr it)))
                                                 (-when-let (new-key (orgtrello-proxy/--transcode-key key))
                                                   (orgtrello-data/puthash-data new-key val acc))
                                                 acc)
                                               (orgtrello-hash/empty-hash)
                                               entities))))

(defun orgtrello-proxy/--elnode-proxy (http-con)
  "Deal with request to trello (for creation/sync request, use orgtrello-proxy/--elnode-proxy-producer)."
  (orgtrello-log/msg *OT/TRACE* "Proxy - Request received. Transmitting...")
  (let* ((query-map-wrapped    (orgtrello-proxy/--extract-trello-query http-con 'unhexify)) ;; wrapped query is mandatory, we unhexify the wrapped query
         (query-map-data       (orgtrello-proxy/parse-query query-map-wrapped))
         (position             (orgtrello-data/entity-position query-map-data)) ;; position is mandatory
         (buffer-name          (orgtrello-data/entity-buffername query-map-data)) ;; buffer-name is mandatory
         (standard-callback    (orgtrello-data/entity-callback query-map-data)) ;; there is the possibility to transmit the callback from the client to the proxy
         (standard-callback-fn (when standard-callback (symbol-function (intern standard-callback)))) ;; the callback is passed as a string, we want it as a function when defined
         (sync                 (orgtrello-data/entity-sync query-map-data)) ;; there is a possibility to enforce the sync between proxy and client
         (query-map            (orgtrello-proxy/--compute-trello-query query-map-data)) ;; extracting the query
         (name                 (orgtrello-data/entity-name query-map-data))) ;; extracting the name of the entity (optional)
    (orgtrello-query/http-trello query-map sync (when standard-callback-fn (funcall standard-callback-fn buffer-name position name))) ;; callback-fn is not a callback, it creates one
    (orgtrello-proxy/response-ok http-con)))

(defun orgtrello-proxy/--compute-metadata-filename (root-dir buffer-name position)
  "Compute the metadata entity filename from a buffer-name"
  (--> buffer-name
    (orgtrello-buffer/defensive-filename-from-buffername it)
    (format "%s%s-%s.el" root-dir it position)))

(defvar *ORGTRELLO-PROXY/DB* nil "Database reference to orgtrello-proxy")

(defun orgtrello-proxy/--elnode-proxy-producer (http-con)
  "A handler which is an entity informations producer on files under the docroot/level-entities/"
  (orgtrello-log/msg *OT/TRACE* "Proxy-producer - Request received. Generating entity file...")
  (let* ((query-map-wrapped    (orgtrello-proxy/--extract-trello-query http-con 'unhexify)) ;; wrapped query is mandatory
         (query-map-data       (orgtrello-proxy/parse-query query-map-wrapped))
         (level                (orgtrello-data/entity-level query-map-data)))
    (orgtrello-db/put level query-map-data *ORGTRELLO-PROXY/DB*)
    (orgtrello-proxy/response-ok http-con)))

(defun orgtrello-proxy/--read-file-content (fPath)
  "Return a list of lines of a file at FPATH."
  (with-temp-buffer
    (insert-file-contents fPath)
    (buffer-string)))

(defun orgtrello-proxy/--update-buffer-to-save (buffer-name buffers-to-save)
  "Add the buffer-name to the list if not already present"
  (if (member buffer-name buffers-to-save)
      buffers-to-save
    (cons buffer-name buffers-to-save)))

(defvar *ORGTRELLO/LIST-BUFFERS-TO-SAVE* nil "A simple flag to order the saving of buffer when needed.")

(defun orgtrello-proxy/update-buffer-to-save! (buffer-name)
  "Side-effect - Mutate the *ORGTRELLO/LIST-BUFFERS-TO-SAVE* by adding buffer-name to it if not already present."
  (setq *ORGTRELLO/LIST-BUFFERS-TO-SAVE* (orgtrello-proxy/--update-buffer-to-save buffer-name *ORGTRELLO/LIST-BUFFERS-TO-SAVE*)))

(defun orgtrello-proxy/--cleanup-and-save-buffer-metadata (level buffer-name)
  "To cleanup metadata after all the actions are done!"
  (orgtrello-proxy/cleanup-archived-entity! level)
  (orgtrello-proxy/update-buffer-to-save! buffer-name)) ;; register the buffer for later saving

(defun orgtrello-proxy/batch-save (buffers)
  "Save sequentially a list of buffers."
  (-each buffers 'save-buffer))

(defun orgtrello-proxy/batch-save! ()
  "Save sequentially the org-trello list of modified buffers."
  (setq *ORGTRELLO/LIST-BUFFERS-TO-SAVE* (orgtrello-proxy/batch-save *ORGTRELLO/LIST-BUFFERS-TO-SAVE*)))

(defmacro orgtrello-proxy/--safe-wrap-or-throw-error (fn)
  "A specific macro to deal with interception of uncaught error when executing the fn call. If error is thrown, send the 'org-trello-timer-go-to-sleep flag."
  `(condition-case ex
       (progn ,fn)
     ('error
      (orgtrello-log/msg *OT/ERROR* (concat "### org-trello - consumer ### Caught exception: [" ex "]"))
      (throw 'org-trello-timer-go-to-sleep t))))

(defun orgtrello-proxy/--getting-back-to-headline (data)
  "Trying another approach to getting back to header computing the normal form of an entry in the buffer."
  (orgtrello-proxy/--getting-back-to-marker (orgtrello-buffer/--compute-entity-to-org-entry data)))

(defun orgtrello-proxy/--compute-pattern-search-from-marker (marker)
  "Given a marker, compute the pattern to look for in the file."
  marker)

(defun orgtrello-proxy/--getting-back-to-marker (marker)
  "Given a marker, getting back to marker function. Move the cursor position."
  (goto-char (point-min))
  (re-search-forward (orgtrello-proxy/--compute-pattern-search-from-marker marker) nil t))

(defun orgtrello-proxy/--get-back-to-marker (marker data)
  "Getting back to the marker. Move the cursor position."
  (-if-let (goto-ok (orgtrello-proxy/--getting-back-to-marker marker))
      goto-ok
    (orgtrello-proxy/--getting-back-to-headline data)))

(defun orgtrello-proxy/--standard-post-or-put-success-callback (entity-to-sync)
  "Return a callback function able to deal with the update of the buffer at a given position."
  (lexical-let ((entry-position    (orgtrello-data/entity-position entity-to-sync))
                (entry-buffer-name (orgtrello-data/entity-buffername entity-to-sync))
                (level             (orgtrello-data/entity-level entity-to-sync))
                (marker-id         (orgtrello-data/entity-id-or-marker entity-to-sync))
                (entity-name       (orgtrello-data/entity-name entity-to-sync)))
    (function* (lambda (&key data &allow-other-keys)
                 (orgtrello-action/safe-wrap
                  (let ((entry-new-id (orgtrello-data/entity-id data)))
                    (set-buffer entry-buffer-name) ;; switch to the right buffer
                    ;; will update via tag the trello id of the new persisted data (if needed)
                    (save-excursion
                      ;; get back to the buffer and update the id if need be
                      (-when-let (str-msg (when (orgtrello-proxy/--get-back-to-marker marker-id data)
                                            (-if-let (entry-id (when (orgtrello-data/id-p marker-id) marker-id)) ;; Already present, we do nothing on the buffer
                                                (format "Entity '%s' with id '%s' synced!" entity-name entry-id)
                                              (let ((entry-name (orgtrello-data/entity-name data))) ;; not present, this was just created, we add a simple property
                                                (orgtrello-buffer/set-property *ORGTRELLO/ID* entry-new-id)
                                                (format "Newly entity '%s' with id '%s' synced!" entry-name entry-new-id)))))
                        (orgtrello-log/msg *OT/INFO* str-msg))))
                  (orgtrello-proxy/--cleanup-and-save-buffer-metadata level entry-buffer-name))))))

(defun orgtrello-proxy/--archived-scanning-file (file)
  "Given a filename, return its archived filename if we were to move such file."
  (format "%s/%s" (orgtrello-elnode/archived-scanning-dir (file-name-directory file)) (file-name-nondirectory file)))

(defun orgtrello-proxy/--archive-entity-file-when-scanning (file-to-archive file-archive-name)
  "Move the file to the running folder to specify a sync is running."
  (rename-file file file-archive-name t))

(defun orgtrello-proxy/--dispatch-action (action)
  "Dispatch action function depending on the flag action"
  (cond ((string= *ORGTRELLO/ACTION-DELETE* action) 'orgtrello-proxy/--delete)
        ((string= *ORGTRELLO/ACTION-SYNC*   action) 'orgtrello-proxy/--sync-entity)))

(defun orgtrello-proxy/--cleanup-meta (entity-full-metadata)
  (unless (-> entity-full-metadata
            orgtrello-data/current
            orgtrello-data/entity-id)
    (orgtrello-cbx/org-delete-property *ORGTRELLO/ID*)))

(defun orgtrello-proxy/--sync-entity (entity-data entity-full-metadata)
  "Execute the entity synchronization."
  (lexical-let ((query-map           (orgtrello-controller/--dispatch-create entity-full-metadata))
                (entity-full-meta    entity-full-metadata)
                (level               (orgtrello-data/entity-level entity-data)))
    (if (hash-table-p query-map)
        ;; execute the request
        (orgtrello-query/http-trello
         query-map
         'synchronous-query
         (orgtrello-proxy/--standard-post-or-put-success-callback entity-data)
         (function* (lambda (&key error-thrown &allow-other-keys)
                      (orgtrello-log/msg *OT/ERROR* "client - Problem during the sync request to the proxy- error-thrown: %s" error-thrown)
                      (orgtrello-proxy/--cleanup-meta entity-full-meta)
                      (orgtrello-proxy/cleanup-archived-entity! level)
                      (throw 'org-trello-timer-go-to-sleep t))))
      ;; cannot execute the request
      (progn
        (orgtrello-log/msg *OT/INFO* query-map)
        (orgtrello-proxy/--cleanup-meta entity-full-metadata)
        (throw 'org-trello-timer-go-to-sleep t)))))

(defun orgtrello-proxy/--deal-with-entity-action (entity-data)
  "Compute the synchronization of an entity (retrieving latest information from buffer)"
  (let* ((position    (orgtrello-data/entity-position entity-data))   ;; position is mandatory
         (buffer-name (orgtrello-data/entity-buffername entity-data))    ;; buffer-name too
         (marker      (orgtrello-data/entity-id-or-marker entity-data))  ;; retrieve the id (which serves as a marker too)
         (level       (orgtrello-data/entity-level entity-data)))
    (orgtrello-log/msg *OT/TRACE* "Proxy-consumer - Searching entity metadata from buffer '%s' at point '%s' to sync..." buffer-name position)
    (set-buffer buffer-name)                                                     ;; switch to the right buffer
    (orgtrello-proxy/--safe-wrap-or-throw-error ;; will update via tag the trello id of the new persisted data (if needed)
     (save-excursion
       (when (orgtrello-proxy/--get-back-to-marker marker entity-data)
         (orgtrello-db/put (orgtrello-proxy/archive-key level) entity-data *ORGTRELLO-PROXY/DB*) ;; keep an archived version
         (-> entity-data
           orgtrello-data/entity-action
           orgtrello-proxy/--dispatch-action
           (funcall entity-data (orgtrello-buffer/entry-get-full-metadata!))))))))

(defun orgtrello-action/--delete-region (start end)
  "Delete a region defined by start and end bound."
  (remove-overlays start end) ;; remove overlays on the card region
  (delete-region start end))

(defun orgtrello-action/--delete-card-region ()
  "Delete the card region (including overlays and line)"
  (org-back-to-heading)
  (let ((orgtrello-action/--starting-point (point))
        (orgtrello-action/--ending-point   (save-excursion (if (org-goto-sibling) (point) (point-max))))) ;; next card or point-max
    (orgtrello-action/--delete-region orgtrello-action/--starting-point orgtrello-action/--ending-point)))

(defun orgtrello-action/--delete-checkbox-checklist-region ()
  "Delete the checklist region"
  (let ((orgtrello-action/--starting-point (point-at-bol))
        (orgtrello-action/--ending-point (save-excursion (-if-let (result (orgtrello-cbx/--goto-next-checkbox-with-same-level! *ORGTRELLO/CHECKLIST-LEVEL*))
                                                             result
                                                           (orgtrello-cbx/compute-next-card-point!))))) ;; next checkbox or next card or point-max
    (orgtrello-action/--delete-region orgtrello-action/--starting-point orgtrello-action/--ending-point)))

(defun orgtrello-action/--delete-checkbox-item-region ()
  "Delete the item region"
  (let ((orgtrello-action/--starting-point (point-at-bol))
        (orgtrello-action/--ending-point (1+ (point-at-eol))))
    (orgtrello-action/--delete-region orgtrello-action/--starting-point orgtrello-action/--ending-point)))

(defun orgtrello-action/delete-region (entity)
  "Delete the region"
  (cond ((orgtrello-data/entity-card-p entity) 'orgtrello-action/--delete-card-region)
        ((orgtrello-data/entity-checklist-p entity) 'orgtrello-action/--delete-checkbox-checklist-region)
        ((orgtrello-data/entity-item-p entity) 'orgtrello-action/--delete-checkbox-item-region)))

(defun orgtrello-proxy/--standard-delete-success-callback (entity-to-del)
  "Return a callback function able to deal with the position."
  (lexical-let ((entry-position    (orgtrello-data/entity-position entity-to-del))
                (entry-buffer-name (orgtrello-data/entity-buffername entity-to-del))
                (entry-level       (orgtrello-data/entity-level entity-to-del))
                (marker            (orgtrello-data/entity-id entity-to-del))
                (level             (orgtrello-data/entity-level entity-to-del)))
    (lambda (&rest response)
      (orgtrello-action/safe-wrap
       (progn
         (set-buffer entry-buffer-name)
         (save-excursion
           (when (orgtrello-proxy/--getting-back-to-marker marker)
             (-> (orgtrello-buffer/entry-get-full-metadata!)
               orgtrello-data/current
               orgtrello-action/delete-region
               funcall))))
       (orgtrello-proxy/--cleanup-and-save-buffer-metadata level entry-buffer-name)))))

(defun orgtrello-proxy/archive-key (level)
  "Compute the key for the archive to store the entity in."
  (format "%s-archived" level))

(defun orgtrello-proxy/cleanup-archived-entity! (level)
  "Clean archived entity from model when done!"
  (-> level
    orgtrello-proxy/archive-key
    (orgtrello-db/pop *ORGTRELLO-PROXY/DB*)))

(defun orgtrello-proxy/--delete (entity-data entity-full-metadata)
  "Execute the entity deletion."
  (lexical-let ((query-map        (orgtrello-controller/--dispatch-delete (orgtrello-data/current entity-full-metadata) (orgtrello-data/parent entity-full-metadata)))
                (entity-full-meta entity-full-metadata)
                (level            (orgtrello-data/entity-level entity-data)))
    (if (hash-table-p query-map)
        (orgtrello-query/http-trello
         query-map
         'synchronous-query
         (orgtrello-proxy/--standard-delete-success-callback entity-data)
         (function* (lambda (&key error-thrown &allow-other-keys)
                      (orgtrello-log/msg *OT/ERROR* "client - Problem during the deletion request to the proxy- error-thrown: %s" error-thrown)
                      (orgtrello-proxy/--cleanup-meta entity-full-meta)
                      (orgtrello-proxy/cleanup-archived-entity! level)
                      (throw 'org-trello-timer-go-to-sleep t))))
      (progn
        (orgtrello-log/msg *OT/INFO* query-map)
        (throw 'org-trello-timer-go-to-sleep t)))))

(defun orgtrello-proxy/--deal-with-entities-at-level (level)
  "Given a level, retrieve entity from this level and do some action on it with trello. Call again if it remains other entities."
  (-when-let (entity (orgtrello-db/pop level *ORGTRELLO-PROXY/DB*))
    (orgtrello-proxy/--deal-with-entity-action entity) ;; deal with entity action
    (unless (orgtrello-proxy/--level-done-p level)     ;; if level is not done, call again for the same level
      (orgtrello-proxy/--deal-with-level level))))

(defun orgtrello-proxy/--level-done-p (level)
  "Does there remain some entities for the level specified?"
  (-> level
    (orgtrello-db/get *ORGTRELLO-PROXY/DB*)
    null))

(defun orgtrello-proxy/--level-inf-done-p (level)
  "Ensure the actions of the lower level is done (except for level 1 which has no deps)!"
  (cond ((= *ORGTRELLO/CARD-LEVEL*      level) t)
        ((= *ORGTRELLO/CHECKLIST-LEVEL* level) (orgtrello-proxy/--level-done-p *ORGTRELLO/CARD-LEVEL*))
        ((= *ORGTRELLO/ITEM-LEVEL*      level) (and (orgtrello-proxy/--level-done-p *ORGTRELLO/CARD-LEVEL*) (orgtrello-proxy/--level-done-p *ORGTRELLO/CHECKLIST-LEVEL*)))))

(defun orgtrello-proxy/--deal-with-level (level)
  "Given a level, retrieve one entity from this level and sync it. Then recall the function recursively (mutually recursive)."
  (if (orgtrello-proxy/--level-inf-done-p level)
      (orgtrello-proxy/--deal-with-entities-at-level level)
    (throw 'org-trello-timer-go-to-sleep t)))

(defun orgtrello-proxy/--deal-with-archived-files (level)
  "Given a level, move all the remaining archived files into the scan folder from the same level."
  (let ((level-dir (orgtrello-elnode/compute-entity-level-dir level)))
    (mapc (lambda (file) (rename-file file (format "%s%s" level-dir (file-name-nondirectory file)) t)) (-> level-dir
                                                                                                         orgtrello-elnode/archived-scanning-dir
                                                                                                         orgtrello-elnode/list-files))))

(defun orgtrello-proxy/--consumer-entity-sync-hierarchically-and-do ()
  "A handler to extract the entity informations (in order card, checklist, items)."
  (with-local-quit
    ;; FIXME deal with problem on some entity later
    ;; (dolist (l *ORGTRELLO/LEVELS*) (orgtrello-proxy/--deal-with-archived-files l))
    (catch 'org-trello-timer-go-to-sleep ;; if some check regarding order fails, we catch and let the timer sleep.
                                         ;; The next time, the trigger will get back normally to the upper level in order
      (dolist (level *ORGTRELLO/LEVELS*)
        (orgtrello-proxy/--deal-with-level level)))
    (orgtrello-proxy/batch-save!))) ;; we need to save the modified buffers

(defun orgtrello-proxy/--compute-lock-filename ()
  "Compute the name of a lock file"
  (format "%s%s/%s" elnode-webserver-docroot "org-trello" "org-trello-already-scanning.lock"))

(defvar *ORGTRELLO/LOCK* (orgtrello-proxy/--compute-lock-filename)
  "Lock file to ensure one timer is running at a time.")

(defun orgtrello-proxy/--timer-put-lock (lock-file)
  "Start triggering the timer."
  (with-temp-file lock-file
    (insert "Timer - Scanning entities...")))

(defun orgtrello-proxy/--timer-delete-lock (lock-file)
  "Cleanup after the timer has been triggered."
  (orgtrello-action/delete-file! lock-file))

(defun orgtrello-proxy/--consumer-lock-sync-entity-hierarchically-and-do ()
  "A handler to extract the entity informations from files (in order card, checklist, items)."
  (undo-boundary)
  ;; only one timer at a time
  (orgtrello-action/safe-wrap
   (progn
     (orgtrello-proxy/--timer-put-lock *ORGTRELLO/LOCK*)
     (orgtrello-proxy/--consumer-entity-sync-hierarchically-and-do))
   (orgtrello-proxy/--timer-delete-lock *ORGTRELLO/LOCK*))
  ;; undo boundary, to make a unit of undo
  (undo-boundary))

(defun orgtrello-proxy/--windows-system-considered-always-with-network ()
  "function 'network-interface-list is not defined on windows system, so we avoid checking this and always return ok." :ok)

(defun orgtrello-proxy/--check-network-ok ()
  "Ensure network exists!" (if (< 1 (length (network-interface-list))) :ok "No network!"))

(defun orgtrello-proxy/--check-network-connection (&optional args)
  "Ensure there is some network running (simply check that there is more than the lo interface)."
  (funcall (if (string-equal system-type "windows-nt") 'orgtrello-proxy/--windows-system-considered-always-with-network 'orgtrello-proxy/--check-network-ok)))

(defun orgtrello-proxy/--check-no-running-timer (&optional args)
  "Ensure there is not another running timer already."
  (if (file-exists-p (orgtrello-proxy/--compute-lock-filename)) "Timer already running!" :ok))

(defun orgtrello-proxy/--controls-and-scan-if-ok ()
  "Execution of the timer which consumes the entities and execute the sync to trello."
  (orgtrello-action/msg-controls-or-actions-then-do
   "Scanning entities to sync"
   '(orgtrello-proxy/--check-network-connection orgtrello-proxy/--check-no-running-timer)
   'orgtrello-proxy/--consumer-lock-sync-entity-hierarchically-and-do
   nil ;; cannot save the buffer
   nil ;; do not need to reload the org-trello setup
   'do-not-display-log));; do no want to log

(defun orgtrello-proxy/--prepare-filesystem ()
  "Prepare the filesystem for every level."
  (dolist (l *ORGTRELLO/LEVELS*)
    (-> l
      orgtrello-elnode/compute-entity-level-dir
      orgtrello-elnode/archived-scanning-dir
      (mkdir t))))

(defvar *ORGTRELLO/TIMER* nil "A timer run by elnode")

(defun orgtrello-proxy/--elnode-timer (http-con)
  "A process on elnode to trigger regularly."
  (let* ((query-map     (-> http-con orgtrello-proxy/--extract-trello-query orgtrello-proxy/parse-query))
         (start-or-stop (orgtrello-data/entity-start query-map)))
    (if start-or-stop
        ;; cleanup before starting anew
        (progn
          (orgtrello-log/msg *OT/DEBUG* "Proxy-timer - Request received. Start timer.")
          ;; cleanup anything that the timer possibly left behind
          (orgtrello-proxy/--timer-delete-lock *ORGTRELLO/LOCK*)
          ;; start the timer
          (setq *ORGTRELLO/TIMER* (run-with-timer 0 5 'orgtrello-proxy/--controls-and-scan-if-ok)))
      ;; otherwise, stop it
      (when *ORGTRELLO/TIMER*
        (orgtrello-log/msg *OT/DEBUG* "Proxy-timer - Request received. Stop timer.")
        ;; stop the timer
        (cancel-timer *ORGTRELLO/TIMER*)
        ;; nil the orgtrello reference
        (setq *ORGTRELLO/TIMER* nil)))
    ;; ok in any case
    (orgtrello-proxy/response-ok http-con)))

(defun orgtrello-proxy/timer-start ()
  "Start the orgtrello-timer."
  (orgtrello-proxy/http-consumer t))

(defun orgtrello-proxy/timer-stop ()
  "Stop the orgtrello-timer."
  (orgtrello-proxy/http-consumer nil))

(defun orgtrello-action/deal-with-consumer-msg-controls-or-actions-then-do (msg control-or-action-fns fn-to-execute &optional save-buffer-p reload-setup-p nolog-p)
  "Decorator fn to execute actions before/after the controls."
  (orgtrello-proxy/timer-stop)
  (orgtrello-action/msg-controls-or-actions-then-do msg control-or-action-fns fn-to-execute save-buffer-p reload-setup-p nolog-p)   ;; Execute as usual
  (orgtrello-proxy/timer-start))

(defvar *ORGTRELLO/QUERY-APP-ROUTES-PROXY*
  '(;; proxy to request trello
    ("^localhost//proxy/trello/\\(.*\\)" . orgtrello-proxy/--elnode-proxy)
    ;; proxy producer to receive async creation request
    ("^localhost//proxy/producer/\\(.*\\)" . orgtrello-proxy/--elnode-proxy-producer)
    ;; proxy to request trello
    ("^localhost//proxy/timer/\\(.*\\)" . orgtrello-proxy/--elnode-timer))
  "Org-trello dispatch routes for the proxy")

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-proxy loaded!")


