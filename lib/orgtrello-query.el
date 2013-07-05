;;; orgtrello-query.el -- Http query 'namespace' to declare the query functions to execute the sync on trello

(require 'org)
(require 'request)
(require 'orgtrello-data)
(require 'cl-lib)

(defvar *TRELLO-URL* "https://api.trello.com/1" "The needed prefix url for trello")

(defun orgtrello-query--make-dispatch-http-query ()
  "Make a map that will dispatch the function to call depending on the http verb :get, :put, :post, etc..."
  (let* ((map-dispatch (make-hash-table :test 'equal)))
    (puthash :get    'orgtrello-query--get         map-dispatch)
    (puthash :put    'orgtrello-query--post-or-put map-dispatch)
    (puthash :post   'orgtrello-query--post-or-put map-dispatch)
    (puthash :delete 'orgtrello-query--delete      map-dispatch)
    map-dispatch))

(defvar *MAP-DISPATCH-HTTP-QUERY* (orgtrello-query--make-dispatch-http-query))

(defun orgtrello-query-http (query-map)
  "Query the trello api asynchronously."
  (let* ((method      (gethash :method query-map))
         (fn-dispatch (gethash method *MAP-DISPATCH-HTTP-QUERY*)))
    (funcall fn-dispatch query-map)))

(defun orgtrello-query-http-sync (query-map)
  "Query the trello api synchronously and return the data of the request."
  (let* ((method      (gethash :method query-map))
         (fn-dispatch (gethash method *MAP-DISPATCH-HTTP-QUERY*)))
    (puthash :sync t query-map)
    (let ((request-response (funcall fn-dispatch query-map)))
      (request-response-data request-response))))

(defun orgtrello-query--map-dispatch-http-verb ()
  (let* ((map-dispatch (make-hash-table :test 'equal)))
    (puthash :get    "GET"    map-dispatch)
    (puthash :put    "PUT"    map-dispatch)
    (puthash :post   "POST"   map-dispatch)
    (puthash :delete "DELETE" map-dispatch)
    map-dispatch))

(defvar *MAP-DISPATCH-HTTP-VERB* (orgtrello-query--map-dispatch-http-verb))

(defun orgtrello-query--compute-method (method)
  "Given the keywords :get, :post, :put, :delete, map them into standard uppercase string."
  (gethash method *MAP-DISPATCH-HTTP-VERB*))

(defun orgtrello-query--compute-url (uri)
  "Compute the trello url from the given uri."
  (format "%s%s" *TRELLO-URL* uri))

(cl-defun standard-error-callback (&key error-thrown &allow-other-keys)
  "Standard error callback"
  (message "There was some problem during the request to trello: %s" error-thrown))

(cl-defun standard-success-callback ()
  "Standard success callback"
  (message "Success."))

(defun orgtrello-query--get (query-map)
  "GET"
  (let* ((method (gethash :method query-map))
         (uri    (gethash :uri    query-map))
         (sync   (gethash :sync   query-map)))
    (request (orgtrello-query--compute-url uri)
             :sync    sync
             :type    (orgtrello-query--compute-method method)
             :params  `((key . ,*consumer-key*)
                        (token . ,*access-token*))
             :parser  'json-read
             :success 'standard-success-callback
             :error   'standard-error-callback)))

(cl-defun orgtrello-query/--post-put-success-callback-update-id (&key data &allow-other-keys)
  "Called back function at the end of the post/put request to update the trello id in the org-mode file."
  (let* ((orgtrello-query/--entry-new-id (assoc-default 'id data))
         (orgtrello-query/--entry-name   (assoc-default 'name data)))
    ;; for testing reasons
    ;; (interactive)
    ;; (defvar data nil)
    ;; (setq data '((id . "1234") (name . "v0.0.1")))
    ;; will update via tag the trello id of the new persisted data (if needed)
    (save-excursion
      ;; find the current entry
      (org-goto-local-search-headings orgtrello-query/--entry-name nil nil)
      ;; now we extract the data
      (let* ((orgtrello-query/--entry-metadata (orgtrello-data-metadata))
             (orgtrello-query/--entry-id       (gethash :id orgtrello-query/--entry-metadata)))
        (if orgtrello-query/--entry-id ;; id already present in the org-mode file
            ;; no need to add another
            (message "entity '%s' synced with id '%s'" orgtrello-query/--entry-name orgtrello-query/--entry-id)
          (progn
            ;; not present, this was just created, we add a simple property
            (org-set-property "orgtrello-id" orgtrello-query/--entry-new-id)
            (message "Newly entity '%s' synced with id '%s'" orgtrello-query/--entry-name orgtrello-query/--entry-new-id)))))))

(defun orgtrello-query--post-or-put (query-map)
  "POST or PUT"
  (let* ((method  (gethash :method query-map))
         (uri     (gethash :uri    query-map))
         (payload (gethash :params query-map))
         (sync    (gethash :sync   query-map)))
    (request (orgtrello-query--compute-url uri)
             :sync    sync
             :type    (orgtrello-query--compute-method method)
             :params  `((key . ,*consumer-key*)
                        (token . ,*access-token*))
             :headers '(("Content-type" . "application/json"))
             :data    (json-encode payload)
             :parser  'json-read
             :success 'orgtrello-query/--post-put-success-callback-update-id
             :error   'standard-error-callback)))

(cl-defun orgtrello-query/--delete-success-callback (&key data response &allow-other-keys)
  "Callback function called at the end of a successful delete request."
  (org-delete-property "orgtrello-id")
  (org-force-cycle-archived)
  (kill-line)
  (kill-line)
  (message "Entity deleted!" ))

(defun orgtrello-query--delete (query-map)
  "DELETE"
  (let* ((method (gethash :method query-map))
         (uri    (gethash :uri    query-map))
         (sync   (gethash :sync   query-map)))
    (request (orgtrello-query--compute-url uri)
             :sync    sync
             :type    (orgtrello-query--compute-method method)
             :params  `((key . ,*consumer-key*)
                        (token . ,*access-token*))
             :success 'orgtrello-query/--delete-success-callback
             :error   'standard-error-callback)))

(message "orgtrello-query loaded!")

(provide 'orgtrello-query)

;;; orgtrello-query.el ends here
