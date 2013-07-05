;;; orgtrello-query.el -- Http query 'namespace' to declare the query functions to execute the sync on trello

(require 'org)
(require 'request)
(require 'orgtrello-data)
(require 'cl)

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

(defun orgtrello-query--get (query-map)
  "GET"
  (let* ((method (gethash :method query-map))
         (uri    (gethash :uri    query-map))
         (sync   (gethash :sync   query-map)))
    (request (orgtrello-query--compute-url uri)
             :sync    sync
             :type    (orgtrello-query--compute-method method)
             :params  `((key . ,consumer-key)
                        (token . ,access-token))
             :parser  'json-read
             :success (function*
                       (lambda (&key data &allow-other-keys)
                         (message "success: %S" data)))
             :error   (function*
                       (lambda (&key error-thrown response &allow-other-keys)
                         (message "error: %S\n%S" error-thrown response))))))

(defun* orgtrello-query/--post-put-success-callback-update-id (&key data &allow-other-keys)
  "Called back function at the end of the post/put request to update the trello id in the org-mode file."
  ;; for testing reasons
  ;; (interactive)
  ;; (defvar data nil)
  ;; (setq data '((id . "1234") (name . "v0.0.1")))
  ;; will update via tag the trello id of the new persisted data (if needed)
  (save-excursion
    (message "entity: %s" (assoc-default 'name data))
    (while (org-up-heading-safe))
    ;; find the current entry
    (org-goto-local-search-headings (assoc-default 'name data) nil nil)
    ;; now we extract the data
    (let* ((metadata    (orgtrello-data-metadata))
           (original-id (gethash :id metadata))
           (id          (assoc-default 'id data)))
      (if original-id ;; id already present in the org-mode file
          ;; no need to add another
          (message "id %s already present" original-id)
        ;; not present, this was just created, we add a simple property
        (org-set-property "orgtrello-id" id)))))

(defun orgtrello-query--post-or-put (query-map)
  "POST or PUT"
  (let* ((method  (gethash :method query-map))
         (uri     (gethash :uri    query-map))
         (payload (gethash :params query-map))
         (sync    (gethash :sync   query-map)))
    (request (orgtrello-query--compute-url uri)
             :sync    sync
             :type    (orgtrello-query--compute-method method)
             :params  `((key . ,consumer-key)
                        (token . ,access-token))
             :headers '(("Content-type" . "application/json"))
             :data    (json-encode payload)
             :parser  'json-read
             :success 'orgtrello-query/--post-put-success-callback-update-id
             ;; :success (lambda (&rest args)
             ;;            (princ (plist-get args :data)))
             :error (function*
                     (lambda (&key error-thrown response &allow-other-keys)
                       (message "error: %S\n%S" error-thrown response))))))

(defun* orgtrello-query/--delete-success-callback (&key data response &allow-other-keys)
  "Callback function called at the end of a successful delete request."
  (org-delete-property "orgtrello-id")
  (org-force-cycle-archived)
  (kill-line)
  (kill-line))

(defun orgtrello-query--delete (query-map)
  "DELETE"
  (let* ((method (gethash :method query-map))
         (uri    (gethash :uri    query-map))
         (sync   (gethash :sync   query-map)))
    (request (orgtrello-query--compute-url uri)
             :sync    sync
             :type    (orgtrello-query--compute-method method)
             :params  `((key . ,consumer-key)
                        (token . ,access-token))
             :success 'orgtrello-query/--delete-success-callback
             :error (function*
                     (lambda (&key error-thrown response &allow-other-keys)
                       (message "error: %S\n%S" error-thrown response))))))

(provide 'orgtrello-query)

;;; orgtrello-query.el ends here
