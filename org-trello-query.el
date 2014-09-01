;;; org-trello-query.el --- HTTP client namespace.
;;; Commentary:
;;; Code:

(if (version< "24.3" emacs-version)
    (require 'cl-lib)
  (progn ;; need to alias the call
    (require 'cl)
    (defalias 'cl-defun 'defun*)
    (defalias 'cl-destructuring-bind 'destructuring-bind)))

(require 'org-trello-log)
(require 'org-trello-setup)
(require 'org-trello-data)
(require 'request-deferred)

(defconst *ORGTRELLO/TRELLO-URL* "https://api.trello.com/1"
  "The needed prefix url for trello.")

(defun orgtrello-query/--compute-url (server uri)
  "Compute the trello url from the given SERVER and URI."
  (format "%s%s" server uri))

(defun* orgtrello-query/--standard-error-callback (&key response error-thrown &allow-other-keys)
  "Standard error callback which expects a RESPONSE.
Simply displays an error message in the minibuffer with the error code."
  (orgtrello-log/msg *OT/INFO* "client - Problem during request - error-thrown: %s" error-thrown)
  (orgtrello-log/msg *OT/DEBUG* "Detailed response: %S" response))

(defun* orgtrello-query/--standard-success-callback (&key response &allow-other-keys)
  "Standard success callback with expects a RESPONSE.
Simply displays a success message in the minibuffer."
  (let ((data (request-response-data response)))
    (orgtrello-log/msg *OT/DEBUG* "Response: %S" response)
    (orgtrello-log/msg *OT/DEBUG* "Data: %S" data)))

(defun orgtrello-query/--authentication-params ()
  "Generate the list of http authentication parameters."
  `((key . ,*consumer-key*) (token . ,*access-token*)))

(defun orgtrello-query/--http-parse ()
  "Parse the http response into an org-trello entity."
  (->> (json-read)
    orgtrello-data/parse-data))

(defun orgtrello-query/--get (server query-map &optional success-callback error-callback authentication-p)
  "Execute the GET request to SERVER with QUERY-MAP with optional SUCCESS-CALLBACK, ERROR-CALLBACK and AUTHENTICATION-P."
  (with-local-quit
    (let ((uri           (->> query-map orgtrello-data/entity-uri (orgtrello-query/--compute-url server)))
          (entity-method (orgtrello-data/entity-method query-map))
          (params        (orgtrello-data/merge-2-lists-without-duplicates (when authentication-p (orgtrello-query/--authentication-params)) (orgtrello-data/entity-params query-map)))
          (parser        'orgtrello-query/--http-parse)
          (success-cbck  (if success-callback success-callback 'orgtrello-query/--standard-success-callback))
          (error-cbck    (if error-callback error-callback 'orgtrello-query/--standard-error-callback)))
      (if (orgtrello-data/entity-sync query-map)
          (request uri
                   :sync    t
                   :type    entity-method
                   :params  params
                   :parser  parser
                   :success success-cbck
                   :error   error-cbck)
        `(deferred:$
           (request-deferred ,uri
                             :type    ,entity-method
                             :params  (quote ,params)
                             :parser  (quote ,parser))
           (deferred:nextc it
             ,success-cbck)
           (deferred:error it
             ,error-cbck))))))

(defun orgtrello-query/--post-or-put (server query-map &optional success-callback error-callback authentication-p)
  "Execute the POST/PUT request to SERVER with QUERY-MAP with optional SUCCESS-CALLBACK, ERROR-CALLBACK and AUTHENTICATION-P."
  (with-local-quit
    (let ((uri           (->> query-map orgtrello-data/entity-uri (orgtrello-query/--compute-url server)))
          (entity-method (orgtrello-data/entity-method query-map))
          (params        (when authentication-p (orgtrello-query/--authentication-params)))
          (parser        'orgtrello-query/--http-parse)
          (headers       '(("Content-type" . "application/json")))
          (data          (->> query-map orgtrello-data/entity-params json-encode))
          (success-cbck  (if success-callback success-callback 'orgtrello-query/--standard-success-callback))
          (error-cbck    (if error-callback error-callback 'orgtrello-query/--standard-error-callback)))
      (if (orgtrello-data/entity-sync query-map)
          (request uri
                   :sync    t
                   :type    entity-method
                   :params  params
                   :headers headers
                   :data    data
                   :parser  parser
                   :success success-cbck
                   :error   error-cbck)
        `(deferred:$
           (request-deferred ,uri
                             :type    ,entity-method
                             :params  (quote ,params)
                             :headers (quote ,headers)
                             :data    ,data
                             :parser  (quote ,parser))
           (deferred:nextc it
             ,success-cbck)
           (deferred:error it
             ,error-cbck))))))

(defun orgtrello-query/--delete (server query-map &optional success-callback error-callback authentication-p)
  "Execute the DELETE request to SERVER with QUERY-MAP with optional SUCCESS-CALLBACK, ERROR-CALLBACK and AUTHENTICATION-P."
  (with-local-quit
    (let ((uri           (->> query-map orgtrello-data/entity-uri (orgtrello-query/--compute-url server)))
          (entity-method (orgtrello-data/entity-method query-map))
          (params        (when authentication-p (orgtrello-query/--authentication-params)))
          (success-cbck  (if success-callback success-callback 'orgtrello-query/--standard-success-callback))
          (error-cbck    (if error-callback error-callback 'orgtrello-query/--standard-error-callback)))
      (if (orgtrello-data/entity-sync query-map)
          (request uri
                   :sync    t
                   :type    entity-method
                   :params  params
                   :success success-cbck
                   :error   error-cbck)
        `(deferred:$
           (request-deferred ,uri
                             :type    ,entity-method
                             :params  (quote ,params))
           (deferred:nextc it
             ,success-cbck)
           (deferred:error it
             ,error-cbck))))))

(defun orgtrello-query/--dispatch-http-query (method)
  "Dispatch the function to call depending on the METHOD key."
  (cond ((string= "GET" method)                              'orgtrello-query/--get)
        ((or (string= "POST" method) (string= "PUT" method)) 'orgtrello-query/--post-or-put)
        ((string= "DELETE" method)                           'orgtrello-query/--delete)))

(defun orgtrello-query/http (server query-map &optional sync success-callback error-callback authentication-p)
  "Execute an HTTP query to the SERVER with QUERY-MAP and optional SYNC, SUCCESS-CALLBACK, ERROR-CALLBACK and AUTHENTICATION-P."
  (let ((dispatch-http-query-fn (-> query-map
                                  orgtrello-data/entity-method
                                  orgtrello-query/--dispatch-http-query)))
    (if sync
        (--> query-map
          (orgtrello-data/put-entity-sync t it)
          (funcall dispatch-http-query-fn server it success-callback error-callback authentication-p)
          (request-response-data it))
      (funcall dispatch-http-query-fn server query-map success-callback error-callback authentication-p))))

(defun orgtrello-query/http-trello (query-map &optional sync success-callback error-callback)
  "Execute an HTTP query to trello with QUERY-MAP and optional SYNC, SUCCESS-CALLBACK, ERROR-CALLBACK."
  (orgtrello-query/http *ORGTRELLO/TRELLO-URL* query-map sync success-callback error-callback 'with-authentication))

(orgtrello-log/msg *OT/DEBUG* "orgtrello-query loaded!")

(provide 'org-trello-query)
;;; org-trello-query.el ends here
