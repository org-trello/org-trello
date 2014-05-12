;;; org-trello-query.el --- HTTP client namespace.
;;; Commentary:
;;; Code:

(if (version< "24.3" emacs-version)
    (require 'cl-lib)
  (progn ;; need to alias the call
    (require 'cl)
    (defalias 'cl-defun 'defun*)
    (defalias 'cl-destructuring-bind 'destructuring-bind)))

(require 'request)
(require 'org-trello-log)
(require 'org-trello-setup)
(require 'org-trello-data)

(defconst *ORGTRELLO/TRELLO-URL* "https://api.trello.com/1"
  "The needed prefix url for trello.")

(defun orgtrello-query/--compute-url (server uri)
  "Compute the trello url from the given SERVER and URI."
  (format "%s%s" server uri))

(cl-defun orgtrello-query/--standard-error-callback (&key error-thrown symbol-status response &allow-other-keys)
  "Standard error callback. Simply displays a message in the minibuffer with the error code."
  (orgtrello-log/msg *OT/DEBUG* "client - Problem during the request to the proxy- error-thrown: %s" error-thrown))

(cl-defun orgtrello-query/--standard-success-callback (&key data &allow-other-keys)
  "Standard success callback. Simply displays a \"Success\" message in the minibuffer."
  (orgtrello-log/msg *OT/DEBUG* "client - Proxy received and acknowledged the request%s" (if data (format " - response data: %S." data) ".")))

(defun orgtrello-query/--authentication-params ()
  "Generate the list of http authentication parameters."
  `((key . ,*consumer-key*) (token . ,*access-token*)))

(defun orgtrello-query/--http-parse ()
  "Parse the http response into an org-trello entity."
  (->> (json-read)
    orgtrello-data/parse-data))

(defun orgtrello-query/--get (server query-map &optional success-callback error-callback authentication-p)
  "Execute the GET request to SERVER with QUERY-MAP with optional SUCCESS-CALLBACK, ERROR-CALLBACK and AUTHENTICATION-P."
  (request (->> query-map orgtrello-data/entity-uri (orgtrello-query/--compute-url server))
           :sync    (orgtrello-data/entity-sync   query-map)
           :type    (orgtrello-data/entity-method query-map)
           :params  (orgtrello-data/merge-2-lists-without-duplicates (when authentication-p (orgtrello-query/--authentication-params)) (orgtrello-data/entity-params query-map))
           :parser  'orgtrello-query/--http-parse
           :success (if success-callback success-callback 'orgtrello-query/--standard-success-callback)
           :error   (if error-callback error-callback 'orgtrello-query/--standard-error-callback)))

(defun orgtrello-query/--post-or-put (server query-map &optional success-callback error-callback authentication-p)
  "Execute the POST/PUT request to SERVER with QUERY-MAP with optional SUCCESS-CALLBACK, ERROR-CALLBACK and AUTHENTICATION-P."
  (request (->> query-map orgtrello-data/entity-uri (orgtrello-query/--compute-url server))
           :sync    (orgtrello-data/entity-sync   query-map)
           :type    (orgtrello-data/entity-method query-map)
           :params  (when authentication-p (orgtrello-query/--authentication-params))
           :headers '(("Content-type" . "application/json"))
           :data    (->> query-map orgtrello-data/entity-params json-encode)
           :parser  'orgtrello-query/--http-parse
           :success (if success-callback success-callback 'orgtrello-query/--standard-success-callback)
           :error   (if error-callback error-callback 'orgtrello-query/--standard-error-callback)))

(defun orgtrello-query/--delete (server query-map &optional success-callback error-callback authentication-p)
  "Execute the DELETE request to SERVER with QUERY-MAP with optional SUCCESS-CALLBACK, ERROR-CALLBACK and AUTHENTICATION-P."
  (request (->> query-map orgtrello-data/entity-uri (orgtrello-query/--compute-url server))
           :sync    (orgtrello-data/entity-sync   query-map)
           :type    (orgtrello-data/entity-method query-map)
           :params  (when authentication-p (orgtrello-query/--authentication-params))
           :success (if success-callback success-callback 'orgtrello-query/--standard-success-callback)
           :error   (if error-callback error-callback 'orgtrello-query/--standard-error-callback)))

(defun orgtrello-query/--dispatch-http-query (method)
  "Dispatch the function to call depending on the METHOD key."
  (cond ((string= "GET" method)                              'orgtrello-query/--get)
        ((or (string= "POST" method) (string= "PUT" method)) 'orgtrello-query/--post-or-put)
        ((string= "DELETE" method)                           'orgtrello-query/--delete)))

(defvar orgtrello-query/--hexify (if (version< emacs-version "24.3") 'orgtrello-query/url-hexify-string 'url-hexify-string)
  "Function to use to hexify depending on Emacs version.")

(defun orgtrello-query/url-hexify-string (value)
  "Wrapper around 'url-hexify-string' VALUE (older Emacs 24 version do not map some symbols)."
  (->> value
    url-hexify-string
    (replace-regexp-in-string "!" "%21")
    (replace-regexp-in-string "'" "%27")
    (replace-regexp-in-string "(" "%28")
    (replace-regexp-in-string ")" "%29")
    (replace-regexp-in-string "*" "%2A")))

(defun orgtrello-query/--prepare-params-assoc! (params)
  "Prepare PARAMS as association list (deal with nested list too)."
  (--map (let ((key   (car it))
               (value (cdr it)))
           (cond ((and value (stringp value)) `(,key . ,(funcall orgtrello-query/--hexify value)))
                 ((and value (listp value))   `(,key . ,(orgtrello-query/--prepare-params-assoc! value)))
                 (t                            it)))
         params))

(defun orgtrello-query/read-data (data)
  "Read the association list DATA (association list of query params)."
  (--map (let ((key   (car it))
               (value (cdr it)))
           (cond ((and value (stringp value)) `(,key . ,(url-unhex-string value)))
                 ((and value (listp value))   `(,key . ,(orgtrello-query/read-data value)))
                 (t  it)))
         data))

(defun orgtrello-query/--prepare-query-params! (params)
  "Given an association list of data, prepare the values of the PARAMS."
  (-> params
    json-encode                               ;; hashtable and association list renders the same result in json
    json-read-from-string                     ;; now getting back an association list
    orgtrello-query/--prepare-params-assoc!))

(defun orgtrello-query/http (server query-map &optional sync success-callback error-callback authentication-p)
  "Execute an HTTP query to the SERVER with QUERY-MAP and optional SYNC, SUCCESS-CALLBACK, ERROR-CALLBACK and AUTHENTICATION-P."
  (let* ((dispatch-http-query-fn (-> query-map
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

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-query loaded!")

(provide 'org-trello-query)
;;; org-trello-query.el ends here
