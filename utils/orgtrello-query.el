;; Main 'namespace' to declare the query function to execute to connect to trello
;; trello-query is the main entry point

;; Load the setup from the $HOME/.trello/config.el
(load (concat (getenv "HOME") "/.trello/config.el"))

(require 'request)
(require 'hash)

(defvar *TRELLO-URL* "https://api.trello.com/1" "The needed prefix url for trello")

(defun orgtrello-http (query-map)
  "Query the trello api. This method will dispatch depending on the method."
  (let ((method (gethash :method query-map)))
    (if (equal :get method)
        (orgtrello--get query-map)
        (trello-post-or-put query-map))))

(defun orgtrello--compute-method (method)
  "Given the keywords :get, :post, :put, :delete, map them into standard uppercase string."
  (cond ((equal :get    method) "GET")
        ((equal :post   method) "POST")
        ((equal :put    method) "PUT")
        ((equal :delete method) "DELETE")))

(ert-deftest testing-orgtrello--compute-method ()
  (should (equal (orgtrello--compute-method :get)    "GET"))
  (should (equal (orgtrello--compute-method :post)   "POST"))
  (should (equal (orgtrello--compute-method :put)    "PUT"))
  (should (equal (orgtrello--compute-method :delete) "DELETE")))

(defun orgtrello--compute-url (uri)
  "Compute the trello url from the given uri."
  (format "%s%s" *TRELLO-URL* uri))

(ert-deftest testing-orgtrello--compute-url ()
  (should (equal (orgtrello--compute-url "/uri")            (format "%s%s" *TRELLO-URL* "/uri")))
  (should (equal (orgtrello--compute-url "/uri/other")      (format "%s%s" *TRELLO-URL* "/uri/other")))
  (should (equal (orgtrello--compute-url "/uri/some/other") (format "%s%s" *TRELLO-URL* "/uri/some/other"))))

(defun orgtrello--get (query-map)
  "GET"
  (let* ((method (gethash :uri    query-map))
         (uri    (gethash :uri    query-map))
         (params (gethash :params query-map)))
    (request
     (orgtrello--compute-url uri)
     :type method
     :params `((key . ,consumer-key)
               (token . ,access-token))
     :parser 'json-read
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (message "%S" data))))))

(defun orgtrello--post-or-put (query-map)
  "POST or PUT"
  (let* ((method (gethash :uri    query-map))
         (uri    (gethash :uri    query-map))
         (params (gethash :params query-map)))
    (request
     (orgtrello--compute-url uri)
     :type    (orgtrello--compute-method method)
     :params  `((key . ,consumer-key)
               (token . ,access-token))
     :headers '(("Content-type" "application/json"))
     :data    (json-encode params)
     :parser  'json-read
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (message "%S"  data))))))

;; trying out the request api
;; (defun api-query (method path)
;;   (request
;;    (format "%s%s" URL path)
;;    :params `((key . ,consumer-key)
;;              (token . ,access-token))
;;    :type method
;;    ;; :params nil
;;    :parser 'json-read
;;    :success (function*
;;              (lambda (&key data &allow-other-keys)
;;                (message "%S"  data)))))


;; (api-query "GET" "/member/me/boards")

;; (request
;;    "http://localhost:3000"
;;    :params '((q . "emacs awesome"))
;;    :parser 'json-read
;;    :success (function*
;;              (lambda (&key data &allow-other-keys)
;;                (let* ((tweet (elt (assoc-default 'results data) 0))
;;                       (text (assoc-default 'text tweet))
;;                       (user (assoc-default 'from_user_name tweet)))
;;                  (message "%s says %s" user text)))))

(provide 'orgtrello-query)

;;; orgtrello-query.el ends here
