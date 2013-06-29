;; Main 'namespace' to declare the query function to execute to connect to trello
;; trello-query is the main entry point

;; Load the setup from the $HOME/.trello/config.el
(load (concat (getenv "HOME") "/.trello/config.el"))

(require 'request)
(require 'hash)

(defvar URL "https://api.trello.com/1" "The needed prefix url for trello")

(defun trello-query (query-map)
  "Query the trello api. This method will dispatch depending on the method."
  (let ((method (gethash :method query-map)))
    (if (equal :get  method)
        (trello--get query-map)
        (trello-post-or-put query-map))))

(defun trello--compute-method (method)
  "Given the keywords :get, :post, :put, :delete, map them into standard uppercase string."
  (cond ((equal :get    method) "GET")
        ((equal :post   method) "POST")
        ((equal :put    method) "PUT")
        ((equal :delete method) "DELETE")))

(ert-deftest testing-trello--compute-method ()
  (should (equal (trello--compute-method :get)    "GET"))
  (should (equal (trello--compute-method :post)   "POST"))
  (should (equal (trello--compute-method :put)    "PUT"))
  (should (equal (trello--compute-method :delete) "DELETE")))

(defun trello--get (query-map)
  "GET"
  (let* ((uri    (gethash :uri    query-map))
         (params (gethash :params query-map)))
    (request
     uri
     :type "GET"
     :params `((key . ,consumer-key)
               (token . ,secret-token))
     :parser 'json-read
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (message "%S"  data))))))

(defun trello--post-or-put (query-map)
  "POST or PUT"
  (let* ((method (gethash :uri    query-map))
         (uri    (gethash :uri    query-map))
         (params (gethash :params query-map)))
    (request
     uri
     :type    (compute-method method)
     :params  `((key . ,consumer-key)
               (token . ,secret-token))
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
;;              (token . ,secret-token))
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

(provide 'query)

;;; query.el ends here
