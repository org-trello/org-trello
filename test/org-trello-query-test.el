(require 'org-trello-query)
(require 'ert)
(require 'el-mock)

(ert-deftest test-orgtrello-query--compute-url ()
  (should (equal (format "%s%s" orgtrello-query--trello-url "/uri")            (orgtrello-query--compute-url orgtrello-query--trello-url "/uri")))
  (should (equal (format "%s%s" orgtrello-query--trello-url "/uri/other")      (orgtrello-query--compute-url orgtrello-query--trello-url "/uri/other")))
  (should (equal (format "some-server/uri/some/other")          (orgtrello-query--compute-url "some-server" "/uri/some/other"))))

(ert-deftest test-orgtrello-query--authentication-params ()
  (should
   (equal '((key . :org-trello-consumer-key) (token . :org-trello-access-token))
          (let ((org-trello-consumer-key :org-trello-consumer-key)
                (org-trello-access-token :org-trello-access-token))
            (orgtrello-query--authentication-params)))))

(ert-deftest test-orgtrello-query--http-parse ()
  (should
   (equal :result
          (with-mock
            (mock (json-read) => :json-output)
            (mock (orgtrello-data-parse-data :json-output) => :result)
            (orgtrello-query--http-parse)))))

(ert-deftest test-orgtrello-query-http ()
  (should (equal :res
                 (with-mock
                   (mock (orgtrello-data-entity-method :query-map) => :entity-method)
                   (mock (orgtrello-query--dispatch-http-query :entity-method) => :http-query-method-fn)
                   (mock (funcall :http-query-method-fn :server :query-map :success-cbk-fn :error-cbk-fn :auth-p) => :res)
                   (orgtrello-query-http :server :query-map nil :success-cbk-fn :error-cbk-fn :auth-p))))
  ;; (should (equal :res-with-synced-query
  ;;                (with-mock
  ;;                  (mock (orgtrello-data-entity-method :query-map)                                                        => :entity-method)
  ;;                  (mock (orgtrello-query--dispatch-http-query :entity-method)                                            => :http-query-method-fn)
  ;;                  (mock (orgtrello-data-put-entity-sync 'sync :query-map)                                                => :updated-query-map)
  ;;                  (mock (funcall :http-query-method-fn :server :updated-query-map :success-cbk-fn :error-cbk-fn :auth-p) => :wrapped-result)
  ;;                  (mock (request-response-data :wrapped-result)                                                          => :res-with-synced-query)
  ;;                  (orgtrello-query-http :server :query-map :sync :success-cbk-fn :error-callback-f))))  ;; not working yet...]
  )

(ert-deftest test-orgtrello-query--dispatch-http-query ()
  (should (equal 'orgtrello-query--get         (orgtrello-query--dispatch-http-query "GET")))
  (should (equal 'orgtrello-query--post-or-put (orgtrello-query--dispatch-http-query "POST")))
  (should (equal 'orgtrello-query--post-or-put (orgtrello-query--dispatch-http-query "PUT")))
  (should (equal 'orgtrello-query--delete      (orgtrello-query--dispatch-http-query "DELETE"))))

(ert-deftest test-orgtrello-query-http-trello ()
  (should (equal :result
                 (with-mock
                   (mock (orgtrello-query-http orgtrello-query--trello-url :query-map :sync :success-callback :error-callback 'with-authentication) => :result)
                   (orgtrello-query-http-trello :query-map :sync :success-callback :error-callback)))))

(provide 'org-trello-query-test)
;;; org-trello-query-test.el ends here
