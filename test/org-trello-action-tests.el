(require 'org-trello-action)
(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(expectations (desc "orgtrello-action/--filter-error-message")
 (expect '("error0" "error1") (orgtrello-action/--filter-error-messages '("error0" :ok "error1")))
 (expect nil                  (orgtrello-action/--filter-error-messages '(:ok :ok :ok))))

(expectations
  (expect '(:ok) (orgtrello-action/--execute-controls '((lambda (e) :ok))))
  (expect '(:ok "ko") (orgtrello-action/--execute-controls '((lambda (e) :ok)
                                                       (lambda (e) "ko"))))
  (expect '(:ok) (orgtrello-action/--execute-controls '((lambda (a) :ok)) 'args))
  (expect '(:ok "ko") (orgtrello-action/--execute-controls '((lambda (a) :ok)
                                                       (lambda (a) "ko")) 'arg0)))

(expectations  (desc "orgtrello-action/--function-controls-then-do - 1")
  (expect   "List of errors:
 - Level too high. Do not deal with entity other than card/checklist/items!
"
      (orgtrello-action/functional-controls-then-do
       '(orgtrello-controller/--right-level-p)
       (orgtrello-data/make-hierarchy (orgtrello-data/make-hash-org :users 4 :kwd :name nil :due :position :buffer-name :desc :comments :tags))
       (lambda (entity s) (format "%S %s" entity s))
       "- hello"))

  (expect "#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:current #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:buffername :buffer-name :position :position :level 3 :keyword :kwd :name :name :id nil :due :due :member-ids :users :desc :desc :comments :comments :tags :tags)) :parent nil :grandparent nil)) - hello"
    (orgtrello-action/functional-controls-then-do
     '(orgtrello-controller/--right-level-p)
     (orgtrello-data/make-hierarchy (orgtrello-data/make-hash-org :users 3 :kwd :name nil :due :position :buffer-name :desc :comments :tags))
     (lambda (entity s) (format "%S %s" entity s))
     "- hello")))

(expectations (desc "orgtrello-action/--function-controls-then-do - 2")
  (expect "List of errors:
 - Entity must been synchronized with trello first!
"
    (orgtrello-action/functional-controls-then-do
     '(orgtrello-controller/--right-level-p orgtrello-controller/--already-synced-p)
     (orgtrello-data/make-hierarchy (orgtrello-data/make-hash-org :users 1 :kwd :name nil :due :position :buffer-name :desc :comments :tags))
     (lambda (entity s) (format "%S %s" entity s))
     "- hello"))
  (expect "#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:current #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:buffername :buffer-name :position :position :level 1 :keyword :kwd :name :name :id \"some-id\" :due :due :member-ids :users :desc :desc :comments :comments :tags :tags)) :parent nil :grandparent nil)) - hello"

    (orgtrello-action/functional-controls-then-do
     '(orgtrello-controller/--right-level-p orgtrello-controller/--already-synced-p)
     (orgtrello-data/make-hierarchy (orgtrello-data/make-hash-org :users 1 :kwd :name "some-id" :due :position :buffer-name :desc :comments :tags))
     (lambda (entity s) (format "%S %s" entity s))
     "- hello")))

(expectations (desc "orgtrello-action/--compute-error-message")
  (expect "- message 1\n- message 2\n" (orgtrello-action/--compute-error-message '("message 1" "message 2"))))

(expectations
  (desc "orgtrello-action/controls-or-actions-then-do")
  ;; with no control the action function is direcly executed
  (expect "some-result"
    (orgtrello-action/controls-or-actions-then-do nil (lambda () "some-result")))
  ;; with all controls ok, the action function is executed
  (expect "some-result"
    (with-mock
      (mock (control0) => :ok)
      (mock (control1) => :ok)
      (orgtrello-action/controls-or-actions-then-do '(control0 control1) (lambda () "some-result"))))
  ;; with all controls ok and the no logs flag, the action function is executed
  (expect "some-result"
    (with-mock
      (mock (control0) => :ok)
      (mock (control1) => :ok)
      (orgtrello-action/controls-or-actions-then-do '(control0 control1) (lambda () "some-result") 'no-logs)))
  ;; with a problem in controls, the action function is not executed and the logs are returned
  (expect "List of errors:\n - some error message from control 1\n- some other error message from control 2\n"
    (with-mock
      (mock (control0)            => :ok)
      (mock (control1-that-fails) => "some error message from control 1")
      (mock (control2-that-fails) => "some other error message from control 2")
      (orgtrello-action/controls-or-actions-then-do '(control0 control1-that-fails control2-that-fails) 'some-uncalled-function-because-control-fail)))
  ;; with a problem in controls, the action function is not executed and the logs are not returned
  (expect nil
    (with-mock
      (mock (control0)            => :ok)
      (mock (control1-that-fails) => "some error message from control 1")
      (mock (control2-that-fails) => "some other error message from control 2")
      (orgtrello-action/controls-or-actions-then-do '(control0 control1-that-fails control2-that-fails) 'some-uncalled-function-because-control-fail 'no-logs))))

(expectations
  (desc "orgtrello-action/functional-controls-then-do")
  ;; with no controls, the action is executed
  (expect "some-result"
    (orgtrello-action/functional-controls-then-do nil 'entity-not-really-used (lambda (entity-not-used args-not-used) "some-result") 'arg-not-really-used))
  ;; with controls ok, the action function is executed
  (with-mock
    (mock (control0 'entity-not-really-used) => :ok)
    (mock (control1 'entity-not-really-used) => :ok)
    (orgtrello-action/functional-controls-then-do '(control0 control1) 'entity-not-really-used (lambda (entity-not-used args-not-used) "some-result") 'arg-not-really-used))
  ;; with some controls ko, the action function is not executed and the logs are returned
  (expect "List of errors:\n - control1 failed!\n- control2 failed!\n"
    (with-mock
      (mock (control0-that-fails 'entity-not-really-used) => "control1 failed!")
      (mock (control1            'entity-not-really-used) => :ok)
      (mock (control2-that-fails 'entity-not-really-used) => "control2 failed!")
      (orgtrello-action/functional-controls-then-do '(control0-that-fails control1 control2-that-fails)
                                                    'entity-not-really-used
                                                    (lambda (entity-not-used args-not-used) "some-result") 'arg-not-really-used))))

(expectations
  (desc "orgtrello-action/msg-controls-or-actions-then-do")
  ;; the execution goes fine and we return the result from the wrapped call to 'orgtrello-action/controls-or-actions-then-do
  (expect :some-result
    (with-mock
      (mock (orgtrello-action/controls-or-actions-then-do :control-or-action-fns :fn-to-execute nil) => :some-result)
      (orgtrello-action/msg-controls-or-actions-then-do "some-msg" :control-or-action-fns :fn-to-execute)))
  ;; the execution goes fine, we save the buffer, reload the setup and return the result from the wrapped call to 'orgtrello-action/controls-or-actions-then-do
  (expect :some-result
    (with-mock
      (mock (orgtrello-action/controls-or-actions-then-do :control-or-action-fns :fn-to-execute nil) => :some-result)
      (mock (save-buffer) => t)
      (mock (orgtrello-action/reload-setup) => t)
      (orgtrello-action/msg-controls-or-actions-then-do "some-msg" :control-or-action-fns :fn-to-execute 'save-buffer 'reload-setup)))
  ;; log nothing, execution goes fine, we save the buffer, reload the setup and return the result from the wrapped call to 'orgtrello-action/controls-or-actions-then-do
  (expect :some-result
    (with-mock
      (mock (orgtrello-action/controls-or-actions-then-do :control-or-action-fns :fn-to-execute 'no-log) => :some-result)
      (mock (save-buffer) => t)
      (mock (orgtrello-action/reload-setup) => t)
      (orgtrello-action/msg-controls-or-actions-then-do "some-msg" :control-or-action-fns :fn-to-execute 'save-buffer 'reload-setup 'no-log)))
  ;; control is ok, but the execution goes awry, an exception is thrown and caught but does not break the call
  (expect '(exception (no-catch some-exception :value))
    (with-mock
      (mock (control0) => :ok)
      (orgtrello-action/msg-controls-or-actions-then-do "some-msg" '(control0) (lambda () (throw 'some-exception :value)))))
  ;; control is ok, but the execution goes awry, an exception is thrown and caught but does not break the call, we do some extra actions in any case and we return the exc.
  (expect '(exception (no-catch some-exception :value))
    (with-mock
      (mock (control0) => :ok)
      (mock (save-buffer) => t)
      (mock (orgtrello-action/reload-setup) => t)
      (orgtrello-action/msg-controls-or-actions-then-do "some-msg" '(control0) (lambda () (throw 'some-exception :value)) 'save-buffer 'reload-setup))))

(provide 'org-trello-action-tests)
;;; org-trello-action-tests.el ends here
