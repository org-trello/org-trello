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
       (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org :users 4 :kwd :name nil :due :position :buffer-name :desc :comments :tags))
       (lambda (entity s) (format "%S %s" entity s))
       "- hello"))

  (expect "#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:current #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:buffername :buffer-name :position :position :level 3 :keyword :kwd :name :name :id nil :due :due :member-ids :users :desc :desc :comments :comments :tags :tags)) :parent nil :grandparent nil)) - hello"
    (orgtrello-action/functional-controls-then-do
     '(orgtrello-controller/--right-level-p)
     (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org :users 3 :kwd :name nil :due :position :buffer-name :desc :comments :tags))
     (lambda (entity s) (format "%S %s" entity s))
     "- hello")))

(expectations (desc "orgtrello-action/--function-controls-then-do - 2")
  (expect "List of errors:
 - Entity must been synchronized with trello first!
"
    (orgtrello-action/functional-controls-then-do
     '(orgtrello-controller/--right-level-p orgtrello-controller/--already-synced-p)
     (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org :users 1 :kwd :name nil :due :position :buffer-name :desc :comments :tags))
     (lambda (entity s) (format "%S %s" entity s))
     "- hello"))
  (expect "#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:current #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (:buffername :buffer-name :position :position :level 1 :keyword :kwd :name :name :id \"some-id\" :due :due :member-ids :users :desc :desc :comments :comments :tags :tags)) :parent nil :grandparent nil)) - hello"

    (orgtrello-action/functional-controls-then-do
     '(orgtrello-controller/--right-level-p orgtrello-controller/--already-synced-p)
     (orgtrello-hash/make-hierarchy (orgtrello-hash/make-hash-org :users 1 :kwd :name "some-id" :due :position :buffer-name :desc :comments :tags))
     (lambda (entity s) (format "%S %s" entity s))
     "- hello")))

(expectations (desc "orgtrello-action/--compute-error-message")
  (expect "- message 1\n- message 2\n" (orgtrello-action/--compute-error-message '("message 1" "message 2"))))
