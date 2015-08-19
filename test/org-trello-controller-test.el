(require 'org-trello-controller)
(require 'ert)
(require 'el-mock)

(ert-deftest test-orgtrello-controller--close-lists ()
  (should (equal
           :result-comp-close-lists
           (with-mock
             (mock (orgtrello-api-close-list :list-id0) => :query-close-list-id-0)
             (mock (orgtrello-query-http-trello :query-close-list-id-0 nil *) => :async-computation-0)
             (mock (orgtrello-proxy-execute-async-computations
                    '(:async-computation-0)
                    "List(s) closed."
                    "FAILURE - Problem during closing list.") => :result-comp-close-lists)
             (orgtrello-controller--close-lists '(:list-id0))))))

(ert-deftest test-orgtrello-controller--create-board ()
  (should (eq :result
              (with-mock
                (mock (orgtrello-api-add-board :board-name :board-description) => :query-create)
                (mock (orgtrello-query-http-trello :query-create 'sync) => :result)
                (orgtrello-controller--create-board :board-name :board-description)))))

(ert-deftest test-orgtrello-controller--update-orgmode-file-with-properties ()
  (should (string=
           ":PROPERTIES:
#+PROPERTY: board-name board's name or title
#+PROPERTY: board-id board-id
#+PROPERTY: done list-id-789
#+PROPERTY: in-progress-2 list-id-456
#+PROPERTY: todo-1 list-id-123
#+TODO: todo-1 in-progress-2 | done
#+PROPERTY: orgtrello-user-orgtrello-user-user3 789
#+PROPERTY: orgtrello-user-orgtrello-user-user2 456
#+PROPERTY: orgtrello-user-orgtrello-user-user1 123
#+PROPERTY: :green green label
#+PROPERTY: :red red label
#+PROPERTY: orgtrello-user-me user3
:END:
"
           (orgtrello-tests-with-temp-buffer-and-return-buffer-content
            ""
            (orgtrello-controller--update-orgmode-file-with-properties
             "board's name or title"
             "board-id"
             (orgtrello-hash-make-properties '(("todo-1" . "list-id-123")
                                               ("in-progress-2" . "list-id-456")
                                               ("done" . "list-id-789")))
             (orgtrello-hash-make-properties '(("orgtrello-user-user1" . "123")
                                               ("orgtrello-user-user2" . "456")
                                               ("orgtrello-user-user3" . "789")))
             "user3"
             (orgtrello-hash-make-properties '((:red . "red label") (:green . "green label")))
             'do-delete-the-todo-line)))))

(ert-deftest test-orgtrello-controller--user-logged-in ()
  (should (equal :result-get-me
                 (with-mock
                   (mock (orgtrello-api-get-me) => :query-get-me)
                   (mock (orgtrello-query-http-trello :query-get-me 'sync) => :result-get-me)
                   (orgtrello-controller--user-logged-in)))))

(ert-deftest test-orgtrello-controller--properties-compute-todo-keywords-as-string ()
  (should (string= "#+TODO: list-id-1 list-id-2 list-id-3"
                   (orgtrello-controller--properties-compute-todo-keywords-as-string
                    (orgtrello-hash-make-properties '(("list-id-1" . "123")
                                                      ("list-id-2" . "456")
                                                      ("list-id-3" . "789")))))))

(ert-deftest test-orgtrello-controller--remove-properties-file ()
  (should (string=
           " #+title: dummy sample to sync with trello
 #+author: Antoine R. Dumont"
           (orgtrello-tests-with-temp-buffer-and-return-buffer-content
            ":PROPERTIES:
 #+PROPERTY: board-name test board api
 #+PROPERTY: board-id board-id-1
 #+PROPERTY: CANCELLED list-1-id
 #+PROPERTY: FAILED list-2-id
 #+PROPERTY: DELEGATED list-3-id
 #+PROPERTY: PENDING list-4-id
 #+PROPERTY: DONE list-5-id
 #+PROPERTY: IN-PROGRESS list-6-id
 #+PROPERTY: TODO list-7-id
 #+TODO: TODO IN-PROGRESS | DONE PENDING DELEGATED FAILED CANCELLED
 #+PROPERTY: orgtrello-user-user1 123
 #+PROPERTY: orgtrello-user-user2 456
 #+PROPERTY: orgtrello-user-user3 789
 #+PROPERTY: :green green label with & char
 #+PROPERTY: :yellow yello
 #+PROPERTY: :orange range
 #+PROPERTY: :red red
 #+PROPERTY: :purple violet
 #+PROPERTY: :blue blue
 #+PROPERTY: orgtrello-user-me user2
 :END:
 #+title: dummy sample to sync with trello
 #+author: Antoine R. Dumont"
            (orgtrello-controller--remove-properties-file
             '("TODO" "IN-PROGRESS" "DONE" "PENDING" "DELEGATED" "FAILED" "CANCELLED")
             (orgtrello-hash-make-properties '(("orgtrello-user-user1" . "123")
                                               ("orgtrello-user-user2" . "456")
                                               ("orgtrello-user-user3" . "789")
                                               ("orgtrello-user-me" . "user2")))
             "user2"
             'do-delete-the-todo-line)))))

(ert-deftest test-orgtrello-controller--compute-hash-name-id-to-list ()
  (should (equal '("#+PROPERTY: orgtrello-user-user3 451"
                   "#+PROPERTY: orgtrello-user-user2 341"
                   "#+PROPERTY: orgtrello-user-user1 231")
                 (orgtrello-controller--compute-hash-name-id-to-list (orgtrello-hash-make-properties '(("user1" . "231")
                                                                                                       ("user2" . "341")
                                                                                                       ("orgtrello-user-user3" . "451")))))))

(ert-deftest test-orgtrello-controller-checks-then-sync-card-to-trello ()
  (should (eq :result-sync
              (with-mock
                (mock (current-buffer) => :buffer)
                (mock (orgtrello-buffer-safe-entry-full-metadata) => :entity)
                (mock (orgtrello-action-functional-controls-then-do
                       '(orgtrello-controller--on-entity-p
                         orgtrello-controller--right-level-p
                         orgtrello-controller--mandatory-name-ok-p)
                       :entity
                       'orgtrello-controller-sync-card-to-trello
                       :buffer) => :result-sync)
                (orgtrello-controller-checks-then-sync-card-to-trello)))))

(ert-deftest test-orgtrello-controller-checks-then-delete-simple ()
  (should (eq :result-delete
              (with-mock
                (mock (current-buffer) => :buffer)
                (mock (orgtrello-buffer-safe-entry-full-metadata) => :entity)
                (mock (orgtrello-action-functional-controls-then-do
                       '(orgtrello-controller--on-entity-p
                         orgtrello-controller--right-level-p
                         orgtrello-controller--already-synced-p)
                       :entity
                       'orgtrello-controller-delete-card
                       :buffer) => :result-delete)
                (orgtrello-controller-checks-then-delete-simple)))))

(ert-deftest test-orgtrello-controller-checks-then-sync-card-from-trello ()
  (should (eq :result-sync-from
              (with-mock
                (mock (current-buffer) => :buffer)
                (mock (orgtrello-buffer-safe-entry-full-metadata) => :entity)
                (mock (orgtrello-action-functional-controls-then-do
                       '(orgtrello-controller--on-entity-p
                         orgtrello-controller--right-level-p
                         orgtrello-controller--already-synced-p)
                       :entity
                       'orgtrello-controller-sync-card-from-trello
                       :buffer) => :result-sync-from)
                (orgtrello-controller-checks-then-sync-card-from-trello)))))


(ert-deftest test-orgtrello-controller--delete-buffer-property ()
  (should (equal "* card
:PROPERTIES:
:END:
"
                 (orgtrello-tests-with-temp-buffer-and-return-buffer-content
                  "* card
:PROPERTIES:
:prop: blah
:END:
"
                  (orgtrello-controller--delete-buffer-property ":prop:")))))

(ert-deftest test-orgtrello-controller-setup-properties ()
  (should
   (-every? (-partial 'eq t)
            (orgtrello-tests-with-temp-buffer
             ":PROPERTIES:
#+PROPERTY: board-name test board
#+PROPERTY: board-id identifier-for-the-board
#+PROPERTY: CANCELLED cancelled-list-id
#+PROPERTY: FAILED failed-list-id
#+PROPERTY: DELEGATED deletegated-list-id
#+PROPERTY: PENDING pending-list-id
#+PROPERTY: DONE done-list-id
#+PROPERTY: IN-PROGRESS in-progress-list-id
#+PROPERTY: TODO todo-list-id
#+TODO: TODO IN-PROGRESS | DONE PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-user1 user1-id
#+PROPERTY: orgtrello-user-user2 user2-id
#+PROPERTY: orgtrello-user-user3 user3-id
#+PROPERTY: :green green label with & char
#+PROPERTY: :yellow yello
#+PROPERTY: :orange range
#+PROPERTY: :red red
#+PROPERTY: :purple violet
#+PROPERTY: :blue blue
#+PROPERTY: orgtrello-user-me user1
:END:
"
             (let ((org-tag-alist nil)
                   (org-trello--user-logged-in))
               (orgtrello-controller-setup-properties)
               (list
                (equal org-trello--org-keyword-trello-list-names '("TODO" "IN-PROGRESS" "DONE" "PENDING" "DELEGATED" "FAILED" "CANCELLED"))
                (orgtrello-tests-hash-equal
                 org-trello--hmap-list-orgkeyword-id-name
                 (orgtrello-hash-make-properties
                  '(("todo-list-id" . "TODO")
                    ("in-progress-list-id" . "IN-PROGRESS")
                    ("done-list-id" . "DONE")
                    ("pending-list-id" . "PENDING")
                    ("deletegated-list-id" . "DELEGATED")
                    ("failed-list-id" . "FAILED")
                    ("cancelled-list-id" . "CANCELLED"))))
                (orgtrello-tests-hash-equal
                 org-trello--hmap-users-id-name
                 (orgtrello-hash-make-properties
                  '(("user1-id" . "orgtrello-user-user1")
                    ("user2-id" . "orgtrello-user-user2")
                    ("user3-id" . "orgtrello-user-user3")
                    ("user1" . "orgtrello-user-me"))))
                (orgtrello-tests-hash-equal
                 org-trello--hmap-users-name-id
                 (orgtrello-hash-make-properties
                  '(("orgtrello-user-user1" . "user1-id")
                    ("orgtrello-user-user2" . "user2-id")
                    ("orgtrello-user-user3" . "user3-id")
                    ("orgtrello-user-me" . "user1"))))
                (string= org-trello--user-logged-in "user1")
                (equal org-tag-alist (nreverse
                                      '(("red" . ?r)
                                        ("green" . ?g)
                                        ("yellow" . ?y)
                                        ("blue" . ?b)
                                        ("purple" . ?p)
                                        ("orange" . ?o))))))))))

(ert-deftest test-orgtrello-controller-control-properties ()
  ;; ok
  (should (equal :ok
                 (orgtrello-tests-with-temp-buffer
                  ":PROPERTIES:
#+PROPERTY: board-name test board
#+PROPERTY: board-id identifier-for-the-board
#+PROPERTY: CANCELLED cancelled-list-id
#+PROPERTY: FAILED failed-list-id
#+PROPERTY: DELEGATED deletegated-list-id
#+PROPERTY: PENDING pending-list-id
#+PROPERTY: DONE done-list-id
#+PROPERTY: IN-PROGRESS in-progress-list-id
#+PROPERTY: TODO todo-list-id
:END:
"
                  (orgtrello-controller-control-properties :args-not-used))))
  ;; missing board id
  (should (string= "Setup problem.
Either you did not connect your org-mode buffer with a trello board, to correct this:
  * attach to a board through C-c o I or M-x org-trello-install-board-metadata
  * or create a board from scratch with C-c o b or M-x org-trello-create-board-and-install-metadata).
Either your org-mode's todo keyword list and your trello board lists are not named the same way (which they must).
For this, connect to trello and rename your board's list according to your org-mode's todo list.
Also, you can specify on your org-mode buffer the todo list you want to work with, for example: #+TODO: TODO DOING | DONE FAIL (hit C-c C-c to refresh the setup)"
                   (orgtrello-tests-with-temp-buffer
                    ":PROPERTIES:
#+PROPERTY: board-name test board
#+PROPERTY: CANCELLED cancelled-list-id
#+PROPERTY: FAILED failed-list-id
#+PROPERTY: DELEGATED deletegated-list-id
#+PROPERTY: PENDING pending-list-id
#+PROPERTY: DONE done-list-id
#+PROPERTY: IN-PROGRESS in-progress-list-id
#+PROPERTY: TODO todo-list-id
:END:
"
                    (orgtrello-controller-control-properties :args-not-used))))
  ;; missing one list id
  (should (string= "Setup problem.
Either you did not connect your org-mode buffer with a trello board, to correct this:
  * attach to a board through C-c o I or M-x org-trello-install-board-metadata
  * or create a board from scratch with C-c o b or M-x org-trello-create-board-and-install-metadata).
Either your org-mode's todo keyword list and your trello board lists are not named the same way (which they must).
For this, connect to trello and rename your board's list according to your org-mode's todo list.
Also, you can specify on your org-mode buffer the todo list you want to work with, for example: #+TODO: TODO DOING | DONE FAIL (hit C-c C-c to refresh the setup)"
                   (orgtrello-tests-with-temp-buffer
                    ":PROPERTIES:
#+PROPERTY: board-name test board
#+PROPERTY: board-id identifier-for-the-board
#+PROPERTY: CANCELLED cancelled-list-id
"
                    (orgtrello-controller-control-properties :args-not-used)))))

(ert-deftest test-orgtrello-controller-migrate-user-setup ()
  ;; nothing to do
  (should (equal :ok
                 (with-mock
                   (mock (file-exists-p org-trello--old-config-dir) => nil)
                   (orgtrello-controller-migrate-user-setup :args-not-used))))
  (should (equal :ok
                 (let ((*consumer-key* :consumer-key)
                       (*access-token* :access-token)
                       (org-trello--old-config-dir :config-dir)
                       (org-trello--old-config-file :config-file))
                   (with-mock
                     (mock (file-exists-p org-trello--old-config-dir) => t)
                     (mock (orgtrello-buffer-me) => :user-logged-in)
                     (mock (load org-trello--old-config-file) => :done)
                     (mock (orgtrello-controller--do-install-config-file
                            :user-logged-in
                            :consumer-key
                            :access-token) => :done)
                     (mock (delete-directory org-trello--old-config-dir 'with-contents) => :done)
                     (orgtrello-controller-migrate-user-setup :args-not-used)))))
  ;; current setup
  (should (equal :ok
                 (let ((*consumer-key* nil)
                       (org-trello--old-config-dir :config-dir)
                       (org-trello--old-config-file :config-file)
                       (org-trello-consumer-key :org-trello-consumer-key)
                       (org-trello-access-token :org-trello-access-token))
                   (with-mock
                     (mock (file-exists-p org-trello--old-config-dir) => t)
                     (mock (orgtrello-buffer-me) => :user-logged-in-2)
                     (mock (load org-trello--old-config-file) => :done)
                     (mock (orgtrello-controller--do-install-config-file :user-logged-in-2 :org-trello-consumer-key :org-trello-access-token) => :done)
                     (mock (delete-directory org-trello--old-config-dir 'with-contents) => :done)
                     (orgtrello-controller-migrate-user-setup :args-not-used))))))

(ert-deftest test-orgtrello-controller-config-file ()
  (should (string= "~/.emacs.d/.trello/tony.el"
                   (let ((org-trello--config-file "~/.emacs.d/.trello/%s.el"))
                     (orgtrello-controller-config-file "tony"))))
  (should (string= "~/.emacs.d/.trello/user.el"
                   (with-mock
                     (mock (orgtrello-setup-user-logged-in) => "user")
                     (let ((org-trello--config-file "~/.emacs.d/.trello/%s.el"))
                       (orgtrello-controller-config-file))))))

(ert-deftest test-orgtrello-controller-user-config-files ()
  (should (equal :list
                 (with-mock
                   (mock (file-exists-p org-trello--config-dir) => t)
                   (mock (directory-files org-trello--config-dir 'full-name "^.*\.el") => :list)
                   (orgtrello-controller-user-config-files))))
  (should-not (with-mock
                (mock (file-exists-p *) => nil)
                (orgtrello-controller-user-config-files))))

(ert-deftest test-orgtrello-controller--on-entity-p ()
  (should (equal :ok
                 (orgtrello-tests-with-temp-buffer
                  "* card"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--on-entity-p))))
  (should (equal :ok
                 (orgtrello-tests-with-temp-buffer
                  "* card
  - [ ] checklist"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--on-entity-p))))
  (should (equal :ok
                 (orgtrello-tests-with-temp-buffer
                  "* card
  - [ ] checklist
    - [ ] item"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--on-entity-p))))
  (should (equal "You need to be on an org-trello entity (card/checklist/item) for this action to occur!"
                 (orgtrello-tests-with-temp-buffer
                  "** not on a card
"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--on-entity-p)))))

(ert-deftest test-orgtrello-controller--right-level-p ()
  (should (equal :ok
                 (orgtrello-tests-with-temp-buffer
                  "* card"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--right-level-p))))
  (should (equal :ok
                 (orgtrello-tests-with-temp-buffer
                  "* card
  - [ ] checklist"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--right-level-p))))
  (should (equal :ok
                 (orgtrello-tests-with-temp-buffer
                  "* card
  - [ ] checklist
    - [ ] item"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--right-level-p))))
  (should (equal "Wrong level. Do not deal with entity other than card/checklist/item!"
                 (orgtrello-tests-with-temp-buffer
                  "** not on a card
"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--right-level-p)))))

(ert-deftest test-orgtrello-controller--already-synced-p ()
  (should (equal :ok
                 (orgtrello-tests-with-temp-buffer
                  "* card
:PROPERTIES:
:orgtrello-id: 123
:END:"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--already-synced-p))))
  (should (equal "Entity must be synchronized with trello first!"
                 (orgtrello-tests-with-temp-buffer
                  "* card"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--already-synced-p))))
  (should (equal :ok
                 (orgtrello-tests-with-temp-buffer
                  "* card
:PROPERTIES:
:orgtrello-id: 123
:END:
  - [ ] checklist :PROPERTIES: {\"orgtrello-id\":\"123\"}"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      (orgtrello-controller--already-synced-p)))))
  (should (equal "Entity must be synchronized with trello first!"
                 (orgtrello-tests-with-temp-buffer
                  "* card
  - [ ] checklist"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--already-synced-p))))

  (should (equal :ok
                 (orgtrello-tests-with-temp-buffer
                  "* card
:PROPERTIES:
:orgtrello-id: 123
:END:
  - [ ] checklist :PROPERTIES: {\"orgtrello-id\":\"456\"}
    - [ ] item :PROPERTIES: {\"orgtrello-id\":\"123456\"}
"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--already-synced-p))))
  (should (equal "Entity must be synchronized with trello first!"
                 (orgtrello-tests-with-temp-buffer
                  "* card
:PROPERTIES:
:orgtrello-id: 123
:END:
  - [ ] checklist
    - [ ] item
"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--already-synced-p)))))

(ert-deftest test-orgtrello-controller--entity-mandatory-name-ok-p ()
  (should (equal :ok
                 (orgtrello-tests-with-temp-buffer
                  "* card with a name
"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-data-current
                      orgtrello-controller--entity-mandatory-name-ok-p))))
  (should (equal "Cannot synchronize the card - missing mandatory name. Skip it..."
                 (orgtrello-tests-with-temp-buffer
                  "* \n"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-data-current
                      orgtrello-controller--entity-mandatory-name-ok-p))))
  (should (equal :ok
                 (orgtrello-tests-with-temp-buffer
                  "* card
  - [ ] checklist
"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-data-current
                      orgtrello-controller--entity-mandatory-name-ok-p))))

  (should (equal "Cannot synchronize the checklist - missing mandatory name. Skip it..."
                 (orgtrello-tests-with-temp-buffer
                  "* card
  - [ ] \n"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-data-current
                      orgtrello-controller--entity-mandatory-name-ok-p))))
  (should (equal :ok
                 (orgtrello-tests-with-temp-buffer
                  "* card
  - [ ] checklist
    - [ ] item
"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-data-current
                      orgtrello-controller--entity-mandatory-name-ok-p))))
  (should (equal "Cannot synchronize the item - missing mandatory name. Skip it..."
                 (orgtrello-tests-with-temp-buffer
                  "* card
  - [ ] checklist
    - [ ] \n"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-data-current
                      orgtrello-controller--entity-mandatory-name-ok-p)))))

(ert-deftest test-orgtrello-controller--mandatory-name-ok-p ()
  (should (equal :ok
                 (orgtrello-tests-with-temp-buffer
                  "* card with a name
"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--mandatory-name-ok-p))))
  (should (equal "Cannot synchronize the card - missing mandatory name. Skip it..."
                 (orgtrello-tests-with-temp-buffer
                  "* \n"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--mandatory-name-ok-p))))
  (should (equal :ok
                 (orgtrello-tests-with-temp-buffer
                  "* card
  - [ ] checklist
"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--mandatory-name-ok-p))))

  (should (equal "Cannot synchronize the checklist - missing mandatory name. Skip it..."
                 (orgtrello-tests-with-temp-buffer
                  "* card
  - [ ] \n"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--mandatory-name-ok-p))))
  (should (equal :ok
                 (orgtrello-tests-with-temp-buffer
                  "* card
  - [ ] checklist
    - [ ] item
"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-controller--mandatory-name-ok-p))))
  (should (equal "Cannot synchronize the item - missing mandatory name. Skip it..."
                 (orgtrello-tests-with-temp-buffer
                  "* card
  - [ ] checklist
    - [ ] \n"
                  (-> (orgtrello-buffer-safe-entry-full-metadata)
                      orgtrello-data-current
                      orgtrello-controller--entity-mandatory-name-ok-p)))))

(ert-deftest test-orgtrello-controller--compute-data-from-entity-meta ()
  (let* ((entry   (orgtrello-data-make-hash-org :member-ids :some-level :some-keyword :some-name "some-id" :some-due :some-point :some-buffername :desc :tags :unknown)))
    (should (equal (orgtrello-data-entity-id entry)          "some-id"))
    (should (equal (orgtrello-data-entity-name entry)        :some-name))
    (should (equal (orgtrello-data-entity-keyword entry)     :some-keyword))
    (should (equal (orgtrello-data-entity-level entry)       :some-level))
    (should (equal (orgtrello-data-entity-due entry)         :some-due))
    (should (equal (orgtrello-data-entity-position entry)    :some-point))
    (should (equal (orgtrello-data-entity-buffername entry)  :some-buffername))
    (should (equal (orgtrello-data-entity-member-ids entry)  :member-ids))
    (should (equal (orgtrello-data-entity-tags entry)        :tags))
    (should (equal (orgtrello-data-entity-description entry) :desc))
    (should (equal (orgtrello-data-entity-unknown-properties entry) :unknown))))

(ert-deftest test-orgtrello-controller--name-id ()
  (let* ((entities (orgtrello-data-parse-data [((id . "id")
                                                (shortUrl . "https://trello.com/b/ePrdEnzC")
                                                (name . "testing board"))
                                               ((id . "another-id")
                                                (shortUrl . "https://trello.com/b/ePrdEnzC")
                                                (name . "testing board 2"))
                                               ((id . "yet-another-id")
                                                (shortUrl . "https://trello.com/b/ePrdEnzC")
                                                (name . "testing board 3"))]))
         (hashtable-result (orgtrello-controller--name-id entities))
         (hashtable-expected (make-hash-table :test 'equal)))
    (orgtrello-hash-puthash-data "testing board" "id" hashtable-expected)
    (orgtrello-hash-puthash-data "testing board 2" "another-id"  hashtable-expected)
    (orgtrello-hash-puthash-data "testing board 3" "yet-another-id"  hashtable-expected)
    (should (equal (gethash "testing board" hashtable-result) (gethash "testing board" hashtable-expected)))
    (should (equal (gethash "testing board 2" hashtable-result) (gethash "testing board 2" hashtable-expected)))
    (should (equal (gethash "testing board 3" hashtable-result) (gethash "testing board 3" hashtable-expected)))
    (should (equal (hash-table-count hashtable-result) (hash-table-count hashtable-expected)))))

(ert-deftest test-orgtrello-controller--compute-user-properties ()
  (should (orgtrello-tests-hash-equal
           (orgtrello-hash-make-properties '((:username . "ardumont")
                                             (:full-name . "Antoine R. Dumont")
                                             (:id . "4f2baa2f72b7c1293501cad3")))
           (car (orgtrello-controller--compute-user-properties
                 (list (orgtrello-hash-make-properties
                        `((:member . ,(orgtrello-hash-make-properties '((:username . "ardumont")
                                                                        (:full-name . "Antoine R. Dumont")
                                                                        (:id . "4f2baa2f72b7c1293501cad3"))))
                          (:id . "51d99bbc1e1d8988390047f6")))
                       (orgtrello-hash-make-properties `((:member . ,(orgtrello-hash-make-properties '((:username . "orgmode")
                                                                                                       (:full-name . "org trello")
                                                                                                       (:id . "5203a0c833fc36360800177f"))))
                                                         (:id . "524855ff8193aec160002cfa"))))))))
  (should (orgtrello-tests-hash-equal
           (orgtrello-hash-make-properties '((:username . "orgmode")
                                             (:full-name . "org trello")
                                             (:id . "5203a0c833fc36360800177f")))
           (cadr (orgtrello-controller--compute-user-properties
                  (list (orgtrello-hash-make-properties
                         `((:member . ,(orgtrello-hash-make-properties '((:username . "ardumont")
                                                                         (:full-name . "Antoine R. Dumont")
                                                                         (:id . "4f2baa2f72b7c1293501cad3"))))
                           (:id . "51d99bbc1e1d8988390047f6")))
                        (orgtrello-hash-make-properties
                         `((:member . ,(orgtrello-hash-make-properties '((:username . "orgmode")
                                                                         (:full-name . "org trello")
                                                                         (:id . "5203a0c833fc36360800177f"))))
                           (:id . "524855ff8193aec160002cfa")))))))))

(ert-deftest test-orgtrello-controller--compute-user-properties-hash ()
  (should (orgtrello-tests-hash-equal
           (orgtrello-hash-make-properties '(("ardumont" . "4f2baa2f72b7c1293501cad3")
                                             ("orgmode" . "5203a0c833fc36360800177f")))
           (orgtrello-controller--compute-user-properties-hash
            (list (orgtrello-hash-make-properties '((:username . "ardumont")
                                                    (:full-name . "Antoine R. Dumont")
                                                    (:id . "4f2baa2f72b7c1293501cad3")))
                  (orgtrello-hash-make-properties '((:username . "orgmode")
                                                    (:full-name . "org trello")
                                                    (:id . "5203a0c833fc36360800177f"))))))))

(ert-deftest test-orgtrello-controller--list-user-entries ()
  (should (equal
           '(("orgtrello-user-ardumont" . "4f2baa2f72b7c1293501cad3")
             ("orgtrello-user-orgmode" . "5203a0c833fc36360800177f"))
           (orgtrello-controller--list-user-entries '(("board-name" . "api test board")
                                                      ("board-id" . "51d99bbc1e1d8988390047f2")
                                                      ("TODO" . "51d99bbc1e1d8988390047f3")
                                                      ("IN-PROGRESS" . "51d99bbc1e1d8988390047f4")
                                                      ("DONE" . "51d99bbc1e1d8988390047f5")
                                                      ("PENDING" . "51e53898ea3d1780690015ca")
                                                      ("DELEGATED" . "51e538a89c05f1e25c0027c6")
                                                      ("FAIL" . "51e538a26f75d07902002d25")
                                                      ("CANCELLED" . "51e538e6c7a68fa0510014ee")
                                                      ("orgtrello-user-ardumont" . "4f2baa2f72b7c1293501cad3")
                                                      ("orgtrello-user-orgmode" . "5203a0c833fc36360800177f"))))))

(ert-deftest test-orgtrello-controller--add-user ()
  (should (equal '("a" "b" "c") (orgtrello-controller--add-user "a" '("a" "b" "c"))))
  (should (equal '("a" "b" "c") (orgtrello-controller--add-user "a" '("b" "c")))))

(ert-deftest test-orgtrello-controller--remove-user ()
  (should (equal '("b")     (orgtrello-controller--remove-user "a" '("a" "b"))))
  (should (equal '("a" "b") (orgtrello-controller--remove-user "c" '("a" "b"))))
  (should (equal nil        (orgtrello-controller--remove-user "c" nil)))
  (should (equal nil        (orgtrello-controller--remove-user nil nil)))
  (should (equal '("a")     (orgtrello-controller--remove-user nil '("a")))))

(ert-deftest test-orgtrello-controller-compute-property ()
  (should (equal "#+PROPERTY: test "      (orgtrello-controller-compute-property "test")))
  (should (equal "#+PROPERTY: test value" (orgtrello-controller-compute-property "test" "value"))))

(ert-deftest test-orgtrello-controller--compute-metadata ()
  (should (equal '(":PROPERTIES:"
                   "#+PROPERTY: board-name some-board-name"
                   "#+PROPERTY: board-id some-board-id"
                   "#+PROPERTY: DONE done-id"
                   "#+PROPERTY: TODO todo-id"
                   ""
                   "#+PROPERTY: orgtrello-user-some-other-user some-other-user-id"
                   "#+PROPERTY: orgtrello-user-user user-id"
                   "#+PROPERTY: :green green label"
                   "#+PROPERTY: :red red label"
                   "#+PROPERTY: orgtrello-user-me user"
                   ":END:")
                 (orgtrello-controller--compute-metadata
                  "some-board-name"
                  "some-board-id"
                  (orgtrello-hash-make-properties '(("TODO" . "todo-id") ("DONE" . "done-id")))
                  (orgtrello-hash-make-properties '(("user" . "user-id") ("some-other-user" . "some-other-user-id")))
                  "user"
                  (orgtrello-hash-make-properties '((:red . "red label") (:green . "green label")))))))

(ert-deftest test-orgtrello-controller--properties-labels ()
  (should (equal
           '("#+PROPERTY: :green green label" "#+PROPERTY: :red red label")
           (orgtrello-controller--properties-labels (orgtrello-hash-make-properties '((:red . "red label") (:green . "green label")))))))

(ert-deftest test-orgtrello-controller-load-keys ()
  (should (equal :ok
                 (with-mock
                   (mock (orgtrello-controller-config-file) => :some-config-file)
                   (mock (file-exists-p :some-config-file)   => t)
                   (mock (load :some-config-file)            => t)
                   (orgtrello-controller-load-keys))))
  (should (equal "Setup problem - Problem during credentials loading (consumer-key and read/write access-token) - C-c o i or M-x org-trello-install-key-and-token"
                 (with-mock
                   (mock (orgtrello-controller-config-file) => :some-config-file)
                   (mock (file-exists-p :some-config-file)   => nil)
                   (orgtrello-controller-load-keys))))
  (should (equal "Setup problem - Problem during credentials loading (consumer-key and read/write access-token) - C-c o i or M-x org-trello-install-key-and-token"
                 (with-mock
                   (mock (orgtrello-controller-config-file) => :some-config-file)
                   (mock (file-exists-p :some-config-file)   => t)
                   (mock (load :some-config-file)            => nil)
                   (orgtrello-controller-load-keys)))))

(ert-deftest test-orgtrello-controller-control-keys ()
  (should (equal :ok
                 (let ((org-trello-consumer-key "some-consumer-key")
                       (org-trello-access-token "some-access-token"))
                   (orgtrello-controller-control-keys))))
  (should (equal "Setup problem - You need to install the consumer-key and the read/write access-token - C-c o i or M-x org-trello-install-key-and-token"
                 (let ((org-trello-consumer-key "some-consumer-key")
                       (org-trello-access-token nil))
                   (orgtrello-controller-control-keys))))
  (should (equal "Setup problem - You need to install the consumer-key and the read/write access-token - C-c o i or M-x org-trello-install-key-and-token"
                 (let ((org-trello-consumer-key nil)
                       (org-trello-access-token "some-access-token"))
                   (orgtrello-controller-control-keys)))))

(ert-deftest test-orgtrello-controller-choose-board ()
  (should (equal :id-board0
                 (with-mock
                   (mock (ido-completing-read *) => "board0-name")
                   (orgtrello-controller-choose-board (orgtrello-hash-make-properties '(("board0-name" . :id-board0) ("board1-name" . :id-board1)))))))
  (should (equal :id-board1
                 (with-mock
                   (mock (ido-completing-read *) => "board1-name")
                   (orgtrello-controller-choose-board (orgtrello-hash-make-properties '(("board0-name" . :id-board0) ("board1-name" . :id-board1))))))))

(ert-deftest test-orgtrello-controller--choose-account ()
  (should (equal "account0"
                 (with-mock
                   (mock (ido-completing-read *) => "account0")
                   (orgtrello-controller--choose-account '("account0" "account1")))))
  (should (equal "account1"
                 (with-mock
                   (mock (ido-completing-read *) => "account1")
                   (orgtrello-controller--choose-account '("account0" "account1"))))))

(ert-deftest test-orgtrello-controller--list-boards ()
  (should (equal t
                 (orgtrello-tests-hash-equal
                  (orgtrello-hash-make-properties '((:id . "id0")
                                                    (:name . "name0")
                                                    (:closed . nil)))
                  (car (with-mock
                         (mock (orgtrello-api-get-boards)                          => :query)
                         (mock (orgtrello-query-http-trello :query 'sync) => (list (orgtrello-hash-make-properties '((:id . "id0") (:name . "name0") (:closed)))
                                                                                   (orgtrello-hash-make-properties '((:id . "id1") (:name . "name1") (:closed)))
                                                                                   (orgtrello-hash-make-properties '((:id . "id1") (:name . "name1") (:closed . t)))))
                         (orgtrello-controller--list-boards))))))
  (should (equal
           t
           (orgtrello-tests-hash-equal
            (orgtrello-hash-make-properties '((:id . "id1")
                                              (:name . "name1")
                                              (:closed . nil)))
            (cadr (with-mock
                    (mock (orgtrello-api-get-boards)                          => :query)
                    (mock (orgtrello-query-http-trello :query 'sync) => (list (orgtrello-hash-make-properties '((:id . "id0") (:name . "name0") (:closed)))
                                                                              (orgtrello-hash-make-properties '((:id . "id1") (:name . "name1") (:closed)))
                                                                              (orgtrello-hash-make-properties '((:id . "id1") (:name . "name1") (:closed . t)))))
                    (orgtrello-controller--list-boards)))))))

(ert-deftest test-orgtrello-controller--list-board-lists ()
  (should (equal :some-result
                 (with-mock
                   (mock (orgtrello-api-get-lists :board-id)        => :query)
                   (mock (orgtrello-query-http-trello :query 'sync) => :some-result)
                   (orgtrello-controller--list-board-lists :board-id)))))

(ert-deftest test-orgtrello-controller--hmap-id-name ()
  (should (equal t
                 (orgtrello-tests-hash-equal (orgtrello-hash-make-properties '(("786" . "CANCELLED")
                                                                               ("456" . "FAILED")
                                                                               ("ijk" . "DONE")
                                                                               ("abc" . "TODO")))
                                             (orgtrello-controller--hmap-id-name '("CANCELLED" "FAILED" "DONE" "TODO")
                                                                                '(("board-name" . "some board")
                                                                                  ("board-id" . "10223")
                                                                                  ("CANCELLED" . "786")
                                                                                  ("FAILED" . "456")
                                                                                  ("DELEGATED" . "123")
                                                                                  ("PENDING" . "efg")
                                                                                  ("DONE" . "ijk")
                                                                                  ("IN-PROGRESS" . "def")
                                                                                  ("TODO" . "abc"))))))
  (should (equal t
                 (orgtrello-tests-hash-equal (orgtrello-hash-empty-hash)
                                             (orgtrello-controller--hmap-id-name '("CANCELLED" "FAILED" "DONE" "TODO")
                                                                                '()))))
  (should (equal t
                 (orgtrello-tests-hash-equal (orgtrello-hash-empty-hash)
                                             (orgtrello-controller--hmap-id-name '()
                                                                                '(("board-name" . "some board"))))))
  (should (equal t
                 (orgtrello-tests-hash-equal (orgtrello-hash-empty-hash)
                                             (orgtrello-controller--hmap-id-name '()
                                                                                '())))))

(ert-deftest test-orgtrello-controller-compute-and-overwrite-card ()
  (should (equal
           ":PROPERTIES:
#+PROPERTY: board-name api test board
#+PROPERTY: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-dude 888
#+PROPERTY: orgtrello-user-ardumont 999
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:
* TODO updated card title                                               :orange:red:green:
  :PROPERTIES:
  :orgtrello-users: dude,ardumont
  :orgtrello-local-checksum: local-card-checksum-678
  :orgtrello-id: some-card-id
  :END:
  updated description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\",\"orgtrello-local-checksum\":\"local-checklist-checksum-678\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\",\"orgtrello-local-checksum\":\"local-item-checksum-678\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\",\"orgtrello-local-checksum\":\"local-item-checksum-678\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\",\"orgtrello-local-checksum\":\"local-checklist-checksum-678\"}

** COMMENT ardumont, 10/10/2010
:PROPERTIES:
:orgtrello-id: some-comment-id
:orgtrello-local-checksum: local-comment-checksum-678
:END:
  some comment

** COMMENT tony, 11/10/2010
:PROPERTIES:
:orgtrello-id: some-comment-id2
:orgtrello-local-checksum: local-comment-checksum-678
:END:
  some second comment


* other card name
"
           (orgtrello-tests-with-temp-buffer-and-return-buffer-content
            ":PROPERTIES:
#+PROPERTY: board-name api test board
#+PROPERTY: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-dude 888
#+PROPERTY: orgtrello-user-ardumont 999
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:
* TODO some card name                                                   :orange:
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-card-comments: ardumont: some comment
:END:
some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* other card name
"
            (with-mock
              (mock (orgtrello-buffer-card-checksum) => "local-card-checksum-678")
              (mock (orgtrello-buffer-checklist-checksum) => "local-checklist-checksum-678")
              (mock (orgtrello-buffer-item-checksum) => "local-item-checksum-678")
              (mock (orgtrello-buffer-comment-checksum) => "local-comment-checksum-678")
              (let* ((trello-card (orgtrello-hash-make-properties `((:keyword . "TODO")
                                                                    (:member-ids . "888,999")
                                                                    (:comments . ,(list (orgtrello-hash-make-properties '((:comment-user . "ardumont")
                                                                                                                          (:comment-date . "10/10/2010")
                                                                                                                          (:comment-id   . "some-comment-id")
                                                                                                                          (:comment-text . "some comment")))
                                                                                        (orgtrello-hash-make-properties '((:comment-user . "tony")
                                                                                                                          (:comment-date . "11/10/2010")
                                                                                                                          (:comment-id   . "some-comment-id2")
                                                                                                                          (:comment-text . "some second comment")))))
                                                                    (:tags . ":red:green:")
                                                                    (:desc . "updated description")
                                                                    (:level . 1)
                                                                    (:name . "updated card title")
                                                                    (:id . "some-card-id")))))
                (orgtrello-controller-compute-and-overwrite-card (current-buffer) trello-card)))
            -2))))

(ert-deftest test-orgtrello-controller-sync-buffer-with-trello-cards-cards-already-present ()
  (should (equal
           ":PROPERTIES:
#+PROPERTY: board-name api test board
#+PROPERTY: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-dude 8881
#+PROPERTY: orgtrello-user-ardumont 9991
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:
* TODO updated card title                                               :orange:red:green:
  :PROPERTIES:
  :orgtrello-users: dude,ardumont
  :orgtrello-local-checksum: card-checksum-12
  :orgtrello-id: some-card-id
  :END:
  updated description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\",\"orgtrello-local-checksum\":\"checklist-checksum-12\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\",\"orgtrello-local-checksum\":\"item-checksum-12\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\",\"orgtrello-local-checksum\":\"item-checksum-12\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\",\"orgtrello-local-checksum\":\"checklist-checksum-12\"}

** COMMENT ardumont, 10/10/2010
:PROPERTIES:
:orgtrello-id: some-comment-id
:orgtrello-local-checksum: comment-checksum-12
:END:
  some comment

** COMMENT tony, 11/10/2010
:PROPERTIES:
:orgtrello-id: some-comment-id2
:orgtrello-local-checksum: comment-checksum-12
:END:
  some second comment

* TODO other card name
  :PROPERTIES:
  :orgtrello-id: some-new-marker
  :orgtrello-local-checksum: card-checksum-12
  :END:

"
           (orgtrello-tests-with-temp-buffer-and-return-buffer-content
            ":PROPERTIES:
#+PROPERTY: board-name api test board
#+PROPERTY: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-dude 8881
#+PROPERTY: orgtrello-user-ardumont 9991
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:
* TODO some card name                                                   :orange:
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-card-comments: ardumont: some comment
:END:
some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* other card name
"
            (with-mock
              (mock (orgtrello-buffer--compute-marker-from-entry *) => "some-new-marker")
              (mock (orgtrello-buffer-card-checksum) => "card-checksum-12")
              (mock (orgtrello-buffer-checklist-checksum) => "checklist-checksum-12")
              (mock (orgtrello-buffer-item-checksum) => "item-checksum-12")
              (mock (orgtrello-buffer-comment-checksum) => "comment-checksum-12")
              (let* ((trello-card0 (orgtrello-hash-make-properties `((:keyword . "TODO")
                                                                     (:member-ids . "8881,9991")
                                                                     (:comments . ,(list (orgtrello-hash-make-properties '((:comment-user . "ardumont")
                                                                                                                           (:comment-date . "10/10/2010")
                                                                                                                           (:comment-id   . "some-comment-id")
                                                                                                                           (:comment-text . "some comment")))
                                                                                         (orgtrello-hash-make-properties '((:comment-user . "tony")
                                                                                                                           (:comment-date . "11/10/2010")
                                                                                                                           (:comment-id   . "some-comment-id2")
                                                                                                                           (:comment-text . "some second comment")))))
                                                                     (:tags . ":red:green:")
                                                                     (:desc . "updated description")
                                                                     (:level . ,org-trello--card-level)
                                                                     (:name . "updated card title")
                                                                     (:id . "some-card-id")))))
                (orgtrello-controller-sync-buffer-with-trello-cards (current-buffer) (list trello-card0))))))))

(ert-deftest test-orgtrello-controller-sync-buffer-with-trello-cards-with-multiple-cards ()
  "Overwrite card"
  (should (equal
           ":PROPERTIES:
#+PROPERTY: board-name api test board
#+PROPERTY: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-dude 8882
#+PROPERTY: orgtrello-user-ardumont 9992
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:
* TODO updated card title                                               :orange:red:green:
  :PROPERTIES:
  :orgtrello-users: dude,ardumont
  :orgtrello-local-checksum: card-checksum-1234
  :orgtrello-id: some-card-id
  :END:
  updated description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\",\"orgtrello-local-checksum\":\"checklist-checksum-1234\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\",\"orgtrello-local-checksum\":\"item-checksum-1234\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\",\"orgtrello-local-checksum\":\"item-checksum-1234\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\",\"orgtrello-local-checksum\":\"checklist-checksum-1234\"}

** COMMENT ardumont, 10/10/2010
:PROPERTIES:
:orgtrello-id: some-comment-id
:orgtrello-local-checksum: comment-checksum-1234
:END:
  some comment

** COMMENT tony, 11/10/2010
:PROPERTIES:
:orgtrello-id: some-comment-id2
:orgtrello-local-checksum: comment-checksum-1234
:END:
  some second comment

* TODO other card name                                                  :green:
  :PROPERTIES:
  :orgtrello-users: dude
  :orgtrello-id: some-card-id2
  :orgtrello-local-checksum: card-checksum-1234
  :END:
  this is a description
* TODO other card name
  :PROPERTIES:
  :orgtrello-id: some-new-marker
  :orgtrello-local-checksum: card-checksum-1234
  :END:

"
           (orgtrello-tests-with-temp-buffer-and-return-buffer-content
            ":PROPERTIES:
#+PROPERTY: board-name api test board
#+PROPERTY: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-dude 8882
#+PROPERTY: orgtrello-user-ardumont 9992
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:
* TODO some card name                                                   :orange:
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-card-comments: ardumont: some comment
:END:
some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* other card name
"
            (with-mock
              (mock (orgtrello-buffer--compute-marker-from-entry *) => "some-new-marker")
              (mock (orgtrello-buffer-card-checksum) => "card-checksum-1234")
              (mock (orgtrello-buffer-checklist-checksum) => "checklist-checksum-1234")
              (mock (orgtrello-buffer-item-checksum) => "item-checksum-1234")
              (mock (orgtrello-buffer-comment-checksum) => "comment-checksum-1234")
              (let* ((trello-card0 (orgtrello-hash-make-properties `((:keyword . "TODO")
                                                                     (:member-ids . "8882,9992")
                                                                     (:comments . ,(list (orgtrello-hash-make-properties '((:comment-user . "ardumont")
                                                                                                                           (:comment-date . "10/10/2010")
                                                                                                                           (:comment-id   . "some-comment-id")
                                                                                                                           (:comment-text . "some comment")))
                                                                                         (orgtrello-hash-make-properties '((:comment-user . "tony")
                                                                                                                           (:comment-date . "11/10/2010")
                                                                                                                           (:comment-id   . "some-comment-id2")
                                                                                                                           (:comment-text . "some second comment")))))
                                                                     (:tags . ":red:green:")
                                                                     (:desc . "updated description")
                                                                     (:level . ,org-trello--card-level)
                                                                     (:name . "updated card title")
                                                                     (:id . "some-card-id"))))
                     (trello-card1 (orgtrello-hash-make-properties `((:keyword . "TODO")
                                                                     (:member-ids . "8882")
                                                                     (:comments . nil)
                                                                     (:tags . ":green:")
                                                                     (:desc . "this is a description")
                                                                     (:level . ,org-trello--card-level)
                                                                     (:name . "other card name")
                                                                     (:id . "some-card-id2")))))
                (orgtrello-controller-sync-buffer-with-trello-cards (current-buffer) (list trello-card0 trello-card1))))))))

(ert-deftest test-orgtrello-controller-sync-buffer-with-trello-cards ()
  "Overwrite multiple cards."
  (should (equal
           ":PROPERTIES:
#+PROPERTY: board-name api test board
#+PROPERTY: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-dude 8883
#+PROPERTY: orgtrello-user-ardumont 9993
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:
* TODO updated card title                                               :orange:red:green:
  :PROPERTIES:
  :orgtrello-users: dude,ardumont
  :orgtrello-local-checksum: card-checksum-123456
  :orgtrello-id: some-card-id
  :END:
  updated description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\",\"orgtrello-local-checksum\":\"checklist-checksum-123456\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\",\"orgtrello-local-checksum\":\"item-checksum-123456\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\",\"orgtrello-local-checksum\":\"item-checksum-123456\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\",\"orgtrello-local-checksum\":\"checklist-checksum-123456\"}

** COMMENT ardumont, 10/10/2010
:PROPERTIES:
:orgtrello-id: some-comment-id
:orgtrello-local-checksum: comment-checksum-123456
:END:
  some comment

** COMMENT tony, 11/10/2010
:PROPERTIES:
:orgtrello-id: some-comment-id2
:orgtrello-local-checksum: comment-checksum-123456
:END:
  some second comment

* DONE other card name                                                  :green:
  :PROPERTIES:
  :orgtrello-users: dude
  :orgtrello-id: some-card-id2
  :orgtrello-local-checksum: card-checksum-123456
  :END:
  this is a description
"
           (orgtrello-tests-with-temp-buffer-and-return-buffer-content
            ":PROPERTIES:
#+PROPERTY: board-name api test board
#+PROPERTY: board-id abc
#+PROPERTY: CANCELLED def
#+PROPERTY: FAILED ijk
#+PROPERTY: DELEGATED lmn
#+PROPERTY: PENDING opq
#+PROPERTY: DONE rst
#+PROPERTY: IN-PROGRESS uvw
#+PROPERTY: TODO xyz
#+TODO: TODO IN-PROGRESS DONE | PENDING DELEGATED FAILED CANCELLED
#+PROPERTY: orgtrello-user-dude 8883
#+PROPERTY: orgtrello-user-ardumont 9993
#+PROPERTY: :yellow yellow label
#+PROPERTY: :red red label
#+PROPERTY: :purple this is the purple label
#+PROPERTY: :orange orange label
#+PROPERTY: :green green label with & char
#+PROPERTY: :blue
#+PROPERTY: orgtrello-user-me ardumont
:END:
* TODO some card name                                                   :orange:
:PROPERTIES:
:orgtrello-id: some-card-id
:orgtrello-card-comments: ardumont: some comment
:END:
some description
  - [-] some checklist name :PROPERTIES: {\"orgtrello-id\":\"some-checklist-id\"}
    - [X] some item :PROPERTIES: {\"orgtrello-id\":\"some-item-id\"}
    - [ ] some other item :PROPERTIES: {\"orgtrello-id\":\"some-other-item-id\"}
  - [-] some other checklist name :PROPERTIES: {\"orgtrello-id\":\"some-other-checklist-id\"}

* other card name
:PROPERTIES:
:orgtrello-id: some-card-id2
:END:
"
            (with-mock
              (mock (orgtrello-buffer-card-checksum) => "card-checksum-123456")
              (mock (orgtrello-buffer-checklist-checksum) => "checklist-checksum-123456")
              (mock (orgtrello-buffer-item-checksum) => "item-checksum-123456")
              (mock (orgtrello-buffer-comment-checksum) => "comment-checksum-123456")
              (let* ((trello-card0 (orgtrello-hash-make-properties `((:keyword . "TODO")
                                                                     (:member-ids . "8883,9993")
                                                                     (:comments . ,(list (orgtrello-hash-make-properties '((:comment-user . "ardumont")
                                                                                                                           (:comment-date . "10/10/2010")
                                                                                                                           (:comment-id   . "some-comment-id")
                                                                                                                           (:comment-text . "some comment")))
                                                                                         (orgtrello-hash-make-properties '((:comment-user . "tony")
                                                                                                                           (:comment-date . "11/10/2010")
                                                                                                                           (:comment-id   . "some-comment-id2")
                                                                                                                           (:comment-text . "some second comment")))))
                                                                     (:tags . ":red:green:")
                                                                     (:desc . "updated description")
                                                                     (:level . ,org-trello--card-level)
                                                                     (:name . "updated card title")
                                                                     (:id . "some-card-id"))))
                     (trello-card1 (orgtrello-hash-make-properties `((:keyword . "DONE")
                                                                     (:member-ids . "8883")
                                                                     (:tags . ":green:")
                                                                     (:desc . "this is a description")
                                                                     (:level . ,org-trello--card-level)
                                                                     (:name . "other card name")
                                                                     (:id . "some-card-id2")))))
                (orgtrello-controller-sync-buffer-with-trello-cards (current-buffer) (list trello-card0 trello-card1))))))))

(ert-deftest test-orgtrello-controller-user-account-from-config-file ()
  (should (string= "config" (orgtrello-controller-user-account-from-config-file "/home/user/.emacs.d/.trello/config.el")))
  (should (string= "ardumont" (orgtrello-controller-user-account-from-config-file "/home/user/.emacs.d/.trello/ardumont.el"))))

(ert-deftest test-orgtrello-controller-list-user-accounts ()
  (should (equal '("ardumont" "config" "orgmode")
                 (orgtrello-controller-list-user-accounts '("/home/user/.emacs.d/.trello/ardumont.el" "/home/user/.emacs.d/.trello/config.el" "/home/user/.emacs.d/.trello/orgmode.el"))))
  (should (equal '("foobar")
                 (orgtrello-controller-list-user-accounts '("/home/user/.emacs.d/.trello/foobar.el")))))


(ert-deftest test-orgtrello-controller-set-account ()
  (should (equal :ok
                 (with-mock
                  (mock (orgtrello-buffer-me) => "some-account")
                  (orgtrello-controller-set-account))))
  (should (equal :ok
                 (with-mock
                  (mock (orgtrello-buffer-me) => nil)
                  (mock (orgtrello-controller-user-config-files) => :some-config-file)
                  (mock (orgtrello-controller-list-user-accounts :some-config-file) => '(account0))
                  (orgtrello-controller-set-account))))
  (should (equal :ok
                 (with-mock
                  (mock (orgtrello-buffer-me) => nil)
                  (mock (orgtrello-controller-user-config-files) => :some-config-file)
                  (mock (orgtrello-controller-list-user-accounts :some-config-file) => '(:account0 :account1))
                  (mock (orgtrello-controller--choose-account '(:account0 :account1)) => :account0)
                  (orgtrello-controller-set-account)))))

(provide 'org-trello-controller-test)
;;; org-trello-controller-test.el ends here
