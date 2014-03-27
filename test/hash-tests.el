(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(expectations (desc "testing orgtrello-hash/make-hash-org")
  (expect "some name"       (gethash :name           (orgtrello-hash/make-hash-org "" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc :comments :tags)))
  (expect "IN PROGRESS"     (gethash :keyword        (orgtrello-hash/make-hash-org "" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc :comments :tags)))
  (expect 0                 (gethash :level          (orgtrello-hash/make-hash-org "" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc :comments :tags)))
  (expect "some id"         (gethash :id             (orgtrello-hash/make-hash-org "" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc :comments :tags)))
  (expect "due-date"        (gethash :due            (orgtrello-hash/make-hash-org "" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc :comments :tags)))
  (expect :point            (gethash :position       (orgtrello-hash/make-hash-org "" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc :comments :tags)))
  (expect "buffer-name.org" (gethash :buffername     (orgtrello-hash/make-hash-org "" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc :comments :tags)))
  (expect "1,2,3"           (gethash :member-ids     (orgtrello-hash/make-hash-org "1,2,3" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc :comments :tags)))
  (expect :desc             (gethash :desc           (orgtrello-hash/make-hash-org "1,2,3" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc :comments :tags)))
  (expect :comments         (gethash :comments       (orgtrello-hash/make-hash-org "1,2,3" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc :comments :tags)))
  (expect :tags             (gethash :tags           (orgtrello-hash/make-hash-org "1,2,3" 0 "IN PROGRESS" "some name" "some id" "due-date" :point "buffer-name.org" :desc :comments :tags))))

(expectations (desc "testing orgtrello-hash/make-hash")
  (expect :some-method (gethash :method (orgtrello-hash/make-hash :some-method :some-uri)))
  (expect :some-uri    (gethash :uri    (orgtrello-hash/make-hash :some-method :some-uri)))
  (expect nil          (gethash :params (orgtrello-hash/make-hash :some-method :some-uri))))

(expectations (desc "orgtrello-hash/key")
              (expect ":key:" (orgtrello-hash/key "key")))

(expectations (desc "orgtrello-hash/make-hierarchy")
 (expect :current (gethash :current (orgtrello-hash/make-hierarchy :current)))
 (expect nil (gethash :parent (orgtrello-hash/make-hierarchy :current)))
 (expect nil (gethash :grandparent (orgtrello-hash/make-hierarchy :current))))

(expectations (desc "orgtrello-hash/make-hierarchy")
 (expect :current (gethash :current (orgtrello-hash/make-hierarchy :current :parent)))
 (expect :parent (gethash :parent (orgtrello-hash/make-hierarchy :current :parent)))
 (expect nil (gethash :grandparent (orgtrello-hash/make-hierarchy :current :parent))))

(expectations (desc "orgtrello-hash/make-hierarchy")
 (expect :current (gethash :current (orgtrello-hash/make-hierarchy :current :parent :grandparent)))
 (expect :parent (gethash :parent (orgtrello-hash/make-hierarchy :current :parent :grandparent)))
 (expect :grandparent (gethash :grandparent (orgtrello-hash/make-hierarchy :current :parent :grandparent))))

(expectations (desc "orgtrello-hash/make-hierarchy")
 (expect :current (gethash :current (orgtrello-hash/make-hierarchy :current nil :grandparent)))
 (expect nil (gethash :parent (orgtrello-hash/make-hierarchy :current nil :grandparent)))
 (expect :grandparent (gethash :grandparent (orgtrello-hash/make-hierarchy nil :parent :grandparent))))

(expectations (desc "orgtrello-hash/make-hierarchy")
  (expect :ok                                 (-> (orgtrello-hash/make-hash-org :users 1 :keyword "some name" :id :due :position :buffer-name :desc :comments :tags)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-CARD-MISSING-NAME*      (-> (orgtrello-hash/make-hash-org :users 1 :keyword "" :id :due :position :buffer-name :desc :comments :tags)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-CARD-MISSING-NAME*      (-> (orgtrello-hash/make-hash-org :users 1 :keyword nil :id :due :position :buffer-name :desc :comments :tags)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p))
  (expect :ok                                 (-> (orgtrello-hash/make-hash-org :users 2 :keyword "some name" :id :due :position :buffer-name :desc :comments :tags)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-CHECKLIST-MISSING-NAME* (-> (orgtrello-hash/make-hash-org :users 2 :keyword "" :id :due :position :buffer-name :desc :comments :tags)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-CHECKLIST-MISSING-NAME* (-> (orgtrello-hash/make-hash-org :users 2 :keyword nil :id :due :position :buffer-name :desc :comments :tags)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p))
  (expect :ok                                 (-> (orgtrello-hash/make-hash-org :users 3 :keyword "some name" :id :due :position :buffer-name :desc :comments :tags)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-ITEM-MISSING-NAME*      (-> (orgtrello-hash/make-hash-org :users 3 :keyword "" :id :due :position :buffer-name :desc :comments :tags)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p))
  (expect *ERROR-SYNC-ITEM-MISSING-NAME*      (-> (orgtrello-hash/make-hash-org :users 3 :keyword nil :id :due :position :buffer-name :desc :comments :tags)
                                                  orgtrello-hash/make-hierarchy
                                                  orgtrello-controller/--mandatory-name-ok-p)))

(ert-deftest testing-orgtrello-hash/init-map-from ()
  (should (hash-equal #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ()) (orgtrello-hash/init-map-from nil))))

(expectations (desc "orgtrello-hash/init-map-from")
              (expect :data (orgtrello-hash/init-map-from :data)))
