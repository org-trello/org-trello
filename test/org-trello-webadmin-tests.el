(require 'org-trello-webadmin)
(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(expectations (desc "compose-fn")
              (expect '(3 5 7) (--map (funcall (compose-fn '((lambda (it) (+ 1 it)) (lambda (it) (* 2 it)))) it) '(1 2 3))))

(expectations (desc "orgtrello-webadmin/--detail-entity")
              (expect "entity name" (orgtrello-webadmin/--detail-entity 3 (orgtrello-hash/make-properties '((:name . "entity name")))))
              (expect "entity name" (gethash :name (orgtrello-webadmin/--detail-entity 5 (orgtrello-hash/make-properties '((:name . "entity name")))))))

(expectations (desc "orgtrello-webadmin/--header-table")
  (expect '(tr nil (td nil) (td nil "Action") (td nil "Entity") (td nil "Delete")) (orgtrello-webadmin/--header-table)))

(expectations (desc "orgtrello-webadmin/--delete-action")
  (expect '(input ((class . "btn btn-danger btn-mini") (type . "button") (onclick . "deleteEntities('/proxy/admin/entities/delete/id');") (value . "x"))) (orgtrello-webadmin/--delete-action
                                                                                                                                                           (orgtrello-hash/make-properties '((:id . "id")))))
  (expect ""                                          (orgtrello-webadmin/--delete-action (orgtrello-hash/make-properties '((:name . "name"))))))

(expectations (desc "orgtrello-webadmin/--entity")
  (expect '(tr
            ((class . "success"))
            (td nil
                (i
                 ((class . "icon-play"))))
            (td nil "test")
            (td nil "name")
            (td nil
                (input
                 ((class . "btn btn-danger btn-mini")
                  (type . "button")
                  (onclick . "deleteEntities('/proxy/admin/entities/delete/id');")
                  (value . "x")))))
    (orgtrello-webadmin/--entity (orgtrello-hash/make-properties '((:action . "test") (:id . "id") (:name . "name"))) "icon-play"))

  (expect '(tr
            ((class . "warning"))
            (td nil
                (i
                 ((class . "icon-pause"))))
            (td nil "delete")
            (td nil "name")
            (td nil
                (input
                 ((class . "btn btn-danger btn-mini")
                  (type . "button")
                  (onclick . "deleteEntities('/proxy/admin/entities/delete/id');")
                  (value . "x")))))
    (orgtrello-webadmin/--entity (orgtrello-hash/make-properties '((:action . "delete") (:id . "id") (:name . "name"))) "icon-pause"))

  (expect '(tr
            ((class . "success"))
            (td nil
                (i
                 ((class . "icon-play"))))
            (td nil "test")
            (td nil "name 0")
            (td nil
                (input
                 ((class . "btn btn-danger btn-mini")
                  (type . "button")
                  (onclick . "deleteEntities('/proxy/admin/entities/delete/id');")
                  (value . "x")))))
    (orgtrello-webadmin/--entity (orgtrello-hash/make-properties '((:action . "test") (:name . "name 0") (:id . "id"))) "icon-play"))

  (expect '(tr
            ((class . "warning"))
            (td nil
                (i
                 ((class . "icon-pause"))))
            (td nil "delete")
            (td nil "name 1")
            (td nil
                (input
                 ((class . "btn btn-danger btn-mini")
                  (type . "button")
                  (onclick . "deleteEntities('/proxy/admin/entities/delete/id');")
                  (value . "x")))))
    (orgtrello-webadmin/--entity (orgtrello-hash/make-properties '((:action . "delete") (:name . "name 1") (:id . "id"))) "icon-pause")))

(expectations (desc "orgtrello-webadmin/--input-button-html")
  (expect '(input
            ((class . "btn btn-danger btn-mini")
             (type . "button")
             (onclick . "deleteEntities('/proxy/admin/entities/delete/');")
             (value . "x")))
    (orgtrello-webadmin/--input-button-html "deleteEntities('/proxy/admin/entities/delete/');" "x")))

(expectations (desc "orgtrello-webadmin/--main-body")
  (expect '(div
            ((class . "row-fluid marketing"))
            (div
             ((class . "span6"))
             (div
              ((style . "font-size: 2em;margin-right: 10px;margin-bottom: 10px"))
              "Current action")
             (span
              ((id . "current-action"))))
            (div
             ((class . "span6"))
             (div
              ((style . "margin-bottom:10px"))
              (span
               ((style . "font-size: 2em;margin-right: 10px"))
               "Next actions")
              (span nil
                    (input
                     ((class . "btn btn-danger btn-mini")
                      (type . "button")
                      (onclick . "deleteEntities('/proxy/admin/entities/delete/');")
                      (value . "Delete all")))))
             (span
              ((id . "next-actions")))))
    (orgtrello-webadmin/--main-body)))

(expectations (desc "orgtrello-webadmin/--render-html")
  (expect
      (esxml-to-xml `(div ((class . "hello")) "world"))
    (orgtrello-webadmin/--render-html `(div ((class . "hello")) "world"))))

(expectations (desc "orgtrello-webadmin/--entities-as-html")
  (expect "None" (orgtrello-webadmin/--entities-as-html nil))
  (expect "None" (orgtrello-webadmin/--entities-as-html nil "icon-arrow-right"))
  (expect "None" (orgtrello-webadmin/--entities-as-html nil "icon-arrow-right" "icon-arrow-left"))
  (expect '(table
            ((class . "table table-striped table-bordered table-hover")
             (style . "font-size: 0.75em"))
            (tr nil
                (td nil)
                (td nil "Action")
                (td nil "Entity")
                (td nil "Delete"))
            (tr
             ((class . ""))
             (td nil
                 (i
                  ((class . "icon-arrow-right"))))
             (td nil "create")
             (td nil "name 0")
             (td nil
                 (input
                  ((class . "btn btn-danger btn-mini")
                   (type . "button")
                   (onclick . "deleteEntities('/proxy/admin/entities/delete/id 0');")
                   (value . "x")))))
            (tr
             ((class . ""))
             (td nil
                 (i
                  ((class . "icon-arrow-up"))))
             (td nil "delete")
             (td nil "name 1")
             (td nil "")))
    (orgtrello-webadmin/--entities-as-html (mapcar 'orgtrello-hash/make-properties
                                                   '(((:action . "create") (:name . "name 0") (:id . "id 0"))
                                                     ((:action . "delete") (:name . "name 1"))))))

  (expect '(table
            ((class . "table table-striped table-bordered table-hover")
             (style . "font-size: 0.75em"))
            (tr nil
                (td nil)
                (td nil "Action")
                (td nil "Entity")
                (td nil "Delete"))
            (tr
             ((class . ""))
             (td nil
                 (i
                  ((class . "icon-arrow-right"))))
             (td nil "create")
             (td nil "name 0")
             (td nil ""))
            (tr
             ((class . ""))
             (td nil
                 (i
                  ((class . "icon-arrow-up"))))
             (td nil "delete")
             (td nil "name 1")
             (td nil "")))
    (orgtrello-webadmin/--entities-as-html (mapcar 'orgtrello-hash/make-properties
                                                   '(((:action . "create") (:name . "name 0"))
                                                     ((:action . "delete") (:name . "name 1")))) "icon-arrow-right"))

  (expect '(table
            ((class . "table table-striped table-bordered table-hover")
             (style . "font-size: 0.75em"))
            (tr nil
                (td nil)
                (td nil "Action")
                (td nil "Entity")
                (td nil "Delete"))
            (tr
             ((class . ""))
             (td nil
                 (i
                  ((class . "icon-arrow-right"))))
             (td nil "create")
             (td nil "name 0")
             (td nil ""))
            (tr
             ((class . ""))
             (td nil
                 (i
                  ((class . "icon-arrow-up"))))
             (td nil "delete")
             (td nil "name 1")
             (td nil "")))
    (orgtrello-webadmin/--entities-as-html (mapcar 'orgtrello-hash/make-properties '(((:action . "create") (:name . "name 0"))
                                                                                     ((:action . "delete") (:name . "name 1")))) nil "icon-arrow-up"))

  (expect '(table
            ((class . "table table-striped table-bordered table-hover")
             (style . "font-size: 0.75em"))
            (tr nil
                (td nil)
                (td nil "Action")
                (td nil "Entity")
                (td nil "Delete"))
            (tr
             ((class . "success"))
             (td nil
                 (i
                  ((class . "icon-play"))))
             (td nil "create")
             (td nil "name 0")
             (td nil ""))
            (tr
             ((class . "warning"))
             (td nil
                 (i
                  ((class . "icon-pause"))))
             (td nil "delete")
             (td nil "name 1")
             (td nil "")))
    (orgtrello-webadmin/--entities-as-html (mapcar 'orgtrello-hash/make-properties
                                                   '(((:action . "create") (:name . "name 0"))
                                                     ((:action . "delete") (:name . "name 1")))) "icon-play" "icon-pause")))

(expectations (desc "orgtrello-webadmin/entities-as-html")
  (expect '((tr
             ((class . ""))
             (td nil
                 (i
                  ((class . "next"))))
             (td nil "action")
             (td nil "nil")
             (td nil
                 (input
                  ((class . "btn btn-danger btn-mini")
                   (type . "button")
                   (onclick . "deleteEntities('/proxy/admin/entities/delete/id');")
                   (value . "x")))))
            (tr
             ((class . ""))
             (td nil
                 (i
                  ((class . "next"))))
             (td nil "action")
             (td nil "nil")
             (td nil
                 (input
                  ((class . "btn btn-danger btn-mini")
                   (type . "button")
                   (onclick . "deleteEntities('/proxy/admin/entities/delete/id2');")
                   (value . "x"))))))
    (orgtrello-webadmin/entities-as-html (mapcar 'orgtrello-hash/make-properties
                                                        '(((:action . "action") (:id . "id") (:marker . "marker"))
                                                          ((:action . "action") (:id . "id2") (:marker . "marker2")))) "next")))

(expectations (desc "orgtrello-webadmin/--compute-class")
  (expect '(class . "success") (orgtrello-webadmin/--compute-class "icon-play"))
  (expect '(class . "warning") (orgtrello-webadmin/--compute-class "icon-pause"))
  (expect '(class . "")        (orgtrello-webadmin/--compute-class nil))
  (expect '(class . "")        (orgtrello-webadmin/--compute-class "any")))
