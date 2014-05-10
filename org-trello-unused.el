;;; org-trello-unused.el --- Some code I'm not set on destroying just yet.
;;; Commentary:
;;; Code:

(defun orgtrello-action/org-delete-property (key)
  "Delete a property depending on the nature of the current entry (org heading or checkbox)."
  (funcall (if (orgtrello-cbx/checkbox-p) 'orgtrello-cbx/org-delete-property 'org-delete-property) key))

(defun orgtrello-api/get-list (list-id)
  "Get a list by id"
  (orgtrello-api/make-query "GET" (format "/lists/%s" list-id)))

(expectations
  (desc "orgtrello-api/get-list")
  (expect "GET"              (gethash :method (orgtrello-api/get-list :list-id)))
  (expect "/lists/:list-id" (gethash :uri    (orgtrello-api/get-list :list-id)))
  (expect nil               (gethash :params (orgtrello-api/get-list :list-id))))

(defun orgtrello-api/get-cards-from-list (list-id)
  "List all the cards"
  (orgtrello-api/make-query "GET" (format "/lists/%s/cards" list-id)))

(expectations
 (desc "orgtrello-api/get-cards-from-list")
 (expect "GET"                    (gethash :method (orgtrello-api/get-cards-from-list :list-id)))
 (expect "/lists/:list-id/cards" (gethash :uri    (orgtrello-api/get-cards-from-list :list-id)))
 (expect nil                     (gethash :params (orgtrello-api/get-cards-from-list :list-id))))

(defun orgtrello-api/get-checklists (card-id)
  "List the checklists of a card"
  (orgtrello-api/make-query "GET" (format "/cards/%s/checklists" card-id)))

(expectations
 (desc "orgtrello-api/get-checklists")
 (expect "GET"                         (gethash :method (orgtrello-api/get-checklists :card-id)))
 (expect "/cards/:card-id/checklists" (gethash :uri    (orgtrello-api/get-checklists :card-id)))
 (expect nil                          (gethash :params (orgtrello-api/get-checklists :card-id))))

(defun orgtrello-api/get-items (checklist-id)
  "List the checklist items."
  (orgtrello-api/make-query "GET" (format "/checklists/%s/checkItems/" checklist-id)))

(expectations
 (desc "orgtrello-api/get-items")
 (expect "GET"                                   (gethash :method (orgtrello-api/get-items :checklist-id)))
 (expect "/checklists/:checklist-id/checkItems/" (gethash :uri    (orgtrello-api/get-items :checklist-id))))

(defun orgtrello-api/get-member (member-id)
  "Retrieve the member by its identifier."
  (orgtrello-api/make-query "GET" (format "/members/%s" member-id)))

(expectations
 (desc "orgtrello-api/get-member")
 (expect "GET"          (gethash :method (orgtrello-api/get-member :id)))
 (expect "/members/:id" (gethash :uri (orgtrello-api/get-member :id))))
