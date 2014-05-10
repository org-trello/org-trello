;;; org-trello-backend.el --- Orchestration namespace to discuss with trello
;;; Commentary:
;;; Code:

(defun orgtrello-backend/compute-items-from-checklist! (checklist entities adjacency)
  "Given a checklist, retrieve its items and update the entities hash and the adjacency list."
  (let ((checklist-id (orgtrello-data/entity-id checklist)))
    (--reduce-from (cl-destructuring-bind (entities adjacency) acc
                     (list (orgtrello-backend/--add-entity-to-entities it entities)
                           (orgtrello-backend/--add-entity-to-adjacency it checklist adjacency)))
                   (list entities adjacency)
                   (orgtrello-data/entity-items checklist))))

(defun orgtrello-backend/retrieve-checklist-from-card! (card)
  "Given a card, retrieve the checklist of the card (using trello). This gives a list of checklist in the trello order."
  (--> card
    (orgtrello-data/entity-checklists it)                                                            ;; retrieve checklist ids
    (-reduce-from (lambda (acc-list checklist-id)
                    (cons (-> checklist-id
                            orgtrello-api/get-checklist
                            (orgtrello-query/http-trello 'synchronous-query)) acc-list))
                  nil
                  it)                                                                         ;; retrieve the trello checklist
    (sort it (lambda (a b) (when (<= (orgtrello-data/entity-position a) (orgtrello-data/entity-position b)) 1)))))          ;; sort them by pos to get back to the right order (reversed)

(defun orgtrello-backend/compute-checklist-entities-from-card! (card entities adjacency)
  "Given a card, retrieve its checklists (with their items) in the right order."
  (let ((card-id (orgtrello-data/entity-id card)))
    (--> card
      (orgtrello-backend/retrieve-checklist-from-card! it)
      (-reduce-from (lambda (acc-entities-adj checklist)
                      (cl-destructuring-bind (entities adjacency) acc-entities-adj
                        (orgtrello-backend/compute-items-from-checklist! checklist (orgtrello-backend/--add-entity-to-entities checklist entities) (orgtrello-backend/--add-entity-to-adjacency checklist card adjacency))))
                    (list entities adjacency)
                    it))));; at last complete checklist with item

;; one map for each complete entity: {entity-id entity} (entity in {card, checklist, item}
;; adjacency list {card-id (checklist-id)
;;                 checklist-id (item-id)}

(defun orgtrello-backend/compute-full-cards-from-trello! (cards)
  "Given a list of cards, compute the full cards data from the trello board. The order from the trello board is kept. Hash result is of the form: {entity-id '(entity-card {checklist-id (checklist (item))})}"
  (--reduce-from (progn
                   (orgtrello-log/msg *OT/INFO* "Computing card '%s' data..."
                                      (orgtrello-data/entity-name it))
                   (cl-destructuring-bind (entities adjacency) acc
                     (orgtrello-backend/compute-checklist-entities-from-card! it (orgtrello-backend/--add-entity-to-entities it entities) adjacency)))
                 (list (orgtrello-hash/empty-hash) (orgtrello-hash/empty-hash))
                 cards))

(defun orgtrello-backend/compute-full-checklist-from-trello! (checklist)
  "Given a checklist, compute the full items data from trello. The order from the trello board is kept. Return result is the list of entities and adjacency in this order."
  (let ((adjacency (orgtrello-hash/empty-hash)))
    (orgtrello-log/msg *OT/INFO* "Computing checklist '%s' data..." (orgtrello-data/entity-name checklist))
    (--> (orgtrello-hash/empty-hash)
      (orgtrello-data/puthash-data (orgtrello-data/entity-id checklist) checklist it)
      (orgtrello-backend/compute-items-from-checklist! checklist it adjacency))))

(defun orgtrello-backend/--add-entity-to-entities (entity entities)
  "Adding entity to the hash entities."
  (-> (orgtrello-data/entity-id-or-marker entity)
    (orgtrello-data/puthash-data entity entities)))

(defun orgtrello-backend/--add-entity-to-adjacency (current-entity parent-entity adjacency)
  "Adding entity to the adjacency entry."
  (let* ((current-id (orgtrello-data/entity-id-or-marker current-entity))
         (parent-id  (orgtrello-data/entity-id-or-marker parent-entity)))
    (orgtrello-data/puthash-data parent-id (-snoc (gethash parent-id adjacency) current-id) adjacency)))

(defun orgtrello-backend/--put-entities-with-adjacency (current-meta entities adjacency)
  "Deal with adding a new item to entities."
  (let ((current-entity (orgtrello-data/current current-meta))
        (parent-entity  (orgtrello-data/parent current-meta)))
    (list (orgtrello-backend/--add-entity-to-entities current-entity entities) (orgtrello-backend/--add-entity-to-adjacency current-entity parent-entity adjacency))))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-backend loaded!")

(provide 'org-trello-backend)
;;; org-trello-backend.el ends here
