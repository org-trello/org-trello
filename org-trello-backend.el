;;; org-trello-backend.el --- Orchestration namespace to discuss with trello
;;; Commentary:
;;; Code:

(require 'dash)
(require 'org-trello-setup)
(require 'org-trello-log)
(require 'org-trello-hash)
(require 'org-trello-data)
(require 'org-trello-query)
(require 'org-trello-api)

(org-trello/require-cl)

(defun orgtrello-backend/compute-items-from-checklist! (checklist entities adjacencies)
  "Given a CHECKLIST, retrieve its items and update the ENTITIES hash and the ADJACENCY list."
  (let ((checklist-id (orgtrello-data/entity-id checklist)))
    (--reduce-from (cl-destructuring-bind (ents adjs) acc
                     (list (orgtrello-backend/--add-entity-to-entities it ents)
                           (orgtrello-backend/--add-entity-to-adjacency it checklist adjs)))
                   (list entities adjacencies)
                   (orgtrello-data/entity-items checklist))))

(defun orgtrello-backend/retrieve-checklist-from-card! (card)
  "Given a CARD, retrieve the checklist of the card (using trello).
This gives a list of checklist in the trello order."
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
  "Given a CARD, retrieve from ENTITIES and ADJACENCY its checklists (with their items) in the right order."
  (let ((card-id (orgtrello-data/entity-id card)))
    (--> card
      (orgtrello-backend/retrieve-checklist-from-card! it)
      (-reduce-from (lambda (acc-entities-adj checklist)
                      (cl-destructuring-bind (entities adjacency) acc-entities-adj
                        (orgtrello-backend/compute-items-from-checklist! checklist (orgtrello-backend/--add-entity-to-entities checklist entities) (orgtrello-backend/--add-entity-to-adjacency checklist card adjacency))))
                    (list entities adjacency)
                    it))));; at last complete checklist with item

(defun orgtrello-backend/compute-full-cards-from-trello! (cards)
  "Given a CARDS list, compute the full cards data from the trello board.
The order from the trello board is kept.
Hash result is of the form: {entity-id '(entity-card {checklist-id (checklist (item))})}"
  (--reduce-from (progn
                   (orgtrello-log/msg *OT/INFO* "Computing card '%s' data..."
                                      (orgtrello-data/entity-name it))
                   (cl-destructuring-bind (entities adjacency) acc
                     (orgtrello-backend/compute-checklist-entities-from-card! it (orgtrello-backend/--add-entity-to-entities it entities) adjacency)))
                 (list (orgtrello-hash/empty-hash) (orgtrello-hash/empty-hash))
                 cards))

(defun orgtrello-backend/--add-entity-to-entities (entity entities)
  "Adding ENTITY to the hash ENTITIES."
  (-> (orgtrello-data/entity-id-or-marker entity)
    (orgtrello-hash/puthash-data entity entities)))

(defun orgtrello-backend/--add-entity-to-adjacency (current-entity parent-entity adjacency)
  "Adding CURRENT-ENTITY to at using the parent id of PARENT-ENTITY as key in ADJACENCY map."
  (let* ((current-id (orgtrello-data/entity-id-or-marker current-entity))
         (parent-id  (orgtrello-data/entity-id-or-marker parent-entity)))
    (orgtrello-hash/puthash-data parent-id (-snoc (gethash parent-id adjacency) current-id) adjacency)))

(defun orgtrello-backend/--put-entities-with-adjacency (current-meta entities adjacency)
  "Add the current-entity from CURRENT-META to ENTITIES and ADJACENCY."
  (let ((current-entity (orgtrello-data/current current-meta))
        (parent-entity  (orgtrello-data/parent current-meta)))
    (list (orgtrello-backend/--add-entity-to-entities current-entity entities) (orgtrello-backend/--add-entity-to-adjacency current-entity parent-entity adjacency))))

(defun orgtrello-backend/compute-org-trello-card-from (trello-cards)
  "Given a TRELLO-CARDS list, compute its org-trello representation."
  (--reduce-from (progn
                   (orgtrello-log/msg *OT/INFO* "Computing card '%s' data..." (orgtrello-data/entity-name it))
                   (cl-destructuring-bind (entities adjacency) acc
                     (orgtrello-backend/compute-org-trello-checklists-from-card!
                      it
                      (orgtrello-backend/--add-entity-to-entities it entities)
                      adjacency)))
                 (list (orgtrello-hash/empty-hash) (orgtrello-hash/empty-hash))
                 trello-cards))

(defun orgtrello-backend/compute-org-trello-checklists-from-card! (trello-card entities adjacencies)
  "Given a CARD, retrieve from ENTITIES and ADJACENCY its checklists (with their items) in the right order."
  (let ((card-id (orgtrello-data/entity-id trello-card)))
    (--> trello-card
      (orgtrello-data/entity-checklists it)
      (-reduce-from (lambda (acc-entities-adj checklist)
                      (cl-destructuring-bind (ents adjs) acc-entities-adj
                        (orgtrello-backend/compute-items-from-checklist!
                         checklist
                         (orgtrello-backend/--add-entity-to-entities checklist ents)
                         (orgtrello-backend/--add-entity-to-adjacency checklist trello-card adjs))))
                    (list entities adjacencies)
                    it))))

(orgtrello-log/msg *OT/DEBUG* "orgtrello-backend loaded!")

(provide 'org-trello-backend)
;;; org-trello-backend.el ends here
