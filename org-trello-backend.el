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

(defun orgtrello-backend-add-entity-to-entities (entity entities)
  "Adding ENTITY to the hash ENTITIES."
  (orgtrello-hash-puthash-data (orgtrello-data-entity-id-or-marker entity)
                               entity
                               entities))

(defun orgtrello-backend--add-entity-to-adjacency (current-entity
                                                   parent-entity
                                                   adjacency)
  "Adding CURRENT-ENTITY using parent id of PARENT-ENTITY as key in ADJACENCY."
  (let* ((current-id (orgtrello-data-entity-id-or-marker current-entity))
         (parent-id  (orgtrello-data-entity-id-or-marker parent-entity)))
    (orgtrello-hash-puthash-data
     parent-id
     (-snoc (gethash parent-id adjacency) current-id)
     adjacency)))


(defun orgtrello-backend--put-entities-with-adjacency (current-meta
                                                       entities
                                                       adjacency)
  "Add the current-entity from CURRENT-META to ENTITIES and ADJACENCY."
  (let ((entity (orgtrello-data-current current-meta))
        (parent (orgtrello-data-parent current-meta)))
    (list
     (orgtrello-backend-add-entity-to-entities entity entities)
     (orgtrello-backend--add-entity-to-adjacency entity parent adjacency))))

(defun orgtrello-backend--compute-items-from-checklist (checklist
                                                        entities
                                                        adjacencies)
  "Given a CHECKLIST, retrieve its items.
Update the ENTITIES hash and the ADJACENCIES list."
  (--> checklist
       (orgtrello-data-entity-items it)
       (sort it (lambda (a b) (when (<= (orgtrello-data-entity-position a)
                                   (orgtrello-data-entity-position b))
                           1)))
       (--reduce-from (-let (((ents adjs) acc))
                        (list
                         (orgtrello-backend-add-entity-to-entities it ents)
                         (orgtrello-backend--add-entity-to-adjacency it
                                                                     checklist
                                                                     adjs)))
                      (list entities adjacencies)
                      it)))

(defun orgtrello-backend--compute-org-trello-checklists-from-card (trello-card
                                                                   entities
                                                                   adjacencies)
  "Given a TRELLO-CARD, retrieve from ENTITIES and ADJACENCIES its checklists.
Checklists with their items in the right order."
  (--> trello-card
       (orgtrello-data-entity-checklists it)
       (sort it (lambda (a b) (when (<= (orgtrello-data-entity-position a)
                                   (orgtrello-data-entity-position b))
                           1)))
       (-reduce-from
        (lambda (acc-entities-adj checklist)
          (-let (((ents adjs) acc-entities-adj))
            (orgtrello-backend--compute-items-from-checklist
             checklist
             (orgtrello-backend-add-entity-to-entities checklist ents)
             (orgtrello-backend--add-entity-to-adjacency
              checklist
              trello-card
              adjs))))
        (list entities adjacencies)
        it)))

(defun orgtrello-backend-compute-org-trello-card-from (trello-cards)
  "Given a TRELLO-CARDS list, compute its org-trello representation."
  (--reduce-from (progn
                   (orgtrello-log-msg orgtrello-log-info
                                      "Computing card '%s' data..."
                                      (orgtrello-data-entity-name it))
                   (-let (((entities adjacency) acc))
                     (orgtrello-backend--compute-org-trello-checklists-from-card
                      it
                      (orgtrello-backend-add-entity-to-entities it entities)
                      adjacency)))
                 (list (orgtrello-hash-empty-hash) (orgtrello-hash-empty-hash))
                 trello-cards))

(orgtrello-log-msg orgtrello-log-debug "orgtrello-backend loaded!")

(provide 'org-trello-backend)
;;; org-trello-backend.el ends here
