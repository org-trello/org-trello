(require 'orgtrello-api)
(require 'orgtrello-query)
(require 'dash)

(setq boards-request-response (orgtrello-http (get-boards)))
(setq boards                  (request-response-data boards-request-response))

(defun boards-properties (boards property)
  (->> boards
       (--map (->> it
                (assq property)
                cdr))))

(-filter (lambda (board)
           (string= (->> board (assq 'name) cdr) "api test board")) boards)

;; (--filter (= it 2) '(1 2 3))
;; (--filter (string= it "abc") '("zref" "abc" "cdf"))

;; (setq all-ids  (boards-properties boards 'id))
;; (setq all-name (boards-properties boards 'name))

(setq board-api-test "50bcfd2f033110476000e768")

;; add list to a board

(setq list-todo-rresponse (-> (add-list "Todo" board-api-test)
                            orgtrello-http))
(setq list-todo (request-response-data list-todo-rresponse))

(setq list-doing-rresponse (-> (add-list "Doing" board-api-test)
                             orgtrello-http))
(setq list-doing (request-response-data list-doing-rresponse))

(assoc-default 'id list-todo)
(assoc-default 'id list-doing)

;; create card to a list

(setq card1-response (-> (add-card "new card" (assoc-default 'id list-todo))
                       orgtrello-http))
(setq card1 (request-response-data card1-response))

;; move card from one list to another

(assoc-default 'id card1)

(setq card1-response (-> (move-card (assoc-default 'id card1) (assoc-default 'id list-doing))
                       orgtrello-http))
(setq card1 (request-response-data card1-response))

(setq card1-response (-> (move-card (assoc-default 'id card1) (assoc-default 'id list-todo))
                       orgtrello-http))
(setq card1 (request-response-data card1-response))

;; add checklist to a card
(setq checklist1-response (-> (add-checklist (assoc-default 'id card1) "first checklist")
                            orgtrello-http))
(setq checklist1 (request-response-data checklist1-response))

;; add task (item) to a checklist

(setq task1-response (-> (add-tasks "some superb task to do" (assoc-default 'id checklist1))
                       orgtrello-http))
(setq task1 (request-response-data task1-response))

;; check a task
(setq task1-response (-> (check-or-uncheck-tasks (assoc-default 'id card1)
                                                 (assoc-default 'id checklist1)
                                                 (assoc-default 'id task1)
                                                 "complete")
                         orgtrello-http))
(setq task1 (request-response-data task1-response))

;; uncheck the same task
(setq task1-response (-> (check-or-uncheck-tasks (assoc-default 'id card1)
                                                 (assoc-default 'id checklist1)
                                                 (assoc-default 'id task1)
                                                 "incomplete")
                         orgtrello-http))
(setq task1 (request-response-data task1-response))
