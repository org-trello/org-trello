(require 'orgtrello-api)
(require 'orgtrello-query)
(require 'dash)

(setq boards-request-response (orgtrello-query-http (orgtrello-api--get-boards)))
(setq boards                  (request-response-data boards-request-response))

(defun boards-properties (boards property)
  (->> boards
       (--map (->> it
                (assq property)
                cdr))))

;; (-filter (lambda (board)
;;            (string= (->> board (assq 'name) cdr) "api test board")) boards)

;; (--filter (= it 2) '(1 2 3))
;; (--filter (string= it "abc") '("zref" "abc" "cdf"))

;; (setq all-ids  (boards-properties boards 'id))
;; (setq all-name (boards-properties boards 'name))

(setq board-api-test "50bcfd2f033110476000e768")

;; add list to a board

(setq list-todo-rresponse (-> (orgtrello-api--add-list "Todo" board-api-test)
                            orgtrello-query-http))
(setq list-todo (request-response-data list-todo-rresponse))

;; (assq 'id list-todo)(id . "51d15c319c93af375200155f")

(setq list-doing-rresponse (-> (orgtrello-api--add-list "Doing" board-api-test)
                             orgtrello-query-http))
(setq list-doing (request-response-data list-doing-rresponse))

;; (assq 'id list-doing)(id . "51d15c98741fd4673a0014b5")

(assoc-default 'id list-todo)
(assoc-default 'id list-doing)

;; create card to a list

(setq card1-response (-> (orgtrello-api--add-card "new card" (assoc-default 'id list-todo))
                       orgtrello-query-http))
(setq card1 (request-response-data card1-response))

;; move card from one list to another

(assoc-default 'id card1)

(setq card1-response (-> (orgtrello-api--move-card (assoc-default 'id card1) (assoc-default 'id list-doing))
                       orgtrello-query-http))
(setq card1 (request-response-data card1-response))

(setq card1-response (-> (orgtrello-api--move-card (assoc-default 'id card1) (assoc-default 'id list-todo))
                       orgtrello-query-http))
(setq card1 (request-response-data card1-response))

;; add checklist to a card
(setq checklist1-response (-> (orgtrello-api--add-checklist (assoc-default 'id card1) "first checklist")
                            orgtrello-query-http))
(setq checklist1 (request-response-data checklist1-response))

;; add task (item) to a checklist

(setq task1-response (-> (orgtrello-api--add-tasks "some superb task to do" (assoc-default 'id checklist1))
                       orgtrello-query-http))
(setq task1 (request-response-data task1-response))

;; check a task
(setq task1-response (-> (orgtrello-api--check-or-uncheck-tasks (assoc-default 'id card1)
                                                                (assoc-default 'id checklist1)
                                                                (assoc-default 'id task1)
                                                                "complete")
                       orgtrello-query-http))
(setq task1 (request-response-data task1-response))

;; uncheck the same task
(setq task1-response (-> (orgtrello-api--check-or-uncheck-tasks (assoc-default 'id card1)
                                                                (assoc-default 'id checklist1)
                                                                (assoc-default 'id task1)
                                                                "incomplete")
                       orgtrello-query-http))
(setq task1 (request-response-data task1-response))
