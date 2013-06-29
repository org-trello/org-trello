(require 'orgtrello-api)
(require 'orgtrello-query)
(require 'dash)

(setq boards-request-response (orgtrello-http (get-boards)))
(setq boards                  (request-response-data boards-request-response))

;; ELISP> (mapcar (lambda (board) (assq 'id board)) boards)
;; ((id . "50aa59502ddab2fc1100115b")
;;  (id . "4f96a984dbb00d733b04d8b5")
;;  (id . "518815ca40491e9a7b006f21")
;;  (id . "51626230b314a2252c0007eb")
;;  (id . "50ceefed35a573c76e001852")
;;  (id . "51a327c85436a5ee3e0045b4")
;;  (id . "4f2baa3072b7c1293501caea")
;;  (id . "4f2ba2054f2cb9d16d3f7f99")
;;  (id . "50bcfd2f033110476000e768")
;;  (id . "50169deffa899b184693ee76")
;;  (id . "51a326c424aa585670000180"))

;; ELISP> (->> boards
;;             (mapcar
;;              (lambda (board)
;;                (->> board
;;                     (assq 'id)
;;                     cdr))))
;; ("50aa59502ddab2fc1100115b" "4f96a984dbb00d733b04d8b5" "518815ca40491e9a7b006f21" "51626230b314a2252c0007eb" "50ceefed35a573c76e001852" "51a327c85436a5ee3e0045b4" "4f2baa3072b7c1293501caea" "4f2ba2054f2cb9d16d3f7f99" "50bcfd2f033110476000e768" "50169deffa899b184693ee76" "51a326c424aa585670000180")

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
