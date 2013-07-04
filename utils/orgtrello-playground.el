
(require 'orgtrello-api)
(require 'orgtrello-query)
(require 'dash)

(setq boards-request-response (orgtrello-query-http (orgtrello-api--get-boards)))
(setq boards                  (request-response-data boards-request-response))

;; Display (name . id) list of boards
(defun orgtrello--pgd-id-name (boards)
  (let* ((id-name (make-hash-table :test 'equal)))
    (->> boards
      (--map (puthash (assoc-default 'id it) (assoc-default 'name it) id-name)))
    id-name))

(orgtrello--pgd-id-name boards)#s

(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ("518815ca40491e9a7b006f21" "Lambda Tree: all happens here ;-)"
                                                                          "4f2baa3072b7c1293501caea" "Welcome Board"
                                                                          "50bcfd2f033110476000e768" "api test board"
                                                                          "50169deffa899b184693ee76" "hmdb-reloaded"))

[((dateLastView . "2012-05-23T08:43:54.205Z") (dateLastActivity) (labelNames (purple . "") (blue . "") (green . "") (yellow . "") (orange . "") (red . "")) (subscribed . :json-false) (shortUrl . "https://trello.com/b/ePrdEnzC") (memberships . [... ... ... ... ... ... ... ... ... ... ... ... ...]) (invitations . []) (prefs (canInvite . t) (canBePrivate . t) (canBeOrg . t) (canBePublic . t) (cardCovers . t) (selfJoin . :json-false) (invitations . "members") (comments . "members") (voting . "members") (permissionLevel . "private")) (url . "https://trello.com/board/devops/4f96a984dbb00d733b04d8b5") (pinned . t) (invited . :json-false) (idOrganization . "") ...)
 ((dateLastView . "2013-06-23T13:32:10.285Z") (dateLastActivity) (labelNames (blue . "Action") (green . "No hurry") (orange . "The sooner the better") (purple . "Aborted") (red . "ASAP") (yellow . "")) (subscribed . :json-false) (shortUrl . "https://trello.com/b/WlphVdIZ") (memberships . [... ... ... ... ...]) (invitations . []) (prefs (canInvite . t) (canBePrivate . t) (canBeOrg . t) (canBePublic . t) (cardCovers . t) (selfJoin . :json-false) (invitations . "members") (comments . "members") (voting . "members") (permissionLevel . "private")) (url . "https://trello.com/board/lambda-tree-all-happens-here/518815ca40491e9a7b006f21") (pinned . t) (invited . :json-false) (idOrganization) ...)
  ((dateLastView . "2013-06-17T14:44:41.862Z") (dateLastActivity) (labelNames (blue . "skill") (green . "env") (orange . "") (purple . "") (red . "administration") (yellow . "US")) (subscribed . :json-false) (shortUrl . "https://trello.com/b/k2UtP4Cx") (memberships . [... ... ... ... ...]) (invitations . []) (prefs (canInvite . t) (canBePrivate . t) (canBeOrg . t) (canBePublic . t) (cardCovers . t) (selfJoin . :json-false) (invitations . "members") (comments . "members") (voting . "members") (permissionLevel . "org")) (url . "https://trello.com/board/web2print/51a327c85436a5ee3e0045b4") (pinned . t) (invited . :json-false) (idOrganization . "50729572036ff018117f153b") ...)

 ((dateLastView . "2013-07-03T22:22:12.647Z") (dateLastActivity . "2013-07-03T22:19:51.840Z") (labelNames (purple . "") (blue . "") (green . "") (yellow . "") (orange . "") (red . "")) (subscribed . :json-false) (shortUrl . "https://trello.com/b/zLAUgleI") (memberships . [...]) (invitations . []) (prefs (canInvite . t) (canBePrivate . t) (canBeOrg . t) (canBePublic . t) (cardCovers . t) (selfJoin . :json-false) (invitations . "members") (comments . "members") (voting . "members") (permissionLevel . "private")) (url . "https://trello.com/board/api-test-board/50bcfd2f033110476000e768") (pinned . t) (invited . :json-false) (idOrganization) ...)
 ((dateLastView . "2012-08-06T20:20:50.435Z") (dateLastActivity) (labelNames (blue . "ops") (green . "dev") (orange . "experiment, tinker, etc...") (purple . "") (red . "bug") (yellow . "fix")) (subscribed . :json-false) (shortUrl . "https://trello.com/b/90lPrbnv") (memberships . [... ...]) (invitations . []) (prefs (canInvite . t) (canBePrivate . t) (canBeOrg . t) (canBePublic . t) (cardCovers . t) (selfJoin . :json-false) (invitations . "members") (comments . "members") (voting . "members") (permissionLevel . "org")) (url . "https://trello.com/board/hmdb-reloaded/50169deffa899b184693ee76") (pinned . t) (invited . :json-false) (idOrganization . "50169dc1bc065e9b7ceaef37") ...)
 ((dateLastView . "2013-05-27T09:45:57.234Z") (dateLastActivity) (labelNames (blue . "") (green . "") (orange . "") (purple . "") (red . "administration") (yellow . "")) (subscribed . :json-false) (shortUrl . "https://trello.com/b/5mOnjIzf") (memberships . [... ...]) (invitations . []) (prefs (canInvite . t) (canBePrivate . t) (canBeOrg . t) (canBePublic . t) (cardCovers . t) (selfJoin . :json-false) (invitations . "members") (comments . "members") (voting . "members") (permissionLevel . "private")) (url . "https://trello.com/board/web2print/51a326c424aa585670000180") (pinned . t) (invited . :json-false) (idOrganization) ...)]

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

(setq lists-response (-> (orgtrello-api--get-lists board-api-test)
                       orgtrello-query-http))
(setq lists (request-response-data lists-response))

'[((subscribed . :json-false) (pos . 1536) (idBoard . "50bcfd2f033110476000e768") (closed . :json-false) (name . "Todo") (id . "51d15c319c93af375200155f"))
  ((subscribed . :json-false) (pos . 3072) (idBoard . "50bcfd2f033110476000e768") (closed . :json-false) (name . "Doing") (id . "51d15c98741fd4673a0014b5"))
  ((subscribed . :json-false) (pos . 263168) (idBoard . "50bcfd2f033110476000e768") (closed . :json-false) (name . "Done") (id . "50bcfd2f033110476000e76b"))]

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
