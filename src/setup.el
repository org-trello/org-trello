(require 'org-trello-header)
(require 'org-trello-log)

;; #################### orgtrello-setup

(defvar *consumer-key*                nil                                               "Id representing the user.")
(defvar *access-token*                nil                                               "Read/write access token to use trello on behalf of the user.")
(defvar *ORGTRELLO-MARKER*            "orgtrello-marker"                                "A marker used inside the org buffer to synchronize entries.")
(defvar *do-sync-query*               t                                                 "An alias to t to make the boolean more significant in the given context.")
(defvar *do-save-buffer*              t                                                 "Another alias to t to make the boolean more significant in the given context.")
(defvar *do-reload-setup*             t                                                 "Another alias to t to make the boolean more significant in the given context.")
(defvar *do-not-display-log*          t                                                 "Another alias to t to make the boolean more significant in the given context.")
(defvar *CARD-LEVEL*                  1                                                 "card level")
(defvar *CHECKLIST-LEVEL*             2                                                 "checkbox level")
(defvar *ITEM-LEVEL*                  3                                                 "item level")
(defvar *OUTOFBOUNDS-LEVEL*           4                                                 "Out of bounds level")
(defvar *ORGTRELLO-LEVELS*            `(,*CARD-LEVEL* ,*CHECKLIST-LEVEL* ,*ITEM-LEVEL*) "Current levels 1 is card, 2 is checklist, 3 is item.")
(defvar *ORGTRELLO-ACTION-SYNC*       "sync-entity"                                     "Possible action regarding the entity synchronization.")
(defvar *ORGTRELLO-ACTION-DELETE*     "delete"                                          "Possible action regarding the entity deletion.")
(defvar *ORGTRELLO-USER-PREFIX*       "orgtrello-user-"                                 "orgtrello prefix to define user to a org-mode level.")
(defvar *ORGTRELLO-USERS-ENTRY*       "orgtrello-users"                                 "orgtrello property entry to store the users assigned to a card.")
(defvar *ORGTRELLO-USER-ME*           "orgtrello-user-me"                               "Current user's property id.")
(defvar *ORGTRELLO-USER-LOGGED-IN*    nil                                               "Current user logged in.")

(defvar *ORGTRELLO-HTTPS*               "https://trello.com"                            "URL https to help in browsing")
(defvar *ORGTRELLO-NATURAL-ORG-CHECKLIST* t
  "Permit the user to choose the natural org checklists over the first org-trello one (present from the start which are more basic).
   To alter this behavior, update in your init.el:
   (require 'org-trello)
   (org-trello/activate-natural-org-checkboxes)")

(defvar *ORGTRELLO-CHECKLIST-UPDATE-ITEMS* nil
  "OBSOLETE: A variable to permit the checklist's status to be pass along to its items. t, if checklist's status is DONE, the items are updated to DONE (org-mode buffer and trello board), nil only the items's status is used.
   To let the user completely choose what status he/she wants for every level, just change in your init.el file:
   (require 'org-trello)
   (setq *ORGTRELLO-CHECKLIST-UPDATE-ITEMS* nil)

If you want to use this, we recommand to use the native org checklists - http://orgmode.org/manual/Checkboxes.html.")

(defvar *ERROR-SYNC-CARD-MISSING-NAME* "Cannot synchronize the card - missing mandatory name. Skip it...")
(defvar *ERROR-SYNC-CHECKLIST-SYNC-CARD-FIRST* "Cannot synchronize the checklist - the card must be synchronized first. Skip it...")
(defvar *ERROR-SYNC-CHECKLIST-MISSING-NAME* "Cannot synchronize the checklist - missing mandatory name. Skip it...")
(defvar *ERROR-SYNC-ITEM-SYNC-CARD-FIRST* "Cannot synchronize the item - the card must be synchronized first. Skip it...")
(defvar *ERROR-SYNC-ITEM-SYNC-CHECKLIST-FIRST* "Cannot synchronize the item - the checklist must be synchronized first. Skip it...")
(defvar *ERROR-SYNC-ITEM-MISSING-NAME* "Cannot synchronize the item - missing mandatory name. Skip it...")
(defvar *ERROR-SYNC-ITEM-SYNC-UPPER-LAYER-FIRST* "The card and the checklist must be synced before syncing the item. Skip it...")

(defun org-trello/https-trello (url-without-base-uri) "An helper method to compute the uri to trello"
  (concat *ORGTRELLO-HTTPS* url-without-base-uri))

;; #################### orgtrello-version

(defun org-trello/version () (interactive) "Version of org-trello"
  (message "org-trello version: %s" *ORGTRELLO-VERSION*))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-setup loaded!")

(provide 'org-trello-setup)


