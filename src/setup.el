(defconst *consumer-key*                nil                                               "Id representing the user.")
(defconst *access-token*                nil                                               "Read/write access token to use trello on behalf of the user.")
(defconst *ORGTRELLO-MARKER*            "orgtrello-marker"                                "A marker used inside the org buffer to synchronize entries.")
(defconst *do-sync-query*               t                                                 "An alias to t to make the boolean more significant in the given context.")
(defconst *do-save-buffer*              t                                                 "Another alias to t to make the boolean more significant in the given context.")
(defconst *do-reload-setup*             t                                                 "Another alias to t to make the boolean more significant in the given context.")
(defconst *do-not-display-log*          t                                                 "Another alias to t to make the boolean more significant in the given context.")
(defconst *CARD-LEVEL*                  1                                                 "card level")
(defconst *CHECKLIST-LEVEL*             2                                                 "checkbox level")
(defconst *ITEM-LEVEL*                  3                                                 "item level")
(defconst *OUTOFBOUNDS-LEVEL*           4                                                 "Out of bounds level")
(defconst *ORGTRELLO-LEVELS*            `(,*CARD-LEVEL* ,*CHECKLIST-LEVEL* ,*ITEM-LEVEL*) "Current levels 1 is card, 2 is checklist, 3 is item.")
(defconst *ORGTRELLO-ACTION-SYNC*       "sync-entity"                                     "Possible action regarding the entity synchronization.")
(defconst *ORGTRELLO-ACTION-DELETE*     "delete"                                          "Possible action regarding the entity deletion.")
(defconst *ORGTRELLO-USER-PREFIX*       "orgtrello-user-"                                 "orgtrello prefix to define user to a org-mode level.")
(defconst *ORGTRELLO-USERS-ENTRY*       "orgtrello-users"                                 "orgtrello property entry to store the users assigned to a card.")
(defconst *ORGTRELLO-USER-ME*           "orgtrello-user-me"                               "Current user's property id.")
(defconst *ORGTRELLO-USER-LOGGED-IN*    nil                                               "Current user logged in.")
(defconst *ORGTRELLO-CARD-COMMENTS*     "orgtrello-card-comments"                         "Current card's comments property.")
(defconst *ORGTRELLO-CARD-COMMENTS-DELIMITER* "###"                                       "Current card's comments delimiter.")
(defconst *ORGTRELLO-CARD-COMMENTS-DELIMITER-PRINT* "\n\n"                                "Current card's comments delimiter to print.")
(defconst *ORGTRELLO-DO-SHOW-CARD-COMMENTS-AFTER-ADDING* nil                              "Show the comment buffer after adding one comment")
(defconst *ORGTRELLO-TITLE-BUFFER-INFORMATION* "*org-trello-information*"                 "Title for the org-trello buffers that display information.")

(defconst *ORGTRELLO-HTTPS*               "https://trello.com"                            "URL https to help in browsing")

(defconst *ERROR-SYNC-CARD-MISSING-NAME* "Cannot synchronize the card - missing mandatory name. Skip it...")
(defconst *ERROR-SYNC-CHECKLIST-SYNC-CARD-FIRST* "Cannot synchronize the checklist - the card must be synchronized first. Skip it...")
(defconst *ERROR-SYNC-CHECKLIST-MISSING-NAME* "Cannot synchronize the checklist - missing mandatory name. Skip it...")
(defconst *ERROR-SYNC-ITEM-SYNC-CARD-FIRST* "Cannot synchronize the item - the card must be synchronized first. Skip it...")
(defconst *ERROR-SYNC-ITEM-SYNC-CHECKLIST-FIRST* "Cannot synchronize the item - the checklist must be synchronized first. Skip it...")
(defconst *ERROR-SYNC-ITEM-MISSING-NAME* "Cannot synchronize the item - missing mandatory name. Skip it...")
(defconst *ERROR-SYNC-ITEM-SYNC-UPPER-LAYER-FIRST* "The card and the checklist must be synced before syncing the item. Skip it...")

(defun org-trello/https-trello (url-without-base-uri) "An helper method to compute the uri to trello"
  (concat *ORGTRELLO-HTTPS* url-without-base-uri))

;; #################### orgtrello-version

(defun org-trello/version () (interactive) "Version of org-trello"
  (message "org-trello version: %s" *ORGTRELLO-VERSION*))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-setup loaded!")


