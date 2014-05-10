;;; org-trello-setup.el --- Almost all constants + variables setup for org-trello namespace.
;;; Commentary:
;;; Code:

(require 'org-trello-log)

(defconst *consumer-key*                nil                                               "Id representing the user.")
(defconst *access-token*                nil                                               "Read/write access token to use trello on behalf of the user.")
(defconst *ORGTRELLO/MARKER*            "orgtrello-marker"                                "A marker used inside the org buffer to synchronize entries.")
(defconst *ORGTRELLO/CARD-LEVEL*                  1                                                 "card level")
(defconst *ORGTRELLO/CHECKLIST-LEVEL*             2                                                 "checkbox level")
(defconst *ORGTRELLO/ITEM-LEVEL*                  3                                                 "item level")
(defconst *ORGTRELLO/OUTOFBOUNDS-LEVEL*           4                                                 "Out of bounds level")
(defconst *ORGTRELLO/LEVELS*            `(,*ORGTRELLO/CARD-LEVEL* ,*ORGTRELLO/CHECKLIST-LEVEL* ,*ORGTRELLO/ITEM-LEVEL*) "Current levels 1 is card, 2 is checklist, 3 is item.")
(defconst *ORGTRELLO/ACTION-SYNC*       "sync-entity"                                     "Possible action regarding the entity synchronization.")
(defconst *ORGTRELLO/ACTION-DELETE*     "delete"                                          "Possible action regarding the entity deletion.")
(defconst *ORGTRELLO/USER-PREFIX*       "orgtrello-user-"                                 "orgtrello prefix to define user to a org-mode level.")
(defconst *ORGTRELLO/USERS-ENTRY*       "orgtrello-users"                                 "orgtrello property entry to store the users assigned to a card.")
(defconst *ORGTRELLO/USER-ME*           "orgtrello-user-me"                               "Current user's property id.")
(defconst *ORGTRELLO/USER-LOGGED-IN*    nil                                               "Current user logged in.")
(defconst *ORGTRELLO/CARD-COMMENTS*     "orgtrello-card-comments"                         "Current card's comments property.")
(defconst *ORGTRELLO/CARD-COMMENTS-DELIMITER* "###"                                       "Current card's comments delimiter.")
(defconst *ORGTRELLO/CARD-COMMENTS-DELIMITER-PRINT* "\n\n"                                "Current card's comments delimiter to print.")
(defconst *ORGTRELLO/DO-SHOW-CARD-COMMENTS-AFTER-ADDING* nil                              "Show the comment buffer after adding one comment")
(defconst *ORGTRELLO/TITLE-BUFFER-INFORMATION* "*org-trello-information*"                 "Title for the org-trello buffers that display information.")
(defconst *ORGTRELLO/DEADLINE-PREFIX*   "DEADLINE:"                                       "Deadline (org's equivalent to trello's due date property) prefix")

(defconst *ORGTRELLO/HTTPS*               "https://trello.com"                            "URL https to help in browsing")

(defconst *ORGTRELLO/ERROR-SYNC-CARD-MISSING-NAME* "Cannot synchronize the card - missing mandatory name. Skip it...")
(defconst *ORGTRELLO/ERROR-SYNC-CHECKLIST-SYNC-CARD-FIRST* "Cannot synchronize the checklist - the card must be synchronized first. Skip it...")
(defconst *ORGTRELLO/ERROR-SYNC-CHECKLIST-MISSING-NAME* "Cannot synchronize the checklist - missing mandatory name. Skip it...")
(defconst *ORGTRELLO/ERROR-SYNC-ITEM-SYNC-CARD-FIRST* "Cannot synchronize the item - the card must be synchronized first. Skip it...")
(defconst *ORGTRELLO/ERROR-SYNC-ITEM-SYNC-CHECKLIST-FIRST* "Cannot synchronize the item - the checklist must be synchronized first. Skip it...")
(defconst *ORGTRELLO/ERROR-SYNC-ITEM-MISSING-NAME* "Cannot synchronize the item - missing mandatory name. Skip it...")

;; Specific state - FIXME check if they do not already exist on org-mode to avoid potential collisions
(defconst *ORGTRELLO/TODO* "TODO" "org-mode todo state")
(defconst *ORGTRELLO/DONE* "DONE" "org-mode done state")

;; Properties key for the orgtrello headers #+PROPERTY board-id, etc...
(defconst *ORGTRELLO/BOARD-ID*   "board-id" "orgtrello property board-id entry")
(defconst *ORGTRELLO/BOARD-NAME* "board-name" "orgtrello property board-name entry")

(defvar *ORGTRELLO/LIST-NAMES*         nil "orgtrello property names of the different lists. This use the standard 'org-todo-keywords property from org-mode.")
(defvar *ORGTRELLO/HMAP-LIST-ORGKEYWORD-ID-NAME*       nil "orgtrello hash map containing for each id, the associated name (or org keyword).")
(defvar *ORGTRELLO/HMAP-USERS-ID-NAME* nil "orgtrello hash map containing for each user name, the associated id.")
(defvar *ORGTRELLO/HMAP-USERS-NAME-ID* nil "orgtrello hash map containing for each user id, the associated name.")

(defconst *ORGTRELLO/CONFIG-DIR*  (concat (getenv "HOME") "/" ".trello"))
(defconst *ORGTRELLO/CONFIG-FILE* (concat *ORGTRELLO/CONFIG-DIR* "/config.el"))

(defconst *ORGTRELLO/ID* "orgtrello-id" "Key entry used for the trello identifier and the trello marker (the first sync).")

(defconst *ORGTRELLO/BUFFER-NUMBER* "org-trello-buffer-number" "Key in the database referencing the number of org-trello buffer opened.")

(defun org-trello/compute-url (url-without-base-uri)
  "An helper method to compute the uri to trello"
  (concat *ORGTRELLO/HTTPS* url-without-base-uri))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-setup loaded!")

(provide 'org-trello-setup)
;;; org-trello-setup.el ends here
