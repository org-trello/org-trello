Minor mode for org-mode to sync org-mode and trello

1) Add the following to your emacs init file
(require 'org-trello)

Automatically
2) Install the consumer-key and the read-write token for org-trello to be able to work in your name with your trello boards
M-x orgtrello-do-install-keys-and-token

3) For each org-mode file, you want to connect your org-mode file with a trello board
M-x orgtrello-do-install-board-and-lists

Manually
2) retrieve your trello api key https://trello.com/1/appKey/generate
Then add those entries inside the ~/.trello/config.el:
(defvar *consumer-key* "consumer-key")

3) then connect to this url with your browser
https://trello.com/1/authorize?response_type=token&name=org-trello&scope=read,write&expiration=never&key=<consumer-key>
Add another entry inside the `~/.trello/config.el`
(defvar *access-token* "your-access-token")

4) You need to make your org-mode buffer aware of trello.

Add this to the top of your org-mode file

#+property: board-id      <BOARD-ID>
#+property: todo-list-id  <TODO-LIST-ID>
#+property: doing-list-id <DOING-LIST-ID>
#+property: done-list-id  <DONE-LIST-ID>
