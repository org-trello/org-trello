;;; orgtrello.el -- Simple mode for syncing org-mode and trello

;; Copyright (C) 2013 Antoine R. Dumont

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.1
;; Keywords: org-mode trello

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Simple mode for synching org-mode and trello

;; This mode requires oauth.el:
;; git clone git://github.com/psanford/emacs-oauth.git
;; and json.el:
;; http://edward.oconnor.cx/2006/03/json.el

;; You will need to register for an oauth key/secret at
;; https://trello.com/1/appKey/generate

;; Once you have a key and secrect, set consumer-key
;; and consumer-secret-key with those values.

;; Add the following to your emacs init file
;; (require 'org-trello)
;; (authenticate unix-user-name)

;; Useful functions:
;; list-messages
;; post-message
;; post-buffer-contents

;;; Code:

;; Personal setup

;; 1) retrieve your trello api key https://trello.com/1/appKey/generate
;; Then add those entries inside the ~/.trello/config.el:
;; ;; -*- lisp -*-
;; (defvar consumer-key "consumer-key")
;; 2) then connect to this url with your browser
;; https://trello.com/1/authorize?response_type=token&name=org-trello&scope=read,write&expiration=never&key=<consumer-key>
;; Add another entry inside the `~/.trello/config.el`
;; (defvar access-token "your-access-token")

;; Static setup

(require 'json)
(defvar app-name "org-trello")
(add-to-list 'load-path "./emacs-request")
(add-to-list 'load-path "./utils")

;; query
(require 'orgtrello-api)
(require 'orgtrello-query)
(require 'clj-thrush)

;; Now we can play around with trello from here

;; (orgtrello-http (get-boards))

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

;;(plist-get id boards)

(provide 'orgtrello)

;;; org-trello.el ends here
