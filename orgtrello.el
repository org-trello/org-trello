;;; org-trello.el -- Simple mode for syncing org-mode and trello

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
;; (defvar consumer-secret-key "consumer-secret-key"})
;; 2) then connect to this url with your browser
;; https://trello.com/1/authorize?response_type=token&name=org-trello&scope=read,write&expiration=never&key=<consumer-key>
;; Add another entry inside the ~/.trello/config.el

;; Static setup

(require 'json)
(defvar app-name "org-trello")
(add-to-list 'load-path "./emacs-request")
(require 'request)

;; Load the setup from this file
(load "/home/tony/.trello/config.el")

;; trello url
(defvar URL "https://api.trello.com/1" "The needed prefix url for trello")

(defun api-query (method path)
  (request
   (format "%s%s" URL path)
   :params `((key . ,consumer-key)
             (token . ,secret-token))
   :type method
   ;; :params nil
   :parser 'json-read
   :success (function*
             (lambda (&key data &allow-other-keys)
               (message "%S"  data)))))

(api-query "GET" "/member/me/boards")

(defvar tmp nil)
(setq tmp
      (request
       (compute-url URL "/member/me/boards")
       ;; :params nil
       :type "GET"
       :parser 'json-read
       :success (function*
                 (lambda (&key data &allow-other-keys)
                   (message "%S" data)))))

;; (request
;;    "http://localhost:3000"
;;    :params '((q . "emacs awesome"))
;;    :parser 'json-read
;;    :success (function*
;;              (lambda (&key data &allow-other-keys)
;;                (let* ((tweet (elt (assoc-default 'results data) 0))
;;                       (text (assoc-default 'text tweet))
;;                       (user (assoc-default 'from_user_name tweet)))
;;                  (message "%s says %s" user text)))))

;; Now we can play around with trello from here

(provide 'org-trello)

;;; org-trello.el ends here
