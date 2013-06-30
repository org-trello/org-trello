;;; orgtrello.el -- Minor mode for org-mode to sync org-mode and trello

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

;; Minor mode for org-mode to sync org-mode and trello

;; 1) retrieve your trello api key https://trello.com/1/appKey/generate
;; Then add those entries inside the ~/.trello/config.el:
;; ;; -*- lisp -*-
;; (defvar consumer-key "consumer-key")
;; 2) then connect to this url with your browser
;; https://trello.com/1/authorize?response_type=token&name=org-trello&scope=read,write&expiration=never&key=<consumer-key>
;; Add another entry inside the `~/.trello/config.el`
;; (defvar access-token "your-access-token")

;; Add the following to your emacs init file
;; (require 'orgtrello)

;;; Code:

(require 'json)
(defvar app-name "org-trello")
(add-to-list 'load-path "./emacs-request")
(add-to-list 'load-path "./utils")

;; query
(require 'orgtrello-api)
(require 'orgtrello-query)
(require 'dash)

;; Now we can play around with trello from here

(defun orgtrello-minor-mode-testing ()
  (interactive)
  (message "hello from orgtrello-mode"))

;;;###autoload
(define-minor-mode orgtrello-mode "Sync your org-mode and your trello together."
  :lighter " ot" ;; the name on the modeline
  :keymap  (let ((map (make-sparse-keymap)))
             (define-key map (kbd "C-c H") 'orgtrello-minor-mode-testing)
             ;; define other bindings...

             map))
;;;###autoload

(add-hook 'org-mode-hook 'orgtrello-mode)

;;; orgtrello.el ends here
