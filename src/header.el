;;; org-trello.el --- Minor mode to synchronize org-mode buffer and trello board

;; Copyright (C) 2013 Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.4.2
;; Package-Requires: ((dash "2.5.0") (request "0.2.0") (elnode "0.9.9.7.6") (esxml "0.3.0") (s "1.7.0") (db "0.0.6"))
;; Keywords: org-mode trello sync org-trello
;; URL: https://github.com/org-trello/org-trello

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

;; Minor mode to sync org-mode buffer and trello board
;;
;; 1) Add the following to your emacs init file
;; (require 'org-trello)
;; (add-hook 'org-mode-hook 'org-trello-mode)
;;
;; 2) Once - Install the consumer-key and the read-write token for org-trello to be able to work in your name with your trello boards (C-c o i)
;; M-x org-trello/install-key-and-token
;;
;; You may want:
;; - to connect your org buffer to an existing board (C-c o I). Beware that this will only install properties needed to speak with trello board (nothing else).
;; M-x org-trello/install-board-and-lists-ids
;;
;; - to create an empty board directly from a org-mode buffer (C-c o b)
;; M-x org-trello/create-board
;;
;; 3) Now check your setup is ok (C-c o d)
;; M-x org-trello/check-setup
;;
;; 6) For some more help (C-c o h)
;; M-x org-trello/help-describing-setup
;;
;; 7) If you attached to an existing trello board, you may want to bootstrap your org-buffer (C-u C-c o s)
;; C-u M-x org-trello/sync-buffer
;;
;; Now you can work with trello from the comfort of org-mode and emacs
;; 8) Sync an entity from org to trello (C-c o c)
;; M-x org-trello/sync-entity
;;
;; 9) Sync an entity and its structure from org to trello (C-c o C)
;; M-x org-trello/sync-full-entity
;;
;; 10) Sync an entity from trello to org (C-u C-c o c)
;; C-u M-x org-trello/sync-entity
;;
;; 11) Sync an entity and its structure from trello to org (C-u C-c o C)
;; C-u M-x org-trello/sync-full-entity
;;
;; 12) Sync all the org buffer to trello (C-c o s)
;; M-x org-trello/sync-buffer
;;
;; 13) As already mentionned, you can sync all the org buffer from trello (C-u C-c o s)
;; C-u M-x org-trello/sync-buffer
;;
;; Enjoy!
;;
;; More informations on https://org-trello.github.io/org-trello

;;; Code:


(defconst *ORGTRELLO/ERROR-INSTALL-MSG* (format "Oops - your emacs isn't supported. org-trello only works on Emacs 24.3+ and you're running version: %s.
Please consider upgrading Emacs." emacs-version) "Error message when installing org-trello with an unsupported emacs version.")

(when (version< emacs-version "24") (error *ORGTRELLO/ERROR-INSTALL-MSG*))

;; Dependency on internal Emacs libs
(require 'org)
(require 'json)
(require 'parse-time)
(require 'timer)
(require 'align)

;; Dependency on external Emacs libs
(require 'dash)
(require 'request)
(require 'elnode)
(require 's)
(require 'esxml)
(require 'db)

(if (version< "24.3" emacs-version)
    (require 'cl-lib)
  (progn ;; need to alias the call
    (require 'cl)
    (defalias 'cl-defun 'defun*)
    (defalias 'cl-destructuring-bind 'destructuring-bind)))

(defconst *ORGTRELLO/VERSION* "0.4.2" "current org-trello version installed.")


