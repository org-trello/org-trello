;;; org-trello-log.el --- Log related functions.
;;; Commentary:
;;; Code:

(defconst *OT/NOLOG* 0)
(defconst *OT/ERROR* 1)
(defconst *OT/WARN*  2)
(defconst *OT/INFO*  3)
(defconst *OT/DEBUG* 4)
(defconst *OT/TRACE* 5)

(defvar *orgtrello-log/level* *OT/INFO*
  "Set log level.
Levels:
0 - no logging   (*OT/NOLOG*)
1 - log errors   (*OT/ERROR*)
2 - log warnings (*OT/WARN*)
3 - log info     (*OT/INFO*)
4 - log debug    (*OT/DEBUG*)
5 - log trace    (*OT/TRACE*)
To change such level, add this to your init.el file: (setq *orgtrello-log/level* *OT/TRACE*)") ;;(setq *orgtrello-log/level* *OT/TRACE*) (setq *orgtrello-log/level* *OT/INFO*)

(defun orgtrello-log/msg (level &rest args)
  "Log message."
  (when (<= level *orgtrello-log/level*)
    (apply 'message args)))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-log loaded!")

(provide 'org-trello-log)
;;; org-trello-log.el ends here
