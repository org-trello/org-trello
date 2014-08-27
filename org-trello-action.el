;;; org-trello-action.el --- Reference some action functions
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-trello-setup)
(require 'org-trello-log)
(require 'dash)

(defun orgtrello-action/reload-setup! ()
  "Reload org-trello setup."
  (org-set-regexps-and-options))

(defun orgtrello-action/--execute-controls (controls-or-actions-fns &optional entity)
  "Given CONTROLS-OR-ACTIONS-FNS, execute them and return the results.
ENTITY is an optional parameter to pass to the list of functions."
  (--map (funcall it entity) controls-or-actions-fns))

(defun orgtrello-action/--filter-error-messages (control-or-actions)
  "Given CONTROL-OR-ACTIONS done, filter only the error messages.
Return nil if no error message."
  (--filter (not (equal :ok it)) control-or-actions))

(defun orgtrello-action/--compute-error-message (error-msgs)
  "Given a list of error messages ERROR-MSGS, compute them as a string."
  (apply 'concat (--map (concat "- " it "\n") error-msgs)))

(defun orgtrello-action/controls-or-actions-then-do (control-or-action-fns fn-to-execute &optional nolog-p)
  "If CONTROL-OR-ACTION-FNS is ok, execute the function FN-TO-EXECUTE.
If there are errors, display them (unless NOLOG-P is set)."
  (if control-or-action-fns
      (-if-let (error-messages (-> control-or-action-fns orgtrello-action/--execute-controls orgtrello-action/--filter-error-messages))
          (unless nolog-p
            ;; there are some trouble, we display all the error messages to help the user understand the problem
            (orgtrello-log/msg *OT/ERROR* "List of errors:\n %s" (orgtrello-action/--compute-error-message error-messages)))
        ;; ok execute the function as the controls are ok
        (funcall fn-to-execute))
    ;; no control, we simply execute the function
    (funcall fn-to-execute)))

(defun orgtrello-action/functional-controls-then-do (control-fns entity fn-to-execute &optional args)
  "If CONTROL-FNS are ok, pass ENTITY as parameter to FN-TO-EXECUTE.
ENTITY and ARGS are function parameter of FN-TO-EXECUTE.
If any errors are thrown during controls, then display them."
  (if control-fns
      (-if-let (error-messages (-> control-fns (orgtrello-action/--execute-controls entity) orgtrello-action/--filter-error-messages))
          ;; there are some trouble, we display all the error messages to help the user understand the problem
          (orgtrello-log/msg *OT/ERROR* "List of errors:\n %s" (orgtrello-action/--compute-error-message error-messages))
        ;; ok execute the function as the controls are ok
        (funcall fn-to-execute entity args))
    ;; no control, we simply execute the function
    (funcall fn-to-execute entity args)))

(defun orgtrello-action/msg-controls-or-actions-then-do (msg control-or-action-fns fn-to-execute &optional nolog-p)
  "A decorator fn to display some log MSG.
Then execute some CONTROL-OR-ACTION-FNS.
If all controls are ok, then execute the parameter-less FN-TO-EXECUTE.
`(Optionally)`
if NOLOG-P is set, this will not log anything."
  (unless nolog-p (orgtrello-log/msg *OT/INFO* (concat msg "...")))
  (orgtrello-action/controls-or-actions-then-do control-or-action-fns fn-to-execute nolog-p))

(defun orgtrello-action/--too-deep-level (entity)
  "Given an ENTITY with level too deep, display an error message about it."
  "Your arborescence depth is too deep. We only support up to depth 3.\nLevel 1 - card\nLevel 2 - checklist\nLevel 3 - items")

(orgtrello-log/msg *OT/DEBUG* "orgtrello-action loaded!")

(provide 'org-trello-action)
;;; org-trello-action.el ends here
