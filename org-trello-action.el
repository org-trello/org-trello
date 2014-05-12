;;; org-trello-action.el --- Reference some action functions
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-trello-setup)
(require 'org-trello-log)

(defun orgtrello-action/reload-setup ()
  "Reload orgtrello setup."
  (org-set-regexps-and-options))

(defmacro orgtrello-action/--safe-wrap-or-throw-error (fn)
  "Macro to catch uncaught error when executing the FN call.
If error is thrown, send the 'org-trello-timer-go-to-sleep flag."
  `(condition-case ex
       (progn ,fn)
     ('error
      (orgtrello-log/msg *OT/ERROR* (concat "### org-trello - consumer ### Caught exception: [" ex "]"))
      (throw 'org-trello-timer-go-to-sleep t))))

(defmacro orgtrello-action/safe-wrap (fn &rest clean-up)
  "A macro to deal with intercept uncaught error when executing the FN call.
The CLEAN-UP body is done whether error are caught or not."
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "### org-trello ### Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))

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

(defun orgtrello-action/functional-controls-then-do (control-fns entity fn-to-execute args)
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

(defun orgtrello-action/msg-controls-or-actions-then-do (msg control-or-action-fns fn-to-execute &optional save-buffer-p reload-setup-p nolog-p)
  "A decorator fn to display some log MSG.
Then execute some CONTROL-OR-ACTION-FNS.
If all controls are ok, then execute the parameter-less FN-TO-EXECUTE.
`(Optionally)` If SAVE-BUFFER-P is set, this will safe the buffer.
If RELOAD-SETUP-P is set, this will reload org-mode's setup.
if NOLOG-P is set, this will not log anything."
  (unless nolog-p (orgtrello-log/msg *OT/INFO* (concat msg "...")))
  ;; now execute the controls and the main action
  (orgtrello-action/safe-wrap
   (orgtrello-action/controls-or-actions-then-do control-or-action-fns fn-to-execute nolog-p)
   (progn
     (when save-buffer-p  (save-buffer))
     (when reload-setup-p (orgtrello-action/reload-setup))
     (unless nolog-p (orgtrello-log/msg *OT/INFO* (concat msg " - done!"))))))

(defun orgtrello-action/delete-file! (file-to-remove)
  "Remove metadata file."
  (when (file-exists-p file-to-remove) (delete-file file-to-remove)))

(defun orgtrello-action/--too-deep-level (meta &optional parent-meta grandparent-meta)
  "Given a META and optional PARENT-META and GRANDPARENT-META, deal with too deep level."
  "Your arborescence depth is too deep. We only support up to depth 3.\nLevel 1 - card\nLevel 2 - checklist\nLevel 3 - items")

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-action loaded!")

(provide 'org-trello-action)
;;; org-trello-action.el ends here
