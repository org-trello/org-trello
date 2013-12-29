

(require 'org-trello-log)
(require 'org-trello-setup)
(require 'org-trello-proxy)

;; #################### orgtrello-action

(defun trace (label e) "Decorator for some inaccessible code to easily 'message'."
  (message "TRACE: %s: %S" label e)
  e)

(defun -trace (e &optional label) "Decorator for some inaccessible code to easily 'message'."
  (progn
    (if label
        (trace label e)
        (message "TRACE: %S" e))
    e))

(defun orgtrello-action/reload-setup () "Reload orgtrello setup."
  (org-set-regexps-and-options))

(defmacro orgtrello-action/safe-wrap (fn &rest clean-up) "A macro to deal with intercept uncaught error when executing the fn call and cleaning up using the clean-up body."
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "### org-trello ### Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))

(defun org-action/--execute-controls (controls-or-actions-fns &optional entity) "Given a series of controls, execute them and return the results."
  (--map (funcall it entity) controls-or-actions-fns))

(defun org-action/--filter-error-messages (control-or-actions) "Given a list of control or actions done, filter only the error message. Return nil if no error message."
  (--filter (not (equal :ok it)) control-or-actions))

(defun org-action/--compute-error-message (error-msgs) "Given a list of error messages, compute them as a string."
  (apply 'concat (--map (concat "- " it "\n") error-msgs)))

(defun org-action/--controls-or-actions-then-do (control-or-action-fns fn-to-execute &optional nolog-p) "Execute the function fn-to-execute if control-or-action-fns is nil or display the error message if problems."
  (if control-or-action-fns
      (let ((org-trello/--error-messages (-> control-or-action-fns org-action/--execute-controls org-action/--filter-error-messages)))
        (if org-trello/--error-messages
            (unless nolog-p
                    ;; there are some trouble, we display all the error messages to help the user understand the problem
                    (orgtrello-log/msg *OT/ERROR* "List of errors:\n %s" (org-action/--compute-error-message org-trello/--error-messages)))
            ;; ok execute the function as the controls are ok
            (funcall fn-to-execute)))
      ;; no control, we simply execute the function
      (funcall fn-to-execute)))

(defun org-action/--functional-controls-then-do (control-fns entity fn-to-execute args) "Execute the function fn if control-fns is nil or if the result of apply every function to fn-to-execute is ok."
  (if control-fns
      (let ((org-trello/--error-messages (-> control-fns (org-action/--execute-controls entity) org-action/--filter-error-messages)))
        (if org-trello/--error-messages
            ;; there are some trouble, we display all the error messages to help the user understand the problem
            (orgtrello-log/msg *OT/ERROR* "List of errors:\n %s" (org-action/--compute-error-message org-trello/--error-messages))
            ;; ok execute the function as the controls are ok
            (funcall fn-to-execute entity args)))
      ;; no control, we simply execute the function
      (funcall fn-to-execute entity args)))

(defun org-action/--msg-controls-or-actions-then-do (msg control-or-action-fns fn-to-execute &optional save-buffer-p reload-setup-p nolog-p) "A decorator fn to execute some action before/after the controls."
  (unless nolog-p (orgtrello-log/msg *OT/INFO* (concat msg "...")))
  ;; now execute the controls and the main action
  (orgtrello-action/safe-wrap
   (org-action/--controls-or-actions-then-do control-or-action-fns fn-to-execute nolog-p)
   (progn
     (when save-buffer-p  (save-buffer))
     (when reload-setup-p (orgtrello-action/reload-setup))
     (unless nolog-p (orgtrello-log/msg *OT/INFO* (concat msg " - done!"))))))

(defun org-action/--deal-with-consumer-msg-controls-or-actions-then-do (msg control-or-action-fns fn-to-execute &optional save-buffer-p reload-setup-p nolog-p) "Decorator fn to execute actions before/after the controls."
  ;; stop the timer
  (orgtrello-proxy/timer-stop)
  ;; Execute as usual
  (org-action/--msg-controls-or-actions-then-do msg control-or-action-fns fn-to-execute save-buffer-p reload-setup-p nolog-p)
  ;; start the timer
  (orgtrello-proxy/timer-start))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-action loaded!")

(provide 'org-trello-action)
