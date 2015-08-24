;;; org-trello-deferred.el --- Deferred computations in org-trello
;;; Commentary:
;;; Code:

(require 'dash)

(defun orgtrello-deferred--compute-deferred-computation (initial-state
                                                         fns
                                                         msg-log
                                                         &optional no-success-log)
  "Given an INITIAL-STATE, thread the FNS together.
MSG-LOG is used in case of failure.
If NO-SUCCESS-LOG is set, do not execute the success message callback."
  (let ((deferred-fns (-map  (lambda (fn) `(deferred:nextc it ,fn)) fns)))
    (if no-success-log
        `(deferred:$
           (deferred:next (lambda () ',initial-state))
           ,@deferred-fns
           (deferred:error it
             (orgtrello-controller-log-error ,msg-log "Error: %S")))
      `(deferred:$
         (deferred:next (lambda () ',initial-state))
         ,@deferred-fns
         (deferred:nextc it
           (orgtrello-controller-log-success ,msg-log))
         (deferred:error it
           (orgtrello-controller-log-error ,msg-log "Error: %S"))))))

(defun orgtrello-deferred-eval-computation (initial-state
                                            fns
                                            msg-log
                                            &optional no-success-log)
  "Given an INITIAL-STATE, thread the FNS together.
MSG-LOG is used in case of failure.
If NO-SUCCESS-LOG is set, do not execute the success message callback."
  (orgtrello-log-msg orgtrello-log-info msg-log)
  (->> (orgtrello-deferred--compute-deferred-computation initial-state
                                                         fns
                                                         msg-log
                                                         no-success-log)
       eval))

(provide 'org-trello-deferred)
;;; org-trello-deferred.el ends here
