;;; org-trello-deferred.el --- Deferred computations in org-trello
;;; Commentary:
;;; Code:

(require 'dash)

(defun orgtrello-deferred--compute-deferred-computation (initial-state fns log)
  "Given an INITIAL-STATE, thread the FNS together.
LOG is used in case of failure."
  (let ((deferred-fns (-map  (lambda (fn) `(deferred:nextc it ,fn)) fns)))
    `(deferred:$
       (deferred:next (lambda () ',initial-state))
       ,@deferred-fns
       (deferred:error it
         (orgtrello-controller-log-error ,log "Error: %S")))))

(defun orgtrello-deferred-eval-computation (initial-state fns log)
  "Given an INITIAL-STATE, thread the FNS together.
LOG is used in case of failure."
  (->> (orgtrello-deferred--compute-deferred-computation initial-state fns log)
       eval))

(provide 'org-trello-deferred)
;;; org-trello-deferred.el ends here
