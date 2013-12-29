

(require 'org-trello-log)
(require 'org-trello-setup)

;; #################### orgtrello-proxy installation

(defvar *ORGTRELLO-QUERY-APP-ROUTES*
  '(;; proxy to request trello
    ("^localhost//proxy/admin/entities/current/\\(.*\\)" . orgtrello-proxy/--elnode-current-entity)
    ("^localhost//proxy/admin/entities/next/\\(.*\\)" . orgtrello-proxy/--elnode-next-entities)
    ("^localhost//proxy/admin/entities/delete/\\(.*\\)" . orgtrello-proxy/--elnode-delete-entity)
    ("^localhost//proxy/admin/\\(.*\\)" . orgtrello-proxy/--elnode-admin)
    ;; proxy to request trello
    ("^localhost//proxy/trello/\\(.*\\)" . orgtrello-proxy/--elnode-proxy)
    ;; proxy producer to receive async creation request
    ("^localhost//proxy/producer/\\(.*\\)" . orgtrello-proxy/--elnode-proxy-producer)
    ;; proxy to request trello
    ("^localhost//proxy/timer/\\(.*\\)" . orgtrello-proxy/--elnode-timer)
    ;; static files
    ("^localhost//static/\\(.*\\)/\\(.*\\)" . orgtrello-proxy/--elnode-static-file))
  "Org-trello dispatch routes for the webserver")

(defun orgtrello-proxy/--proxy-handler (http-con) "Proxy handler."
  (elnode-hostpath-dispatcher http-con *ORGTRELLO-QUERY-APP-ROUTES*))

(defun orgtrello-proxy/--start (port host) "Starting the proxy."
  (orgtrello-log/msg *OT/TRACE* "Proxy-server starting...")
  (elnode-start 'orgtrello-proxy/--proxy-handler :port port :host host)
  (setq elnode--do-error-logging nil)
  (orgtrello-log/msg *OT/TRACE* "Proxy-server started!"))

(defun orgtrello-proxy/start () "Start the proxy."
  ;; update with the new port the user possibly changed
  (setq *ORGTRELLO-PROXY-URL* (format "http://%s:%d/proxy" *ORGTRELLO-PROXY-HOST* *ORGTRELLO-PROXY-PORT*))
  ;; start the proxy
  (orgtrello-proxy/--start *ORGTRELLO-PROXY-PORT* *ORGTRELLO-PROXY-HOST*)
  ;; and the timer
  (orgtrello-timer/start))

(defun orgtrello-proxy/stop () "Stopping the proxy."
  (orgtrello-log/msg *OT/TRACE* "Proxy-server stopping...")
  ;; stop the timer
  (orgtrello-timer/stop)
  ;; then stop the proxy
  (elnode-stop *ORGTRELLO-PROXY-PORT*)
  (orgtrello-log/msg *OT/TRACE* "Proxy-server stopped!"))

(defun orgtrello-proxy/reload () "Reload the proxy server."
  (interactive)
  (orgtrello-proxy/stop)
  ;; stop the default port (only useful if the user changed from the default port)
  (elnode-stop *ORGTRELLO-PROXY-DEFAULT-PORT*)
  (orgtrello-proxy/start))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-proxy-install loaded!")

(provide 'org-trello-install)
