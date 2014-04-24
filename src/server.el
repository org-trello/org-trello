(defvar *ORGTRELLO-SERVER/DB*   nil         "Database reference to orgtrello-proxy")
(defvar *ORGTRELLO/SERVER-HOST* "localhost" "Proxy host")
(defvar *ORGTRELLO/SERVER-PORT* nil         "Proxy port")
(defvar *ORGTRELLO/SERVER-URL*  nil         "Proxy url")

(defvar *ORGTRELLO/SERVER-DEFAULT-PORT* 9876 "Default proxy port") (setq *ORGTRELLO/SERVER-PORT* *ORGTRELLO/SERVER-DEFAULT-PORT*)

(defun orgtrello-server/--proxy-handler (http-con)
  "Proxy handler."
  (elnode-hostpath-dispatcher http-con (append *ORGTRELLO/QUERY-APP-ROUTES-WEBADMIN* *ORGTRELLO/QUERY-APP-ROUTES-PROXY*)))

(defun orgtrello-server/--start (port host)
  "Starting the proxy."
  (orgtrello-log/msg *OT/TRACE* "Proxy-server starting...")
  (elnode-start 'orgtrello-server/--proxy-handler :port port :host host)
  (setq elnode--do-error-logging nil)
  (orgtrello-log/msg *OT/TRACE* "Proxy-server started!"))

(defun orgtrello-server/start ()
  "Start the proxy."
  ;; update with the new port the user possibly changed
  (setq *ORGTRELLO/SERVER-URL* (format "http://%s:%d/proxy" *ORGTRELLO/SERVER-HOST* *ORGTRELLO/SERVER-PORT*))
  ;; initialize the database
  (setq *ORGTRELLO-SERVER/DB* (if *ORGTRELLO-SERVER/DB* *ORGTRELLO-SERVER/DB* (orgtrello-db/init)))
  ;; start the proxy
  (orgtrello-server/--start *ORGTRELLO/SERVER-PORT* *ORGTRELLO/SERVER-HOST*)
  ;; and the timer
  (orgtrello-proxy/timer-start))

(defun orgtrello-server/stop ()
  "Stopping the proxy."
  (orgtrello-log/msg *OT/TRACE* "Proxy-server stopping...")
  ;; flush the database to disk
  (orgtrello-db/save! *ORGTRELLO-SERVER/DB*)
  ;; stop the timer
  (orgtrello-proxy/timer-stop)
  ;; then stop the proxy
  (elnode-stop *ORGTRELLO/SERVER-PORT*)
  (orgtrello-log/msg *OT/TRACE* "Proxy-server stopped!"))

(defun orgtrello-server/reload ()
  "Reload the proxy server."
  (orgtrello-server/stop)
  ;; stop the default port (only useful if the user changed from the default port)
  (elnode-stop *ORGTRELLO/SERVER-DEFAULT-PORT*)
  (orgtrello-server/start))

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-server loaded!")


