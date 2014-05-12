;;; org-trello-webadmin.el --- Web admin front namespace.
;;; Commentary:
;;; Code:

(require 'elnode)
(require 'esxml)
(require 'org-trello-log)
(require 'org-trello-setup)
(require 'org-trello-data)
(require 'org-trello-hash)
(require 'org-trello-db)

(defun orgtrello-webadmin/--compute-root-static-files ()
  "Root files under which css and js files are installed."
  (format "%s%s" elnode-webserver-docroot "org-trello/bootstrap"))

(defun orgtrello-webadmin/--installation-needed-p ()
  "Determine if the installation is needed."
  (let ((dir (orgtrello-webadmin/--compute-root-static-files)))
    (not (and (file-exists-p dir)
              (< 3 (-> dir
                     directory-files
                     length)))))) ;; . and .. are returned by default

(defvar *ORGTRELLO/FILES* (->> (orgtrello-hash/empty-hash)
                            (orgtrello-hash/puthash-data :bootstrap `("http://getbootstrap.com/2.3.2/assets/bootstrap.zip" "/tmp/bootstrap.zip" ,(orgtrello-webadmin/--compute-root-static-files)))
                            (orgtrello-hash/puthash-data :jquery    `("http://code.jquery.com/jquery-2.0.3.min.js"         "/tmp/jquery.js"     ,(format "%s/js" (orgtrello-webadmin/--compute-root-static-files)))))
  "List of needed files for the webadmin rendering.")

(defun orgtrello-webadmin/--unzip-and-install (file dest)
  "Unzip and install FILE in DEST folder.
Beware, there is a system dependency on unzip."
  (shell-command (format "unzip -o %s -d %s" file dest)))

(defun orgtrello-webadmin/--install-file (file file-dest)
  "Install the FILE from temporary location to the FILE-DEST folder."
  (when (file-exists-p file)
    (rename-file file file-dest t)))

(defun orgtrello-webadmin/--download-and-install-file (key-file)
  "Download the FILE represented by the parameter KEY-FILE.
Also, if the archive downloaded is a zip, unzip it."
  (let* ((url-tmp-dest (gethash key-file *ORGTRELLO/FILES*))
         (url          (car  url-tmp-dest))
         (tmp-dest     (cadr url-tmp-dest))
         (final-dest   (cadr (cdr url-tmp-dest)))
         (extension    (file-name-extension url)))
    ;; download the file
    (url-copy-file url tmp-dest t)
    (if (equal "zip" extension)
        (orgtrello-webadmin/--unzip-and-install tmp-dest (file-name-directory final-dest))
      (orgtrello-webadmin/--install-file tmp-dest final-dest))))

(defun orgtrello-webadmin/--install-css-js-files-once ()
  "Install bootstrap and jquery if need be."
  (when (orgtrello-webadmin/--installation-needed-p)
    (mapc (lambda (key-file) (orgtrello-webadmin/--download-and-install-file key-file)) '(:bootstrap :jquery))))

(defun orgtrello-webadmin/--render-html (data)
  "Render the DATA in html."
  (esxml-to-xml data))

(defun orgtrello-webadmin/html (project-name author-name description)
  "Main html page using PROJECT-NAME, AUTHOR-NAME and DESCRIPTION as meta."
  `(html
    ()
    ,(orgtrello-webadmin/head project-name author-name description)
    ,(orgtrello-webadmin/body project-name)))

(defun orgtrello-webadmin/head (project-name author-name description)
  "Generate html header using PROJECT-NAME, AUTHOR-NAME and DESCRIPTION."
  `(head ()
         (meta ((charset . "utf-8")))
         (title () ,project-name)
         (meta ((name . "viewport")
                (content . "width=device-width, initial-scale=1.0")))
         (meta ((name . "author")
                (content . ,author-name)))
         (meta ((name . "description")
                (content . ,description)))
         (style ()
                "
      body {
        padding-top: 20px;
        padding-bottom: 40px;
      }

      /* Custom container */
      .container-narrow {
        margin: 0 auto;
        max-width: 700px;
      }
      .container-narrow > hr {
        margin: 30px 0;
      }

      /* Main marketing message and sign up button */
      .jumbotron {
        margin: 60px 0;
        text-align: center;
      }
      .jumbotron h1 {
        font-size: 72px;
        line-height: 1;
      }
      .jumbotron .btn {
        font-size: 21px;
        padding: 14px 24px;
      }

      /* Supporting marketing content */
      .marketing {
        margin: 60px 0;
      }
      .marketing p + h4 {
        margin-top: 28px;
      }")
         (link ((href . "/static/css/bootstrap.css")
                (rel . "stylesheet")))
         (link ((href . "/static/css/bootstrap-responsive.min.css")
                (rel . "stylesheet")))
         "
    <!-- HTML5 shim, for IE6-8 support of HTML5 elements -->
    <!--[if lt IE 9]>
      <script src=\"http://html5shim.googlecode.com/svn/trunk/html5.js\"></script>
    <![endif]-->
"))

(defun orgtrello-webadmin/--main-body ()
  "Build the main body where we will display informations (without all the html boilerplate)."
  `(div ((class . "row-fluid marketing"))
        (div ((class . "span6"))
             (div ((style . "font-size: 2em;margin-right: 10px;margin-bottom: 10px")) "Current action")
             (span ((id . "current-action"))))
        (div ((class . "span6"))
             (div ((style . "margin-bottom:10px"))
                  (span ((style . "font-size: 2em;margin-right: 10px")) "Next actions")
                  (span () ,(orgtrello-webadmin/--input-button-html "deleteEntities('/proxy/admin/entities/delete/');" "Delete all")))
             (span ((id . "next-actions"))))))

(defun orgtrello-webadmin/body (project-name)
  "Display the data inside the html body using PROJECT-NAME."
  `(body
    ()
    (div ((class . "navbar navbar-inverse navbar-fixed-top"))
         (div ((class . "navbar-inner"))
              (div ((class . "container"))
                   (button ((type . "button")
                            (class . "btn btn-navbar")
                            (data-toggle . "collapse")
                            (data-target . "nav-collapse"))
                           (span ((class . "icon-bar")))
                           (span ((class . "icon-bar")))
                           (span ((class . "icon-bar"))))
                   (a ((class . "brand")
                       (href . "#"))
                      ,project-name)
                   (div ((class . "nav-collapse collapse"))
                        (ul ((class . "nav"))
                            (li ((class . "active"))
                                (a ((href . "#"))
                                   "Home"))
                            (li ((class . "active"))
                                (a ((href . "#about"))
                                   "About"))
                            (li ((class . "active"))
                                (a ((href . "#contact"))
                                   "Contact")))))))
    (div ((class . "container"))
         (div ((class . "container-narrow"))
              ,(orgtrello-webadmin/--main-body)))
    (script ((src . "/static/js/bootstrap.min.js")) "")
    (script ((src . "/static/js/jquery.js")) "")
    (script ()
            "
function refresh (url, id) {
    $.ajax({
        url: url
    }).done(function (data) {
        $(id).html(data);
        setTimeout(function() { refresh(url, id); }, 500);
    });
}

function deleteEntities(url) {
    $.ajax({
        url:  url
    }).done(function (data) {

    });
}

refresh(\"/proxy/admin/entities/next/\", '#next-actions');
refresh(\"/proxy/admin/entities/current/\", '#current-action');
")))

(defun orgtrello-webadmin/--header-table ()
  "Generate table headers."
  `(tr () (td ()) (td () "Action") (td () "Entity") (td () "Delete")))

(defun orgtrello-webadmin/--detail-entity (log-level entity-data)
  "Depending on the LOG-LEVEL, will display either the full ENTITY-DATA or simply its name."
  (if (= log-level *OT/INFO*) (orgtrello-data/entity-name entity-data) entity-data))

(defun orgtrello-webadmin/--input-button-html (action value)
  "Given a javascript ACTION and a VALUE, compute an html input button."
  `(input ((class . "btn btn-danger btn-mini")
           (type . "button")
           (onclick . ,action)
           (value . ,value))))

(defun orgtrello-webadmin/--delete-action (entity)
  "Generate the button to permit the delete action of an ENTITY."
  (-if-let (entity-id (orgtrello-data/entity-id-or-marker entity))
      (orgtrello-webadmin/--input-button-html (format "deleteEntities('/proxy/admin/entities/delete/%s');" entity-id) "x")
    ""))

(defun orgtrello-webadmin/--compute-class (tr-class)
  "Compute the TR-CLASS css attribute."
  `(class . ,(cond ((string= tr-class "icon-play")  "success")
                   ((string= tr-class "icon-pause") "warning")
                   (t                               ""))))

(defun orgtrello-webadmin/--entity (entity icon &optional tr-class)
  "Compute the ENTITY file with class ICON dispay rendering.
TR-CLASS is not used, present due to an implementation detail."
  `(tr
    (,(orgtrello-webadmin/--compute-class icon))
    (td () (i ((class . ,icon))))
    (td () ,(orgtrello-data/entity-action entity))
    (td () ,(format "%s" (orgtrello-webadmin/--detail-entity *orgtrello-log/level* entity)))
    (td () ,(orgtrello-webadmin/--delete-action entity))))

(defun orgtrello-webadmin/entities-as-html (entities icon-array-nxt)
  "Given a list of ENTITIES and ICON-ARRAY-NXT, return as html data."
  (--map (orgtrello-webadmin/--entity it icon-array-nxt) entities))

(defun orgtrello-webadmin/--entities-as-html (entities &optional icon-array-running icon-array-next)
  "Given a list of ENTITIES and optional ICON-ARRAY-RUNNING, ICON-ARRAY-NEXT, return the html data structure files to send to trello."
  (let ((icon-array-run (if icon-array-running icon-array-running "icon-arrow-right"))
        (icon-array-nxt (if icon-array-next icon-array-next "icon-arrow-up")))
    (if entities
        `(table ((class . "table table-striped table-bordered table-hover")
                 (style . "font-size: 0.75em"))
                ;; header
                ,(orgtrello-webadmin/--header-table)
                ;; first next running action
                ,(orgtrello-webadmin/--entity (car entities) icon-array-run)
                ;; next running actions
                ,@(orgtrello-webadmin/entities-as-html (cdr entities) icon-array-nxt))
      "None")))

(defun orgtrello-webadmin/--response-html (data http-con)
  "A response wrapper using DATA as response body and HTTP-CON as http connexion."
  (elnode-http-start http-con 201 '("Content-type" . "text/html"))
  (elnode-http-return http-con (orgtrello-webadmin/--render-html data)))

(defun orgtrello-webadmin/--elnode-admin (http-con)
  "A basic display of data to answer to HTTP-CON."
  (-> (orgtrello-webadmin/html "org-trello/proxy-admin" "Commiters" "Administer the running queries to Trello")
    (orgtrello-webadmin/--response-html http-con)))

(defun orgtrello-webadmin/keys (levels &optional with-archive-flag)
  "Compute the keys of the database to look depending on the entities LEVELS.
Optional WITH-ARCHIVE-FLAG will add the archived keys too."
  (concatenate 'list levels (when with-archive-flag (mapcar 'orgtrello-proxy/archive-key levels))))

(defun orgtrello-webadmin/entities (levels &optional with-archive-flag)
  "Depending on the LEVELS and optional WITH-ARCHIVE-FLAG, compute the actions running (sync, delete)."
  (->> levels
    orgtrello-webadmin/keys
    (--mapcat (orgtrello-db/get it *ORGTRELLO-SERVER/DB*))))

(defun orgtrello-webadmin/elnode-current-entity (http-con)
  "A basic display of the current scanned entity to answer on HTTP-CON."
  (-if-let (current-entity (-> *ORGTRELLO/LEVELS* orgtrello-webadmin/entities car))
      (-> (list current-entity)
        (orgtrello-webadmin/--entities-as-html "icon-play" "icon-pause")
        (orgtrello-webadmin/--response-html http-con))
    (-> nil
      (orgtrello-webadmin/--entities-as-html "icon-play" "icon-pause")
      (orgtrello-webadmin/--response-html http-con))))

(defun orgtrello-webadmin/elnode-next-entities (http-con)
  "A basic display of the list of the next entities to scan to answer as HTTP-CON."
  (-> *ORGTRELLO/LEVELS*
    orgtrello-webadmin/entities
    cdr
    orgtrello-webadmin/--entities-as-html
    (orgtrello-webadmin/--response-html http-con)))

(defun orgtrello-webadmin/elnode-static-file (http-con)
  "Serve static files if they exist to HTTP-CON.
Throw 404 if it does not exists.
Also, install bootstrap and jquery the first time round."
  ;; the first request will ask for installing bootstrap and jquery
  (orgtrello-webadmin/--install-css-js-files-once)
  (let ((full-file (format "%s/%s/%s" (orgtrello-webadmin/--compute-root-static-files) (elnode-http-mapping http-con 1) (elnode-http-mapping http-con 2))))
    (if (file-exists-p full-file)
        (elnode-send-file http-con full-file)
      (elnode-send-404 http-con (format "Resource file '%s' not found!" full-file)))))

(defun orgtrello-webadmin/--delete-entity-with-id (id)
  "Remove the entity/file which match the id ID."
  (orgtrello-db/clear-entity-with-id (orgtrello-webadmin/keys *ORGTRELLO/LEVELS* 'with-archived-entities) id *ORGTRELLO-SERVER/DB*))

(defun orgtrello-webadmin/delete-entities! ()
  "Remove the entities/files."
  (orgtrello-db/clear-keys (orgtrello-webadmin/keys *ORGTRELLO/LEVELS* 'with-archived-entities) *ORGTRELLO-SERVER/DB*))

(defun orgtrello-webadmin/elnode-delete-entity (http-con)
  "Will read the entity id from the HTTP-CON, delete the action on such entity.
If no id is specified, all actions are deleted."
  (let ((id (elnode-http-mapping http-con 1)))
    (if (string= "" id) (orgtrello-webadmin/delete-entities!) (orgtrello-webadmin/--delete-entity-with-id id))))

(defvar *ORGTRELLO/QUERY-APP-ROUTES-WEBADMIN*
  '(("^localhost//proxy/admin/entities/current/\\(.*\\)"  . orgtrello-webadmin/elnode-current-entity)
    ("^localhost//proxy/admin/entities/next/\\(.*\\)"     . orgtrello-webadmin/elnode-next-entities)
    ("^localhost//proxy/admin/entities/delete/\\(.*\\)"   . orgtrello-webadmin/elnode-delete-entity)
    ("^localhost//proxy/admin/\\(.*\\)"                   . orgtrello-webadmin/--elnode-admin)
    ("^localhost//static/\\(.*\\)/\\(.*\\)"               . orgtrello-webadmin/elnode-static-file))
  "Webadmin routes.")

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-webadmin loaded!")

(provide 'org-trello-webadmin)
;;; org-trello-webadmin.el ends here
