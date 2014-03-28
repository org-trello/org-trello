(defun orgtrello-webadmin/--compute-root-static-files () "Root files under which css and js files are installed."
  (format "%s%s" elnode-webserver-docroot "org-trello/bootstrap"))

(defun orgtrello-webadmin/--installation-needed-p () "Determine if the installation is needed."
  (let ((dir (orgtrello-webadmin/--compute-root-static-files)))
    (not (and (file-exists-p dir)
              (< 3 (-> dir
                       directory-files
                       length)))))) ;; . and .. are returned by default

(defvar *ORGTRELLO-FILES* (let ((tmp (orgtrello-hash/empty-hash)))
                            ;;                    url                                                  temp file            install destination
                            (puthash :bootstrap `("http://getbootstrap.com/2.3.2/assets/bootstrap.zip" "/tmp/bootstrap.zip" ,(orgtrello-webadmin/--compute-root-static-files)) tmp)
                            (puthash :jquery    `("http://code.jquery.com/jquery-2.0.3.min.js"         "/tmp/jquery.js"     ,(format "%s/js" (orgtrello-webadmin/--compute-root-static-files))) tmp)
                            tmp))

(defun orgtrello-webadmin/--unzip-and-install (file dest) "Execute the unarchive command. Dependency on unzip on the system."
  (shell-command (format "unzip -o %s -d %s" file dest)))

(defun orgtrello-webadmin/--install-file (file file-dest) "Install the file from temporary location to the final destination."
  (when (file-exists-p file)
        (rename-file file file-dest t)))

(defun orgtrello-webadmin/--download-and-install-file (key-file) "Download the file represented by the parameter. Also, if the archive downloaded is a zip, unzip it."
  (let* ((url-tmp-dest (gethash key-file *ORGTRELLO-FILES*))
         (url          (first  url-tmp-dest))
         (tmp-dest     (second url-tmp-dest))
         (final-dest   (third  url-tmp-dest))
         (extension    (file-name-extension url)))
    ;; download the file
    (url-copy-file url tmp-dest t)
    (if (equal "zip" extension)
        (orgtrello-webadmin/--unzip-and-install tmp-dest (file-name-directory final-dest))
        (orgtrello-webadmin/--install-file tmp-dest final-dest))))

(defun orgtrello-webadmin/--install-css-js-files-once () "Install bootstrap and jquery if need be."
  (when (orgtrello-webadmin/--installation-needed-p)
        (mapc (lambda (key-file) (orgtrello-webadmin/--download-and-install-file key-file)) '(:bootstrap :jquery))))

(defun orgtrello-webadmin/--render-html (data) "Render the data in html."
  (esxml-to-xml data))

(defun orgtrello-webadmin/html (project-name author-name description) "Main html page"
  `(html
    ()
    ,(orgtrello-webadmin/head project-name author-name description)
    ,(orgtrello-webadmin/body project-name)))

(defun orgtrello-webadmin/head (project-name author-name description) "Generate html <head>"
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

(defun orgtrello-webadmin/--main-body () "Build the main body where we will display informations (without all the html boilerplate)."
  `(div ((class . "row-fluid marketing"))
        (div ((class . "span6"))
             (div ((style . "font-size: 2em;margin-right: 10px;margin-bottom: 10px")) "Current action")
             (span ((id . "current-action"))))
        (div ((class . "span6"))
             (div ((style . "margin-bottom:10px"))
                  (span ((style . "font-size: 2em;margin-right: 10px")) "Next actions")
                  (span () ,(orgtrello-webadmin/--input-button-html "deleteEntities('/proxy/admin/entities/delete/');" "Delete all")))
             (span ((id . "next-actions"))))))

(defun orgtrello-webadmin/body (project-name) "Display the data inside the html body"
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

(defun orgtrello-webadmin/--content-file (file) "Return the content of a file (absolute name)."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun orgtrello-webadmin/--header-table () "Generate headers."
  `(tr () (td ()) (td () "Action") (td () "Entity") (td () "Delete")))

(defun orgtrello-webadmin/--detail-entity (log-level entity-data) "Depending on the debug level, will display either the full entity data or simply its name."
  (if (= log-level *OT/INFO*) (orgtrello-data/entity-name entity-data) entity-data))

(defun orgtrello-webadmin/--input-button-html (action value) "Given a javascript action and a value, compute an html input button."
  `(input ((class . "btn btn-danger btn-mini")
           (type . "button")
           (onclick . ,action)
           (value . ,value))))

(defun orgtrello-webadmin/--delete-action (entity) "Generate the button to delete some action."
  (-if-let (entity-id (orgtrello-data/entity-id-or-marker entity))
      (orgtrello-webadmin/--input-button-html (format "deleteEntities('/proxy/admin/entities/delete/%s');" entity-id) "x")
    ""))

(defun orgtrello-webadmin/--compute-class (tr-class) "Compute the tr-class"
  `(class . ,(cond ((string= tr-class "icon-play")  "success")
                   ((string= tr-class "icon-pause") "warning")
                   (t                               ""))))

(defun orgtrello-webadmin/--entity (entity icon &optional tr-class) "Compute the entity file display rendering."
  `(tr
    (,(orgtrello-webadmin/--compute-class icon))
    (td () (i ((class . ,icon))))
    (td () ,(orgtrello-data/entity-action entity))
    (td () ,(format "%s" (orgtrello-webadmin/--detail-entity *orgtrello-log/level* entity)))
    (td () ,(orgtrello-webadmin/--delete-action entity))))

(defun orgtrello-webadmin/--list-entities-as-html (entities icon-array-nxt) "Given a list of entities, return as html data."
  (--map (orgtrello-webadmin/--entity it icon-array-nxt) entities))

(defun orgtrello-webadmin/--entities-as-html (entities &optional icon-array-running icon-array-next) "Return the list of files to send to trello"
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
                ,@(orgtrello-webadmin/--list-entities-as-html (cdr entities) icon-array-nxt))
        "None")))

(defun orgtrello-webadmin/--response-html (data http-con) "A response wrapper."
  (elnode-http-start http-con 201 '("Content-type" . "text/html"))
  (elnode-http-return http-con (orgtrello-webadmin/--render-html data)))

(defun orgtrello-webadmin/--elnode-admin (http-con) "A basic display of data"
  (-> (orgtrello-webadmin/html "org-trello/proxy-admin" "Commiters" "Administration the running queries to trello")
      (orgtrello-webadmin/--response-html  http-con)))

(defun compose-fn (funcs) "Composes several functions into one."
  (lexical-let ((intern-funcs funcs))
    (lambda (arg)
      (if intern-funcs
          (funcall (car intern-funcs)
                   (funcall (compose-fn (cdr intern-funcs)) arg))
          arg))))

(defun orgtrello-webadmin/--list-entities (levels &optional scan-flag) "Compute the actions into list."
  (let* ((list-fns '(orgtrello-elnode/compute-entity-level-dir))
         (scan-fns (if scan-flag (cons 'orgtrello-elnode/archived-scanning-dir list-fns) list-fns)) ;; build the list of functions to create the composed function
         (composed-fn (compose-fn scan-fns)))
    (--map
     (orgtrello-proxy/parse-query (read (orgtrello-webadmin/--content-file it)))
     (--mapcat (orgtrello-elnode/list-files (funcall composed-fn it)) levels))))

(defun orgtrello-webadmin/elnode-current-entity (http-con) "A basic display of the list of entities to scan."
  (-> *ORGTRELLO-LEVELS*
      (orgtrello-webadmin/--list-entities 'scan-folder)
      nreverse
      (orgtrello-webadmin/--entities-as-html "icon-play" "icon-pause")
      (orgtrello-webadmin/--response-html http-con)))

(defun orgtrello-webadmin/elnode-next-entities (http-con) "A basic display of the list of entities to scan."
  (-> *ORGTRELLO-LEVELS*
       orgtrello-webadmin/--list-entities
       orgtrello-webadmin/--entities-as-html
       (orgtrello-webadmin/--response-html http-con)))

(defun orgtrello-webadmin/elnode-static-file (http-con) "Serve static files if they exist. Throw 404 if it does not exists. Also, install bootstrap and jquery the first time round."
  ;; the first request will ask for installing bootstrap and jquery
  (orgtrello-webadmin/--install-css-js-files-once)
  (let ((full-file (format "%s/%s/%s" (orgtrello-webadmin/--compute-root-static-files) (elnode-http-mapping http-con 1) (elnode-http-mapping http-con 2))))
    (if (file-exists-p full-file)
        (elnode-send-file http-con full-file)
        (elnode-send-404 http-con (format "Resource file '%s' not found!" full-file)))))

(defun orgtrello-webadmin/--compute-filename-from-entity (entity) "Compute the filename of a file given an entity."
  (format "%s%s-%s.el" (orgtrello-elnode/compute-entity-level-dir (orgtrello-data/entity-level entity)) (orgtrello-data/entity-buffername entity) (orgtrello-data/entity-position entity)))

(defun orgtrello-webadmin/--delete-entity-file! (entity-file-name)
  "Given an entity, retrieve its full path name and delete it"
  (-> entity-file-name
    orgtrello-webadmin/--compute-filename-from-entity
    orgtrello-action/delete-file!))

(defun orgtrello-webadmin/--delete-entity-with-id (id) "Remove the entity/file which match the id id."
  (-if-let (entity-to-delete (->> *ORGTRELLO-LEVELS*
                                  orgtrello-webadmin/--list-entities
                                  (--filter (string= id (orgtrello-data/entity-id it)))
                                  first))
      (orgtrello-webadmin/--delete-entity-file! entity-to-delete)))

(defun orgtrello-webadmin/delete-entities! () "Remove the entities/files."
  (->> *ORGTRELLO-LEVELS*
       orgtrello-webadmin/--list-entities
       (--map (orgtrello-webadmin/--delete-entity-file! it))))

(defun orgtrello-webadmin/elnode-delete-entity (http-con) "Deal with actions to do on 'action' (entities)."
  (let ((id (elnode-http-mapping http-con 1)))
    (if (string= "" id) (orgtrello-webadmin/delete-entities!) (orgtrello-webadmin/--delete-entity-with-id id))))

(defvar *ORGTRELLO-QUERY-APP-ROUTES-WEBADMIN*
  '(("^localhost//proxy/admin/entities/current/\\(.*\\)" . orgtrello-webadmin/elnode-current-entity)
    ("^localhost//proxy/admin/entities/next/\\(.*\\)" . orgtrello-webadmin/elnode-next-entities)
    ("^localhost//proxy/admin/entities/delete/\\(.*\\)" . orgtrello-webadmin/elnode-delete-entity)
    ("^localhost//proxy/admin/\\(.*\\)" . orgtrello-webadmin/--elnode-admin)
    ;; static files
    ("^localhost//static/\\(.*\\)/\\(.*\\)" . orgtrello-webadmin/elnode-static-file))    ;; proxy to request trello
  "Webadmin routes")

(orgtrello-log/msg *OT/DEBUG* "org-trello - orgtrello-webadmin loaded!")


