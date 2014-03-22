(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(expectations (desc "orgtrello-proxy/--dispatch-action")
              (expect 'orgtrello-proxy/--delete      (orgtrello-proxy/--dispatch-action "delete"))
              (expect 'orgtrello-proxy/--sync-entity (orgtrello-proxy/--dispatch-action "sync-entity"))
              (expect nil                            (orgtrello-proxy/--dispatch-action "nothing")))

(expectations (desc "orgtrello-proxy/--compute-pattern-search-from-marker")
              (expect "marker-is-a-trello-id" (orgtrello-proxy/--compute-pattern-search-from-marker "marker-is-a-trello-id"))
              (expect "orgtrello-marker-tony" (orgtrello-proxy/--compute-pattern-search-from-marker "orgtrello-marker-tony")))

(expectations (desc "orgtrello-proxy/--archived-scanning-file")
              (expect "test/folder/.scanning/filename" (orgtrello-proxy/--archived-scanning-file "test/folder/filename")))

(expectations (desc "orgtrello-proxy/--update-buffer-to-save")
              (expect '(:a) (orgtrello-proxy/--update-buffer-to-save :a nil))
              (expect '(:a) (orgtrello-proxy/--update-buffer-to-save :a '(:a)))
              (expect '(:a :b) (orgtrello-proxy/--update-buffer-to-save :a '(:b))))

(expectations (desc "orgtrello-proxy/update-buffer-to-save!")
              (setq *ORGTRELLO-LIST-BUFFERS-TO-SAVE* nil)
              (expect '(:a) (orgtrello-proxy/update-buffer-to-save! :a))
              (expect '(:a) (orgtrello-proxy/update-buffer-to-save! :a))
              (expect '(:b :a) (orgtrello-proxy/update-buffer-to-save! :b)))
