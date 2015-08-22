;;; load-org-trello-tests.el --- Load the namespaces of org-trello and org-trello-tests in a test context
;;; Commentary:
;;; Code:

(require 'ert)
(require 'el-mock)

(message "Launching tests!")

;; load code prod
(load-file "load-org-trello.el")

(defconst orgtrello-tests-test-folder "./test"
  "Folder where tests files are defined.")

;; Add test folder to the load path
(add-to-list 'load-path (expand-file-name orgtrello-tests-test-folder))

(message "Loading tests done!")

;; force loading

(require 'load-org-trello)

(defvar orgtrello-tests--namespaces '() "Org-trello test namespaces for development purposes.")
(setq orgtrello-tests--namespaces '("test/utilities-test.el"
                                    "test/org-trello-setup-test.el"
                                    "test/org-trello-action-test.el"
                                    "test/org-trello-api-test.el"
                                    "test/org-trello-backend-test.el"
                                    "test/org-trello-entity-test.el"
                                    "test/org-trello-entity-test.el"
                                    "test/org-trello-cbx-test.el"
                                    "test/org-trello-date-test.el"
                                    "test/org-trello-buffer-test.el"
                                    "test/org-trello-controller-test.el"
                                    "test/org-trello-data-test.el"
                                    "test/org-trello-hash-test.el"
                                    "test/org-trello-proxy-test.el"
                                    "test/org-trello-query-test.el"
                                    "test/org-trello-utils-test.el"))

(defun orgtrello-tests-execute-fn-on-buffer (fn buffer-file)
  "Execute the function FN on buffer-file.
FN is a parameter-less function that compute something from BUFFER-FILE.
The buffer-file is not modified and the position is not changed."
  (save-excursion
    (with-temp-buffer
      (insert-file buffer-file)
      (goto-char (point-min))
      (funcall fn))))

(defun orgtrello-tests-number-of (regexp buffer-file)
  "Given a REGEXP, count all occurences on BUFFER-FILE."
  (orgtrello-tests-execute-fn-on-buffer
   (lambda ()
     (let ((c 0))
       (while (re-search-forward regexp nil t)
         (setq c (1+ c)))
       c))
   buffer-file))

(defun orgtrello-tests-ask-for-buffer-or-fallback-to-default (&optional ask-buffer)
  "Compute the desired buffer.
If region is active, use the region.
Otherwise, if ask-buffer is set, ask for input.
Otherwise, fallback to the current buffer name."
  (if (region-active-p)
      (buffer-substring (region-beginning) (region-end))
    (if ask-buffer
        (read-string "Buffer name: ")
      (buffer-name (current-buffer)))))

(ert-deftest test-orgtrello-tests-ask-for-buffer-or-fallback-to-default ()
  (should (equal :default-buffer
                 (with-mock
                   (mock (region-active-p) => nil)
                   (mock (current-buffer) => :current)
                   (mock (buffer-name :current) => :default-buffer)
                   (orgtrello-tests-ask-for-buffer-or-fallback-to-default))))
  (should (equal :result-with-region
                 (with-mock
                   (mock (region-active-p) => t)
                   (mock (region-beginning) => :start)
                   (mock (region-end) => :end)
                   (mock (buffer-substring :start :end) => :result-with-region)
                   (orgtrello-tests-ask-for-buffer-or-fallback-to-default))))
  (should (equal :res
                 (with-mock
                   (mock (read-string "Buffer name: ") => :res)
                   (orgtrello-tests-ask-for-buffer-or-fallback-to-default :buffer)))))

(defun orgtrello-tests-interactive-number-of (regexp &optional ask-buffer)
  "Given a REGEXP, compute a number of occurrences.
If region is active, will use the region highlight as buffer.
Otherwise, if ASK-BUFFER is not nil, will ask the user.
Otherwise, default to current buffer."
  (let ((buf (orgtrello-tests-ask-for-buffer-or-fallback-to-default ask-buffer)))
    (-when-let (c (orgtrello-tests-number-of regexp buf))
      (insert (int-to-string c)))))

(defun orgtrello-tests-list-functions-in-buffer (buffer-file)
  "Compute the number of `defun' in the BUFFER-FILE."
  (orgtrello-tests-execute-fn-on-buffer
   (lambda ()
     (let ((functions))
       (while (re-search-forward "\(defun \\(.*\\) \(" nil t)
         (push (match-string-no-properties 1) functions))
       (nreverse functions)))
   buffer-file))

(require 'f)

(defun orgtrello-tests-ns-file-from-current-buffer (ns-filename)
  "Compute the test namespace file from the namespace file."
  (let ((buff (concat (f-no-ext ns-filename) "-test")))
    (->> (directory-files orgtrello-tests-test-folder)
         (-filter (-partial 'string-match-p buff))
         car
         (list orgtrello-tests-test-folder)
         (s-join "/"))))

(ert-deftest test-orgtrello-tests-ns-file-from-current-buffer ()
  (should (equal "./test/org-trello-proxy-test.el"
                 (orgtrello-tests-ns-file-from-current-buffer "org-trello-proxy.el")))
  (should (equal "./test/org-trello-buffer-test.el"
                 (orgtrello-tests-ns-file-from-current-buffer "org-trello-buffer.el")))
  (should (equal "./test/org-trello-test.el"
                 (orgtrello-tests-ns-file-from-current-buffer "org-trello.el"))))

(require 'helm)

(defun orgtrello-tests-function-covered-p (fname &optional ask-buffer)
  (interactive "P")
  (let* ((actual-buffer-file (buffer-name (current-buffer)))
         (buffer-test-file (orgtrello-tests-ns-file-from-current-buffer
                            actual-buffer-file))
         (fn-name (if (region-active-p)
                      (buffer-substring-no-properties (region-beginning)
                                                      (region-end))
                    (let ((fn-names (orgtrello-tests-list-functions-in-buffer
                                     actual-buffer-file)))
                      (helm-comp-read "Choose a function to check for coverage: "
                                      fn-names)))))
    (message
     (if (< 0 (orgtrello-tests-number-of (format "\(ert-deftest test-%s" fn-name)
                                         buffer-test-file))
         (message "Tested!")
       (message "'%s' is not covered in %s!"
                fn-name
                buffer-test-file)))))

(defun orgtrello-tests-function-coverage ()
  "Determine the function not covered in the current namespace."
  (let* ((actual-buffer-file (buffer-name (current-buffer)))
         (buffer-test (orgtrello-tests-ns-file-from-current-buffer actual-buffer-file))
         (fn-names (orgtrello-tests-list-functions-in-buffer actual-buffer-file)))
    (->> fn-names
         (--map (list it (< 0 (orgtrello-tests-number-of (format "\(ert-deftest test-%s" it) buffer-test))))
         (-filter (-compose 'not 'cadr))
         (-map 'car)))) ;; FIXME: -reduce...

(defun orgtrello-tests-next-uncovered-function ()
  (interactive)
  (-if-let (uncovered-functions (orgtrello-tests-function-coverage))
      (let ((fn-name (helm-comp-read "Next uncovered function: " uncovered-functions)))
        (-if-let (pos (save-excursion
                        (goto-char (point-min))
                        (search-forward (format "\(defun %s" fn-name))))
            (goto-char pos)
          (message "Curiously enough, I did not find '%s'... Sorry about that."
                   fn-name)))
    (message "Congrats! Namespace seems fully covered!")))

(defun orgtrello-tests-count-functions (&optional ask-buffer)
  "Count the number of `def-un' or `def-alias'.
If region is active, will use the region highlight as buffer.
Otherwise, if ASK-BUFFER is not nil, will ask the user.
Otherwise, default to current buffer."
  (interactive "P")
  (orgtrello-tests-interactive-number-of "\\(defun\\).*" ask-buffer))

(defun orgtrello-tests-count-number-tests (&optional ask-buffer)
  "Count the number of `ert-def-test'.
If region is active, will use the region highlight as buffer.
Otherwise, if ASK-BUFFER is not nil, will ask the user.
Otherwise, default to current buffer."
  (interactive "P")
  (orgtrello-tests-interactive-number-of "\\(ert-deftest\\).*" ask-buffer))

(defun orgtrello-tests-load-namespaces ()
  "Load the org-trello namespaces."
  (interactive)
  (mapc #'load-file orgtrello-tests--namespaces)
  (require 'org-trello)
  (orgtrello-log-msg orgtrello-log-info "Tests loaded!"))

(defun orgtrello-tests-find-next-error ()
  "Find the next test error"
  (interactive)
  (with-current-buffer "*compilation*"
    (goto-char (point-min))
    (if (search-forward "(ert-test-failed" nil 'noerror)
        (progn
          (switch-to-buffer "*compilation*" nil 'same-window)
          (search-forward "(ert-test-failed" nil 'noerror)
          (forward-line 10))
      (message "All is good!"))))

(orgtrello-tests-load-namespaces)

(define-key emacs-lisp-mode-map (kbd "C-c o d") 'orgtrello-tests-load-namespaces)
(define-key emacs-lisp-mode-map (kbd "C-c o D") 'orgtrello-tests-find-next-error)

(fset 'orgtrello-tests-org-raw-coverage
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([tab tab tab 201326624 201326624 backspace tab S-iso-lefttab S-iso-lefttab S-iso-lefttab 201326624 201326624 backspace tab S-iso-lefttab S-iso-lefttab 201326624 201326624 201326624 134217848 111 114 103 116 114 101 108 108 111 45 116 101 115 116 115 45 99 111 117 110 116 45 102 117 110 99 116 105 111 110 115 13 134217736 tab 25 46 48 9 201326624 201326624 201326624 134217848 111 114 103 116 114 101 108 108 111 45 116 101 115 116 115 45 99 111 117 110 116 45 110 117 109 98 101 114 45 116 101 115 116 115 13 134217736 9 25 46 48 9 3 42 21 3 42 9] 0 "%d")) arg)))

(define-key org-trello-mode-map (kbd "C-c o o") 'org-trello-dev-find-unused-definitions)
(define-key emacs-lisp-mode-map (kbd "C-c o o") 'orgtrello-tests-org-raw-coverage)

(require 'org-trello-action-test)
(require 'org-trello-api-test)
(require 'org-trello-backend-test)
(require 'org-trello-buffer-test)
(require 'org-trello-cbx-test)
(require 'org-trello-controller-test)
(require 'org-trello-data-test)
(require 'org-trello-hash-test)
(require 'org-trello-setup-test)
(require 'org-trello-proxy-test)

(provide 'load-org-trello-tests)
;;; load-org-trello-tests ends here
