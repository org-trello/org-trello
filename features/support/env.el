;; This is an example of how you could set up this file. This setup
;; requires a directory called util in the project root and that the
;; util directory contains the testing tools ert and espuds.

(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (setq org-trello-root-path project-directory)
  (setq org-trello-util-path (expand-file-name "util" org-trello-root-path)))

(add-to-list 'load-path org-trello-root-path)
(add-to-list 'load-path (expand-file-name "espuds" org-trello-util-path))
(add-to-list 'load-path (expand-file-name "ert" org-trello-util-path))

(require 'espuds)
(require 'ert)
(require 'org-trello)

;; ######### setup function

(defun setup-access-properties ()
  "Setup the trello access properties."
  (setq *consumer-key* "60c8eed6536f2d70c8bf64c6b17cd378")
  (setq *access-token* "44ae917559a130342ef339556f54413a079c06ebf39178c355f269df5d9f5efc")
  (setq *ORGTRELLO-MARKER* "orgtrello-marker-60c8eed6536f2d70c8bf64c6b17cd378"))

(defun prepare-buffer (buffer-name)
  "Prepare the buffer with the needed metadata to discuss with a trello board."
  ;; create a buffer dedicated
  (switch-to-buffer
   (get-buffer-create buffer-name))
  ;; erase everything inside
  (erase-buffer)
  ;; place at the beginning of the buffer
  (goto-char (point-min))
  ;; insert needed metadata
  (insert "#+property: board-name    one-board-to-rule-them-all\n")
  (insert "#+property: board-id      5203a64ec80562a65a001899\n")
  (insert "#+property: CANCELLED 5203a64f2e459759080030f7\n")
  (insert "#+property: DELEGATED 5203a64fb0c0c4a85a0027cc\n")
  (insert "#+property: FAIL 5203a650e84055a908002a0e\n")
  (insert "#+property: DONE 5203a6507ea220e17b002775\n")
  (insert "#+property: PENDING 5203a652911ff0c45c001931\n")
  (insert "#+property: IN-PROGRESS 5203a6531ec4f9d7070023eb\n")
  (insert "#+property: TODO 5203a654dd8182a83e001f32\n")
  (insert "#+TODO: TODO IN-PROGRESS PENDING | DONE FAIL DELEGATED CANCELLED\n\n")
  ;; restart org to take into account the setup
  (let (features)
    (org-mode-restart)))

;; ######### setup test

;;(require 'ecukes) ;; for debugging purposes only

(Setup
 (progn
   ;; Setup the account to use with trello
   (setup-access-properties)
   ;; Prepare the buffer with the needed data
   (prepare-buffer "*org-trello-testing-buffer*")
   ;; done
   (message "setup done!")))

(Before
 ;; Before each scenario is run
 ;; Prepare the buffer with the needed data
 (prepare-buffer "*org-trello-testing-buffer*"))

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
