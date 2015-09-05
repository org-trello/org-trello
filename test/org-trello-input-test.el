(require 'org-trello-input)

(ert-deftest test-orgtrello-input-read-string-completion ()
  (should (eq :res-with-ido
              (let ((org-trello-input-completion-mechanism 'default))
                (with-mock
                  (mock (ido-completing-read :prompt :choices nil 'do-match) => :res-with-ido)
                  (orgtrello-input-read-string-completion :prompt :choices)))))
  ;; can't work without requiring helm -> not declared as a dependency on org-trello
  ;; (should (eq :res-with-helm
  ;;             (let ((org-trello-input-completion-mechanism 'other))
  ;;               (with-mock
  ;;                 (mock (helm-comp-read :prompt :choices nil 'do-match) => :res-with-helm)
  ;;                 (orgtrello-input-read-string-completion :prompt :choices)))))
  )

(ert-deftest test-orgtrello-input-read-not-empty ()
  (should (equal :something
                 (with-mock
                   (mock (read-string "prompt: ") => :something)
                   (orgtrello-input-read-not-empty "prompt: ")))))

(ert-deftest test-orgtrello-input-read-string ()
  (should (equal :something
                 (with-mock
                   (mock (read-string "prompt: ") => :something)
                   (orgtrello-input-read-string "prompt: ")))))
