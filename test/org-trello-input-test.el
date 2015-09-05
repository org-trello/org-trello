(require 'org-trello-input)

(ert-deftest test-orgtrello-input-read-string-completion ()
  (should (eq :res
              (with-mock
                (mock (ido-completing-read :prompt :choices nil 'do-match) => :res)
                (orgtrello-input-read-string-completion :prompt :choices)))))

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
