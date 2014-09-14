(require 'org-trello-utils)
(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(expectations
 (expect "something-to-be-replaced" (orgtrello-utils/replace-in-string " " "-" "something to be replaced"))
 (expect "something-to-be-replaced" (orgtrello-utils/replace-in-string "###" "-" "something###to###be###replaced")))

(expectations
 (desc "orgtrello-utils/symbol")
 (expect ""      (orgtrello-utils/symbol " "  0))
 (expect "*"     (orgtrello-utils/symbol "*"  1))
 (expect "****"  (orgtrello-utils/symbol "**" 2))
 (expect "   "   (orgtrello-utils/symbol " "  3)))

(expectations
 (desc "orgtrello-utils/space")
 (expect ""    (orgtrello-utils/space 0))
 (expect " "   (orgtrello-utils/space 1))
 (expect "  "  (orgtrello-utils/space 2))
 (expect "   " (orgtrello-utils/space 3)))

(provide 'org-trello-utils-tests)
;;; org-trello-utils-tests.el ends here
