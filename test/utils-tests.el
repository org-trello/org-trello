(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(expectations
 (expect "something-to-be-replaced" (orgtrello-utils/replace-in-string " " "-" "something to be replaced"))
 (expect "something-to-be-replaced" (orgtrello-utils/replace-in-string "###" "-" "something###to###be###replaced")))

(expectations (desc "orgtrello-utils/conj")
              (expect '(1 2 3 4) (orgtrello-utils/conj '(1 2 3) 4)))
