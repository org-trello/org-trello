(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(expectations
 (expect "something-to-be-replaced" (orgtrello-utils/replace-in-string " " "-" "something to be replaced"))
 (expect "something-to-be-replaced" (orgtrello-utils/replace-in-string "###" "-" "something###to###be###replaced")))
