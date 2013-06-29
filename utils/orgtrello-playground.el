(require 'orgtrello-api)
(require 'orgtrello-query)
(require 'dash)

(setq boards-request-response (orgtrello-http (get-boards)))
(setq boards                  (request-response-data boards-request-response))

;; ELISP> (mapcar (lambda (board) (assq 'id board)) boards)
;; ((id . "50aa59502ddab2fc1100115b")
;;  (id . "4f96a984dbb00d733b04d8b5")
;;  (id . "518815ca40491e9a7b006f21")
;;  (id . "51626230b314a2252c0007eb")
;;  (id . "50ceefed35a573c76e001852")
;;  (id . "51a327c85436a5ee3e0045b4")
;;  (id . "4f2baa3072b7c1293501caea")
;;  (id . "4f2ba2054f2cb9d16d3f7f99")
;;  (id . "50bcfd2f033110476000e768")
;;  (id . "50169deffa899b184693ee76")
;;  (id . "51a326c424aa585670000180"))

;; ELISP> (->> boards
;;             (mapcar
;;              (lambda (board)
;;                (->> board
;;                     (assq 'id)
;;                     cdr))))
;; ("50aa59502ddab2fc1100115b" "4f96a984dbb00d733b04d8b5" "518815ca40491e9a7b006f21" "51626230b314a2252c0007eb" "50ceefed35a573c76e001852" "51a327c85436a5ee3e0045b4" "4f2baa3072b7c1293501caea" "4f2ba2054f2cb9d16d3f7f99" "50bcfd2f033110476000e768" "50169deffa899b184693ee76" "51a326c424aa585670000180")

(defun boards-properties (boards property)
  (->> boards
       (mapcar
        (lambda (board)
          (->> board
               (assq property)
               cdr)))))

(setq all-ids  (boards-properties boards 'id))
(setq all-name (boards-properties boards 'name))
