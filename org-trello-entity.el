;;; org-trello-entity.el --- Predicates to determine if we are currently on a card/checklist/item + some default movments
;;; Commentary:
;;; Code:

(require 'org-trello-setup)
(require 'org-trello-utils)
(require 's)

(defun orgtrello-entity/org-checkbox-p! ()
  "Is there a checkbox at current point?"
  (and
   (org-at-item-checkbox-p)
   (save-excursion
     (beginning-of-line)
     (goto-char (+ (org-get-indentation) (point)))
     (org-at-item-bullet-p))))

(defalias 'orgtrello-entity/org-card-p! 'org-at-heading-p)

(defun orgtrello-entity/--org-checkbox-p! (indent)
  "Determine if current position is a checkbox.
Provided indent as the denominator for the checkbox's nature."
  (-when-let (s (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
    (string-match-p (format "^%s%s" (orgtrello-utils/space indent) "- \\\[.?\\\].*") s)))

(defun orgtrello-entity/org-checklist-p! ()
  "Given the current position, determine if we are on a checklist."
  (orgtrello-entity/--org-checkbox-p! *ORGTRELLO/CHECKLIST-INDENT*))

(defun orgtrello-entity/org-item-p! ()
  "Given the current position, determine if we are on an item."
  (orgtrello-entity/--org-checkbox-p! *ORGTRELLO/ITEM-INDENT*))

(defun orgtrello-entity/back-to-card! ()
  "Given the current position, goes on the card's heading.
Does not preserve position."
  (org-back-to-heading))

(defun orgtrello-entity/card-start-point! ()
  "Compute the first character of the card."
  (save-excursion (orgtrello-entity/back-to-card!) (point-at-bol)))

(defun orgtrello-entity/level! ()
  "Compute the levels from the current position (which is `bol`)"
  (save-excursion
    (beginning-of-line)
    (cond ((orgtrello-entity/org-card-p!)      *ORGTRELLO/CARD-LEVEL*)
          ((orgtrello-entity/org-checklist-p!) *ORGTRELLO/CHECKLIST-LEVEL*)
          ((orgtrello-entity/org-item-p!)      *ORGTRELLO/ITEM-LEVEL*)
          (t                                   -1))))

(defun orgtrello-entity/goto-next-checkbox ()
  "Go to the next checkbox.
Does not preserve the current position.
If hitting a heading or the end of the file, return nil."
  (forward-line)
  (when (and (< (point) (point-max)) (not (orgtrello-entity/org-card-p!)) (not (orgtrello-entity/org-checkbox-p!)))
    (orgtrello-entity/goto-next-checkbox)))

(defun orgtrello-entity/card-metadata-end-point! ()
  "Compute the first position of the card's next checkbox."
  (save-excursion
    (orgtrello-entity/back-to-card!)
    (orgtrello-entity/goto-next-checkbox)
    (1- (point))))

(defun orgtrello-entity/card-at-pt! ()
  "Determine if currently on the card region."
  (let ((pt (point)))
    (and (<= (orgtrello-entity/card-start-point!) pt) (<= pt (orgtrello-entity/card-metadata-end-point!)))))

(defun orgtrello-entity/checklist-at-pt! ()
  "Determine if currently on the checklist region."
  (= (orgtrello-entity/level!) *ORGTRELLO/CHECKLIST-LEVEL*))

(defun orgtrello-entity/item-at-pt! ()
  "Determine if currently on the item region."
  (= (orgtrello-entity/level!) *ORGTRELLO/ITEM-LEVEL*))

(defun orgtrello-entity/card-description-start-point! ()
  "Compute the first character of the card's description content."
  (save-excursion
    (orgtrello-entity/back-to-card!)
    (search-forward ":END:" nil t) ;; if not found, return nil and do not move point
    (1+ (point-at-eol))));; in any case, the description is then just 1 point more than the current position

(defun orgtrello-entity/compute-next-card-point! ()
  "Compute the next card's position.
Does preserve position.
If a sibling is found, return the point-at-bol, otherwise return the max point in buffer."
  (save-excursion
    (org-back-to-heading)
    (if (org-goto-sibling) (point-at-bol) (point-max))))

(defun orgtrello-entity/compute-checklist-header-region! ()
  "Compute the checklist's region (only the header, without computing the zone occupied by items) couple '(start end)."
  `(,(point-at-bol) ,(1+ (point-at-eol))))

(defun orgtrello-entity/goto-next-checkbox-with-same-level! (level)
  "Compute the next checkbox's beginning of line (with the same LEVEL).
 Does not preserve the current position.
If hitting a heading or the end of the file, return nil.
Otherwise, return the current position."
  (forward-line)
  (if (= level (orgtrello-entity/level!))
      (point)
    (if (or (orgtrello-entity/org-card-p!) (<= (point-max) (point)))
        nil
      (orgtrello-entity/goto-next-checkbox-with-same-level! level))))

(defun orgtrello-entity/next-checklist-point! ()
  "Compute the next checklist position from the current position."
  (save-excursion
    (org-end-of-item)
    (1- (point))))

(defun orgtrello-entity/compute-checklist-region! ()
  "Compute the checklist's region (including the items) couple '(start end)."
  `(,(orgtrello-buffer/checklist-beginning-pt!) ,(1- (save-excursion (org-end-of-item) (point)))))

(defun orgtrello-entity/compute-item-region! ()
  "Compute the item region couple '(start end)."
  `(,(point-at-bol) ,(point-at-eol)))

(defun orgtrello-entity/compute-card-region! ()
  "Compute the card region zone couple '(start end)."
  `(,(orgtrello-entity/card-start-point!) ,(orgtrello-entity/compute-next-card-point!)))

(defun orgtrello-entity/card-metadata-region! ()
  "Compute the card's metadata (description) region couple '(start end)."
  `(,(orgtrello-entity/card-description-start-point!) ,(orgtrello-entity/card-metadata-end-point!)))

(defun orgtrello-entity/card-data-region! ()
  "Compute the card's data region (checklists/items) couple '(start end)."
  `(,(1+ (orgtrello-entity/card-metadata-end-point!)) ,(1- (orgtrello-entity/compute-next-card-point!))))

(provide 'org-trello-entity)
;;; org-trello-entity.el ends here
