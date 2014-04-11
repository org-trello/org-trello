(defun orgtrello-utils/replace-in-string (expression-to-replace replacement-expression string-input)
  "Given a string-input, an expression-to-replace (regexp/string) and a replacement-expression, replace the expression-to-replace by replacement-expression in string-input"
  (replace-regexp-in-string expression-to-replace replacement-expression string-input 'fixed-case))

(defun orgtrello-utils/conj (v l)
  "Adding the value to the list in last position."
  (->> l nreverse (cons v) nreverse))
