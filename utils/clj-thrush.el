;; source: https://github.com/sroccaserra/emacs/blob/master/tools.el

(defmacro -> (x &optional form &rest more)
  (cond ((not (null more))
         `(-> (-> ,x ,form) ,@more))
        ((not (null form))
         (if (sequencep form)
             `(,(first form) ,x ,@(rest form))
           (list form x)))
        (t x)))

(defmacro ->> (x form &rest more)
  (cond ((not (null more)) `(->> (->> ,x ,form) ,@more))
        (t (if (sequencep form)
               `(,(first form) ,@(rest form) ,x)
             (list form x)))))

;; (defmacro -?> (x form &rest more)
;;   (cond ((not (null more)) `(-?> (-?> ,x ,form) ,@more))
;;         (t (if (sequencep form)
;;                `(if (null ,x) nil
;;                   (,(first form) ,x ,@(rest form)))
;;              `(if (null ,x) nil
;;                 ,(list form x))))))

;; (defmacro -?>> (x form &rest more)
;;   (cond ((not (null more)) `(-?>> (-?>> ,x ,form) ,@more))
;;         (t (if (sequencep form)
;;                `(if (null ,x) nil
;;                   (,(first form) ,@(rest form) ,x))
;;              `(if (null ,x) nil
;;                 ,(list form x))))))

(provide 'clj-thrush)
