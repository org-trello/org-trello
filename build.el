;; from shell: emacs --batch -l ./build.el -- package-file-name

(package-initialize)
(package-refresh-contents)
(let ((package
       (concat
        (file-name-directory
         (or (buffer-file-name)
             load-file-name))
        (car (reverse command-line-args)))))
  (message "the package is: %s" package)
  (package-install-file package))
;; End
