;; from shell: emacs --batch -l ./build.el -- package-file-name

(require 'package)
(package-initialize)
(package-refresh-contents)

(setq package-user-dir (concat (file-name-directory (or (buffer-file-name) load-file-name default-directory)) ".elpa"))

(let ((package (concat "./" (car (reverse command-line-args)))))
  (message "the package is: %s" package)
  (package-install-file package))

;; End
