#| -*-Scheme-*- |#

;;;; Load the MD5 option.

(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (load-package-set "md5")))

(add-subsystem-identification! "MD5" '(0 1))