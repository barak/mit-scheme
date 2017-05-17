#| -*-Scheme-*- |#

;;;; Load the mcrypt option.

(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (load-package-set "mcrypt")))

(add-subsystem-identification! "mcrypt" '(0 2))