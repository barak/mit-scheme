#| -*-Scheme-*- |#

;;;; Load the Mcrypt option.

(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (load-package-set "mcrypt")))

(add-subsystem-identification! "Mcrypt" '(0 2))