#| -*-Scheme-*- |#

;;;; Load the mhash option.

(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (load-package-set "mhash")))

(add-subsystem-identification! "mhash" '(0 1))