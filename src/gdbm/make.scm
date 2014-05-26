#| -*-Scheme-*- |#

;;;; Load the GDBM option.

(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (load-package-set "gdbm")))

(add-subsystem-identification! "GDBM2" '(0 1))