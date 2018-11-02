#| -*-Scheme-*- |#

;;;; Load the PGSQL option.

(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (load-package-set "pgsql")))

(add-subsystem-identification! "PostgreSQL" '(1 0))