#| -*-Scheme-*- |#

;;;; Load the Blowfish option.

(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (load-package-set "blowfish")))

(add-subsystem-identification! "Blowfish" '(1 1))