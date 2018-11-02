#| -*-Scheme-*- |#

;;;; Load the BLOWFISH option.

(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (load-package-set "blowfish")))

(add-subsystem-identification! "Blowfish" '(1 1))