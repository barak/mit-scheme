#| -*-Scheme-*- |#

;;;; Compile the BLOWFISH option.

(load-option 'cref)
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (with-working-directory-pathname (merge-pathnames "../ffi")
      (lambda ()
	(load "make")))
    (compile-file "blowfish" '() (->environment '(runtime)))
    (cref/generate-constructors "blowfish")))