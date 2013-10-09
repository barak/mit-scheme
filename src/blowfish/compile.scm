#| -*-Scheme-*- |#

;;;; Compile the Blowfish wrapper.

(load-option 'CREF)
(load-option 'FFI)
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (with-system-library-directories
	'("./")
      (lambda ()
	(compile-file "blowfish" '() (->environment '(RUNTIME)))))
    (cref/generate-constructors "blowfish" 'ALL)))