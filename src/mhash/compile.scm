#| -*-Scheme-*- |#

;;;; Compile the mhash wrapper.

(load-option 'CREF)
(load-option 'FFI)
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (with-system-library-directories
	'("./")
      (lambda ()
	(compile-file "mhash" '() (->environment '(RUNTIME)))))
    (cref/generate-constructors "mhash" 'ALL)))