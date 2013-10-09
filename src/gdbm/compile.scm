#| -*-Scheme-*- |#

;;;; Compile the GDBM wrapper.

(load-option 'CREF)
(load-option 'FFI)
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (with-system-library-directories
	'("./")
      (lambda ()
	(compile-file "gdbm" '() (->environment '(RUNTIME)))))
    (cref/generate-constructors "gdbm" 'ALL)))