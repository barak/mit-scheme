#| -*-Scheme-*- |#

;;;; Compile the MCRYPT option.

(load-option 'CREF)
(load-option 'FFI)
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (with-system-library-directories
	'("./")
      (lambda ()
	(compile-file "mcrypt" '() (->environment '(RUNTIME)))))
    (cref/generate-constructors "mcrypt" 'ALL)))