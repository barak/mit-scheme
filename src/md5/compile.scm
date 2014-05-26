#| -*-Scheme-*- |#

;;;; Compile the MD5 wrapper.

(load-option 'CREF)
(load-option 'FFI)
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (with-system-library-directories
	'("./")
      (lambda ()
	(compile-file "md5" '() (->environment '(RUNTIME)))))
    (cref/generate-constructors "md5" 'ALL)))