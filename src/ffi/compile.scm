#| -*-Scheme-*- |#

;;;; Compile the C/FFI.

(load-option 'CREF)
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (for-each (lambda (file)
		(compile-file file '() (->environment '(RUNTIME))))
	      '("ctypes" "cdecls" "syntax" "generator" "build"))
    (cref/generate-constructors "ffi" 'ALL)))