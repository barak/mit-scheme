#| -*-Scheme-*-

$Id: $

Compile the FFI system. |#

(load-option 'CREF)
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (let ((ffi-files '("ctypes" "cdecls" "syntax" "generator")))

      ;; Build an empty package for use at syntax-time.
      ;; The imports should bind ucode-primitive (ffi).
      (if (not (name->package '(FFI)))
	  (let ((package-set (package-set-pathname "ffi")))
	    (if (not (file-exists? package-set))
		(cref/generate-trivial-constructor "ffi"))
	    (construct-packages-from-file (fasload package-set))))

      ;; Syntax in (ffi).
      (fluid-let ((sf/default-syntax-table (->environment '(ffi)))
		  (sf/default-declarations
		   (cons '(usual-integrations) sf/default-declarations)))
	(for-each (lambda (f) (sf-conditionally f #t)) ffi-files))

      ;; Cross-check.
      (cref/generate-constructors "ffi" 'ALL)

      ;; Compile.
      (for-each compile-file ffi-files))))