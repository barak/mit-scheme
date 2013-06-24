#| -*-Scheme-*-

Compile the FFI system. |#

(load-option 'CREF)

;; Temporay hack.  Remove when (runtime ffi) is in the release.
(if (not (name->package '(RUNTIME FFI)))
    (let ((path (package-set-pathname "../runtime/runtime")))
      (if (not (file-exists? path))
	  (cref/generate-trivial-constructor "../runtime/runtime"))
      (eval `(for-each-vector-element
	      (package-file/descriptions (fasload ,path))
	      (lambda (description)
		(if (equal? (package-description/name description) '(RUNTIME FFI))
		    (begin
		      (construct-normal-package-from-description description)
		      (create-links-from-description description)
		      (load "../runtime/ffi" (->environment '(RUNTIME FFI))
			    'ignored #t)))))
	    (->environment '(PACKAGE)))))

;; Temporary hack.  Remove when compile-system is in the release.
(if (not (environment-bound? (->environment '()) 'compile-system))
    (let ((butil-env (->environment '(cross-reference)))
	  (global-env (->environment '())))
      (load "../cref/butils" butil-env)
      (environment-link-name global-env butil-env 'compile-system)))

(compile-system "ffi" (directory-pathname (current-load-pathname)))