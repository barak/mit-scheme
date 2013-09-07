;;;-*-Scheme-*-

(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (let ((code
	   (with-notification
	    (lambda (port)
	      (write-string "make build" port)
	      (newline port))
	    (lambda ()
	      (run-synchronous-subprocess "make" (list "all"))))))
      (if (not (zero? code))
	  (warn "Test library build failed:" code)
	  (begin
	    (fluid-let ((load/suppress-loading-message? #t))
	      (load-option 'FFI))
	    (with-system-library-directories '("./")
	      (lambda ()
		(compile-file "test-ffi-wrapper")))
	    (load "test-ffi-wrapper"))))))

(define-test 'ffi test-ffi)