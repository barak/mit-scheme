;; -*- Scheme -*-

(define (guarded-system-loader package-name place #!optional filename)
  (let ((directory
	 (merge-pathnames place
			  (directory-pathname (current-load-pathname)))))
    (lambda ()
      (if (not (name->package package-name))
	  (with-working-directory-pathname directory
	    (lambda ()
	      (load
	       (let ((test
		      (lambda (name)
			(or (file-exists? name)
			    (there-exists? load/default-types
			      (lambda (type)
				(file-exists?
				 (pathname-new-type name (car type)))))))))
		 (cond ((not (default-object? filename)) filename)
		       ((test "make") "make")
		       ((test "load") "load")
		       (else (error "Can't find loader.")))))))))))

(define-load-option 'SOS
  (guarded-system-loader '(runtime object-system) "sos"))

(define-load-option 'IMAIL
  (guarded-system-loader '(edwin imail) "imail"))

(further-load-options standard-load-options)