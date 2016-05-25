#| -*-Scheme-*- |#

(define-load-option 'MCRYPT
  (standard-system-loader "."))

(further-load-options
 (named-lambda (system-load-options)
   (merge-pathnames "optiondb"
		    (cadr (access library-directory-path
				  (->environment '(runtime pathname)))))))