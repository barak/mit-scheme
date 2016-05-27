#| -*-Scheme-*- |#

(define-load-option 'X11-SCREEN
  (standard-system-loader "."))

(further-load-options
 (named-lambda (system-load-options)
   (merge-pathnames "optiondb"
		    (cadr (access library-directory-path
				  (->environment '(runtime pathname)))))))