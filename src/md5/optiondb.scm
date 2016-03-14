#| -*-Scheme-*- |#

(define-load-option 'MD5
  (standard-system-loader "."))

(further-load-options
 (merge-pathnames "optiondb"
		  (cadr (access library-directory-path
				(->environment '(runtime pathname))))))