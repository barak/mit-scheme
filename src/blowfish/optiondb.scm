#| -*-Scheme-*- |#

;;;; Test optiondb, includes the installed system's optiondb.

(define-load-option 'BLOWFISH
  (standard-system-loader "."))

(further-load-options
 (merge-pathnames "optiondb"
		  (cadr (access library-directory-path
				(->environment '(runtime pathname))))))