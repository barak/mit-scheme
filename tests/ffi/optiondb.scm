#| -*-Scheme-*- |#

;;;; Test optiondb, includes the installed system's optiondb.

(further-load-options
 (merge-pathnames "optiondb"
		  (cadr (access library-directory-path
				(->environment '(runtime pathname))))))