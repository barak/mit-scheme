;;; -*-Scheme-*-
(let ((rcs-directory-log (access rcs-directory-log (->environment '(RCS)))))
  (for-each (lambda (directory)
	      (rcs-directory-log
	       (merge-pathnames "RCS.log"
				(pathname-as-directory directory))
	       directory))
	    '("/scheme/src/6001"
	      "/scheme/src/compiler"
	      "/scheme/src/cref"
	      "/scheme/src/edwin"
	      "/scheme/src/microcode"
	      "/scheme/src/rcs"
	      "/scheme/src/runtime"
	      "/scheme/src/sf"
	      "/scheme/src/sicp"
	      "/scheme/src/win32")))