;;; -*-Scheme-*-
(let ((rcs-directory-log (access rcs-directory-log (->environment '(RCS)))))
  (define (make-log directory . subdirectories)
    (with-working-directory-pathname directory
      (lambda ()
	(apply rcs-directory-log
	       "RCS.log"
	       (cons "RCS"
		     (map (lambda (subdirectory)
			    (string-append subdirectory "/RCS"))
			  subdirectories))))))
  (make-log "/scheme/src/6001")
  (make-log "/scheme/src/compiler" "back" "base" "fggen" "fgopt"
	    "rtlbase" "rtlgen" "rtlopt" "machines/bobcat" "machines/i386"
	    "machines/mips" "machines/spectrum" "machines/vax")
  (make-log "/scheme/src/cref")
  (make-log "/scheme/src/edwin")
  (make-log "/scheme/src/microcode" "m" "s")
  (make-log "/scheme/src/rcs")
  (make-log "/scheme/src/runtime")
  (make-log "/scheme/src/sf")
  (make-log "/scheme/src/sicp"))