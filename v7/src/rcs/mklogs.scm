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
  (make-log "/scheme/microcode" "m" "s")
  (make-log "/scheme/runtime")
  (make-log "/scheme/sf")
  (make-log "/scheme/cref")
  (make-log "/scheme/edwin")
  (make-log "/scheme/sicp")
  (make-log "/scheme/compiler" "back" "base" "documentation" "etc" "fggen"
	    "fgopt" "rtlbase" "rtlgen" "rtlopt" "machines/bobcat"
	    "machines/mips" "machines/spectrum" "machines/vax"))