#!/bin/sh
# -*-Scheme-*-
#
# Test the X11-SCREEN option.

set -e
${MIT_SCHEME_EXE} --prepend-library . <<\EOF
(begin
  (if (let ((display (get-environment-variable "DISPLAY")))
	(or (not (string? display))
	    (string-null? display)))
      (warn "DISPLAY not set")
      (let ((edwin (->environment '(edwin))))
	(set! (access os/init-file-name edwin)
	      (let ((pathname (merge-pathnames "x11-screen-test.scm")))
		(named-lambda (os/init-file-name/x11-screen-test)
		  pathname)))
	(edit)))
  )
EOF
