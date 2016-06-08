#!/bin/sh
# -*-Scheme-*-
#
# Test the X11 option.

set -e
${MIT_SCHEME_EXE} --prepend-library . <<\EOF
(begin
  (if (let ((display (get-environment-variable "DISPLAY")))
	(or (not (string? display))
	    (string-null? display)))
      (warn "DISPLAY not set")
      (begin
	(load-option 'X11)
	(load "x11-test.scm" (->environment '(x11)))))
  )
EOF
