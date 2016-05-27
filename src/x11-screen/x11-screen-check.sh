#!/bin/sh
#
# Test the X11-SCREEN option.

set -e
${MIT_SCHEME_EXE} --prepend-library . <<\EOF
(begin
  (load-option 'X11-SCREEN)

  (if (let ((display (get-environment-variable "DISPLAY")))
	(or (not (string? display))
	    (string-null? display)))
      (warn "DISPLAY not set")
      (edit))
  )
EOF
