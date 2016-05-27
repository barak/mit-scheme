#!/bin/sh
#
# Test the X11 option.

set -e
${MIT_SCHEME_EXE} --prepend-library . <<\EOF
(begin
  (load-option 'X11)

  (if (let ((display (get-environment-variable "DISPLAY")))
	(or (not (string? display))
	    (string-null? display)))
      (warn "DISPLAY not set")
      (let ((dev (make-graphics-device)))
	  (if (not (eq? 'X11 (graphics-type-name (graphics-type dev))))
	      (error "The X11 graphics type is NOT the default."))
	  (graphics-draw-point dev 0 .1)
	  (graphics-draw-point dev 0 .2)
	  (graphics-draw-point dev 0 .3)
	  (graphics-erase-point dev 0 .2)
	  (graphics-draw-text dev 0. .4 "Hello!")
	  (graphics-draw-line dev -.5 -.5 .5 .5)
	  (graphics-move-cursor dev -.5 .5)
	  (graphics-drag-cursor dev .5 -.5)
	  (display "Waiting for graphics window to close...\n")
	  (let wait ()
	    (sleep-current-thread 1000)
	    (if ((access x-window/xw (->environment '(runtime x-graphics)))
		 (graphics-device/descriptor dev))
		(wait)))
	  (display "Graphics window closed.\n")))
  )
EOF
