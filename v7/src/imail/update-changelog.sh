#!/bin/sh
scheme -large <<***EOF***
(load-option 'RCS)
(rcs-directory-log "."
		   `((CHANGELOG? #t)
		     (CHANGELOG-MAP
		      ("zurich.ai.mit.edu"
		       ("cph" "Chris Hanson")))))
***EOF***
