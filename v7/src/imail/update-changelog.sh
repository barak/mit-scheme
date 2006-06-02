#!/bin/sh
if [ -z "${SCHEME_LARGE}" ]; then
    SCHEME_LARGE="scheme --large"
fi
${SCHEME_LARGE} <<***EOF***
(load-option 'RCS)
(rcs-directory-log "."
		   '((CHANGELOG? #t)
		     (CHANGELOG-MAP
		      ("zurich.ai.mit.edu"
		       ("cph" "Chris Hanson")))))
***EOF***
