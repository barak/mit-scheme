#!/bin/sh
#
# Test the MHASH option.

set -e
${MIT_SCHEME_EXE} --prepend-library . <<EOF
(load-option 'MHASH)
(load "mhash-check" (->environment '(mhash)))
EOF
