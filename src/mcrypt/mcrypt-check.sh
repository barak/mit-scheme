#!/bin/sh
#
# Test the MCRYPT option.

set -e
${MIT_SCHEME_EXE} --prepend-library . <<\EOF
(load-option 'MCRYPT)
(load "mcrypt-check" (->environment '(mcrypt)))
EOF
