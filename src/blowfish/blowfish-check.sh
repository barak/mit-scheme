#!/bin/sh
#
# Test the BLOWFISH option.

set -e
${MIT_SCHEME_EXE} --prepend-library . <<\EOF
(load-option 'BLOWFISH)
(load "blowfish-check" (->environment '(blowfish)))
EOF
