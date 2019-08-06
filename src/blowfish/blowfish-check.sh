#!/bin/sh
#
# Test the Blowfish option.

set -e
${MIT_SCHEME_EXE} --prepend-library . <<\EOF
(load-option 'blowfish)
(load "blowfish-check" (->environment '(blowfish)))
EOF
