#!/bin/sh
#
# Test the GDBM2 option.

set -e
${MIT_SCHEME_EXE} --prepend-library . <<\EOF
(load-option 'GDBM2)
(load "gdbm-check" (->environment '(gdbm)))
EOF
