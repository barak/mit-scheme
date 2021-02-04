#!/bin/sh
#
# Test the GDBM option.

set -e
${MIT_SCHEME_EXE} --prepend-library . <<\EOF
(load-option 'gdbm)
(load "gdbm-check" (->environment '(gdbm)))
EOF
