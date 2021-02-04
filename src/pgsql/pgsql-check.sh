#!/bin/sh
#
# Test the PostgreSQL option.

set -e
${MIT_SCHEME_EXE} --prepend-library . <<\EOF
(load-option 'pgsql)
(load "pgsql-check" (->environment '(postgresql)))
EOF
