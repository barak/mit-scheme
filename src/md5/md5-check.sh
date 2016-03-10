#!/bin/sh
#
# Test the MD5 option.

set -e
${MIT_SCHEME_EXE} --prepend-library . <<\EOF
(load-option 'MD5)
(load "md5-check" (->environment '(md5)))
EOF
