#!/bin/sh
# -*-Scheme-*-
#
# Test the MCRYPT option.

set -e
${MIT_SCHEME_EXE} --prepend-library . <<\EOF
(begin
  (load-option 'synchronous-subprocess)
  (load-option 'mcrypt)
  (load "mcrypt-check" (->environment '(mcrypt))))
EOF
