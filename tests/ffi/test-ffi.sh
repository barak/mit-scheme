#!/bin/sh

set -e
${MIT_SCHEME_EXE} --library .:${MITSCHEME_LIBRARY_PATH} --batch-mode <<EOF
(begin
  (parameterize ((param:suppress-loading-message? #t))
    (load-option 'ffi))
  (compile-file "test-ffi-wrapper")
  (load "test-ffi-wrapper")
  (test-ffi))
EOF
