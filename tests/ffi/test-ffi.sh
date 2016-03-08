#!/bin/sh

set -e
../../src/microcode/scheme --library .:../../src/lib --batch-mode <<EOF
(begin
  (parameterize ((param:suppress-loading-message? #t))
    (load-option 'FFI))
  (compile-file "test-ffi-wrapper")
  (load "test-ffi-wrapper")
  (test-ffi))
EOF
