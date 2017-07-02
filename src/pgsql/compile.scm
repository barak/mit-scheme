#| -*-Scheme-*- |#

;;;; Compile the PGSQL option.

(load-option 'CREF)
(load-option 'FFI)
(compile-file "pgsql" '() (->environment '()))
(cref/generate-constructors "pgsql")