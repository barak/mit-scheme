#| -*-Scheme-*- |#

;;;; Compile the PGSQL option.

(load-option 'cref)
(load-option 'ffi)
(compile-file "pgsql" '() (->environment '()))
(cref/generate-constructors "pgsql")