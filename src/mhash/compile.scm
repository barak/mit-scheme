#| -*-Scheme-*- |#

;;;; Compile the MHASH option.

(load-option 'CREF)
(load-option 'FFI)
(compile-file "mhash" '() (->environment '(RUNTIME)))
(cref/generate-constructors "mhash" 'ALL)