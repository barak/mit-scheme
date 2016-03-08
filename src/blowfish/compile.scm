#| -*-Scheme-*- |#

;;;; Compile the BLOWFISH option.

(load-option 'CREF)
(load-option 'FFI)
(compile-file "blowfish" '() (->environment '(RUNTIME)))
(cref/generate-constructors "blowfish" 'ALL)