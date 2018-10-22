#| -*-Scheme-*- |#

;;;; Compile the BLOWFISH option.

(load-option 'cref)
(load-option 'ffi)
(compile-file "blowfish" '() (->environment '(runtime)))
(cref/generate-constructors "blowfish")