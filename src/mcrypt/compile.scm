#| -*-Scheme-*- |#

;;;; Compile the MCRYPT option.

(load-option 'CREF)
(load-option 'FFI)
(compile-file "mcrypt" '() (->environment '(RUNTIME)))
(cref/generate-constructors "mcrypt" 'ALL)