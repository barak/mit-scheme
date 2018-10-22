#| -*-Scheme-*- |#

;;;; Compile the MCRYPT option.

(load-option 'cref)
(load-option 'ffi)
(compile-file "mcrypt" '() (->environment '(runtime)))
(cref/generate-constructors "mcrypt")