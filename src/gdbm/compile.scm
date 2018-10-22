#| -*-Scheme-*- |#

;;;; Compile the GDBM option.

(load-option 'cref)
(load-option 'ffi)
(compile-file "gdbm" '() (->environment '(runtime)))
(cref/generate-constructors "gdbm")