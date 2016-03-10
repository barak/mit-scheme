#| -*-Scheme-*- |#

;;;; Compile the GDBM option.

(load-option 'CREF)
(load-option 'FFI)
(compile-file "gdbm" '() (->environment '(RUNTIME)))
(cref/generate-constructors "gdbm")