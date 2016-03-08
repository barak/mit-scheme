#| -*-Scheme-*- |#

;;;; Compile the MD5 option.

(load-option 'CREF)
(load-option 'FFI)
(compile-file "md5" '() (->environment '(RUNTIME)))
(cref/generate-constructors "md5" 'ALL)