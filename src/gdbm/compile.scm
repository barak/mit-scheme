#| -*-Scheme-*- |#

;;;; Compile the GDBM adapter

(fluid-let ((load/suppress-loading-message? #t))
  (load-option 'CREF)
  (load-option 'FFI))

(compile-system "gdbm" (directory-pathname (current-load-pathname)))