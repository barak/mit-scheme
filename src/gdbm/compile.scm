#| -*-Scheme-*- |#

;;;; Compile the GDBM wrapper.

(fluid-let ((load/suppress-loading-message? #t))
  (load-option 'CREF)
  (load-option 'FFI))

(with-system-library-directories
 '("./")
 (lambda ()
   (compile-system "gdbm" (directory-pathname (current-load-pathname)))))