#| -*-Scheme-*- |#

;;;; Compile the mhash wrapper.

(fluid-let ((load/suppress-loading-message? #t))
  (load-option 'CREF)
  (load-option 'FFI))

(with-system-library-directories
 '("./")
 (lambda ()
   (compile-system "mhash" (directory-pathname (current-load-pathname)))))