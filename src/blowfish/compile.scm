#| -*-Scheme-*- |#

;;;; Compile the Blowfish wrapper.

(fluid-let ((load/suppress-loading-message? #t))
  (load-option 'CREF)
  (load-option 'FFI))

(with-system-library-directories
 '("./")
 (lambda ()
   (compile-system "blowfish" (directory-pathname (current-load-pathname)))))