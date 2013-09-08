#| -*-Scheme-*- |#

;;;; Compile the MD5 wrapper.

(fluid-let ((load/suppress-loading-message? #t))
  (load-option 'CREF)
  (load-option 'FFI))

(with-system-library-directories
 '("./")
 (lambda ()
   (compile-system "md5" (directory-pathname (current-load-pathname)))))