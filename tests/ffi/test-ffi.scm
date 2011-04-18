;;;-*-Scheme-*-

(load-option 'FFI)
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (compile-file "test-ffi-wrapper")
    (load "test-ffi-wrapper")))
(define-test 'ffi test-ffi)