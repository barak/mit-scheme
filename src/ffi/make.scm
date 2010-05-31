#| -*-Scheme-*-

Build the FFI system. |#

(with-loader-base-uri (system-library-uri "ffi/")
  (lambda ()
    (load-package-set "ffi")))
(add-subsystem-identification! "FFI" '(0 1))