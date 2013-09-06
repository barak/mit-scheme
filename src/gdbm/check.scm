#| -*-Scheme-*- |#

;;;; Test the GDBM wrapper.

(load "make")
(with-system-library-directories
 '("./")
 (lambda ()
   (load "gdbm-check" (->environment '(gdbm)))))