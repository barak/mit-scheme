#| -*-Scheme-*- |#

;;;; Test the GDBM adapter.

(load "make")
(with-system-library-directories
 '("./")
 (lambda ()
   (load "gdbm-check" (->environment '(gdbm)))))