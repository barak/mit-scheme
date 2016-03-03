#| -*-Scheme-*- |#

;;;; Test the GDBM option.

(load "make")
(with-system-library-directories
 '("./")
 (lambda ()
   (load "gdbm-check" (->environment '(gdbm)))))