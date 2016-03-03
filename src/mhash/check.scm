#| -*-Scheme-*- |#

;;;; Test the MHASH option.

(load "make")
(with-system-library-directories
 '("./")
 (lambda ()
   (load "mhash-check" (->environment '(mhash)))))