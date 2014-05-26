#| -*-Scheme-*- |#

;;;; Test the mhash wrapper.

(load "make")
(with-system-library-directories
 '("./")
 (lambda ()
   (load "mhash-check" (->environment '(mhash)))))