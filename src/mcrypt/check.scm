#| -*-Scheme-*- |#

;;;; Test the mcrypt wrapper.

(load "make")
(with-system-library-directories
 '("./")
 (lambda ()
   (load "mcrypt-check" (->environment '(mcrypt)))))