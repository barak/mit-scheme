#| -*-Scheme-*- |#

;;;; Test the MCRYPT option.

(load "make")
(with-system-library-directories
 '("./")
 (lambda ()
   (load "mcrypt-check" (->environment '(mcrypt)))))