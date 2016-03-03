#| -*-Scheme-*- |#

;;;; Test the BLOWFISH option.

(load "make")
(with-system-library-directories
 '("./")
 (lambda ()
   (load "blowfish-check" (->environment '(blowfish)))))