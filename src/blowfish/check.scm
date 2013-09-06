#| -*-Scheme-*- |#

;;;; Test the Blowfish wrapper.

(load "make")
(with-system-library-directories
 '("./")
 (lambda ()
   (load "blowfish-check" (->environment '(blowfish)))))