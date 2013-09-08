#| -*-Scheme-*- |#

;;;; Test the MD5 wrapper.

(load "make")
(with-system-library-directories
 '("./")
 (lambda ()
   (load "md5-check" (->environment '(md5)))))