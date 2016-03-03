#| -*-Scheme-*- |#

;;;; Test the MD5 option.

(load "make")
(with-system-library-directories
 '("./")
 (lambda ()
   (load "md5-check" (->environment '(md5)))))