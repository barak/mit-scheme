n;;; -*- Scheme -*-
;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/etc/mkband.scm,v 1.4 1989/07/26 23:14:57 cph Rel $
;;; Input file to build standard bands.
;;; NOTE: The `n' at the beginning of this file is not a typo!

(set-working-directory-pathname! "../sf")
(load "make")
n
(disk-save "../lib/runtime.com")
(%exit)