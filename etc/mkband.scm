;;; -*- Scheme -*-
;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/etc/mkband.scm,v 1.5 1990/12/12 03:07:58 cph Exp $
;;; Input file to build standard bands.

(set-working-directory-pathname! "../sf")
(load "make")
(disk-save "../lib/runtime.com")
(%exit)