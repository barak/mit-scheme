n;;; -*- Scheme -*-
;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/etc/mkband.scm,v 1.3 1989/07/01 11:05:49 cph Exp $
;;; Input file to build standard bands.
;;; NOTE: The `n' at the beginning of this file is not a typo!

(set-working-directory-pathname! "../sf")
(load "make")
n
(disk-save "../lib/runtime.com")
(set-working-directory-pathname! "../compiler")
(load "machines/bobcat/make")
n
(disk-save "../lib/compiler.com")(%exit)