;;; -*- Scheme -*-
;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/etc/mkband.scm,v 1.2 1988/03/31 04:39:47 cph Exp $
;;; Input file to build standard bands.
((access finish-load runtime-system))
(set-working-directory-pathname! "../sf")
(load "make.bin" system-global-environment)
(set-working-directory-pathname! "../runtime")
(disk-save "runtime.bin")
(load "sbuild.bin" system-global-environment)
(disk-save "sicp.bin")
(%exit)