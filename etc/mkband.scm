;;; -*- Scheme -*-
;;; $Id: mkband.scm,v 1.6 2000/03/21 17:32:59 cph Exp $
;;; Input file to build standard bands.

(set-working-directory-pathname! "../sf")
(load "make")
(disk-save "../lib/runtime.com")
(%exit)