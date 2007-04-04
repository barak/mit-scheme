#| -*-Scheme-*-

$Id: c-compile.scm,v 1.1 2007/04/04 05:08:19 riastradh Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;; This generates correct debugging information for the parts of the
;;; Scheme code that were statically linked to the microcode, and also
;;; compiles the rest of the system that was not statically linked.

;;; (Ideally, this file would be identical to compile.scm.  But we do
;;; not live in an ideal world, and we shall have to settle for a file
;;; that is merely very similar.  The main difference is that we need
;;; to avoid running the C compiler on some of the files but not
;;; others.  This is bogus.)

(fluid-let ((compiler:invoke-c-compiler? #f))
  (define (compile-package-descriptions subsystem)
    (for-each
     (lambda (os-suffix)
       (cbf (pathname-new-type (string-append subsystem "-" os-suffix)
			       "pkd")))
     '("unx" "w32" "os2")))
  (with-working-directory-pathname "cref"
    (lambda ()
      (load "cref.sf")
      (load "cref.cbf")
      ;; This conditional is probably not necessary.
      (if (not (name->package '(CROSS-REFERENCE)))
	  (load "make"))))
  (for-each
   (lambda (subsystem)
     (with-working-directory-pathname subsystem
       (lambda ()
	 (load (pathname-new-type subsystem "sf"))
	 (load (pathname-new-type subsystem "cbf"))
	 (if (string=? subsystem "compiler")     ;++ kludge
	     (cbf "compiler-unx.pkd")
	     (compile-package-descriptions subsystem)))))
   '("runtime" "win32" "sf" "compiler" "edwin" "6001"))
  (with-working-directory-pathname "star-parser"
    (lambda ()
      (load "compile")
      (compile-package-descriptions "parser")))
  (for-each (lambda (subsystem)
	      (load (merge-pathnames "compile"
				     (pathname-as-directory subsystem)))
	      (with-working-directory-pathname subsystem
		(lambda ()
		  (compile-package-descriptions subsystem))))
	    '("sos" "imail" "xml" "ssp" "xdoc")))
