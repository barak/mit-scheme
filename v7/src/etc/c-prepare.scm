#| -*-Scheme-*-

$Id: c-prepare.scm,v 1.1 2007/04/04 05:08:19 riastradh Exp $

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

;;; This compiles the Scheme code that will be statically linked to
;;; the microcode when using the C back end.

(fluid-let ((compiler:invoke-c-compiler? #f))
  (define (compile-package-descriptions subsystem)
    (for-each (lambda (os-suffix)
		(cbf (pathname-new-type (string-append subsystem "-" os-suffix)
					"pkd")))
	      '("unx" "w32" "os2")))
  (with-working-directory-pathname "microcode"
    (lambda ()
      (if (or (not (file-exists? "utabmd.bin"))
	      (> (file-modification-time-indirect "utabmd.scm")
		 (file-modification-time-indirect "utabmd.bin")))
	  (sf "utabmd"))
      (cbf "utabmd")))
  (for-each (lambda (subsystem)
	      (with-working-directory-pathname subsystem
		(lambda ()
		  (load (pathname-new-type subsystem "sf"))
		  (load (pathname-new-type subsystem "cbf"))
		  (compile-package-descriptions subsystem))))
	    '("runtime" "sf" "cref"))
  (with-working-directory-pathname "compiler"
    (lambda ()
      (load "compiler.sf")
      (load "compiler.cbf")
      (cbf "compiler-unx.pkd")))
  (with-working-directory-pathname "star-parser"
    (lambda ()
      (load "compile")
      (compile-package-descriptions "parser"))))
