#| -*-Scheme-*-

$Id: compile.scm,v 1.11 2007/01/05 15:33:06 cph Exp $

Copyright 2000,2001,2002,2004,2006 Massachusetts Institute of Technology

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

;;;; Program to compile MIT/GNU Scheme.

;;; This compiles the part of the system written in Scheme.
;;; The part written in C is compiled using "make".

(begin
  (with-working-directory-pathname "cref"
    (lambda ()
      (load "cref.sf")
      (load "cref.cbf")
      (if (not (name->package '(CROSS-REFERENCE)))
	  (load "make"))))
  (for-each (lambda (name)
	      (with-working-directory-pathname name
		(lambda ()
		  (load (pathname-new-type name "sf"))
		  (load (pathname-new-type name "cbf")))))
	    '("runtime" "win32" "sf" "compiler" "edwin" "6001"))
  (with-working-directory-pathname "cref"
    (lambda ()
      (if (not (file-exists? "cref.con"))
	  (load "cref.sf"))))
  (for-each (lambda (name)
	      (load (merge-pathnames "compile" (pathname-as-directory name))))
	    '("sos" "star-parser" "imail" "xml" "ssp" "xdoc"))
  (with-working-directory-pathname "runtime-check"
    (lambda ()
      (load "runtime.cbf"))))