#| -*-Scheme-*-

$Id: compile.scm,v 1.4 2002/03/02 04:21:44 cph Exp $

Copyright (c) 2000-2002 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; Program to compile MIT Scheme.

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
	    '("runtime" "sf" "compiler" "edwin" "6001"))
  (with-working-directory-pathname "cref"
    (lambda ()
      (if (not (file-exists? "cref.con"))
	  (load "cref.sf"))))
  (for-each (lambda (name)
	      (load (merge-pathnames "compile" (pathname-as-directory name))))
	    '("sos" "star-parser" "imail" "xml"))
  (with-working-directory-pathname "runtime-check"
    (lambda ()
      (load "runtime.cbf"))))