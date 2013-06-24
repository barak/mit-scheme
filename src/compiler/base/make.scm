#| -*-Scheme-*-

$Id: make.scm,v 4.124 2004/12/13 03:22:21 cph Exp $

Copyright 1991,1992,1993,1994,1997,1998 Massachusetts Institute of Technology
Copyright 1999,2001,2002,2003,2004 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Compiler: System Construction

(declare (usual-integrations))

(lambda (architecture-name)
  architecture-name
  (load-option 'COMPRESS)
  (load-option 'RB-TREE)
  ((access with-directory-rewriting-rule
	   (->environment '(RUNTIME COMPILER-INFO)))
   (working-directory-pathname)
   (pathname-as-directory "compiler")
   (lambda ()
     (load-package-set "compiler")))
  (add-subsystem-identification! "LIAR" '(4 117)))
