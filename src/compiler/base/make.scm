#| -*-Scheme-*-

$Id: make.scm,v 4.122 2003/04/25 03:50:34 cph Exp $

Copyright (c) 1991,1992,1993,1994,1997 Massachusetts Institute of Technology
Copyright (c) 1998,1999,2001,2002,2003 Massachusetts Institute of Technology

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
  ((access with-directory-rewriting-rule
	   (->environment '(RUNTIME COMPILER-INFO)))
   (working-directory-pathname)
   (pathname-as-directory "compiler")
   (lambda ()
     (load-option 'COMPRESS)
     (load-option 'RB-TREE)
     (load-package-set "compiler")))
  (add-identification! "LIAR" 4 116))