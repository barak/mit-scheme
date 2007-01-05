#| -*-Scheme-*-

$Id: make.scm,v 1.29 2007/01/05 15:33:05 cph Exp $

Copyright 1988,1989,1990,1991,1993,1994 Massachusetts Institute of Technology
Copyright 1995,1996,1998,1999,2000,2001 Massachusetts Institute of Technology
Copyright 2002,2004 Massachusetts Institute of Technology

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

;;;; Package Model: System Construction

(declare (usual-integrations))

(load-option 'RB-TREE)
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    ((access with-directory-rewriting-rule
	     (->environment '(RUNTIME COMPILER-INFO)))
     (working-directory-pathname)
     (pathname-as-directory "cref")
     (lambda ()
       (load-package-set "cref")))))
(add-subsystem-identification! "CREF" '(2 3))