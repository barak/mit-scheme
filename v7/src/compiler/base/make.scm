#| -*-Scheme-*-

$Id: make.scm,v 4.110 1999/01/03 05:23:02 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Compiler: System Construction

(declare (usual-integrations))

(lambda (architecture-name)
  ((access with-directory-rewriting-rule
	   (->environment '(RUNTIME COMPILER-INFO)))
   (working-directory-pathname)
   (pathname-as-directory "compiler")
   (lambda ()
     (load-option 'COMPRESS)
     (load-option 'HASH-TABLE)
     (load-option 'RB-TREE)
     (package/system-loader "compiler" '() 'QUERY)))
  (let ((initialize-package!
	 (lambda (package-name)
	   ((environment-lookup (->environment package-name)
				'INITIALIZE-PACKAGE!)))))
    (initialize-package! '(COMPILER MACROS))
    (initialize-package! '(COMPILER DECLARATIONS)))
  (add-identification! (string-append "Liar (" architecture-name ")") 4 110))