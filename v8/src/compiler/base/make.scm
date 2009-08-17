#| -*-Scheme-*-

$Id: c776639c2956491e18347d47029a12b6fa5f4462 $

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
  (let ((core
	 (lambda ()
	   (load-option 'COMPRESS)
	   (load-option 'HASH-TABLE)
	   (load-option 'RB-TREE)
	   (package/system-loader "compiler" '() 'QUERY))))
    #|
    ((access with-directory-rewriting-rule
	     (->environment '(RUNTIME COMPILER-INFO)))
     (working-directory-pathname)
     (pathname-as-directory "compiler")
     core)
    |#
    (core)
    (let ((initialize-package!
	   (lambda (package-name)
	     ((environment-lookup (->environment package-name)
				  'INITIALIZE-PACKAGE!)))))
      (initialize-package! '(COMPILER MACROS))
      (initialize-package! '(COMPILER DECLARATIONS)))
    (add-identification! (string-append "Liar (" 
					(if (procedure? architecture-name)
					    (architecture-name)
					    architecture-name)
					")")
			 5 0)))