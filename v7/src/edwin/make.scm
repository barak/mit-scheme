#| -*-Scheme-*-

$Id: make.scm,v 3.106 2001/02/28 20:03:21 cph Exp $

Copyright (c) 1989-2001 Massachusetts Institute of Technology

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

;;;; Edwin: System Construction

(declare (usual-integrations))

(with-working-directory-pathname
    (directory-pathname (current-load-pathname))
  (lambda ()
    ((access with-directory-rewriting-rule
	     (->environment '(RUNTIME COMPILER-INFO)))
     (working-directory-pathname)
     (pathname-as-directory "edwin")
     (lambda ()
       (declare-shared-library "edwin" (lambda () true))
       (package/system-loader
	"edwin"
	`((os-type . ,microcode-id/operating-system)
	  (make-constructor-name
	   . ,(lambda (pathname)
		(pathname-new-name pathname
				   (case microcode-id/operating-system
				     ((DOS) "edwindos")
				     ((NT) "edwinw32")
				     ((OS/2) "edwinos2")
				     ((UNIX) "edwinunx")
				     (else "edwinunk"))))))
	'QUERY)))))
(add-identification! "Edwin" 3 107)