#| -*-Scheme-*-

$Id: optiondb.scm,v 1.2 2000/10/16 18:16:24 cph Exp $

Copyright (c) 2000 Massachusetts Institute of Technology

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

(define (guarded-system-loader package-name place #!optional filename)
  (let ((directory
	 (merge-pathnames place
			  (directory-pathname (current-load-pathname)))))
    (lambda ()
      (if (not (name->package package-name))
	  (with-working-directory-pathname directory
	    (lambda ()
	      (load
	       (let ((test
		      (lambda (name)
			(or (file-exists? name)
			    (there-exists? load/default-types
			      (lambda (type)
				(file-exists?
				 (pathname-new-type name (car type)))))))))
		 (cond ((not (default-object? filename)) filename)
		       ((test "make") "make")
		       ((test "load") "load")
		       (else (error "Can't find loader.")))))))))))

(define-load-option 'SOS
  (guarded-system-loader '(runtime object-system) "sos"))

(define-load-option 'IMAIL
  (guarded-system-loader '(edwin imail) "imail"))

(further-load-options standard-load-options)