#| -*-Scheme-*-

$Id: optiondb.scm,v 1.3 2000/12/07 21:56:29 cph Exp $

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
	 (let ((here (directory-pathname (current-load-pathname))))
	   (let ((directory
		  (pathname-new-directory here
					  (append (pathname-directory here)
						  (list 'UP place)))))
	     (if (file-directory? directory)
		 directory
		 (merge-pathnames place here))))))
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

(define-load-option 'CREF
  (guarded-system-loader '(cross-reference) "cref"))

(define-load-option 'COMPILER
  (lambda () (load-option 'SF))
  (guarded-system-loader '(compiler) "compiler"))

(define-load-option 'EDWIN
  (guarded-system-loader '(edwin) "edwin"))

(define-load-option 'IMAIL
  (guarded-system-loader '(edwin imail) "imail"))

(define-load-option 'RCS
  (guarded-system-loader '(rcs) "rcs"))

(define-load-option 'SF
  (guarded-system-loader '(scode-optimizer) "sf"))

(define-load-option 'STUDENT
  (guarded-system-loader '(student) "6001"))

(define-load-option 'SOS
  (guarded-system-loader '(runtime object-system) "sos"))

(further-load-options standard-load-options)