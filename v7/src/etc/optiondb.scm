#| -*-Scheme-*-

$Id: optiondb.scm,v 1.5 2000/12/23 06:22:37 cph Exp $

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
  (let ((dirs
	 (list (directory-pathname (current-load-pathname))
	       (or (get-environment-variable "MITSCHEME_INF_DIRECTORY")
		   "/scheme/v7/linux")))
	(files
	 (if (default-object? filename)
	     (list "make" "load")
	     (list filename)))
	(test
	 (lambda (name)
	   (or (file-exists? name)
	       (there-exists? load/default-types
		 (lambda (type)
		   (file-exists?
		    (pathname-new-type name (car type)))))))))
    (lambda ()
      (if (not (name->package package-name))
	  (let dir-loop ((dirs dirs))
	    (if (pair? dirs)
		(let ((directory
		       (merge-pathnames place
					(pathname-as-directory (car dirs)))))
		  (if (file-directory? directory)
		      (let file-loop ((files files))
			(if (pair? files)
			    (if (test
				 (merge-pathnames
				  (car files)
				  (pathname-as-directory directory)))
				(with-working-directory-pathname directory
				  (lambda ()
				    (load (car files))))
				(file-loop (cdr files)))
			    (dir-loop (cdr dirs))))
		      (dir-loop (cdr dirs))))
		(error "Unable to find package directory:" place)))))))

(define-load-option 'EDWIN
  (guarded-system-loader '(edwin) "edwin"))

(define-load-option 'COMPILER
  (lambda () (load-option 'SF))
  (guarded-system-loader '(compiler) "compiler"))

(define-load-option 'CREF
  (guarded-system-loader '(cross-reference) "cref"))

(define-load-option 'IMAIL
  (guarded-system-loader '(edwin imail) "imail"))

(define-load-option 'PC-SAMPLE
  (guarded-system-loader '(pc-sample) "pcsample"))

(define-load-option 'RCS
  (guarded-system-loader '(rcs) "rcs"))

(define-load-option 'SF
  (guarded-system-loader '(scode-optimizer) "sf"))

(define-load-option 'SOS
  (guarded-system-loader '(runtime object-system) "sos"))

(define-load-option 'STUDENT
  (guarded-system-loader '(student) "6001"))

(define-load-option 'SWAT
  (guarded-system-loader '(swat) "swat"))

(define-load-option 'WIN32
  (guarded-system-loader '(win32) "win32"))

(further-load-options standard-load-options)