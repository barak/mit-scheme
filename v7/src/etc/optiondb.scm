#| -*-Scheme-*-

$Id: optiondb.scm,v 1.13 2004/11/01 19:22:29 cph Exp $

Copyright 2000,2001,2002,2004 Massachusetts Institute of Technology

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

(define (guarded-system-loader package-name place #!optional filename)
  (let ((dirs
	 (let ((here (directory-pathname (current-load-pathname))))
	   `(,here
	     ,@(let ((d (pathname-directory here)))
		 (if (pair? (cdr d))
		     (list (pathname-new-directory here (except-last-pair d)))
		     '()))
	     ,@(let ((d (get-environment-variable "MITSCHEME_INF_DIRECTORY")))
		 (if d
		     (list d)
		     '()))
	     "/usr/local/scheme/linux"
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

(define-load-option '*PARSER
  (guarded-system-loader '(runtime *parser) "star-parser"))

(define-load-option 'PC-SAMPLE
  (guarded-system-loader '(pc-sample) "pcsample"))

(define-load-option 'RCS
  (guarded-system-loader '(rcs) "rcs"))

(define-load-option 'SF
  (guarded-system-loader '(scode-optimizer) "sf"))

(define-load-option 'SOS
  (guarded-system-loader '(sos) "sos"))

(define-load-option 'SSP
  (guarded-system-loader '(runtime ssp) "ssp"))

(define-load-option 'STUDENT
  (guarded-system-loader '(student) "6001"))

(define-load-option 'SWAT
  (guarded-system-loader '(swat) "swat"))

(define-load-option 'WIN32
  (guarded-system-loader '(win32) "win32"))

(define-load-option 'XDOC
  (guarded-system-loader '(runtime ssp xdoc) "xdoc"))

(define-load-option 'XML
  (guarded-system-loader '(runtime xml) "xml"))

(further-load-options standard-load-options)