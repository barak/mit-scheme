#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Build utilities
;;; package: (scode-optimizer build-utilities)

(declare (usual-integrations))

(define (directory-processor input-type output-type process-file
			     #!optional map-pathname)
  (let ((directory-read
	 (let ((input-pattern
		(make-pathname #f #f #f 'wild input-type 'newest)))
	   (lambda (directory)
	     (directory-read
	      (merge-pathnames
	       (pathname-as-directory (merge-pathnames directory))
	       input-pattern)))))
	(map-pathname
	 (if (default-object? map-pathname)
	     (lambda (pathname) pathname)
	     map-pathname)))
    (lambda (input-directory #!optional output-directory force?)
      (let ((output-directory
	     (if (default-object? output-directory) #f output-directory))
	    (force? (if (default-object? force?) #f force?))
	    (output-type (output-type)))
	(for-each (lambda (pathname)
		    (if (or force?
			    (not (file-modification-time<=?
				  (pathname-default-type pathname input-type)
				  (map-pathname
				   (let ((output-pathname
					  (pathname-new-type pathname
							     output-type)))
				     (if output-directory
					 (merge-pathnames output-directory
							  output-pathname)
					 output-pathname))))))
			(process-file pathname output-directory)))
		  (if (pair? input-directory)
		      (append-map! directory-read input-directory)
		      (directory-read input-directory)))))))

(define sf-directory
  (directory-processor
   "scm"
   (lambda () (if sf/cross-compiling? "nib" "bin"))
   (lambda (pathname output-directory)
     (sf pathname output-directory))
   (lambda (pathname)
     (merge-pathnames
      (enough-pathname (merge-pathnames pathname) sf/source-root)
      sf/object-root))))

(define (sf-conditionally filename #!optional echo-up-to-date?)
  (let ((kernel
	 (lambda (filename)
	   (receive (input output spec) (sf/pathname-defaulting filename #f #f)
	     spec
	     (cond ((not (file-modification-time<=? input output))
		    (sf filename))
		   ((and (not (default-object? echo-up-to-date?))
			 echo-up-to-date?)
		    (newline)
		    (write-string "Syntax file: ")
		    (write filename)
		    (write-string " is up to date")))))))
    (if (pair? filename)
	(for-each kernel filename)
	(kernel filename))))