#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; R7RS libraries: import/export
;;; package: (runtime library import/export r7rs)

(declare (usual-integrations))

(define (r7rs-parsed-import-libraries parsed-import library libraries)
  (declare (ignore library))
  (fold import-set-library libraries (cdr parsed-import)))

(define (import-set-library import-set libraries)
  (library-name-adjoin (let loop ((import-set import-set))
			 (if (library-name? import-set)
			     import-set
			     (loop (cadr import-set))))
		       libraries))

(define (r7rs-expand-parsed-import parsed-import db library imports)
  (declare (ignore library))
  (fold (lambda (import-set imports)
	  (expand-import-set import-set db imports))
	imports
	(cdr parsed-import)))

(define (expand-import-set import-set db imports)
  (let loop ((import-set import-set) (filter (lambda (name) name)))
    (if (library-name? import-set)
	(fold (lambda (export imports)
		(let ((to (filter (library-ixport-to export))))
		  (if to
		      (lset-adjoin library-ixport=?
				   imports
				   (make-library-ixport
				    (library-ixport-from-library export)
				    (library-ixport-to export)
				    to))
		      imports)))
	      imports
	      (get-exports import-set db))
	(case (car import-set)
	  ((only)
	   (loop (cadr import-set)
		 (let ((names (cddr import-set)))
		   (lambda (name)
		     (and (memq name names)
			  (filter name))))))
	  ((except)
	   (loop (cadr import-set)
		 (let ((names (cddr import-set)))
		   (lambda (name)
		     (and (not (memq name names))
			  (filter name))))))
	  ((prefix)
	   (loop (cadr import-set)
		 (let ((prefix (caddr import-set)))
		   (lambda (name)
		     (filter (symbol prefix name))))))
	  ((rename)
	   (loop (cadr import-set)
		 (let ((renames (cddr import-set)))
		   (lambda (name)
		     (filter
		      (let ((p (assq name renames)))
			(if p
			    (cadr p)
			    name)))))))
	  (else
	   (error "Unrecognized import set:" import-set))))))

(define (r7rs-parsed-export-libraries parsed-export library libraries)
  (declare (ignore parsed-export library))
  libraries)

(define (r7rs-expand-parsed-export parsed-export names library)
  (declare (ignore names))
  (make-export-group
   (cadr parsed-export)
   (map (lambda (spec)
	  (if (symbol? spec)
	      (make-library-ixport (library-name library) spec)
	      (case (car spec)
		((rename)
		 (make-library-ixport (library-name library)
				      (cadr spec)
				      (caddr spec)))
		(else
		 (error "Unrecognized export spec:" spec)))))
	(cddr parsed-export))))