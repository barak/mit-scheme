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

;;;; R7RS libraries: imports
;;; package: (runtime library imports)

(declare (usual-integrations))

(define (expand-parsed-imports imports db)
  (let ((unusable
	 (remove (lambda (import)
		   (let ((name (parsed-import-library import)))
		     (and (registered-library? name db)
			  ((registered-library name db) 'has? 'exports))))
		 imports)))
    (if (pair? unusable)
	(error "Unknown imports:" (map parsed-import-library unusable))))
  (reduce-right append!
		'()
		(map (lambda (import)
		       (expand-parsed-import import db))
		     imports)))

(define-automatic-property 'imports '(parsed-imports db)
  #f
  expand-parsed-imports)

;;; Returns a list of library-import elements.
(define (expand-parsed-import import-set db)
  (let ((converted-set
	 (let loop ((import-set import-set) (filter (lambda (name) name)))
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
	      (if (not (library-name? import-set))
		  (error "Unrecognized import set:" import-set))
	      (filter-map (lambda (export)
			    (let* ((to (library-export-to export))
				   (filtered (filter to)))
			      (and filtered
				   (make-library-import import-set
							to
							filtered))))
			  ((registered-library import-set db)
			   'get 'exports)))))))
    (if (duplicate-names? (map library-import-to converted-set))
	(error "Import set has duplicate names:" import-set))
    converted-set))

(define (duplicate-names? names)
  (and (pair? names)
       (let loop ((names (sort names symbol<?)))
	 (and (pair? (cdr names))
	      (or (eq? (car names) (cadr names))
		  (loop (cdr names)))))))