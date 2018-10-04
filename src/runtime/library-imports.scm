#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018 Massachusetts Institute of Technology

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

(define (expand-import-sets import-sets library-db)
  (library-db 'require-metadata (import-sets->libraries import-sets))
  (let ((converted-sets
	 (map (lambda (import-set)
		(expand-import-set import-set library-db))
	      import-sets)))
    (let ((intersections (find-intersections converted-sets)))
      (if (pair? intersections)
	  (error "Import sets intersect:"
		 (unconvert-intersections intersections
					  converted-sets
					  import-sets))))
    (append-map (lambda (set) set) converted-sets)))

(define (import-sets->libraries import-sets)
  (delete-duplicates (map import-set->library import-sets)
		     equal?))

(define (import-set->library import-set)
  (case (car import-set)
    ((library) (cadr import-set))
    ((only except prefix rename) (import-set->library (cadr import-set)))
    (else (error "Unrecognized import set:" import-set))))

(define (find-intersections converted-sets)
  (if (pair? converted-sets)
      (let* ((links1 (car converted-sets))
	     (names1 (map library-import-to links1)))
	(append (filter-map (lambda (links2)
			      (and (intersecting-names?
				    names1
				    (map library-import-to links2))
				   (list links1 links2)))
			    (cdr converted-sets))
		(find-intersections (cdr converted-sets))))
      '()))

(define (intersecting-names? names1 names2)
  (pair? (lset-intersection eq? names1 names2)))

(define (unconvert-intersections intersections converted-sets imported-sets)
  (let ((alist (map cons converted-sets imported-sets)))
    (map (lambda (intersection)
	   (map (lambda (converted-set)
		  (cdr (assq converted-set alist)))
		intersection))
	 intersections)))

;;; Returns a list of (<to-name> <from-name> <from-library>) elements.
(define (expand-import-set import-set library-db)
  (let ((converted-set
	 (let loop ((import-set import-set) (filter (lambda (name) name)))
	   (case (car import-set)
	     ((library)
	      (let ((library-name (cadr import-set)))
		(filter-map (lambda (name)
			      (let ((filtered (filter name)))
				(and filtered
				     (make-library-import filtered
							  name
							  library-name))))
			    (library-metadata-exports
			     (library-db 'get-metadata library-name)))))
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
			       (cdr p)
			       name)))))))
	     (else
	      (error "Unrecognized import set:" import-set))))))
    (if (duplicate-names? (map library-import-to converted-set))
	(error "Import set has duplicate names:" import-set))
    converted-set))

(define (duplicate-names? names)
  (and (pair? names)
       (let loop ((names (sort names symbol<?)))
	 (and (pair? (cdr names))
	      (or (eq? (car names) (cadr names))
		  (loop (cdr names)))))))

(define-record-type <library-import>
    (make-library-import to from from-library)
    library-import?
  (to library-import-to)
  (from library-import-from)
  (from-library library-import-from-library))

(define-print-method library-import?
  (standard-print-method 'library-import
    (lambda (import)
      (list (library-import-to import)
	    (library-import-from import)
	    (library-import-from-library import)))))

(define (library-import=? e1 e2)
  (and (eq? (library-import-to e1)
	    (library-import-to e2))
       (eq? (library-import-from e1)
	    (library-import-from e2))
       (equal? (library-import-from-library e1)
	       (library-import-from-library e2))))