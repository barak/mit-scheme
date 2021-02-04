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

;;;; Libraries: import/export
;;; package: (runtime library import/export)

(declare (usual-integrations))

(define (make-library-ixport from-library from #!optional to)
  (guarantee library-name? from-library 'make-library-ixport)
  (guarantee symbol? from 'make-library-ixport)
  (%make-library-ixport from-library from
			(if (default-object? to)
			    from
			    (begin
			      (guarantee symbol? to 'make-library-ixport)
			      to))))

(define-record-type <library-ixport>
    (%make-library-ixport from-library from to)
    library-ixport?
  (from-library library-ixport-from-library)
  (from library-ixport-from)
  (to library-ixport-to))

(define (library-ixport=? ixport1 ixport2)
  (and (library-name=? (library-ixport-from-library ixport1)
		       (library-ixport-from-library ixport2))
       (eq? (library-ixport-from ixport1)
	    (library-ixport-from ixport2))
       (eq? (library-ixport-to ixport1)
	    (library-ixport-to ixport2))))

(define (library-ixport->list ixport)
  (cons* (library-ixport-from-library ixport)
	 (library-ixport-from ixport)
	 (if (eq? (library-ixport-from ixport) (library-ixport-to ixport))
	     '()
	     (list (library-ixport-to ixport)))))

(define (list->library-ixport list)
  (apply make-library-ixport list))

(define (library-ixports->library-names ixports)
  (fold (lambda (ixport libraries)
	  (lset-adjoin library-name=?
		       libraries
		       (library-ixport-from-library ixport)))
	'()
	ixports))

(define (library-ixports->libraries ixports db)
  (let ((names (library-ixports->library-names ixports)))
    (let ((unregistered
	   (remove (lambda (name)
		     (registered-library? name db))
		   names)))
      (if (pair? unregistered)
	  (error "Unknown libraries:" unregistered)))
    (map (lambda (name) (registered-library name db))
	 names)))

(define-print-method library-ixport?
  (standard-print-method 'library-ixport
    library-ixport->list))

(define (expand-parsed-imports parsed-imports db #!optional library)
  (let ((library (if (default-object? library) #f library)))
    (check-libraries-exist parsed-import-libraries parsed-imports db library)
    (let ((imports
	   (fold (lambda (parsed-import imports)
		   (expand-parsed-import parsed-import db library imports))
		 '()
		 parsed-imports)))
      (check-dupes "imports"
		   library-ixport-to
		   (lambda (import libraries)
		     (lset-adjoin library-name=?
				  libraries
				  (library-ixport-from-library import)))
		   imports)
      imports)))

(define-automatic-property 'imports '(parsed-imports db library)
  #f
  expand-parsed-imports)

(define (parsed-import-libraries parsed-import library libraries)
  ((case (car parsed-import)
     ((r7rs-import) r7rs-parsed-import-libraries)
     ((mit-import) mit-parsed-import-libraries)
     (else (error "Unknown parsed import:" parsed-import)))
   parsed-import library libraries))

(define (expand-parsed-import parsed-import db library imports)
  ((case (car parsed-import)
     ((r7rs-import) r7rs-expand-parsed-import)
     ((mit-import) mit-expand-parsed-import)
     (else (error "Unknown parsed import:" parsed-import)))
   parsed-import db library imports))

(define-automatic-property 'exports
  '(parsed-exports bound-names imports library)
  #f
  (lambda (parsed-exports bound-names imports library)
    (check-libraries-exist parsed-export-libraries
			   parsed-exports
			   (library-db library)
			   library)
    (let* ((imports-to (map library-ixport-to imports))
	   (names (lset-union eq? bound-names imports-to))
	   (exports
	    (fold (lambda (parsed-export exports)
		    (expand-parsed-export parsed-export names library exports))
		  '()
		  parsed-exports)))
      (let ((missing
	     (lset-difference eq?
			      (map library-ixport-from exports)
			      names)))
	(if (pair? missing)
	    (warn "Library exports refer to unbound identifiers:" missing)))
      (check-dupes "exports"
		   library-ixport-to
		   (lambda (export froms)
		     (lset-adjoin eq? froms (library-ixport-from export)))
		   exports)
      (map (lambda (export)
	     ;; If this is a re-export, export directly from the source.
	     (let* ((export-from (library-ixport-from export))
		    (import
		     (find (lambda (import)
			     (eq? (library-ixport-to import) export-from))
			   imports)))
	       (if import
		   (make-library-ixport (library-ixport-from-library import)
					(library-ixport-from import)
					(library-ixport-to export))
		   export)))
	   exports))))

(define (parsed-export-libraries parsed-export library libraries)
  ((case (car parsed-export)
     ((r7rs-export) r7rs-parsed-export-libraries)
     ((mit-export) mit-parsed-export-libraries)
     (else (error "Unknown parsed export:" parsed-export)))
   parsed-export library libraries))

(define (expand-parsed-export parsed-export names library exports)
  ((case (car parsed-export)
     ((r7rs-export) r7rs-expand-parsed-export)
     ((mit-export) mit-expand-parsed-export)
     (else (error "Unknown parsed export:" parsed-export)))
   parsed-export names library exports))

(define (check-libraries-exist folder items db library)
  (let ((unusable
	 (remove (lambda (name)
		   (and (registered-library? name db)
			(library-has? 'exports (registered-library name db))))
		 (fold (lambda (item acc)
			 (folder item library acc))
		       '()
		       items))))
    (if (pair? unusable)
	(error "Unregistered import libraries:" unusable))))

(define (check-dupes noun get-key adjoin-value items)
  (let ((dupes (find-dupes get-key adjoin-value items)))
    (if (pair? dupes)
	(warn (string-append "Duplicate " noun ":") dupes))))

(define (find-dupes get-key adjoin-value items)
  (let ((table (make-strong-eq-hash-table)))
    (for-each (lambda (item)
		(hash-table-update! table
				    (get-key item)
				    (lambda (acc)
				      (adjoin-value item acc))
				    (lambda ()
				      '())))
	      items)
    (sort (filter (lambda (p)
		    (pair? (cddr p)))
		  (hash-table->alist table))
	  (lambda (a b)
	    (symbol<? (car a) (car b))))))

(define (get-exports library db)
  (library-exports (registered-library library db)))