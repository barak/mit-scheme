#| -*-Scheme-*-

$Id: enumer.scm,v 4.9 2008/01/30 20:01:43 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Support for enumerations

(declare (usual-integrations))

;;;; Enumerations

(define-structure (enumeration
		   (conc-name enumeration/)
		   (constructor %make-enumeration))
  (enumerands false read-only true))

(define-structure (enumerand
		   (conc-name enumerand/)
		   (print-procedure
		    (standard-unparser (symbol->string 'ENUMERAND)
		      (lambda (state enumerand)
			(unparse-object state (enumerand/name enumerand))))))
  (enumeration false read-only true)
  (name false read-only true)
  (index false read-only true))

(define (make-enumeration names)
  (let ((enumerands (make-vector (length names))))
    (let ((enumeration (%make-enumeration enumerands)))
      (let loop ((names names) (index 0))
	(if (not (null? names))
	    (begin
	      (vector-set! enumerands
			   index
			   (make-enumerand enumeration (car names) index))
	      (loop (cdr names) (1+ index)))))
      enumeration)))

(define-integrable (enumeration/cardinality enumeration)
  (vector-length (enumeration/enumerands enumeration)))

(define-integrable (enumeration/index->enumerand enumeration index)
  (vector-ref (enumeration/enumerands enumeration) index))

(define-integrable (enumeration/index->name enumeration index)
  (enumerand/name (enumeration/index->enumerand enumeration index)))

(define (enumeration/name->enumerand enumeration name)
  (let ((end (enumeration/cardinality enumeration)))
    (let loop ((index 0))
      (if (< index end)
	  (let ((enumerand (enumeration/index->enumerand enumeration index)))
	    (if (eqv? (enumerand/name enumerand) name)
		enumerand
		(loop (1+ index))))
	  (error "Unknown enumeration name" name)))))

(define-integrable (enumeration/name->index enumeration name)
  (enumerand/index (enumeration/name->enumerand enumeration name)))

;;;; Method Tables

(define-structure (method-table (constructor %make-method-table))
  (enumeration false read-only true)
  (vector false read-only true))

(define (make-method-table enumeration default-method . method-alist)
  (let ((table
	 (%make-method-table enumeration
			     (make-vector (enumeration/cardinality enumeration)
					  default-method))))
    (for-each (lambda (entry)
		(define-method-table-entry table (car entry) (cdr entry)))
	      method-alist)
    table))

(define (define-method-table-entry name method-table method)
  (vector-set! (method-table-vector method-table)
	       (enumeration/name->index (method-table-enumeration method-table)
					name)
	       method)
  name)

(define (define-method-table-entries names method-table method)
  (for-each (lambda (name)
	      (define-method-table-entry name method-table method))
	    names)
  names)

(define-integrable (method-table-lookup method-table index)
  (vector-ref (method-table-vector method-table) index))