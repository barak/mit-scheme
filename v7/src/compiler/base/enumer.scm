#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/enumer.scm,v 4.3 1989/08/10 11:05:13 cph Rel $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

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