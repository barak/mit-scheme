#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/record.scm,v 1.1 1989/02/28 18:34:09 cph Exp $

Copyright (c) 1989 Massachusetts Institute of Technology

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

;;;; Records
;;; written by Jonathan Rees

(declare (usual-integrations))

(define (make-record-type type-id field-names)
  (let ((size (+ (length field-names) 1)))

    (define (constructor names)
      (let ((number-of-inits (length names))
	    (indexes (map field-index names)))
	(lambda field-values
	  (if (not (= (length field-values) number-of-inits))
	      (error "wrong number of arguments to record constructor"
		     field-values type-id names))
	  (let ((record (make-vector size)))
	    (vector-set! record 0 the-descriptor)
	    (for-each (lambda (index value) (vector-set! record index value))
		      indexes
		      field-values)
	    record))))

    (define (predicate obj)
      (and (vector? obj)
	   (= (vector-length obj) size)
	   (eq? (vector-ref obj 0) the-descriptor)))

    (define (guarantee record)
      (if (not (predicate record))
	  (error "invalid argument to record accessor" record type-id)))

    (define (accessor name)
      (let ((index (field-index name)))
	(lambda (record)
	  (guarantee record)
	  (vector-ref record index))))

    (define (updater name)
      (let ((index (field-index name)))
	(lambda (record new-value)
	  (guarantee record)
	  (vector-set! record index new-value))))

    (define (describe record)
      (guarantee record)
      (map (lambda (name index)
	     (list name (vector-ref record (field-index name))))
	   field-names))

    (define (field-index name)
      (let loop ((names field-names) (index 1))
	(cond ((null? names) (error "bad field name" name))
	      ((eq? name (car names)) index)
	      (else (loop (cdr names) (+ index 1))))))

    (define (the-descriptor request)
      (case request
	((CONSTRUCTOR) constructor)
	((PREDICATE) predicate)
	((ACCESSOR) accessor)
	((UPDATER) updater)
	(else (error "invalid request to record type" type-id request))))

    (unparser/set-tagged-vector-method! the-descriptor
					(unparser/standard-method type-id))
    (named-structure/set-tag-description! the-descriptor describe)
    the-descriptor))

(define (record-constructor record-type names)
  ((record-type 'CONSTRUCTOR) names))

(define (record-predicate record-type)
  (record-type 'PREDICATE))

(define (record-accessor record-type field-name)
  ((record-type 'ACCESSOR) field-name))

(define (record-updater record-type field-name)
  ((record-type 'UPDATER) field-name))