#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/record.scm,v 1.3 1990/02/07 23:25:58 cph Exp $

Copyright (c) 1989, 1990 Massachusetts Institute of Technology

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
;;; adapted from JAR's implementation
;;; conforms to R4RS proposal

(declare (usual-integrations))

(define (make-record-type type-name field-names)
  (let ((size (+ (length field-names) 1))
	(the-descriptor (make-vector 7)))

    (define (predicate object)
      (and (vector? object)
	   (= (vector-length object) size)
	   (eq? (vector-ref object 0) the-descriptor)))

    (define (guarantee record)
      (if (not (predicate record))
	  (error "invalid argument to record accessor" record type-name)))

    (define (field-index name)
      (let loop ((names field-names) (index 1))
	(if (null? names)
	    (error "bad field name" name))
	(if (eq? name (car names))
	    index
	    (loop (cdr names) (+ index 1)))))

    (vector-set! the-descriptor 0 "record-type-descriptor")
    (vector-set! the-descriptor 1 predicate)
    (vector-set! the-descriptor 2
      (lambda (names)
	(let ((number-of-inits (length names))
	      (indexes (map field-index names)))
	  (lambda field-values
	    (if (not (= (length field-values) number-of-inits))
		(error "wrong number of arguments to record constructor"
		       field-values type-name names))
	    (let ((record (make-vector size)))
	      (vector-set! record 0 the-descriptor)
	      (for-each (lambda (index value)
			  (vector-set! record index value))
			indexes
			field-values)
	      record)))))
    (vector-set! the-descriptor 3
      (lambda (name)
	(let ((index (field-index name)))
	  (lambda (record)
	    (guarantee record)
	    (vector-ref record index)))))
    (vector-set! the-descriptor 4
      (lambda (name)
	(let ((index (field-index name)))
	  (lambda (record new-value)
	    (guarantee record)
	    (vector-set! record index new-value)))))
    (vector-set! the-descriptor 5 type-name)
    (vector-set! the-descriptor 6 (list-copy field-names))
    (unparser/set-tagged-vector-method! the-descriptor
					(unparser/standard-method type-name))
    (named-structure/set-tag-description! the-descriptor
      (lambda (record)
	(guarantee record)
	(map (lambda (name)
	       (list name (vector-ref record (field-index name))))
	     field-names)))
    the-descriptor))

(define (record-constructor record-type #!optional field-names)
  (guarantee-record-type record-type)
  ((vector-ref record-type 2)
   (if (default-object? field-names)
       (record-type-field-names record-type)
       field-names)))

(define (record-predicate record-type)
  (guarantee-record-type record-type)
  (vector-ref record-type 1))

(define (record-accessor record-type field-name)
  (guarantee-record-type record-type)
  ((vector-ref record-type 3) field-name))

(define (record-updater record-type field-name)
  (guarantee-record-type record-type)
  ((vector-ref record-type 4) field-name))

(define (set-record-type-unparser-method! record-type method)
  (guarantee-record-type record-type)
  (unparser/set-tagged-vector-method! record-type method))

;;; Abstraction-Breaking Operations

(define record-type?
  (let ((record-type (make-record-type "foo" '())))
    (let ((size (vector-length record-type))
	  (tag (vector-ref record-type 0)))
      (unparser/set-tagged-vector-method!
       tag
       (unparser/standard-method 'RECORD-TYPE-DESCRIPTOR
	 (lambda (state record-type)
	   (unparse-object state (vector-ref record-type 5)))))
      (named-structure/set-tag-description! tag
	(lambda (record-type)
	  (guarantee-record-type record-type)
	  `((PREDICATE ,(vector-ref record-type 1))
	    (CONSTRUCTOR-CONSTRUCTOR ,(vector-ref record-type 2))
	    (ACCESSOR-CONSTRUCTOR ,(vector-ref record-type 3))
	    (UPDATER-CONSTRUCTOR ,(vector-ref record-type 4))
	    (TYPE-NAME ,(vector-ref record-type 5))
	    (FIELD-NAMES ,(vector-ref record-type 6)))))
      (lambda (object)
	(and (vector? object)
	     (= (vector-length object) size)
	     (eq? (vector-ref object 0) tag))))))

(define (guarantee-record-type object)
  (if (not (record-type? object))
      (error "not a record type descriptor" object))
  object)

(define (record-type-name record-type)
  (guarantee-record-type record-type)
  (vector-ref record-type 5))

(define (record-type-field-names record-type)
  (guarantee-record-type record-type)
  (list-copy (vector-ref record-type 6)))

(define (record? object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (record-type? (vector-ref object 0))))

(define (guarantee-record object)
  (if (not (record? object))
      (error "not a record" object))
  object)

(define (record-type-descriptor record)
  (guarantee-record record)
  (vector-ref record 0))