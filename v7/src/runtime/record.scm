#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/record.scm,v 1.9 1991/05/06 02:25:42 cph Exp $

Copyright (c) 1989-91 Massachusetts Institute of Technology

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
;;; package: (runtime record)

;;; adapted from JAR's implementation
;;; conforms to R4RS proposal

(declare (usual-integrations))

(define (make-record-type type-name field-names)
  (let ((record-type
	 (vector record-type-marker type-name (list-copy field-names))))
    (unparser/set-tagged-vector-method! record-type
					(unparser/standard-method type-name))
    (named-structure/set-tag-description! record-type
      (letrec ((description
		(let ((predicate (record-predicate record-type))
		      (record-name
		       (string-append "record of type "
				      (if (string? type-name)
					  type-name
					  (write-to-string type-name)))))
		  (lambda (record)
		    (if (not (predicate record))
			(error:wrong-type-argument record record-name
						   description))
		    (map (lambda (field-name)
			   (list field-name
				 (vector-ref
				  record
				  (record-type-field-index record-type
							   field-name
							   description))))
			 (vector-ref record-type 2))))))
	description))
    record-type))

(define (record-type? object)
  (and (vector? object)
       (= (vector-length object) 3)
       (eq? (vector-ref object 0) record-type-marker)))

(define (record-type-name record-type)
  (if (not (record-type? record-type))
      (error:wrong-type-argument record-type "record type" 'RECORD-TYPE-NAME))
  (vector-ref record-type 1))

(define (record-type-field-names record-type)
  (if (not (record-type? record-type))
      (error:wrong-type-argument record-type "record type"
				 'RECORD-TYPE-FIELD-NAMES))
  (list-copy (vector-ref record-type 2)))

(define-integrable (record-type-record-length record-type)
  (+ (length (vector-ref record-type 2)) 1))

(define (record-type-field-index record-type field-name procedure-name)
  (let loop ((field-names (vector-ref record-type 2)) (index 1))
    (if (null? field-names)
	(error:bad-range-argument field-name procedure-name))
    (if (eq? field-name (car field-names))
	index
	(loop (cdr field-names) (+ index 1)))))

(define (set-record-type-unparser-method! record-type method)
  (if (not (record-type? record-type))
      (error:wrong-type-argument record-type "record type"
				 'SET-RECORD-TYPE-UNPARSER-METHOD!))
  (unparser/set-tagged-vector-method! record-type method))

(define record-type-marker)

(define (initialize-package!)
  (set! record-type-marker
	(string->symbol "#[(runtime record)record-type-marker]"))
  (unparser/set-tagged-vector-method!
   record-type-marker
   (unparser/standard-method 'RECORD-TYPE-DESCRIPTOR
     (lambda (state record-type)
       (unparse-object state (record-type-name record-type)))))
  (named-structure/set-tag-description! record-type-marker
    (lambda (record-type)
      (if (not (record-type? record-type))
	  (error:wrong-type-argument record-type "record type" false))
      `((TYPE-NAME ,(record-type-name record-type))
	(FIELD-NAMES ,(record-type-field-names record-type))))))

(define (record-constructor record-type #!optional field-names)
  (if (not (record-type? record-type))
      (error:wrong-type-argument record-type "record type"
				 'RECORD-CONSTRUCTOR))
  (let ((field-names
	 (if (default-object? field-names)
	     (vector-ref record-type 2)
	     field-names)))
    (let ((record-length (record-type-record-length record-type))
	  (number-of-inits (length field-names))
	  (indexes
	   (map (lambda (field-name)
		  (record-type-field-index record-type
					   field-name
					   'RECORD-CONSTRUCTOR))
		field-names)))
      (lambda field-values
	(if (not (= (length field-values) number-of-inits))
	    (error "wrong number of arguments to record constructor"
		   field-values record-type field-names))
	(let ((record (make-vector record-length)))
	  (vector-set! record 0 record-type)
	  (for-each (lambda (index value) (vector-set! record index value))
		    indexes
		    field-values)
	  record)))))

(define (record? object)
  (and (vector? object)
       (positive? (vector-length object))
       (record-type? (vector-ref object 0))))

(define (record-type-descriptor record)
  (if (not (record? record))
      (error:wrong-type-argument record "record" 'RECORD-TYPE-DESCRIPTOR))
  (vector-ref record 0))

(define (record-predicate record-type)
  (if (not (record-type? record-type))
      (error:wrong-type-argument record-type "record type" 'RECORD-PREDICATE))
  (let ((record-length (record-type-record-length record-type)))
    (lambda (object)
      (and (vector? object)
	   (= (vector-length object) record-length)
	   (eq? (vector-ref object 0) record-type)))))

(define (record-accessor record-type field-name)
  (if (not (record-type? record-type))
      (error:wrong-type-argument record-type "record type" 'RECORD-ACCESSOR))
  (let ((record-length (record-type-record-length record-type))
	(procedure-name `(RECORD-ACCESSOR ,record-type ',field-name))
	(index
	 (record-type-field-index record-type field-name 'RECORD-ACCESSOR)))
    (lambda (record)
      (if (not (and (vector? record)
		    (= (vector-length record) record-length)
		    (eq? (vector-ref record 0) record-type)))
	  (error:wrong-type-argument record "record" procedure-name))
      (vector-ref record index))))

(define (record-updater record-type field-name)
  (if (not (record-type? record-type))
      (error:wrong-type-argument record-type "record type" 'RECORD-UPDATER))
  (let ((record-length (record-type-record-length record-type))
	(procedure-name `(RECORD-UPDATER ,record-type ',field-name))
	(index
	 (record-type-field-index record-type field-name 'RECORD-UPDATER)))
    (lambda (record field-value)
      (if (not (and (vector? record)
		    (= (vector-length record) record-length)
		    (eq? (vector-ref record 0) record-type)))
	  (error:wrong-type-argument record "record" procedure-name))
      (vector-set! record index field-value))))