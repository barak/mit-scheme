#| -*-Scheme-*-

$Id: record.scm,v 1.19 1992/12/17 00:05:34 cph Exp $

Copyright (c) 1989-1992 Massachusetts Institute of Technology

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

(define-primitives
  (%record -1)
  (%record-length 1)
  (%record-ref 2)
  (%record-set! 3)
  (primitive-object-ref 2)
  (primitive-object-set! 3)
  (primitive-object-set-type 2))

(define-integrable (%record? object)
  (object-type? (ucode-type record) object))

(define (%make-record length #!optional object)
  (if (not (exact-integer? length))
      (error:wrong-type-argument length "exact integer" '%MAKE-RECORD))
  (if (not (> length 0))
      (error:bad-range-argument length '%MAKE-RECORD))
  (if (default-object? object)
      (object-new-type (ucode-type record) (make-vector length))
      (object-new-type (ucode-type record) (make-vector length object))))

(define (%record-copy record)
  (let ((length (%record-length record)))
    (let ((result (object-new-type (ucode-type record) (make-vector length))))
      ;; Clobber RESULT's length field with that of RECORD, since
      ;; there is important information in the type of that field that
      ;; is not preserved by %RECORD-LENGTH.
      (primitive-object-set! result 0 (primitive-object-ref record 0))
      (do ((index 0 (+ index 1)))
	  ((= index length))
	(%record-set! result index (%record-ref record index)))
      result)))

(define (%record-application-method record)
  ;; This procedure must match the code in "microcode/interp.c".
  (let ((record-type (%record-ref record 0)))
    (and (and (object-type? (ucode-type constant)
			    (primitive-object-ref record-type 0))
	      (>= (%record-length record-type) 2))
	 (let ((method (%record-ref record-type 1)))
	   (and (not (eq? method record))
		method)))))

(define (%record-type-has-application-method! record-type)
  (primitive-object-set!
   record-type
   0
   (primitive-object-set-type (ucode-type constant)
			      (primitive-object-ref record-type 0))))

(define (make-record-type type-name field-names)
  (guarantee-list-of-unique-symbols field-names 'MAKE-RECORD-TYPE)
  (let ((record-type
	 (%record record-type-type
		  false
		  (->string type-name)
		  (list-copy field-names)
		  false)))
    (%record-type-has-application-method! record-type)
    record-type))

(define (record-type? object)
  (and (%record? object)
       (eq? (%record-ref object 0) record-type-type)))

(define (record-type-application-method record-type)
  (guarantee-record-type record-type 'RECORD-TYPE-APPLICATION-METHOD)
  (%record-ref record-type 1))

(define (set-record-type-application-method! record-type method)
  (guarantee-record-type record-type 'SET-RECORD-TYPE-APPLICATION-METHOD!)
  (if (not (or (not method) (procedure? method)))
      (error:wrong-type-argument method "application method"
				 'SET-RECORD-TYPE-APPLICATION-METHOD!))
  (%record-set! record-type 1 method))

(define (record-type-name record-type)
  (guarantee-record-type record-type 'RECORD-TYPE-NAME)
  (%record-type/name record-type))

(define-integrable (%record-type/name record-type)
  (%record-ref record-type 2))

(define (record-type-field-names record-type)
  (guarantee-record-type record-type 'RECORD-TYPE-FIELD-NAMES)
  (list-copy (%record-type/field-names record-type)))

(define-integrable (%record-type/field-names record-type)
  (%record-ref record-type 3))

(define (record-type-unparser-method record-type)
  (guarantee-record-type record-type 'RECORD-TYPE-UNPARSER-METHOD)
  (%record-type/unparser-method record-type))

(define-integrable (%record-type/unparser-method record-type)
  (%record-ref record-type 4))

(define (set-record-type-unparser-method! record-type method)
  (guarantee-record-type record-type 'SET-RECORD-TYPE-UNPARSER-METHOD!)
  (if (not (or (not method) (procedure? method)))
      (error:wrong-type-argument method "unparser method"
				 'SET-RECORD-TYPE-UNPARSER-METHOD!))
  (%record-set! record-type 4 method))

(define record-type-type)

(define (initialize-package!)
  (set! record-type-type
	(let ((record-type-type
	       (%record false
			false
			"record-type"
			'(RECORD-TYPE-APPLICATION-METHOD
			  RECORD-TYPE-NAME
			  RECORD-TYPE-FIELD-NAMES
			  RECORD-TYPE-UNPARSER-METHOD)
			false)))
	  (%record-set! record-type-type 0 record-type-type)
	  (%record-type-has-application-method! record-type-type)
	  record-type-type))
  unspecific)

(define (record-type-field-index record-type field-name procedure-name)
  (let loop ((field-names (%record-type/field-names record-type)) (index 1))
    (if (null? field-names)
	(error:bad-range-argument field-name procedure-name))
    (if (eq? field-name (car field-names))
	index
	(loop (cdr field-names) (+ index 1)))))

(define (record-constructor record-type #!optional field-names)
  (guarantee-record-type record-type 'RECORD-CONSTRUCTOR)
  (let ((all-field-names (%record-type/field-names record-type)))
    (let ((field-names
	   (if (default-object? field-names) all-field-names field-names))
	  (record-length (+ 1 (length all-field-names))))
      (let ((number-of-inits (length field-names))
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
	  (let ((record
		 (object-new-type (ucode-type record)
				  (make-vector record-length))))
	    (%record-set! record 0 record-type)
	    (do ((indexes indexes (cdr indexes))
		 (field-values field-values (cdr field-values)))
		((null? indexes))
	      (%record-set! record (car indexes) (car field-values)))
	    record))))))

(define (record? object)
  (and (%record? object)
       (record-type? (%record-ref object 0))))

(define (record-type-descriptor record)
  (guarantee-record record 'RECORD-TYPE-DESCRIPTOR)
  (%record-ref record 0))

(define (record-copy record)
  (guarantee-record record 'RECORD-COPY)
  (%record-copy record))

(define (%record-unparser-method record)
  ;; Used by unparser.  Assumes RECORD has type-code RECORD.
  (let ((type (%record-ref record 0)))
    (and (record-type? type)
	 (or (%record-type/unparser-method type)
	     (unparser/standard-method (record-type-name type))))))

(define (record-description record)
  (let ((type (record-type-descriptor record)))
    (map (lambda (field-name)
	   `(,field-name ,((record-accessor type field-name) record)))
	 (record-type-field-names type))))

(define (record-predicate record-type)
  (guarantee-record-type record-type 'RECORD-PREDICATE)
  (lambda (object)
    (and (%record? object)
	 (eq? (%record-ref object 0) record-type))))

(define (record-accessor record-type field-name)
  (guarantee-record-type record-type 'RECORD-ACCESSOR)
  (let ((procedure-name `(RECORD-ACCESSOR ,record-type ',field-name))
	(index
	 (record-type-field-index record-type field-name 'RECORD-ACCESSOR)))
    (lambda (record)
      (guarantee-record-of-type record record-type procedure-name)
      (%record-ref record index))))

(define (record-modifier record-type field-name)
  (guarantee-record-type record-type 'RECORD-MODIFIER)
  (let ((procedure-name `(RECORD-ACCESSOR ,record-type ',field-name))
	(index
	 (record-type-field-index record-type field-name 'RECORD-MODIFIER)))
    (lambda (record field-value)
      (guarantee-record-of-type record record-type procedure-name)
      (%record-set! record index field-value))))

(define record-updater
  record-modifier)

(define (->string object)
  (if (string? object)
      object
      (write-to-string object)))

(define-integrable (guarantee-list-of-unique-symbols object procedure)
  (if (not (list-of-unique-symbols? object))
      (error:wrong-type-argument object "list of unique symbols" procedure)))

(define (list-of-unique-symbols? object)
  (and (list? object)
       (let loop ((elements object))
	 (or (null? elements)
	     (and (symbol? (car elements))
		  (not (memq (car elements) (cdr elements)))
		  (loop (cdr elements)))))))

(define-integrable (guarantee-record-type record-type procedure)
  (if (not (record-type? record-type))
      (error:wrong-type-argument record-type "record type" procedure)))

(define-integrable (guarantee-record-of-type record record-type procedure-name)
  (if (not (and (%record? record)
		(eq? (%record-ref record 0) record-type)))
      (error:wrong-type-argument
       record
       (string-append "record of type " (%record-type/name record-type))
       procedure-name)))

(define-integrable (guarantee-record record procedure-name)
  (if (not (record? record))
      (error:wrong-type-argument record "record" procedure-name)))