#| -*-Scheme-*-

$Id: record.scm,v 1.24 1996/04/24 04:23:11 cph Exp $

Copyright (c) 1989-96 Massachusetts Institute of Technology

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
  (object-new-type
   (ucode-type record)
   ((ucode-primitive vector-cons) length
				  (if (default-object? object) #f object))))

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

(define record-type-type-tag)
(define unparse-record)
(define record-description)

(define (initialize-record-type-type!)
  (let ((type
	 (%record #f
		  "record-type"
		  '(RECORD-TYPE-NAME
		    RECORD-TYPE-FIELD-NAMES
		    RECORD-TYPE-DISPATCH-TAG)
		  #f)))
    (set! record-type-type-tag (make-dispatch-tag type))
    (%record-set! type 0 record-type-type-tag)
    (%record-set! type 3 record-type-type-tag)))

(define (initialize-record-procedures!)
  (set! unparse-record (make-generic-procedure 2 'UNPARSE-RECORD))
  (set-generic-procedure-default-generator! unparse-record
    (let ((record-method (standard-unparser-method 'RECORD #f)))
      (lambda (generic tags)
	generic
	(let ((tag (cadr tags)))
	  (cond ((record-type? (dispatch-tag-contents tag))
		 (standard-unparser-method
		  (record-type-name (dispatch-tag-contents tag))
		  #f))
		((eq? tag record-type-type-tag)
		 (standard-unparser-method 'TYPE
		   (lambda (type port)
		     (write-char #\space port)
		     (display (record-type-name type) port))))
		((eq? tag (built-in-dispatch-tag 'DISPATCH-TAG))
		 (standard-unparser-method 'DISPATCH-TAG
		   (lambda (tag port)
		     (write-char #\space port)
		     (write (dispatch-tag-contents tag) port))))
		(else record-method))))))
  (set! set-record-type-unparser-method!
	set-record-type-unparser-method!/after-boot)
  (for-each (lambda (t.m)
	      (set-record-type-unparser-method! (car t.m) (cdr t.m)))
	    deferred-unparser-methods)
  (set! deferred-unparser-methods)
  (set! record-description (make-generic-procedure 1 'RECORD-DESCRIPTION))
  (set-generic-procedure-default-generator! record-description
    (lambda (generic tags)
      generic
      (if (record-type? (dispatch-tag-contents (car tags)))
	  (lambda (record)
	    (let ((type (record-type-descriptor record)))
	      (map (lambda (field-name)
		     `(,field-name
		       ,((record-accessor type field-name) record)))
		   (record-type-field-names type))))
	  (lambda (record)
	    (let loop ((i (fix:- (%record-length record) 1)) (d '()))
	      (if (fix:< i 0)
		  d
		  (loop (fix:- i 1)
			(cons (list i (%record-ref record i)) d)))))))))

(define (make-record-type type-name field-names #!optional print-method)
  (guarantee-list-of-unique-symbols field-names 'MAKE-RECORD-TYPE)
  (let ((record-type
	 (%record record-type-type-tag
		  (->string type-name)
		  (list-copy field-names)
		  #f)))
    (%record-set! record-type 3 (make-dispatch-tag record-type))
    (if (not (default-object? print-method))
	(set-record-type-unparser-method! record-type print-method))
    record-type))

(define (record-type? object)
  (and (%record? object)
       (eq? (%record-ref object 0) record-type-type-tag)))

(define (record-type-name record-type)
  (guarantee-record-type record-type 'RECORD-TYPE-NAME)
  (%record-ref record-type 1))

(define (record-type-field-names record-type)
  (guarantee-record-type record-type 'RECORD-TYPE-FIELD-NAMES)
  (%record-ref record-type 2))

(define (record-type-dispatch-tag record-type)
  (guarantee-record-type record-type 'RECORD-TYPE-DISPATCH-TAG)
  (%record-ref record-type 3))

(define (set-record-type-unparser-method! record-type method)
  (set! deferred-unparser-methods
	(cons (cons record-type method) deferred-unparser-methods))
  unspecific)

(define deferred-unparser-methods '())

(define (set-record-type-unparser-method!/after-boot record-type method)
  (if (not (or (not method) (procedure? method)))
      (error:wrong-type-argument method "unparser method"
				 'SET-RECORD-TYPE-UNPARSER-METHOD!))
  (remove-generic-procedure-generators
   unparse-record
   (list (make-dispatch-tag #f) record-type))
  (add-generic-procedure-generator unparse-record
    (lambda (generic tags)
      generic
      (and (eq? (cadr tags) (record-type-dispatch-tag record-type))
	   method))))

(define (record-constructor record-type #!optional field-names)
  (guarantee-record-type record-type 'RECORD-CONSTRUCTOR)
  (let ((all-field-names (record-type-field-names record-type))
	(tag (record-type-dispatch-tag record-type)))
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
	    (%record-set! record 0 tag)
	    (do ((indexes indexes (cdr indexes))
		 (field-values field-values (cdr field-values)))
		((null? indexes))
	      (%record-set! record (car indexes) (car field-values)))
	    record))))))

(define (record? object)
  (and (%record? object)
       (dispatch-tag? (%record-ref object 0))
       (record-type? (dispatch-tag-contents (%record-ref object 0)))))

(define (record-type-descriptor record)
  (guarantee-record record 'RECORD-TYPE-DESCRIPTOR)
  (dispatch-tag-contents (%record-ref record 0)))

(define (record-copy record)
  (guarantee-record record 'RECORD-COPY)
  (%record-copy record))

(define (record-predicate record-type)
  (guarantee-record-type record-type 'RECORD-PREDICATE)
  (let ((tag (record-type-dispatch-tag record-type)))
    (lambda (object)
      (and (%record? object)
	   (eq? (%record-ref object 0) tag)))))

(define (record-accessor record-type field-name)
  (guarantee-record-type record-type 'RECORD-ACCESSOR)
  (let ((tag (record-type-dispatch-tag record-type))
	(type-name (record-type-name record-type))
	(procedure-name `(RECORD-ACCESSOR ,record-type ',field-name))
	(index
	 (record-type-field-index record-type field-name 'RECORD-ACCESSOR)))
    (lambda (record)
      (guarantee-record-of-type record tag type-name procedure-name)
      (%record-ref record index))))

(define (record-modifier record-type field-name)
  (guarantee-record-type record-type 'RECORD-MODIFIER)
  (let ((tag (record-type-dispatch-tag record-type))
	(type-name (record-type-name record-type))
	(procedure-name `(RECORD-ACCESSOR ,record-type ',field-name))
	(index
	 (record-type-field-index record-type field-name 'RECORD-MODIFIER)))
    (lambda (record field-value)
      (guarantee-record-of-type record tag type-name procedure-name)
      (%record-set! record index field-value))))

(define record-updater
  record-modifier)

(define (record-type-field-index record-type field-name error-name)
  (let loop ((field-names (record-type-field-names record-type)) (index 1))
    (cond ((null? field-names)
	   (and error-name (error:bad-range-argument field-name error-name)))
	  ((eq? field-name (car field-names)) index)
	  (else (loop (cdr field-names) (+ index 1))))))

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

(define-integrable (guarantee-record-of-type record tag type-name
					     procedure-name)
  (if (not (and (%record? record)
		(eq? (%record-ref record 0) tag)))
      (error:wrong-type-argument record
				 (string-append "record of type " type-name)
				 procedure-name)))

(define-integrable (guarantee-record record procedure-name)
  (if (not (record? record))
      (error:wrong-type-argument record "record" procedure-name)))