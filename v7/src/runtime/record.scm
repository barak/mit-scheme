#| -*-Scheme-*-

$Id: record.scm,v 1.30 2002/11/20 19:46:22 cph Exp $

Copyright (c) 1989-1999, 2002 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Records
;;; package: (runtime record)

;;; adapted from JAR's implementation
;;; conforms to R4RS proposal

(declare (usual-integrations))

(define-primitives
  (%record? 1)
  (%record -1)
  (%record-length 1)
  (%record-ref 2)
  (%record-set! 3)
  (primitive-object-ref 2)
  (primitive-object-set! 3)
  (primitive-object-set-type 2))

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
    (%record-set! type 3 record-type-type-tag))
  (initialize-structure-type-type!))

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
  (let ((tag (record-type-dispatch-tag record-type)))
    (remove-generic-procedure-generators unparse-record
					 (list (make-dispatch-tag #f) tag))
    (add-generic-procedure-generator unparse-record
      (lambda (generic tags)
	generic
	(and (eq? (cadr tags) tag) method)))))

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

(define (record-type-field-index record-type field-name error?)
  (let loop ((field-names (record-type-field-names record-type)) (index 1))
    (cond ((null? field-names)
	   (and error?
		(record-type-field-index
		 record-type
		 (error:no-such-slot record-type field-name)
		 error?)))
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

;;;; Runtime support for DEFINE-STRUCTURE

(define structure-type-rtd)
(define make-define-structure-type)
(define structure-type?)
(define structure-type/type)
(define structure-type/name)
(define structure-type/field-names)
(define structure-type/field-indexes)
(define structure-type/unparser-method)
(define set-structure-type/unparser-method!)

(define (initialize-structure-type-type!)
  (set! structure-type-rtd
	(make-record-type "structure-type"
			  '(TYPE NAME FIELD-NAMES FIELD-INDEXES
				 UNPARSER-METHOD)))
  (set! make-define-structure-type
	(record-constructor structure-type-rtd))
  (set! structure-type?
	(record-predicate structure-type-rtd))
  (set! structure-type/type
	(record-accessor structure-type-rtd 'TYPE))
  (set! structure-type/name
	(record-accessor structure-type-rtd 'NAME))
  (set! structure-type/field-names
	(record-accessor structure-type-rtd 'FIELD-NAMES))
  (set! structure-type/field-indexes
	(record-accessor structure-type-rtd 'FIELD-INDEXES))
  (set! structure-type/unparser-method
	(record-accessor structure-type-rtd 'UNPARSER-METHOD))
  (set! set-structure-type/unparser-method!
	(record-modifier structure-type-rtd 'UNPARSER-METHOD))
  unspecific)

(define (structure-tag/unparser-method tag type)
  (let ((structure-type (tag->structure-type tag type)))
    (and structure-type
	 (structure-type/unparser-method structure-type))))

(define (named-structure? object)
  (cond ((record? object)
	 true)
	((vector? object)
	 (and (not (zero? (vector-length object)))
	      (tag->structure-type (vector-ref object 0) 'VECTOR)))
	((pair? object)
	 (tag->structure-type (car object) 'LIST))
	(else
	 false)))

(define (named-structure/description structure)
  (cond ((record? structure)
	 (record-description structure))
	((named-structure? structure)
	 =>
	 (lambda (type)
	   (let ((accessor (if (pair? structure) list-ref vector-ref)))
	     (map (lambda (field-name index)
		    `(,field-name ,(accessor structure index)))
		  (structure-type/field-names type)
		  (structure-type/field-indexes type)))))
	(else
	 (error:wrong-type-argument structure "named structure"
				    'NAMED-STRUCTURE/DESCRIPTION))))

(define (tag->structure-type tag type)
  (if (structure-type? tag)
      (and (eq? (structure-type/type tag) type)
	   tag)
      (let ((structure-type (named-structure/get-tag-description tag)))
	(and (structure-type? structure-type)
	     (eq? (structure-type/type structure-type) type)
	     structure-type))))

;;;; Support for safe accessors

(define (define-structure/vector-accessor tag field-name)
  (call-with-values
      (lambda () (accessor-parameters tag field-name 'VECTOR 'ACCESSOR))
    (lambda (tag index type-name accessor-name)
      (if tag
	  (lambda (structure)
	    (check-vector structure tag index type-name accessor-name)
	    (vector-ref structure index))
	  (lambda (structure)
	    (check-vector-untagged structure index type-name accessor-name)
	    (vector-ref structure index))))))

(define (define-structure/vector-modifier tag field-name)
  (call-with-values
      (lambda () (accessor-parameters tag field-name 'VECTOR 'MODIFIER))
    (lambda (tag index type-name accessor-name)
      (if tag
	  (lambda (structure value)
	    (check-vector structure tag index type-name accessor-name)
	    (vector-set! structure index value))
	  (lambda (structure value)
	    (check-vector-untagged structure index type-name accessor-name)
	    (vector-set! structure index value))))))

(define (define-structure/list-accessor tag field-name)
  (call-with-values
      (lambda () (accessor-parameters tag field-name 'LIST 'ACCESSOR))
    (lambda (tag index type-name accessor-name)
      (if tag
	  (lambda (structure)
	    (check-list structure tag index type-name accessor-name)
	    (list-ref structure index))
	  (lambda (structure)
	    (check-list-untagged structure index type-name accessor-name)
	    (list-ref structure index))))))

(define (define-structure/list-modifier tag field-name)
  (call-with-values
      (lambda () (accessor-parameters tag field-name 'LIST 'MODIFIER))
    (lambda (tag index type-name accessor-name)
      (if tag
	  (lambda (structure value)
	    (check-list structure tag index type-name accessor-name)
	    (set-car! (list-tail structure index) value))
	  (lambda (structure value)
	    (check-list-untagged structure index type-name accessor-name)
	    (set-car! (list-tail structure index) value))))))

(define-integrable (check-vector structure tag index type accessor-name)
  (if (not (and (vector? structure)
		(fix:> (vector-length structure) index)
		(eq? tag (vector-ref structure 0))))
      (error:wrong-type-argument structure type accessor-name)))

(define-integrable (check-vector-untagged structure index type accessor-name)
  (if (not (and (vector? structure)
		(fix:> (vector-length structure) index)))
      (error:wrong-type-argument structure type accessor-name)))

(define-integrable (check-list structure tag index type accessor-name)
  (if (not (and (list-to-index? structure index)
		(eq? tag (car structure))))
      (error:wrong-type-argument structure type accessor-name)))

(define-integrable (check-list-untagged structure index type accessor-name)
  (if (not (list-to-index? structure index))
      (error:wrong-type-argument structure type accessor-name)))

(define (list-to-index? object index)
  (and (pair? object)
       (or (fix:= 0 index)
	   (list-to-index? (cdr object) (fix:- index 1)))))

(define (accessor-parameters tag field-name structure-type accessor-type)
  (if (exact-nonnegative-integer? tag)
      (values #f
	      tag
	      (string-append (symbol->string structure-type)
			     " of length >= "
			     (number->string (+ tag 1)))
	      `(,accessor-type ,tag ',field-name))
      (let ((type (tag->structure-type tag structure-type)))
	(if (not type)
	    (error:wrong-type-argument tag "structure tag" accessor-type))
	(values tag
		(structure-type/field-index type field-name)
		(structure-type/name type)
		`(,accessor-type ,type ',field-name)))))

(define (structure-type/field-index type name)
  (let loop
      ((names (structure-type/field-names type))
       (indexes (structure-type/field-indexes type)))
    (if (pair? names)
	(if (eq? name (car names))
	    (car indexes)
	    (loop (cdr names) (cdr indexes)))
	(error:bad-range-argument name 'STRUCTURE-TYPE/FIELD-INDEX))))

(define (define-structure/keyword-parser argument-list default-alist)
  (if (null? argument-list)
      (map cdr default-alist)
      (let ((alist
	     (map (lambda (entry) (cons (car entry) (cdr entry)))
		  default-alist)))
	(let loop ((arguments argument-list))
	  (if (not (null? arguments))
	      (begin
		(if (null? (cdr arguments))
		    (error "Keyword list does not have even length:"
			   argument-list))
		(set-cdr! (or (assq (car arguments) alist)
			      (error "Unknown keyword:" (car arguments)))
			  (cadr arguments))
		(loop (cddr arguments)))))
	(map cdr alist))))