#| -*-Scheme-*-

$Id: record.scm,v 1.32 2003/03/07 05:48:28 cph Exp $

Copyright 1989,1990,1991,1993,1994,1996 Massachusetts Institute of Technology
Copyright 1997,2002,2003 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

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
  (primitive-object-set-type 2)
  (vector-cons 2))

(define-integrable (%make-record length object)
  (object-new-type (ucode-type record) (vector-cons length object)))

(define-integrable (%record-tag record)
  (%record-ref record 0))

(define-integrable (%tagged-record? tag object)
  (and (%record? object)
       (eq? (%record-tag object) tag)))

(define (%copy-record record)
  (let ((length (%record-length record)))
    (let ((result (%make-record length #f)))
      (do ((index 0 (fix:+ index 1)))
	  ((fix:= index length))
	(%record-set! result index (%record-ref record index)))
      result)))

(define record-type-type-tag)
(define unparse-record)
(define record-description)

(define (initialize-record-type-type!)
  (let* ((type
	  (%record #f
		   "record-type"
		   '#(RECORD-TYPE-NAME
		      RECORD-TYPE-FIELD-NAMES
		      RECORD-TYPE-DISPATCH-TAG
		      RECORD-TYPE-DEFAULT-RECORD)
		   #f
		   #f)))
    (set! record-type-type-tag (make-dispatch-tag type))
    (%record-set! type 0 record-type-type-tag)
    (%record-set! type 3 record-type-type-tag)
    (let ((default-record (%copy-record type)))
      (%record-set! type 4 default-record)
      (%record-set! default-record 4 default-record)))
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
		  (%record-type-name (dispatch-tag-contents tag))
		  #f))
		((eq? tag record-type-type-tag)
		 (standard-unparser-method 'TYPE
		   (lambda (type port)
		     (write-char #\space port)
		     (display (%record-type-name type) port))))
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
	    (let ((type (%record-type-descriptor record)))
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

(define (make-record-type type-name field-names #!optional default-values)
  (let ((caller 'MAKE-RECORD-TYPE))
    (guarantee-list-of-unique-symbols field-names caller)
    (let* ((names (list->vector field-names))
	   (n (vector-length names))
	   (default-record (%make-record (fix:+ 1 n) #f))
	   (record-type
	    (%record record-type-type-tag
		     (->string type-name)
		     names
		     #f
		     default-record))
	   (tag (make-dispatch-tag record-type)))
      (%record-set! record-type 3 tag)
      (%record-set! default-record 0 tag)
      (if (not (default-object? default-values))
	  (%set-record-type-default-values! record-type default-values caller))
      record-type)))

(define (record-type? object)
  (%tagged-record? record-type-type-tag object))

(define-integrable (%record-type-descriptor record)
  (dispatch-tag-contents (%record-tag record)))

(define-integrable (%record-type-name record-type)
  (%record-ref record-type 1))

(define-integrable (%record-type-field-names record-type)
  (%record-ref record-type 2))

(define-integrable (%record-type-dispatch-tag record-type)
  (%record-ref record-type 3))

(define-integrable (%record-type-default-record record-type)
  (%record-ref record-type 4))

(define-integrable (%record-type-n-fields record-type)
  (vector-length (%record-type-field-names record-type)))

(define-integrable (%record-type-length record-type)
  (%record-length (%record-type-default-record record-type)))

(define (record-type-name record-type)
  (guarantee-record-type record-type 'RECORD-TYPE-NAME)
  (%record-type-name record-type))

(define (record-type-field-names record-type)
  (guarantee-record-type record-type 'RECORD-TYPE-FIELD-NAMES)
  ;; Can't use VECTOR->LIST here because it isn't available at cold load.
  (let ((v (%record-type-field-names record-type)))
    (subvector->list v 0 (vector-length v))))

(define (record-type-default-values record-type)
  (guarantee-record-type record-type 'RECORD-TYPE-DEFAULT-VALUES)
  (let* ((default-record (%record-type-default-record record-type))
	 (n (%record-length default-record))
	 (v (make-vector (fix:- n 1))))
    (do ((i 1 (fix:+ i 1)))
	((not (fix:< i n)))
      (vector-set! v (fix:- i 1) (%record-ref default-record i)))
    v))

(define (set-record-type-default-values! record-type default-values)
  (let ((caller 'SET-RECORD-TYPE-DEFAULT-VALUES!))
    (guarantee-record-type record-type caller)
    (%set-record-type-default-values! record-type default-values caller)))

(define (%set-record-type-default-values! record-type default-values caller)
  (if (not (fix:= (guarantee-list->length default-values caller)
		  (%record-type-n-fields record-type)))
      (error:bad-range-argument default-values caller))
  (let ((default-record (%record-type-default-record record-type)))
    (do ((values default-values (cdr values))
	 (i 1 (fix:+ i 1)))
	((not (pair? values)))
      (%record-set! default-record i (car values)))))

(define (record-type-dispatch-tag record-type)
  (guarantee-record-type record-type 'RECORD-TYPE-DISPATCH-TAG)
  (%record-type-dispatch-tag record-type))

(define set-record-type-unparser-method!
  (named-lambda (set-record-type-unparser-method!/booting record-type method)
    (set! deferred-unparser-methods
	  (cons (cons record-type method) deferred-unparser-methods))
    unspecific))

(define deferred-unparser-methods '())

(define set-record-type-unparser-method!/after-boot
  (named-lambda (set-record-type-unparser-method! record-type method)
    (if (not (or (not method) (procedure? method)))
	(error:wrong-type-argument method "unparser method"
				   'SET-RECORD-TYPE-UNPARSER-METHOD!))
    (let ((tag (record-type-dispatch-tag record-type)))
      (remove-generic-procedure-generators unparse-record
					   (list (make-dispatch-tag #f) tag))
      (add-generic-procedure-generator unparse-record
	(lambda (generic tags)
	  generic
	  (and (eq? (cadr tags) tag) method))))))

(define (record-constructor record-type #!optional field-names)
  (guarantee-record-type record-type 'RECORD-CONSTRUCTOR)
  (if (or (default-object? field-names)
	  (equal? field-names (record-type-field-names record-type)))
      (%record-constructor-default-names record-type)
      (begin
	(guarantee-list field-names 'RECORD-CONSTRUCTOR)
	(%record-constructor-given-names record-type field-names))))

(define %record-constructor-default-names
  (let-syntax
      ((expand-cases
	(sc-macro-transformer
	 (lambda (form environment)
	   (let ((tag (close-syntax (list-ref form 1) environment))
		 (n-fields (close-syntax (list-ref form 2) environment))
		 (limit (close-syntax (list-ref form 3) environment))
		 (default (close-syntax (list-ref form 4) environment))
		 (make-name
		  (lambda (i)
		    (intern (string-append "v" (number->string i))))))
	     (let loop ((i 0) (names '()))
	       (if (fix:< i limit)
		   `(IF (FIX:= ,n-fields ,i)
			(LAMBDA (,@names) (%RECORD ,tag ,@names))
			,(loop (fix:+ i 1)
			       (append names (list (make-name i)))))
		   default)))))))
    (lambda (record-type)
      (let ((tag (%record-type-dispatch-tag record-type))
	    (n-fields (%record-type-n-fields record-type)))
	(expand-cases tag n-fields 16
	  (let ((length (fix:+ 1 n-fields)))
	    (letrec
		((constructor
		  (lambda field-values
		    (let ((record (%make-record length #f))
			  (lose
			   (lambda ()
			     (error:wrong-number-of-arguments constructor
							      n-fields
							      field-values))))
		      (%record-set! record 0 tag)
		      (let loop ((i 1) (values field-values))
			(if (fix:< i length)
			    (begin
			      (if (not (pair? values)) (lose))
			      (%record-set! record i (car values))
			      (loop (cdr values) (fix:+ i 1)))
			    (if (not (null? values)) (lose))))
		      record))))
	      constructor)))))))

(define (%record-constructor-given-names record-type field-names)
  (let ((indexes
	 (map (lambda (field-name)
		(record-type-field-index record-type field-name #t))
	      field-names))
	(template (%record-type-default-record record-type)))
    (letrec
	((constructor
	  (lambda field-values
	    (let ((lose
		   (lambda ()
		     (error:wrong-number-of-arguments constructor
						      (length indexes)
						      field-values))))
	      (let ((record (%copy-record template)))
		(let loop ((indexes indexes) (values field-values))
		  (if (pair? indexes)
		      (begin
			(if (not (pair? values)) (lose))
			(%record-set! record (car indexes) (car values))
			(loop (cdr indexes) (cdr values)))
		      (if (not (null? values)) (lose))))
		record)))))
      constructor)))

(define (record? object)
  (and (%record? object)
       (dispatch-tag? (%record-tag object))
       (record-type? (dispatch-tag-contents (%record-tag object)))))

(define (record-type-descriptor record)
  (guarantee-record record 'RECORD-TYPE-DESCRIPTOR)
  (%record-type-descriptor record))

(define (copy-record record)
  (guarantee-record record 'COPY-RECORD)
  (%copy-record record))

(define (record-predicate record-type)
  (guarantee-record-type record-type 'RECORD-PREDICATE)
  (let ((tag (record-type-dispatch-tag record-type)))
    (lambda (object)
      (%tagged-record? tag object))))

(define (record-accessor record-type field-name)
  (guarantee-record-type record-type 'RECORD-ACCESSOR)
  (let ((tag (record-type-dispatch-tag record-type))
	(index (record-type-field-index record-type field-name #t)))
    (letrec ((accessor
	      (lambda (record)
		(if (not (%tagged-record? tag record))
		    (error:not-tagged-record record record-type accessor))
		(%record-ref record index))))
      accessor)))

(define (record-modifier record-type field-name)
  (guarantee-record-type record-type 'RECORD-MODIFIER)
  (let ((tag (record-type-dispatch-tag record-type))
	(index (record-type-field-index record-type field-name #t)))
    (letrec ((modifier
	      (lambda (record field-value)
		(if (not (%tagged-record? tag record))
		    (error:not-tagged-record record record-type modifier))
		(%record-set! record index field-value))))
      modifier)))

(define (error:not-tagged-record record record-type modifier)
  (error:wrong-type-argument record
			     (string-append "record of type "
					    (%record-type-name record-type))
			     modifier))

(define record-copy copy-record)
(define record-updater record-modifier)

(define (record-type-field-index record-type name error?)
  ;; Can't use VECTOR->LIST here because it isn't available at cold load.
  (let* ((names (%record-type-field-names record-type))
	 (n (vector-length names)))
    (let loop ((i 0))
      (if (fix:< i n)
	  (if (eq? (vector-ref names i) name)
	      (fix:+ i 1)
	      (loop (fix:+ i 1)))
	  (and error?
	       (record-type-field-index record-type
					(error:no-such-slot record-type name)
					error?))))))

(define (->string object)
  (if (string? object)
      object
      (write-to-string object)))

(define-integrable (guarantee-list-of-unique-symbols object procedure)
  (if (not (list-of-unique-symbols? object))
      (error:wrong-type-argument object "list of unique symbols" procedure)))

(define (list-of-unique-symbols? object)
  (and (list-of-type? object symbol?)
       (let loop ((elements object))
	 (if (pair? elements)
	     (if (memq (car elements) (cdr elements))
		 #f
		 (loop (cdr elements)))
	     #t))))

(define-integrable (guarantee-record-type record-type procedure)
  (if (not (record-type? record-type))
      (error:wrong-type-argument record-type "record type" procedure)))

(define-integrable (guarantee-record record caller)
  (if (not (record? record))
      (error:wrong-type-argument record "record" caller)))

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
  (cond ((record? object) #t)
	((vector? object)
	 (and (not (fix:= (vector-length object) 0))
	      (tag->structure-type (vector-ref object 0) 'VECTOR)))
	((pair? object) (tag->structure-type (car object) 'LIST))
	(else #f)))

(define (named-structure/description structure)
  (cond ((record? structure)
	 (record-description structure))
	((named-structure? structure)
	 => (lambda (type)
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
  (receive (tag index type-name accessor-name)
      (accessor-parameters tag field-name 'VECTOR 'ACCESSOR)
    (if tag
	(lambda (structure)
	  (check-vector structure tag index type-name accessor-name)
	  (vector-ref structure index))
	(lambda (structure)
	  (check-vector-untagged structure index type-name accessor-name)
	  (vector-ref structure index)))))

(define (define-structure/vector-modifier tag field-name)
  (receive (tag index type-name accessor-name)
      (accessor-parameters tag field-name 'VECTOR 'MODIFIER)
    (if tag
	(lambda (structure value)
	  (check-vector structure tag index type-name accessor-name)
	  (vector-set! structure index value))
	(lambda (structure value)
	  (check-vector-untagged structure index type-name accessor-name)
	  (vector-set! structure index value)))))

(define (define-structure/list-accessor tag field-name)
  (receive (tag index type-name accessor-name)
      (accessor-parameters tag field-name 'LIST 'ACCESSOR)
    (if tag
	(lambda (structure)
	  (check-list structure tag index type-name accessor-name)
	  (list-ref structure index))
	(lambda (structure)
	  (check-list-untagged structure index type-name accessor-name)
	  (list-ref structure index)))))

(define (define-structure/list-modifier tag field-name)
  (receive (tag index type-name accessor-name)
      (accessor-parameters tag field-name 'LIST 'MODIFIER)
    (if tag
	(lambda (structure value)
	  (check-list structure tag index type-name accessor-name)
	  (set-car! (list-tail structure index) value))
	(lambda (structure value)
	  (check-list-untagged structure index type-name accessor-name)
	  (set-car! (list-tail structure index) value)))))

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
  (if (pair? argument-list)
      (let ((alist
	     (map (lambda (entry) (cons (car entry) (cdr entry)))
		  default-alist)))
	(let loop ((arguments argument-list))
	  (if (pair? arguments)
	      (begin
		(if (not (pair? (cdr arguments)))
		    (error "Keyword list does not have even length:"
			   argument-list))
		(set-cdr! (or (assq (car arguments) alist)
			      (error "Unknown keyword:" (car arguments)))
			  (cadr arguments))
		(loop (cddr arguments)))))
	(map cdr alist))
      (map cdr default-alist)))