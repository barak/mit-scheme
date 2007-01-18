#| -*-Scheme-*-

$Id: record.scm,v 1.59 2007/01/18 02:30:37 riastradh Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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
		   #f
		   "record-type"
		   '#(DISPATCH-TAG NAME FIELD-NAMES DEFAULT-INITS EXTENSION)
		   (vector-cons 5 (lambda () #f))
		   #f)))
    (set! record-type-type-tag (make-dispatch-tag type))
    (%record-set! type 0 record-type-type-tag)
    (%record-set! type 1 record-type-type-tag))
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
		 (standard-unparser-method 'RECORD-TYPE
		   (lambda (type port)
		     (write-char #\space port)
		     (display (%record-type-name type) port))))
		((eq? tag (built-in-dispatch-tag 'DISPATCH-TAG))
		 (standard-unparser-method 'DISPATCH-TAG
		   (lambda (tag port)
		     (write-char #\space port)
		     (write (dispatch-tag-contents tag) port))))
		(else record-method))))))
  (set! %set-record-type-default-inits!
	%set-record-type-default-inits!/after-boot)
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

(define (make-record-type type-name field-names
			  #!optional default-inits unparser-method)
  (let ((caller 'MAKE-RECORD-TYPE))
    (guarantee-list-of-unique-symbols field-names caller)
    (let* ((names (list->vector field-names))
	   (n (vector-length names))
	   (record-type
	    (%record record-type-type-tag
		     #f
		     (->type-name type-name)
		     names
		     (vector-cons n (lambda () #f))
		     #f))
	   (tag (make-dispatch-tag record-type)))
      (%record-set! record-type 1 tag)
      (if (not (default-object? default-inits))
	  (%set-record-type-default-inits! record-type default-inits caller))
      (if (not (default-object? unparser-method))
	  (set-record-type-unparser-method! record-type unparser-method))
      record-type)))

(define (record-type? object)
  (%tagged-record? record-type-type-tag object))

(define-integrable (%record-type-descriptor record)
  (dispatch-tag-contents (%record-tag record)))

(define-integrable (%record-type-dispatch-tag record-type)
  (%record-ref record-type 1))

(define-integrable (%record-type-name record-type)
  (%record-ref record-type 2))

(define-integrable (%record-type-field-names record-type)
  (%record-ref record-type 3))

(define-integrable (%record-type-default-inits record-type)
  (%record-ref record-type 4))

(define-integrable (%record-type-extension record-type)
  (%record-ref record-type 5))

(define-integrable (%set-record-type-extension! record-type extension)
  (%record-set! record-type 5 extension))

(define-integrable (%record-type-n-fields record-type)
  (vector-length (%record-type-field-names record-type)))

(define-integrable (%record-type-length record-type)
  (fix:+ 1 (%record-type-n-fields record-type)))

(define (record-type-dispatch-tag record-type)
  (guarantee-record-type record-type 'RECORD-TYPE-DISPATCH-TAG)
  (%record-type-dispatch-tag record-type))

(define (record-type-name record-type)
  (guarantee-record-type record-type 'RECORD-TYPE-NAME)
  (%record-type-name record-type))

(define (record-type-field-names record-type)
  (guarantee-record-type record-type 'RECORD-TYPE-FIELD-NAMES)
  ;; Can't use VECTOR->LIST here because it isn't available at cold load.
  (let ((v (%record-type-field-names record-type)))
    (subvector->list v 0 (vector-length v))))

(define (record-type-default-inits record-type)
  (guarantee-record-type record-type 'RECORD-TYPE-DEFAULT-INITS)
  (vector->list (%record-type-default-inits record-type)))

(define (set-record-type-default-inits! record-type default-inits)
  (let ((caller 'SET-RECORD-TYPE-DEFAULT-INITS!))
    (guarantee-record-type record-type caller)
    (%set-record-type-default-inits! record-type default-inits caller)))

(define %set-record-type-default-inits!
  (lambda (record-type default-inits caller)
    caller
    (let ((v (%record-type-default-inits record-type)))
      (do ((values default-inits (cdr values))
	   (i 0 (fix:+ i 1)))
	  ((not (pair? values)))
	(vector-set! v i (car values))))))

(define %set-record-type-default-inits!/after-boot
  (named-lambda (%set-record-type-default-inits! record-type default-inits
						 caller)
    (let ((v (%record-type-default-inits record-type)))
      (if (not (fix:= (guarantee-list-of-type->length
		       default-inits thunk? "default initializers" caller)
		      (vector-length v)))
	  (error:bad-range-argument default-inits caller))
      (do ((values default-inits (cdr values))
	   (i 0 (fix:+ i 1)))
	  ((not (pair? values)))
	(vector-set! v i (car values))))))

(define (record-type-default-value record-type field-name)
  ((vector-ref (%record-type-default-inits record-type)
	       (fix:- (record-type-field-index record-type field-name #t) 1))))

(define set-record-type-unparser-method!
  (named-lambda (set-record-type-unparser-method!/booting record-type method)
    (let loop ((ms deferred-unparser-methods))
      (if (pair? ms)
	  (if (eq? (caar ms) record-type)
	      (set-cdr! (car ms) method)
	      (loop (cdr ms)))
	  (begin
	    (set! deferred-unparser-methods
		  (cons (cons record-type method) deferred-unparser-methods))
	    unspecific)))))

(define deferred-unparser-methods '())

(define set-record-type-unparser-method!/after-boot
  (named-lambda (set-record-type-unparser-method! record-type method)
    (guarantee-record-type record-type 'SET-RECORD-TYPE-UNPARSER-METHOD!)
    (if method
	(guarantee-unparser-method method 'SET-RECORD-TYPE-UNPARSER-METHOD!))
    (let ((tag (%record-type-dispatch-tag record-type)))
      (remove-generic-procedure-generators
       unparse-record
       (list (record-type-dispatch-tag rtd:unparser-state) tag))
      (if method
	  (add-generic-procedure-generator unparse-record
	    (lambda (generic tags)
	      generic
	      (and (eq? (cadr tags) tag) method)))))))

(define (record-type-extension record-type)
  (guarantee-record-type record-type 'RECORD-TYPE-EXTENSION)
  (%record-type-extension record-type))

(define (set-record-type-extension! record-type extension)
  (guarantee-record-type record-type 'SET-RECORD-TYPE-EXTENSION!)
  (%set-record-type-extension! record-type extension))

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
	  (let ((reclen (fix:+ 1 n-fields)))
	    (letrec
		((constructor
		  (lambda field-values
		    (let ((record (%make-record reclen #f))
			  (lose
			   (lambda ()
			     (error:wrong-number-of-arguments constructor
							      n-fields
							      field-values))))
		      (%record-set! record 0 tag)
		      (do ((i 1 (fix:+ i 1))
			   (vals field-values (cdr vals)))
			  ((not (fix:< i reclen))
			   (if (not (null? vals)) (lose)))
			(if (not (pair? vals)) (lose))
			(%record-set! record i (car vals)))
		      record))))
	      constructor)))))))

(define (%record-constructor-given-names record-type field-names)
  (let* ((indexes
	  (map (lambda (field-name)
		 (record-type-field-index record-type field-name #t))
	       field-names))
	 (defaults
	   (let* ((n (%record-type-length record-type))
		 (seen? (vector-cons n #f)))
	     (do ((indexes indexes (cdr indexes)))
		 ((not (pair? indexes)))
	       (vector-set! seen? (car indexes) #t))
	     (do ((i 1 (fix:+ i 1))
		  (k 0 (if (vector-ref seen? i) k (fix:+ k 1))))
		 ((not (fix:< i n))
		  (let ((v (vector-cons k #f)))
		    (do ((i 1 (fix:+ i 1))
			 (j 0
			    (if (vector-ref seen? i)
				j
				(begin
				  (vector-set! v j i)
				  (fix:+ j 1)))))
			((not (fix:< i n))))
		    v))))))
    (letrec
	((constructor
	  (lambda field-values
	    (let ((lose
		   (lambda ()
		     (error:wrong-number-of-arguments constructor
						      (length indexes)
						      field-values))))
	      (let ((record
		     (%make-record (%record-type-length record-type) #f)))
		(%record-set! record 0 (%record-type-dispatch-tag record-type))
		(do ((indexes indexes (cdr indexes))
		     (values field-values (cdr values)))
		    ((not (pair? indexes))
		     (if (not (null? values)) (lose)))
		  (if (not (pair? values)) (lose))
		  (%record-set! record (car indexes) (car values)))
		(let ((v (%record-type-default-inits record-type))
		      (n (vector-length defaults)))
		  (do ((i 0 (fix:+ i 1)))
		      ((not (fix:< i n)))
		    (%record-set!
		     record
		     (vector-ref defaults i)
		     ((vector-ref v (fix:- (vector-ref defaults i) 1))))))
		record)))))
      constructor)))

(define (record-keyword-constructor record-type)
  (letrec
      ((constructor
	(lambda keyword-list
	  (let ((n (%record-type-length record-type)))
	    (let ((record (%make-record n #f))
		  (seen? (vector-cons n #f)))
	      (%record-set! record 0 (%record-type-dispatch-tag record-type))
	      (do ((kl keyword-list (cddr kl)))
		  ((not (and (pair? kl)
			     (symbol? (car kl))
			     (pair? (cdr kl))))
		   (if (not (null? kl))
		       (error:not-keyword-list keyword-list constructor)))
		(let ((i (record-type-field-index record-type (car kl) #t)))
		  (if (not (vector-ref seen? i))
		      (begin
			(%record-set! record i (cadr kl))
			(vector-set! seen? i #t)))))
	      (let ((v (%record-type-default-inits record-type)))
		(do ((i 1 (fix:+ i 1)))
		    ((not (fix:< i n)))
		  (if (not (vector-ref seen? i))
		      (%record-set! record i ((vector-ref v (fix:- i 1)))))))
	      record)))))
    constructor))

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

(define (->type-name object)
  (let* ((string
	  (cond ((string? object) object)
		((symbol? object) (symbol-name object))
		(else (error:wrong-type-argument object "type name" #f))))
	 (n (string-length string)))
    (if (and (fix:> n 2)
	     (char=? (string-ref string 0) #\<)
	     (char=? (string-ref string (fix:- n 1)) #\>))
	(substring string 1 (fix:- n 1))
	string)))

(define (list-of-unique-symbols? object)
  (and (list-of-type? object symbol?)
       (let loop ((elements object))
	 (if (pair? elements)
	     (if (memq (car elements) (cdr elements))
		 #f
		 (loop (cdr elements)))
	     #t))))

(define-guarantee list-of-unique-symbols "list of unique symbols")
(define-guarantee record-type "record type")
(define-guarantee record "record")

;;;; Runtime support for DEFINE-STRUCTURE

(define rtd:structure-type)
(define make-define-structure-type)
(define structure-type?)
(define structure-type/physical-type)
(define structure-type/name)
(define structure-type/field-names)
(define structure-type/field-indexes)
(define structure-type/default-inits)
(define structure-type/unparser-method)
(define set-structure-type/unparser-method!)
(define structure-type/tag)
(define structure-type/length)

(define (initialize-structure-type-type!)
  (set! rtd:structure-type
	(make-record-type "structure-type"
			  '(PHYSICAL-TYPE NAME FIELD-NAMES FIELD-INDEXES
					  DEFAULT-INITS UNPARSER-METHOD TAG
					  LENGTH)))
  (set! make-define-structure-type
	(let ((constructor (record-constructor rtd:structure-type)))
	  (lambda (physical-type name field-names field-indexes default-inits
				 unparser-method tag length)
	    (constructor physical-type
			 name
			 field-names
			 field-indexes
			 default-inits
			 unparser-method
			 tag
			 length))))
  (set! structure-type?
	(record-predicate rtd:structure-type))
  (set! structure-type/physical-type
	(record-accessor rtd:structure-type 'PHYSICAL-TYPE))
  (set! structure-type/name
	(record-accessor rtd:structure-type 'NAME))
  (set! structure-type/field-names
	(record-accessor rtd:structure-type 'FIELD-NAMES))
  (set! structure-type/field-indexes
	(record-accessor rtd:structure-type 'FIELD-INDEXES))
  (set! structure-type/default-inits
	(record-accessor rtd:structure-type 'DEFAULT-INITS))
  (set! structure-type/unparser-method
	(record-accessor rtd:structure-type 'UNPARSER-METHOD))
  (set! set-structure-type/unparser-method!
	(record-modifier rtd:structure-type 'UNPARSER-METHOD))
  (set! structure-type/tag
	(record-accessor rtd:structure-type 'TAG))
  (set! structure-type/length
	(record-accessor rtd:structure-type 'LENGTH))
  unspecific)

(define-integrable (structure-type/field-index type field-name)
  (vector-ref (structure-type/field-indexes type)
	      (structure-type/field-name-index type field-name)))

(define-integrable (structure-type/default-init type field-name)
  (vector-ref (structure-type/default-inits type)
	      (structure-type/field-name-index type field-name)))

(define (structure-type/field-name-index type field-name)
  (let ((names (structure-type/field-names type)))
    (let ((n (vector-length names)))
      (let loop ((i 0))
	(if (not (fix:< i n))
	    (error:no-such-slot type field-name))
	(if (eq? (vector-ref names i) field-name)
	    i
	    (loop (fix:+ i 1)))))))

(define (structure-tag/unparser-method tag physical-type)
  (let ((type (tag->structure-type tag physical-type)))
    (and type
	 (structure-type/unparser-method type))))

(define (named-structure? object)
  (cond ((record? object) #t)
	((vector? object)
	 (and (not (fix:= (vector-length object) 0))
	      (tag->structure-type (vector-ref object 0) 'VECTOR)))
	((pair? object) (tag->structure-type (car object) 'LIST))
	(else #f)))

(define (tag->structure-type tag physical-type)
  (if (structure-type? tag)
      (and (eq? (structure-type/physical-type tag) physical-type)
	   tag)
      (let ((type (named-structure/get-tag-description tag)))
	(and (structure-type? type)
	     (eq? (structure-type/physical-type type) physical-type)
	     type))))

(define (named-structure/description structure)
  (cond ((record? structure)
	 (record-description structure))
	((named-structure? structure)
	 => (lambda (type)
	      (let ((accessor (if (pair? structure) list-ref vector-ref)))
		(map (lambda (field-name index)
		       `(,field-name ,(accessor structure index)))
		     (vector->list (structure-type/field-names type))
		     (vector->list (structure-type/field-indexes type))))))
	(else
	 (error:wrong-type-argument structure "named structure"
				    'NAMED-STRUCTURE/DESCRIPTION))))

(define (define-structure/default-value type field-name)
  ((structure-type/default-init type field-name)))

(define (define-structure/keyword-constructor type)
  (let ((names (structure-type/field-names type))
	(indexes (structure-type/field-indexes type))
	(inits (structure-type/default-inits type))
	(tag (structure-type/tag type))
	(len (structure-type/length type)))
    (let ((n (vector-length names)))
      (lambda arguments
	(let ((v (vector-cons len #f)))
	  (if tag
	      (vector-set! v 0 tag))
	  (let ((seen? (make-vector n #f)))
	    (do ((args arguments (cddr args)))
		((not (pair? args)))
	      (if (not (pair? (cdr args)))
		  (error:not-keyword-list arguments #f))
	      (let ((field-name (car args)))
		(let loop ((i 0))
		  (if (not (fix:< i n))
		      (error:no-such-slot type field-name))
		  (if (eq? (vector-ref names i) field-name)
		      (if (not (vector-ref seen? i))
			  (begin
			    (vector-set! v
					 (vector-ref indexes i)
					 (cadr args))
			    (vector-set! seen? i #t)))
		      (loop (fix:+ i 1))))))
	    (do ((i 0 (fix:+ i 1)))
		((not (fix:< i n)))
	      (if (not (vector-ref seen? i))
		  (vector-set! v
			       (vector-ref indexes i)
			       ((vector-ref inits i))))))
	  (if (eq? (structure-type/physical-type type) 'LIST)
	      (do ((i (fix:- len 1) (fix:- i 1))
		   (list '() (cons (vector-ref v i) list)))
		  ((not (fix:>= i 0)) list))
	      v))))))

;;;; Support for safe accessors

(define (define-structure/vector-accessor type field-name)
  (let ((index (structure-type/field-index type field-name)))
    (if (structure-type/tag type)
	(lambda (structure)
	  (check-vector-tagged structure type)
	  (vector-ref structure index))
	(lambda (structure)
	  (check-vector-untagged structure type)
	  (vector-ref structure index)))))

(define (define-structure/vector-modifier type field-name)
  (let ((index (structure-type/field-index type field-name)))
    (if (structure-type/tag type)
	(lambda (structure value)
	  (check-vector-tagged structure type)
	  (vector-set! structure index value))
	(lambda (structure value)
	  (check-vector-untagged structure type)
	  (vector-set! structure index value)))))

(define (define-structure/list-accessor type field-name)
  (let ((index (structure-type/field-index type field-name)))
    (if (structure-type/tag type)
	(lambda (structure)
	  (check-list-tagged structure type)
	  (list-ref structure index))
	(lambda (structure)
	  (check-list-untagged structure type)
	  (list-ref structure index)))))

(define (define-structure/list-modifier type field-name)
  (let ((index (structure-type/field-index type field-name)))
    (if (structure-type/tag type)
	(lambda (structure value)
	  (check-list-tagged structure type)
	  (set-car! (list-tail structure index) value))
	(lambda (structure value)
	  (check-list-untagged structure type)
	  (set-car! (list-tail structure index) value)))))

(define-integrable (check-vector-tagged structure type)
  (if (not (and (vector? structure)
		(fix:= (vector-length structure)
		       (structure-type/length type))
		(eq? (vector-ref structure 0) (structure-type/tag type))))
      (error:wrong-type-argument structure type #f)))

(define-integrable (check-vector-untagged structure type)
  (if (not (and (vector? structure)
		(fix:= (vector-length structure)
		       (structure-type/length type))))
      (error:wrong-type-argument structure type #f)))

(define-integrable (check-list-tagged structure type)
  (if (not (and (eq? (list?->length structure) (structure-type/length type))
		(eq? (car structure) (structure-type/tag type))))
      (error:wrong-type-argument structure type #f)))

(define-integrable (check-list-untagged structure type)
  (if (not (eq? (list?->length structure) (structure-type/length type)))
      (error:wrong-type-argument structure type #f)))