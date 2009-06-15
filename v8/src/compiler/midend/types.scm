#| -*-Scheme-*-

$Id$

Copyright (c) 1995-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Types
;;; package: (compiler midend)

(declare (usual-integrations))

;; Types denote a set of values.

(define-structure
    (type
     (named (string->symbol "#[liar:type]"))
     (type vector)
     (conc-name type/)
     (constructor type:%make ())
     (constructor type:%make/bits (bits-0 bits-1 bits-2))
     (print-procedure
      (standard-unparser-method 'TYPE
	(lambda (type port)
	  (write-char #\space port)
	  (write (type:description type) port)))))

  (bits-0 0)
  (bits-1 0)
  (bits-2 0))

;; Primitive types.  Primitive types are disjoint.  Any particular value
;; may belong to only one primitive type.

(define-integrable type:*max-number-of-primitive-types* 48)

(define type:primitive-types
  (make-vector type:*max-number-of-primitive-types* #F))
(define type:number-of-primitive-types 0)
(define type:primitive-type-characteristic-predicates
  (make-vector type:*max-number-of-primitive-types* #F))
(define type:*names* '())

(define (define-type-name type name)
  (set! type:*names* (cons (cons type name) type:*names*)))

(define (type:description type)
  (let loop ((type type) (pairs type:*names*) (description '()))
    (cond ((null? pairs)
	   (cond ((null? description) 'type:empty)
		 ((null? (cdr description)) (car description))
		 (else (cons 'or (reverse description)))))
	  ((type:subset? (caar pairs) type)
	   (loop (type:except type (caar pairs)) (cdr pairs)
		 (cons (cdar pairs) description)))
	  (else
	   (loop type (cdr pairs) description)))))

(define (type:user-description type accurate?)
  (define (try t name)
    (and (type:subset? type t) name))
  (or (try type:boolean "a boolean")
      (try type:exact-non-negative-integer  "an exact non-negative integer")
      (and accurate? (try type:fixnum "a small exact integer (fixnum)"))
      (try type:exact-integer "an exact integer")
      (and accurate? (try type:flonum  "an inexact real (flonum)"))
      (try type:number  "a number")
      (try type:string  "a string")
      (try type:string  "a character")
      (try type:symbol  "a symbol")
      (try type:vector  "a vector")
      (try type:pair    "a pair")
      (try type:list    "a list")
      (try type:procedure "a procedure")
      (if (type:subset? type:any type)
	  "an object"
	  (with-output-to-string
	    (lambda () (write (type:description type)))))))

(define type:empty (type:%make))

(define (make-primitive-type)
  (let ((bit type:number-of-primitive-types))
    (if (>= bit type:*max-number-of-primitive-types*)
	(internal-error "Not enough type bits"))
    (let ((type (type:%make)))
      (vector-set! type (fix:+ 1 (fix:lsh bit -4))
		   (fix:lsh 1 (fix:and bit #xF)))
      (vector-set! type:primitive-types bit type)
      (set! type:number-of-primitive-types (+ bit 1))
      type)))


(define-integrable (type:bitwise op receiver)
  (lambda (t1 t2)
    (receiver (op (type/bits-0 t1) (type/bits-0 t2))
	      (op (type/bits-1 t1) (type/bits-1 t2))
	      (op (type/bits-2 t1) (type/bits-2 t2)))))
		   
(define type:or     (type:bitwise fix:or   type:%make/bits))
(define type:and    (type:bitwise fix:and  type:%make/bits))
(define type:except (type:bitwise fix:andc type:%make/bits))

(define (type:not t) (type:except type:any t))

(define type:subset?
  (type:bitwise (lambda (b1 b2) (fix:= (fix:or b1 b2) b2))
		(lambda (r1 r2 r3)
		  (and r1 r2 r3))))

(define type:disjoint?
  (type:bitwise (lambda (b1 b2) (fix:zero? (fix:and b1 b2)))
		(lambda (r1 r2 r3)
		  (and r1 r2 r3))))

(define type:equal?
  (type:bitwise fix:= (lambda (r1 r2 r3) (and r1 r2 r3))))
		
(define (type:or* . types)
  (reduce type:or type:empty types))

(define (type:for-each-primitive-type type procedure)
  (define (try-bits offset bits)
    (let loop ((bits bits) (offset offset))
      (cond ((fix:= 0 bits))
	    ((fix:= 0 (fix:and bits 1)) 
	     (loop (fix:lsh bits -1) (fix:+ offset 1)))
	    (else
	     (procedure (vector-ref type:primitive-types offset))
	     (loop (fix:lsh bits -1) (fix:+ offset 1))))))
  (try-bits  0 (type/bits-0 type))
  (try-bits 16 (type/bits-1 type))
  (try-bits 32 (type/bits-2 type)))


(let-syntax ((primitive-types
	      (macro names
		(define (def name)
		  `(BEGIN
		     (DEFINE ,name (MAKE-PRIMITIVE-TYPE))
		     (DEFINE-TYPE-NAME ,name ',name)))
		`(BEGIN
		   ,@(map def names)))))

  (primitive-types type:exact-zero	; special numbers
		   type:exact-one
		   type:exact-minus-one
		   type:small-fixnum:2..255
		   type:small-fixnum>255; numbers which will not overflow 
		   type:small-fixnum<-1 ;   when added or subtracted
		   type:big-fixnum+ve	; other fixnums
		   type:big-fixnum-ve	; other fixnums
		   type:bignum<0
		   type:bignum>0
		   type:ratnum
		   type:flonum
		   type:exact-recnum
		   type:inexact-recnum

		   type:interned-symbol
		   type:uninterned-symbol
		   type:pair
		   type:vector
		   type:%record
		   type:string
		   type:character
		   type:cell
		   type:bit-string

		   type:true		; special values
		   type:false
		   type:empty-list
		   type:unspecific-frob
		   type:other-constant

		   type:primitive-procedure
		   type:compiled-procedure
		   type:other-compiled-entry

		   type:entity
		   type:compiled-code-block

		   type:other		; anything else
		   ))

;; For more readable reporting
(set! type:*names* (reverse type:*names*))

(define type:any
  (let loop ((i 0) (type type:empty))
    (if (= i type:number-of-primitive-types)
	type
	(loop (+ i 1) (type:or type (vector-ref type:primitive-types i))))))

(define type:not-false (type:except type:any type:false))

(let-syntax ((alias
	      (macro (name . parts)
		`(BEGIN
		   (DEFINE ,name (TYPE:OR* ,@parts)))))
	     (alias*
	      (macro (name . parts)
		`(BEGIN
		   (DEFINE ,name (TYPE:OR* ,@parts))
		   (DEFINE-TYPE-NAME ,name ',name)))))

  (alias  type:small-fixnum>1   type:small-fixnum:2..255 type:small-fixnum>255)
  (alias  type:unsigned-byte
	  type:exact-zero type:exact-one type:small-fixnum:2..255)
  (alias  type:small-fixnum+ve  type:exact-one type:small-fixnum>1)
  (alias  type:small-fixnum-ve  type:exact-minus-one type:small-fixnum<-1)
  (alias  type:small-fixnum
	  type:exact-zero type:small-fixnum-ve type:small-fixnum+ve)

  (alias  type:big-fixnum       type:big-fixnum-ve type:big-fixnum+ve)

  (alias  type:small-fixnum>=0  type:exact-zero type:small-fixnum+ve)
  (alias* type:fixnum>=0        type:small-fixnum>=0 type:big-fixnum+ve)

  (alias  type:fixnum+ve        type:small-fixnum+ve type:big-fixnum+ve)
  (alias  type:fixnum-ve        type:small-fixnum-ve type:big-fixnum-ve)

  (alias* type:fixnum           type:small-fixnum type:big-fixnum)
  (alias* type:bignum           type:bignum<0 type:bignum>0)
  (alias* type:exact-non-negative-integer  type:bignum>0 type:fixnum>=0)
  (alias* type:exact-integer    type:fixnum type:bignum)
  (alias  type:exact-real       type:fixnum type:bignum type:ratnum)
  (alias  type:inexact-real     type:flonum)
  (alias  type:real             type:exact-real type:inexact-real)
  (alias  type:recnum           type:exact-recnum type:inexact-recnum)
  (alias* type:exact-number     type:exact-recnum type:exact-real)
  (alias* type:inexact-number   type:inexact-recnum type:inexact-real)
  (alias* type:number           type:exact-number type:inexact-number)

  (alias* type:symbol           type:interned-symbol type:uninterned-symbol)
  (alias  type:boolean          type:true type:false)
  (alias  type:vector-length    type:small-fixnum>=0)
  (alias  type:string-length    type:fixnum>=0)

  (alias  type:list             type:empty-list type:pair)

  (alias  type:tc-constant
	  type:true type:false type:empty-list type:unspecific-frob
	  type:other-constant)

  (alias* type:compiled-entry
	  type:compiled-procedure type:other-compiled-entry)

  (alias  type:procedure
	  type:compiled-procedure type:entity type:primitive-procedure)

  ;; It is horrible, but this is how flonum vectors are represented
  (alias  type:flonum-vector    type:flonum)

  (alias  type:unspecified type:any)

  )

;; Note: these are processed in last-to-first order to construct a description.

(define-type-name type:boolean        'BOOLEAN)
(define-type-name (type:except type:fixnum type:fixnum>=0) 'NEGATIVE-FIXNUM)
(define-type-name type:fixnum         'FIXNUM)
(define-type-name type:exact-integer  'EXACT-INTEGER)
(define-type-name type:exact-number   'EXACT-NUMBER)
(define-type-name type:inexact-number 'INEXACT-NUMBER)
(define-type-name type:number         'NUMBER)
(define-type-name type:not-false      'NOT-FALSE)
(define-type-name type:any    'type:ANY)


;;;; Correspondence between typecodes and primitive types
;;

;; The tag is `covered' by this type.

(define type:number-of-typecodes 64)

(define type:tag->covering-type (make-vector type:number-of-typecodes #F))

(define (type:typecode->type typecode)
  (vector-ref type:tag->covering-type typecode))

(define type:tag->type-pair (make-vector type:number-of-typecodes #F))

;; This primitive type is `covered' by these tags.
(define type:primitive-type->covering-tags
  (make-vector type:number-of-primitive-types '()))

(let ()
  (define (define-tag tag-name type)
    (let ((tag (machine-tag tag-name)))
      (if (vector-ref type:tag->covering-type tag)
	  (internal-error "TYPECODE <-> type: configuarion error"))
      (vector-set! type:tag->covering-type tag type)))

  (define-tag 'POSITIVE-FIXNUM     type:fixnum>=0)
  (define-tag 'NEGATIVE-FIXNUM     (type:except type:fixnum type:fixnum>=0))
  (define-tag 'BIGNUM              type:bignum)
  (define-tag 'RATNUM              type:ratnum)
  (define-tag 'RECNUM              type:recnum)
  (define-tag 'FLONUM              type:flonum)
  (define-tag 'PAIR                type:pair)
  (define-tag 'VECTOR              type:vector)
  (define-tag 'RECORD              type:%record)
  (define-tag 'VECTOR-1B           type:bit-string)
  (define-tag 'VECTOR-8B           type:string)
  (define-tag 'CONSTANT            type:tc-constant)
  (define-tag 'PRIMITIVE           type:primitive-procedure)
  (define-tag 'ENTITY              type:entity)
  (define-tag 'COMPILED-ENTRY      type:compiled-entry)
  (define-tag 'CELL                type:cell)
  (define-tag 'COMPILED-CODE-BLOCK type:compiled-code-block)
  (define-tag 'UNINTERNED-SYMBOL   type:uninterned-symbol)
  (define-tag 'INTERNED-SYMBOL     type:interned-symbol)
  (define-tag 'CHARACTER           type:character)

  (let ((unallocated-types
	 (do ((i 0 (+ i 1))  
	      (t type:empty
		 (type:or t (or (vector-ref type:tag->covering-type i) type:empty))))
	     ((= i type:number-of-typecodes) (type:not t)))))
    (type:for-each-primitive-type
     (type:except unallocated-types type:other)
     (lambda (t)
       (internal-warning "Type has not been allocated to typecode(s)" t)))
				  
    (do ((i 0 (+ i 1)))
	((= i type:number-of-typecodes))
      (if (not (vector-ref type:tag->covering-type i))
	  (vector-set! type:tag->covering-type i unallocated-types)))

    ;; Now for each typecode, calculate type-pairs
    (do ((i 0 (+ i 1)))
	((= i type:number-of-typecodes))
      (do ((j 0 (+ j 1))
	   (t type:empty
	      (if (= i j)
		  t
		  (type:or t (vector-ref type:tag->covering-type j)))))
	  ((= j type:number-of-typecodes)
	   (vector-set! type:tag->type-pair i
			(cons (vector-ref type:tag->covering-type i) t)))))))

(define type:of-object
  (let* ((max-fixnum        (object-new-type 0 -1))
	 (max-small-fixnum  (quotient max-fixnum 2))
	 (min-small-fixnum  (- -1 max-small-fixnum)))
    (lambda (x)
      ;; This should do lots of magic with typecodes
      (cond ((fixnum? x)
	     (cond ((eqv? x 0)                  type:exact-zero)
		   ((eqv? x 1)                  type:exact-one)
		   ((eqv? x -1)                 type:exact-minus-one)
		   ((<= 2 x 255)                type:small-fixnum:2..255)
		   ((<= 256 x max-small-fixnum) type:small-fixnum>255)
		   ((> x max-small-fixnum)      type:big-fixnum+ve)
		   ((<= min-small-fixnum x -2)  type:small-fixnum<-1)
		   ((< x min-small-fixnum)      type:big-fixnum-ve)
		   (else (internal-error "Unclassified FIXNUM" x))))
	    ((object-type? (object-type #F) x)
	     (cond ((eq? x #F)          type:false)
		   ((eq? x #T)          type:true)
		   ((eq? x unspecific)  type:unspecific-frob)
		   ((eq? x '())         type:empty-list)
		   (else                type:other-constant)))
	    (else
	     ;; The returned value might not be unitary.
	     (type:typecode->type (object-type x)))))))


(define type:->constant?
  ;; return a quoted constant if the type describes that constant,
  ;; otherwise return #F
  (let ((cover (type:or* type:exact-zero type:exact-one type:exact-minus-one
			 type:false type:true
			 type:empty-list type:unspecific-frob)))
    (lambda (type)
      (cond ((not (type:subset? type cover))    #F) ; quick exit
	    ((type:equal? type type:false)            `(QUOTE #F))
	    ((type:equal? type type:true)             `(QUOTE #T))
	    ((type:equal? type type:exact-zero)       `(QUOTE 0))
	    ((type:equal? type type:exact-one)        `(QUOTE 1))
	    ((type:equal? type type:exact-minus-one)  `(QUOTE -1))
	    ((type:equal? type type:empty-list)       `(QUOTE ()))
	    ((type:equal? type type:unspecific-frob)  `(QUOTE ,unspecific))
	    (else  #F)))))

;;(define (type:->covering-tags type)
;;  "Return a list of tags that cover TYPE")
;;
;;(define (type:->predicate-tags type)
;;  "Return a list of tags that exactly match TYPE, or return #F")

;; Known simple predicates
;;
;; *OPERATOR-PREDICATE-TEST-TYPES* holds pairs of types. The CAR is the
;; potentially positive cases, CDR for potentially negative cases.
;; They may overlap and *must* union to all types.

(define *operator-predicate-test-types* (make-monotonic-strong-eq-hash-table))

(define (operator-predicate-test-type op)
  (monotonic-strong-eq-hash-table/get *operator-predicate-test-types* op #F))

(let ()
  (define (define-predicate-test-types op type1 #!optional type2)
    (monotonic-strong-eq-hash-table/put!
     *operator-predicate-test-types* op
     (cons type1 
	   (if (default-object? type2)
	       (type:not type1)
	       type2))))

  (define (def-prim name . types)
    (apply define-predicate-test-types (make-primitive-procedure name) types))

  (def-prim '%RECORD?       type:%record)
  (def-prim 'BIT-STRING?    type:bit-string)
  (def-prim 'CELL?          type:cell)
  (def-prim 'CHAR?          type:character)
  (def-prim 'FIXNUM?        type:fixnum)
  (def-prim 'FLONUM?        type:flonum)
  (def-prim 'INDEX-FIXNUM?  type:fixnum>=0)
  (def-prim 'NOT            type:false)
  (def-prim 'NULL?          type:empty-list)
  (def-prim 'PAIR?          type:pair)
  (def-prim 'STRING?        type:string)
  (def-prim 'VECTOR?        type:vector)
  (def-prim 'INTEGER?       type:exact-integer)
  (define-predicate-test-types %compiled-entry? type:compiled-entry)
  )

(define (type:tag->test-types tag)
  (vector-ref type:tag->type-pair tag))
  

;;  Procedure types
;;


(define-structure
    (%procedure-type
     (type vector)
     (named (string->symbol "#[liar:procedure-type]"))
     (constructor make-procedure-type)
     (predicate procedure-type?)
     (conc-name procedure-type/))
  argument-types			; can be called on these types
  argument-assertions			; returning guarantees these types
  result-type
  effects-performed
  effects-observed
  (implementation-type))

;; Note[1] The RESULT-TYPE should be TYPE:UNSPECIFIED for an operator
;; without a specified return value (e.g SET-CAR!).  TYPE:NONE means
;; that there is no value because the procedure never returns
;; (divergence) and TYPE:UNSPECIFIC-FROB means exactly the
;; `unspecific' object.
;;
;; Note[2] If the ARGUMENT-ASSERTIONS are a subset of the ARGUMENT-TYPES
;; then the procedure has a control flow element (i.e. signalling an
;; error), even if it is otherwise effect-insensitive.
;;
;; Note that we cannot make inferences from the ARGUMENT-ASSERTIONS of a
;; procedure that, like the primitives, is allowed to restart with an
;; new value, or return


(define *operator-types* (make-monotonic-strong-eq-hash-table))
;;(define *operator-variants* (make-monotonic-strong-eq-hash-table))


;;(define (operator-variants op)
;;  (monotonic-strong-eq-hash-table/get *operator-variants* op '()))


(define (define-operator-type op type)
  (monotonic-strong-eq-hash-table/put! *operator-types* op type))

(define (operator-type op)
  (monotonic-strong-eq-hash-table/get *operator-types* op #F))

(define (operator-sensitive-effects op)
  (cond ((operator-type op)
	 => procedure-type/effects-observed)
	((operator/satisfies? op '(SIDE-EFFECT-INSENSITIVE))
	 effect:none)
	(else effect:unknown)))

(define (procedure-type base . initial-qualifiers)
  ;; alternative interface: (procedure-type domain range . <qualifiers>)
  (define (full-domain domain)
    (let loop ((Ts domain))
      (cond ((null? Ts) '())
	    ((pair? Ts) (cons type:any (loop (cdr Ts))))
	    (else       type:any))))
  (define (qualify qualifiers
		   asserted-domain domain range
		   effects-observed effects-performed
		   implementation-type)
    (let loop ((qualifiers qualifiers))
      (cond ((not (pair? qualifiers))
	     (make-procedure-type asserted-domain domain range
				  effects-performed effects-observed
				  implementation-type))
	    ((eq? (car qualifiers) 'EFFECT-FREE)
	     (set! effects-performed effect:none)
	     (loop (cdr qualifiers)))
	    ((eq? (car qualifiers) 'EFFECT-INSENSITIVE)
	     (set! effects-observed effect:none)
	     (loop (cdr qualifiers)))
	    ((eq? (car qualifiers) 'FUNCTION)
	     (loop (cons* 'EFFECT-FREE 'EFFECT-INSENSITIVE (cdr qualifiers))))
	    ((eq? (car qualifiers) 'EFFECT)
	     (set! effects-performed (cadr qualifiers))
	     (loop (cddr qualifiers)))
	    ((eq? (car qualifiers) 'EFFECT-SENSITIVE)
	     (set! effects-observed (cadr qualifiers))
	     (loop (cddr qualifiers)))
	    ((eq? (car qualifiers) 'IMPLEMENTATION-TYPE)
	     (set! implementation-type (cadr qualifiers))
	     (loop (cddr qualifiers)))
	    ((eq? (car qualifiers) 'UNCHECKED)
	     ;; Not sure what this really means.
	     (loop (cdr qualifiers)))
	    (else (internal-error "Bad PROCEDURE-TYPE qualifiers"
				  base initial-qualifiers)))))

  (if (procedure-type? base)
      (qualify initial-qualifiers
	       (procedure-type/argument-assertions base)
	       (procedure-type/argument-types base)
	       (procedure-type/result-type base)
	       (procedure-type/effects-observed base)
	       (procedure-type/effects-performed base)
	       (procedure-type/implementation-type base))
      (let ((domain base)
	    (range (car initial-qualifiers)))
	(qualify (cdr initial-qualifiers)
		 (and domain (full-domain domain))
		 domain
		 range
		 effect:unknown effect:unknown
		 type:procedure))))

(define (primitive-procedure-type . spec)
  (apply procedure-type
	 (append spec (list 'implementation-type type:primitive-procedure))))
