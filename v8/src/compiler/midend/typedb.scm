#| -*-Scheme-*-

$Id$

Copyright (c) 1996-1999 Massachusetts Institute of Technology

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

;;;; Types of known operators
;;
;; Note: this table is the initial table of type behaviours.  typerew has
;; lots of its own tables, especially for numerical operations.
;; typerew uses this information to pad out its own tables.
;;
;; Remember that global procedures need to be integrated as
;;   (access NAME system-blocbal-environment)
;; for this info to be used.

;; NOTE: we need to think of what to do for #!optionals and how typerew
;; could be made to use the info.  The simplest but least refined
;; option is just encode #!optionals as a rest list of type:any.  At
;; least typerew could be made to work with the mandatory argument and
;; return types.

;;; package: (compiler midend)

(declare (usual-integrations))

(define-operator-type 'STRING->SYMBOL
  (procedure-type (list type:string) type:interned-symbol
		  'effect-sensitive effect:string-set!))

(define-operator-type 'INTERN
  (procedure-type (list type:string) type:interned-symbol
		  'effect-sensitive effect:string-set!))

(define-operator-type 'SYMBOL->STRING
  (procedure-type (list type:symbol) type:string
		  'effect effect:allocation))

;; string * string -> bool  predicates

(for-each
    (lambda (name)
      (define-operator-type name
	(procedure-type (list type:string type:string) type:boolean
			'effect-sensitive effect:string-set!)))
  '(STRING<?    STRING=?    STRING>?    STRING<=?    STRING>=?
    STRING-CI<? STRING-CI=? STRING-CI>? STRING-CI<=? STRING-CI>=?
    STRING-PREFIX?     STRING-SUFFIX?
    STRING-PREFIX-CI?  STRING-SUFFIX-CI?))

(define-operator-type 'SUBSTRING?
  (procedure-type (list type:string type:string)
		  (type:or type:false type:fixnum>=0)
		  'effect-sensitive effect:string-set!))

(define-operator-type 'STRING-APPEND
  (procedure-type type:string
		  type:string
		  'effect-sensitive effect:string-set!
		  'effect effect:allocation))

(define-operator-type 'STRING-COPY
  (procedure-type type:string
		  type:string
		  'effect-sensitive effect:string-set!
		  'effect effect:allocation))

(let ((list-or-vector (type:or type:list type:vector)))
  (define-operator-type 'SORT
    (procedure-type (list list-or-vector type:procedure)
		    list-or-vector
		    'effect-sensitive effect:unknown))
  (define-operator-type 'SORT!
    (procedure-type (list list-or-vector type:procedure)
		    list-or-vector
		    'effect-sensitive effect:unknown)))

(define-operator-type 'FOR-EACH
  (procedure-type (cons* type:procedure type:list)
		  type:unspecified
		  'effect-sensitive effect:unknown))

(define-operator-type 'MAP
  (procedure-type (cons* type:procedure type:list)
		  type:list
		  'effect-sensitive effect:unknown))

;; The following error:* procedures have return type empty, which means
;; the procedure never returns.  This is only true if there are no
;; restarts for the relevant error conditions.

(define-operator-type 'ERROR:WRONG-TYPE-ARGUMENT
  (procedure-type (list type:any type:any type:any) type:empty
		  'function))

(define-operator-type 'ERROR:WRONG-TYPE-DATUM
  (procedure-type (list type:any type:any) type:empty
		  'function))

(define-operator-type 'ERROR:BAD-RANGE-ARGUMENT
  (procedure-type (list type:any type:any) type:empty
		  'function))

(define-operator-type 'EXACT->INEXACT
  (procedure-type (list type:number) type:inexact-number 'function))

(define-operator-type 'INEXACT->EXACT
  (procedure-type (list type:number) type:exact-number 'function))

(define-operator-type 'CEILING->EXACT
  (procedure-type (list type:number) type:exact-integer 'function))

(define-operator-type 'FLOOR->EXACT
  (procedure-type (list type:number) type:exact-integer 'function))

(define-operator-type 'ROUND->EXACT
  (procedure-type (list type:number) type:exact-integer 'function))

(define-operator-type 'TRUNCATE->EXACT
  (procedure-type (list type:number) type:exact-integer 'function))

(define-operator-type (make-primitive-procedure 'CONS)
  (primitive-procedure-type (list type:any type:any) type:pair
			    'effect-insensitive
			    'effect effect:allocation))

(define-operator-type (make-primitive-procedure 'CAR)
  (primitive-procedure-type (list type:pair) type:any
			    'effect-free
			    'effect-sensitive effect:set-car!))

(define-operator-type (make-primitive-procedure 'CDR)
  (primitive-procedure-type (list type:pair) type:any
			    'effect-free
			    'effect-sensitive effect:set-cdr!))

(define-operator-type (make-primitive-procedure 'SET-CAR!)
  (primitive-procedure-type (list type:pair type:any) type:unspecified
			    'effect-insensitive
			    'effect effect:set-car!))

(define-operator-type (make-primitive-procedure 'SET-CDR!)
  (primitive-procedure-type (list type:pair type:any) type:unspecified
			    'effect-insensitive
			    'effect effect:set-cdr!))

(define-operator-type (make-primitive-procedure 'LENGTH)
  (primitive-procedure-type (list type:list) type:fixnum>=0
			    'effect-free
			    'effect-sensitive (effect:union effect:set-car!
							    effect:set-cdr!)))

(define-operator-type (make-primitive-procedure 'SYSTEM-VECTOR-SIZE)
  (primitive-procedure-type (list type:any) type:vector-length
			    'function))

(define-operator-type (make-primitive-procedure 'VECTOR)
  (primitive-procedure-type type:any type:vector
			    'effect-insensitive
			    'effect effect:allocation))

(define-operator-type (make-primitive-procedure '%RECORD)
  (primitive-procedure-type (cons* type:any type:any) type:%record
			    'effect-insensitive
			    'effect effect:allocation))

(define-operator-type (make-primitive-procedure 'VECTOR-CONS)
  (primitive-procedure-type (list type:vector-length type:any)  type:vector
			    'effect-insensitive
			    'effect  effect:allocation))

(define-operator-type %vector-cons
  (primitive-procedure-type (list type:vector-length type:any)  type:vector
			    'effect-insensitive
			    'effect  effect:allocation))

(define-operator-type 'MAKE-VECTOR
  (procedure-type (cons* type:vector-length   type:any)  type:vector
		  'effect-insensitive
		  'effect  effect:allocation))

(define-operator-type (make-primitive-procedure 'VECTOR-LENGTH)
  (primitive-procedure-type (list type:vector) type:vector-length
			    'function))

(define-operator-type (make-primitive-procedure '%RECORD-LENGTH)
  (primitive-procedure-type (list type:%record) type:vector-length
			    'function))

(define-operator-type 'STRING
  (procedure-type type:any	; should be charatcer but it doesn't check.
		  type:string
		  'effect-insensitive
		  'effect effect:allocation))

(define-operator-type (make-primitive-procedure 'STRING-ALLOCATE)
  (primitive-procedure-type (list type:string-length)  type:string
			    'effect-insensitive
			    'effect  effect:allocation))

(define-operator-type %string-allocate
  (primitive-procedure-type (list type:string-length)  type:string
			    'effect-insensitive
			    'effect  effect:allocation))

(define-operator-type 'MAKE-STRING
  (procedure-type (cons* type:string-length  type:character)  type:string
		  'effect-insensitive
		  'effect  effect:allocation))

(define-operator-type (make-primitive-procedure 'STRING-LENGTH)
  (primitive-procedure-type (list type:string) type:string-length
			    'effect-free
			    'effect effect:other)) ; set-string-length!

(define-operator-type (make-primitive-procedure 'FLOATING-VECTOR-LENGTH)
  (primitive-procedure-type (list type:flonum-vector) type:vector-length
			    'function))

(define-operator-type (make-primitive-procedure 'BIT-STRING-LENGTH)
  (primitive-procedure-type (list type:bit-string) type:string-length
			    'function))


(define-operator-type (make-primitive-procedure 'COERCE-TO-COMPILED-PROCEDURE)
  (primitive-procedure-type (list type:any type:fixnum) type:compiled-procedure
			    'function))


;;; MIT Scheme characters have a 16 code-bits + 5 bucky-bits encoding,
;;; hence some results will fit in bytes and others in small
;;; non-negative fixnums.

(define-operator-type (make-primitive-procedure 'CHAR-CODE)
  (primitive-procedure-type (list type:character) type:small-fixnum>=0
			    'function))

(define-operator-type (make-primitive-procedure 'CHAR-BITS)
  (primitive-procedure-type (list type:character) type:unsigned-byte
			    'function))

(define-operator-type (make-primitive-procedure 'MAKE-CHAR)
  (primitive-procedure-type (list type:small-fixnum>=0 type:unsigned-byte)
			    type:character
			    'function))

(define-operator-type (make-primitive-procedure 'CHAR->INTEGER)
  (primitive-procedure-type (list type:character) type:small-fixnum>=0
			    'function))

(define-operator-type (make-primitive-procedure 'INTEGER->CHAR)
  (primitive-procedure-type (list type:small-fixnum>=0) type:character
			    'function))

(define-operator-type (make-primitive-procedure 'CHAR->ASCII)
  (primitive-procedure-type (list type:character) type:unsigned-byte
			    'function))

(define-operator-type (make-primitive-procedure 'ASCII->CHAR)
  (primitive-procedure-type (list type:unsigned-byte) type:character
			    'function))



;; If we had more refined integer types, we could do better with
;; OBJECT-TYPE.
(define-operator-type (make-primitive-procedure 'OBJECT-TYPE)
  (primitive-procedure-type (list type:any) type:unsigned-byte
			    'function))

(define-operator-type (make-primitive-procedure 'OBJECT-DATUM)
  (primitive-procedure-type (list type:any) type:fixnum>=0
			    'function))



(let ()

  (define (define-indexed thing-ref thing-set!
	    vector-type index-type element-type effect)
    (define-operator-type (make-primitive-procedure thing-ref)
      (primitive-procedure-type (list vector-type index-type) element-type
				'effect-free
				'effect-sensitive effect))
    (define-operator-type (make-primitive-procedure thing-set!)
      (primitive-procedure-type (list vector-type index-type element-type)
				type:any
				'effect effect
				'effect-insensitive)))

  (define-indexed 'VECTOR-REF  'VECTOR-SET!
    type:vector type:vector-length type:any   effect:vector-set!)
  (define-indexed 'STRING-REF  'STRING-SET!
    type:string type:string-length type:character effect:string-set!)
  (define-indexed 'VECTOR-8B-REF 'VECTOR-8B-SET!
    type:string type:string-length type:unsigned-byte effect:string-set!)
  (define-indexed '%RECORD-REF  '%RECORD-SET!
    type:%record type:vector-length type:any   effect:%record-set!)
  (define-indexed 'FLOATING-VECTOR-REF 'FLOATING-VECTOR-SET!
    type:flonum-vector type:vector-length type:flonum  effect:flo:vector-set!)
  (define-indexed 'BIT-STRING-REF  'BIT-STRING-SET!
    type:bit-string type:string-length type:boolean effect:bit-string-set!))

(let ()
  (define ((unchecked-function domain* range) . ops)
    (define (process-element op)
      (define-operator-type op
	(primitive-procedure-type 
	 (make-list (primitive-procedure-arity op) domain*) range
	 'function 'unchecked)))
    (for-each process-element ops))

  (define ((checked-function domain* range) . ops)
    (let ((arity #F))
      (define (process-element op)
	(if (exact-integer? op)
	    (set! arity op)
	    (define-operator-type op
	      (primitive-procedure-type (make-list arity domain*) range
					'function))))
      (for-each process-element ops)))

  ((unchecked-function type:flonum type:flonum)
   flo:+ flo:- flo:* flo:/ flo:negate flo:abs flo:sqrt
   flo:floor flo:ceiling flo:truncate flo:round
   flo:exp flo:log flo:sin flo:cos flo:tan flo:asin
   flo:acos flo:atan flo:atan2 flo:expt)

  ((unchecked-function type:flonum type:boolean)
   flo:= flo:< flo:> flo:zero? flo:negative? flo:positive?)

  ((unchecked-function type:fixnum type:fixnum)
   fix:-1+ fix:1+ fix:+ fix:- fix:*  fix:quotient fix:remainder	; fix:gcd
   fix:andc fix:and fix:or fix:xor fix:not fix:lsh)

  ((unchecked-function type:fixnum type:boolean)
   fix:= fix:< fix:> fix:zero? fix:negative? fix:positive?)

  ((checked-function type:exact-integer type:exact-integer)
   2 int:+ int:- int:* int:quotient int:remainder)

  ((checked-function type:exact-integer  type:boolean)
   2 int:= int:< int:>
   1 int:zero? int:negative? int:positive?)

  ((unchecked-function type:fixnum type:flonum)
   (ucode-primitive fixnum->flonum 1))
  
  (let-syntax ((p (macro spec (apply make-primitive-procedure spec))))
    ((checked-function type:number type:number)
     2 %+ %- %* %/ (p &+) (p &-) (p &*) (p &/))
    ((checked-function type:number type:boolean)
     2 %< %= %> (p &=) (p &<) (p &>)))
  )
   

(for-each
    (lambda (name)
      (define-operator-type (make-primitive-procedure name)
	(primitive-procedure-type (list type:any) type:boolean 'function)))
  '(BIT-STRING?  CELL?  CHAR?    FIXNUM?  FLONUM?  INDEX-FIXNUM?  NOT  NULL?
		 PAIR?  STRING?  INTEGER? VECTOR?  %RECORD?))
