#| -*-Scheme-*-

$Id: typedb.scm,v 1.5 1995/11/04 16:36:29 adams Exp $

Copyright (c) 1995 Massachusetts Institute of Technology

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

;;;; Types of known operators
;;; package: (compiler midend)

(declare (usual-integrations))

(define-operator-type 'STRING->SYMBOL
  (procedure-type (list type:string) type:interned-symbol
		  'effect-sensitive effect:string-set!))

(define-operator-type 'SYMBOL->STRING
  (procedure-type (list type:symbol) type:string
		  'effect effect:allocation))

(define-operator-type 'SUBSTRING?
  (procedure-type (list type:string type:string) type:boolean
		  'effect-sensitive effect:string-set!))

(define-operator-type 'ERROR
  ;; return type empty => Never returns 
  (procedure-type (cons* type:any type:any) type:empty
		  'function))

(define-operator-type 'ERROR:WRONG-TYPE-ARGUMENT
  ;; return type empty => Never returns 
  (procedure-type (list type:any type:any type:any) type:empty
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

(define-operator-type (make-primitive-procedure 'SYSTEM-VECTOR-SIZE)
  (primitive-procedure-type (list type:any) type:vector-length
			    'function))

(define-operator-type (make-primitive-procedure 'VECTOR-LENGTH)
  (primitive-procedure-type (list type:vector) type:vector-length
			    'function))

(define-operator-type (make-primitive-procedure '%RECORD-LENGTH)
  (primitive-procedure-type (list type:%record) type:vector-length
			    'function))

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
  (primitive-procedure-type (list type:any) type:compiled-procedure
			    'function))


;;; MIT Scheme charatcers have a 7 code-bits + 5 bucky-bits encoding,
;;; hence some results are fix in bytes:

(define-operator-type (make-primitive-procedure 'CHAR-CODE)
  (primitive-procedure-type (list type:character) type:unsigned-byte
			    'function))

(define-operator-type (make-primitive-procedure 'CHAR-BITS)
  (primitive-procedure-type (list type:character) type:unsigned-byte
			    'function))

(define-operator-type (make-primitive-procedure 'MAKE-CHAR)
  (primitive-procedure-type (list type:unsigned-byte type:unsigned-byte)
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


(for-each
    (lambda (op)
      (define-operator-type op
	(primitive-procedure-type 
	 (make-list (primitive-procedure-arity op) type:flonum) type:flonum
	 'function)))
  (list flo:+ flo:- flo:* flo:/
	flo:negate flo:abs flo:sqrt
	flo:floor flo:ceiling flo:truncate flo:round
	flo:exp flo:log flo:sin flo:cos flo:tan flo:asin
	flo:acos flo:atan flo:atan2 flo:expt))

(for-each
    (lambda (op)
      (define-operator-type op
	(primitive-procedure-type 
	 (make-list (primitive-procedure-arity op) type:fixnum) type:fixnum
	 'function 'unchecked)))
  (list fix:-1+ fix:1+ fix:+ fix:- fix:*
	fix:quotient fix:remainder ; fix:gcd
	fix:andc fix:and fix:or fix:xor fix:not fix:lsh))

(for-each
    (lambda (name)
      (define-operator-type (make-primitive-procedure name)
	(primitive-procedure-type (list type:any) type:boolean 'function)))
  '(BIT-STRING?  CELL?  FIXNUM?  FLONUM?  INDEX-FIXNUM?  NOT  NULL?
		 PAIR?  STRING?  INTEGER?))
