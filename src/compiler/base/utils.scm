#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; Compiler Utilities
;; package: (compiler)

(declare (usual-integrations))

;;;; Miscellaneous

(define (three-way-sort = set set* receiver)
  (let ((member? (member-procedure =)))
    (define (loop set set* receiver)
      (if (null? set)
	  (receiver '() '() set*)
	  (let ((item (member? (car set) set*)))
	    (if item
		(loop (cdr set) (delq! (car item) set*)
		  (lambda (set-only both set*-only)
		    (receiver set-only
			      (cons (cons (car set) (car item)) both)
			      set*-only)))
		(loop (cdr set) set*
		  (lambda (set-only both set*-only)
		    (receiver (cons (car set) set-only)
			      both
			      set*-only)))))))
    (loop set (list-copy set*) receiver)))

(define (discriminate-items items predicate)
  (let loop ((items items) (passed '()) (failed '()))
    (cond ((null? items)
	   (values (reverse! passed) (reverse! failed)))
	  ((predicate (car items))
	   (loop (cdr items) (cons (car items) passed) failed))
	  (else
	   (loop (cdr items) passed (cons (car items) failed))))))

(define (generate-label #!optional prefix)
  (if (default-object? prefix) (set! prefix 'LABEL))
  (string->uninterned-symbol
   (canonicalize-label-name
    (string-append
     (symbol->string
      (cond ((eq? prefix lambda-tag:unnamed) 'LAMBDA)
	    ((eq? prefix lambda-tag:let) 'LET)
	    ((eq? prefix lambda-tag:fluid-let) 'FLUID-LET)
	    (else prefix)))
     "-"
     (number->string (generate-label-number))))))

(define *current-label-number*)

(define (generate-label-number)
  (let ((number *current-label-number*))
    (set! *current-label-number* (1+ *current-label-number*))
    number))

(define (list-filter-indices items indices)
  (let loop ((items items) (indices indices) (index 0))
    (cond ((null? indices) '())
	  ((= (car indices) index)
	   (cons (car items)
		 (loop (cdr items) (cdr indices) (1+ index))))
	  (else
	   (loop (cdr items) indices (1+ index))))))

(define (all-eq? items)
  (if (null? items)
      (error "ALL-EQ?: undefined for empty set"))
  (or (null? (cdr items))
      (for-all? (cdr items)
	(let ((item (car items)))
	  (lambda (item*)
	    (eq? item item*))))))

(define (all-eq-map? items map)
  (if (null? items)
      (error "ALL-EQ-MAP?: undefined for empty set"))
  (let ((item (map (car items))))
    (if (or (null? (cdr items))
	    (for-all? (cdr items) (lambda (item*) (eq? item (map item*)))))
	(values true item)
	(values false false))))

(define (eq-set-union* set sets)
  (let loop ((set set) (sets sets) (accum '()))
    (if (null? sets)
	(eq-set-union set accum)
	(loop (car sets) (cdr sets) (eq-set-union set accum)))))

(package (transitive-closure enqueue-node! enqueue-nodes!)

(define *queue*)

(define-export (transitive-closure initialization process-node nodes)
  (fluid-let ((*queue* true))
    (if initialization (initialization))
    (set! *queue* nodes)
    (let loop ()
      (if (not (null? *queue*))
	  (begin (let ((node (car *queue*)))
		   (set! *queue* (cdr *queue*))
		   (process-node node))
		 (loop))))))

(define-export (enqueue-node! node)
  (if (and (not (eq? *queue* true))
	   (not (memq node *queue*)))
      (set! *queue* (cons node *queue*))))

(define-export (enqueue-nodes! nodes)
  (if (not (eq? *queue* true))
      (set! *queue* (eq-set-union nodes *queue*))))

)

;;;; Type Codes

(define-syntax define-type-code
  (sc-macro-transformer
   (lambda (form environment)
     environment
     `(DEFINE-INTEGRABLE ,(symbol-append 'TYPE-CODE: (cadr form))
	',(microcode-type (cadr form))))))

(define-type-code lambda)
(define-type-code extended-lambda)
(define-type-code procedure)
(define-type-code extended-procedure)
(define-type-code cell)
(define-type-code environment)
(define-type-code unassigned)
(define-type-code stack-environment)
(define-type-code compiled-entry)

(define (scode/procedure-type-code *lambda)
  (cond ((object-type? type-code:lambda *lambda)
	 type-code:procedure)
	((object-type? type-code:extended-lambda *lambda)
	 type-code:extended-procedure)
	(else
	 (error "SCODE/PROCEDURE-TYPE-CODE: Unknown lambda type" *lambda))))

;;; Primitive Procedures

(define (primitive-procedure? object)
  (or (eq? compiled-error-procedure object)
      (scode/primitive-procedure? object)))

(define (primitive-arity-correct? primitive argument-count)
  (if (eq? primitive compiled-error-procedure)
      (positive? argument-count)
      (let ((arity (primitive-procedure-arity primitive)))
	(or (= arity -1)
	    (= arity argument-count)))))

;;;; Special Compiler Support

(define compiled-error-procedure
  "Compiled error procedure")

(define-integrable lambda-tag:delay
  '|#[delay-lambda]|)

(define (non-pointer-object? object)
  ;; We can't use `object/non-pointer?' here because the C
  ;; back-end requires more stringent constraints on fixnums.
  ;; It may have other constraints on other types
  (or (object-type? (ucode-type false) object)
      (object-type? (ucode-type true) object)
      (and (fix:fixnum? object)
	   (>= object signed-fixnum/lower-limit)
	   (< object signed-fixnum/upper-limit))
      (object-type? (ucode-type character) object)
      (object-type? (ucode-type unassigned) object)
      (object-type? (ucode-type the-environment) object)
      (object-type? (ucode-type manifest-nm-vector) object)))

(define (object-immutable? object)
  (or (non-pointer-object? object)
      (number? object)
      (symbol? object)
      (scode/primitive-procedure? object)
      (eq? object compiled-error-procedure)))

(define boolean-valued-function-names
  '(
    OBJECT-TYPE? EQ? FALSE? NULL? PAIR? VECTOR? SYMBOL? STRING?
    NUMBER? CHAR? PROMISE? BIT-STRING? CELL?
    COMPLEX? REAL? RATIONAL? INTEGER? EXACT? INEXACT?
    ZERO? POSITIVE? NEGATIVE? ODD? EVEN?
    = < > <= >=
    FIX:FIXNUM? FIX:ZERO? FIX:NEGATIVE? FIX:POSITIVE? FIX:= FIX:< FIX:>
    FLO:FLONUM? FLO:ZERO? FLO:NEGATIVE? FLO:POSITIVE? FLO:= FLO:< FLO:>
    INT:INTEGER? INT:ZERO? INT:NEGATIVE? INT:POSITIVE? INT:= INT:< INT:>
    NOT BIT-STRING-REF
    ))

(define function-additional-names
  '(
    ;; Numbers
    MAX MIN + - * / 1+ -1+ CONJUGATE ABS QUOTIENT REMAINDER MODULO
    INTEGER-DIVIDE GCD LCM NUMERATOR DENOMINATOR FLOOR CEILING TRUNCATE ROUND
    FLOOR->EXACT CEILING->EXACT TRUNCATE->EXACT ROUND->EXACT
    RATIONALIZE RATIONALIZE->EXACT SIMPLEST-RATIONAL SIMPLEST-EXACT-RATIONAL
    EXP LOG SIN COS TAN ASIN ACOS ATAN SQRT EXPT MAKE-RECTANGULAR MAKE-POLAR
    REAL-PART IMAG-PART MAGNITUDE ANGLE EXACT->INEXACT INEXACT->EXACT
    FIX:1+ FIX:-1+ FIX:+ FIX:- FIX:*
    FIX:DIVIDE FIX:GCD FIX:QUOTIENT FIX:REMAINDER
    FIX:AND FIX:ANDC FIX:NOT FIX:OR FIX:XOR

    INT:+ INT:- INT:* INT:DIVIDE INT:QUOTIENT INT:REMAINDER INT:ABS
    INT:1+ INT:-1+ INT:NEGATE
    FLO:+ FLO:- FLO:* FLO:/ FLO:NEGATE FLO:ABS
    FLO:EXP FLO:EXPM1 FLO:LOG FLO:LOG1P FLO:SIN FLO:COS FLO:TAN
    FLO:ASIN FLO:ACOS FLO:ATAN FLO:ATAN2 FLO:SQRT FLO:EXPT FLO:FLOOR
    FLO:CEILING FLO:TRUNCATE FLO:ROUND FLO:FLOOR->EXACT FLO:CEILING->EXACT
    FLO:TRUNCATE->EXACT FLO:ROUND->EXACT

    ;; Random
    OBJECT-TYPE CHAR-ASCII? ASCII->CHAR CHAR->INTEGER CHAR-BITS CHAR-CODE
    CHAR-DOWNCASE CHAR-UPCASE INTEGER->CHAR MAKE-CHAR
    PRIMITIVE-PROCEDURE-ARITY

    ;; References (assumes immediate constants are immutable)
    CAR CDR LENGTH
    VECTOR-REF VECTOR-LENGTH
    STRING-REF STRING-LENGTH STRING-MAXIMUM-LENGTH
    BIT-STRING-LENGTH
    ))

;; The following definition is used to avoid computation if possible.
;; Not to avoid recomputation.  To avoid recomputation, function-names
;; should be used.
;;
;; Example: CONS has no side effects, yet it is not a function.
;; Thus if the result of a CONS is not going to be used, we can avoid the
;; CONS operation, yet we can't reuse its result even when given the same
;; arguments again because the two pairs should not be EQ?.

(define side-effect-free-additional-names
  `(
    ;; Constructors
    CONS LIST CONS* MAKE-STRING VECTOR MAKE-VECTOR LIST-COPY VECTOR-COPY
    LIST->VECTOR VECTOR->LIST MAKE-BIT-STRING MAKE-CELL STRING->SYMBOL
    ))

;;; Since the values of global variables corresponding with primitives
;;; are usually not the primitives themselves, but compiled procedures
;;; that call the primitives (possibly in-line, to avoid the cost of
;;; switching context between Scheme and C), and since SF turns
;;; references to the variables into primitive constants, we must
;;; separately detect them for constant-folding.  This list is
;;; approximate -- it covers everything that the open-coders may need
;;; to avoid generating bad RTL with entirely constant operands that
;;; the back ends' RTL->LAP rules are not prepared for.  All this is a
;;; horrible mess: none of this information should be hard-coded into
;;; the compiler.

(define boolean-valued-function-primitives
  (list (ucode-primitive %record?)
	(ucode-primitive &<)
	(ucode-primitive &=)
	(ucode-primitive &>)
	(ucode-primitive bit-string?)
	(ucode-primitive char?)
	(ucode-primitive eq?)
	(ucode-primitive equal-fixnum?)
	(ucode-primitive fixnum?)
	(ucode-primitive flonum-equal?)
	(ucode-primitive flonum-greater?)
	(ucode-primitive flonum-less?)
	(ucode-primitive flonum-negative?)
	(ucode-primitive flonum-positive?)
	(ucode-primitive flonum-zero?)
	(ucode-primitive flonum?)
	(ucode-primitive greater-than-fixnum?)
	(ucode-primitive index-fixnum?)
	(ucode-primitive integer-equal?)
	(ucode-primitive integer-greater?)
	(ucode-primitive integer-less?)
	(ucode-primitive integer-negative?)
	(ucode-primitive integer-positive?)
	(ucode-primitive integer-zero?)
	(ucode-primitive less-than-fixnum?)
	(ucode-primitive negative-fixnum?)
	(ucode-primitive negative?)
	(ucode-primitive null?)
	(ucode-primitive object-type?)
	(ucode-primitive pair?)
	(ucode-primitive positive-fixnum?)
	(ucode-primitive positive?)
	(ucode-primitive string?)
	(ucode-primitive vector?)
	(ucode-primitive zero-fixnum?)
	(ucode-primitive zero?)))

(define additional-side-effect-free-primitives
  (list (ucode-primitive %record)
	(ucode-primitive cons)
	(ucode-primitive floating-vector-cons)
	(ucode-primitive get-interrupt-enables)
	(ucode-primitive heap-available?)
	(ucode-primitive string-allocate)
	(ucode-primitive system-pair-cons)
	(ucode-primitive vector)
	(ucode-primitive vector-cons)))

(define additional-function-primitives
  (list (ucode-primitive %record-length)
	(ucode-primitive %record-ref)
	(ucode-primitive &*)
	(ucode-primitive &+)
	(ucode-primitive &-)
	(ucode-primitive &/)
	(ucode-primitive -1+)
	(ucode-primitive 1+)
	(ucode-primitive bit-string-length)
	(ucode-primitive car)
	(ucode-primitive cdr)
	(ucode-primitive char->integer)
	(ucode-primitive divide-fixnum)
	(ucode-primitive fixnum-and)
	(ucode-primitive fixnum-andc)
	(ucode-primitive fixnum-lsh)
	(ucode-primitive fixnum-not)
	(ucode-primitive fixnum-or)
	(ucode-primitive fixnum-quotient)
	(ucode-primitive fixnum-remainder)
	(ucode-primitive fixnum-xor)
	(ucode-primitive floating-vector-length)
	(ucode-primitive floating-vector-ref)
	(ucode-primitive flonum-abs)
	(ucode-primitive flonum-acos)
	(ucode-primitive flonum-add)
	(ucode-primitive flonum-asin)
	(ucode-primitive flonum-atan)
	(ucode-primitive flonum-atan2)
	(ucode-primitive flonum-ceiling)
	(ucode-primitive flonum-cos)
	(ucode-primitive flonum-divide)
	(ucode-primitive flonum-exp)
	(ucode-primitive flonum-floor)
	(ucode-primitive flonum-log)
	(ucode-primitive flonum-multiply)
	(ucode-primitive flonum-negate)
	(ucode-primitive flonum-round)
	(ucode-primitive flonum-sin)
	(ucode-primitive flonum-sqrt)
	(ucode-primitive flonum-subtract)
	(ucode-primitive flonum-tan)
	(ucode-primitive flonum-truncate)
	(ucode-primitive gcd-fixnum)
	(ucode-primitive integer->char)
	(ucode-primitive integer-add)
	(ucode-primitive integer-add-1)
	(ucode-primitive integer-multiply)
	(ucode-primitive integer-quotient)
	(ucode-primitive integer-remainder)
	(ucode-primitive integer-subtract)
	(ucode-primitive integer-subtract-1)
	(ucode-primitive minus-fixnum)
	(ucode-primitive minus-one-plus-fixnum)
	(ucode-primitive multiply-fixnum)
	(ucode-primitive object-type)
	(ucode-primitive one-plus-fixnum)
	(ucode-primitive plus-fixnum)
	(ucode-primitive primitive-object-ref)
	(ucode-primitive primitive-object-type)
	(ucode-primitive quotient)
	(ucode-primitive remainder)
	(ucode-primitive string-length)
	(ucode-primitive string-ref)
	(ucode-primitive system-hunk3-cxr0)
	(ucode-primitive system-hunk3-cxr1)
	(ucode-primitive system-hunk3-cxr2)
	(ucode-primitive system-pair-car)
	(ucode-primitive system-pair-cdr)
	(ucode-primitive system-vector-ref)
	(ucode-primitive system-vector-size)
	(ucode-primitive vector-8b-ref)
	(ucode-primitive vector-length)
	(ucode-primitive vector-ref)))

;;;; "Foldable" and side-effect-free operators

(define boolean-valued-function-variables)
(define function-variables)
(define side-effect-free-variables)

(let ((global-valued
       (lambda (names)
	 (list-transform-negative names
	   (lambda (name)
	     (lexical-unreferenceable? system-global-environment name)))))
      (global-value
       (lambda (name)
	 (lexical-reference system-global-environment name))))
  (let ((names (global-valued boolean-valued-function-names)))
    (let ((procedures (map global-value names)))
      (set! boolean-valued-function-variables (map cons names procedures))))
  (let ((names (global-valued function-additional-names)))
    (let ((procedures (map global-value names)))
      (set! function-variables
	    (map* boolean-valued-function-variables cons names procedures))))
  (let ((names (global-valued side-effect-free-additional-names)))
    (let ((procedures (map global-value names)))
      (set! side-effect-free-variables
	    (map* function-variables cons names procedures))))
  unspecific)

(define function-primitives
  (append additional-function-primitives
          boolean-valued-function-primitives))

(define side-effect-free-primitives
  (append additional-side-effect-free-primitives
          function-primitives))

(define-integrable (boolean-valued-function-variable? name)
  (assq name boolean-valued-function-variables))

(define-integrable (constant-foldable-variable? name)
  (assq name function-variables))

(define-integrable (side-effect-free-variable? name)
  (assq name side-effect-free-variables))

(define (variable-usual-definition name)
  (let ((place (assq name side-effect-free-variables)))
    (and place
	 (cdr place))))

(define-integrable (boolean-valued-function-primitive? operator)
  (memq operator boolean-valued-function-primitives))

(define-integrable (constant-foldable-primitive? operator)
  (memq operator function-primitives))

(define-integrable (side-effect-free-primitive? operator)
  (memq operator side-effect-free-primitives))

(define procedure-object?
  (lexical-reference system-global-environment 'PROCEDURE?))

(define (careful-object-datum object)
  ;; This works correctly when cross-compiling.
  (if (and (fix:fixnum? object)
	   (negative? object))
      (+ object unsigned-fixnum/upper-limit)
      (object-datum object)))