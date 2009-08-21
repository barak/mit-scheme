#| -*-Scheme-*-

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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
	    ((eq? prefix lambda-tag:make-environment) 'MAKE-ENVIRONMENT)
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

(let-syntax ((define-type-code
	       (macro (var-name #!optional type-name)
		 (if (default-object? type-name) (set! type-name var-name))
		 `(DEFINE-INTEGRABLE ,(symbol-append 'TYPE-CODE: var-name)
		    ',(microcode-type type-name)))))
  (define-type-code lambda)
  (define-type-code extended-lambda)
  (define-type-code procedure)
  (define-type-code extended-procedure)
  (define-type-code cell)
  (define-type-code environment)
  (define-type-code unassigned)
  (define-type-code stack-environment)
  (define-type-code compiled-entry))

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

(define lambda-tag:delay
  (intern "#[delay-lambda]"))

(define (non-pointer-object? object)
  ;; Any reason not to use `object/non-pointer?' here? -- cph
  (or (object-type? (ucode-type false) object)
      (object-type? (ucode-type true) object)
      (fix:fixnum? object)
      (object-type? (ucode-type character) object)
      (object-type? (ucode-type unassigned) object)
      (object-type? (ucode-type the-environment) object)
      (object-type? (ucode-type manifest-nm-vector) object)
      (object-type? (ucode-type manifest-special-nm-vector) object)))

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
    INDEX-FIXNUM?
    FIX:FIXNUM? FIX:ZERO? FIX:NEGATIVE? FIX:POSITIVE? FIX:= FIX:< FIX:>
    FLO:FLONUM? FLO:ZERO? FLO:NEGATIVE? FLO:POSITIVE? FLO:= FLO:< FLO:>
    INT:INTEGER? INT:ZERO? INT:NEGATIVE? INT:POSITIVE? INT:= INT:< INT:>
    NOT BIT-STRING-REF
    ))

(define function-names
  (append
   boolean-valued-function-names
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
     FLO:+ FLO:- FLO:* FLO:/ FLO:NEGATE FLO:ABS FLO:EXP FLO:LOG FLO:SIN FLO:COS
     FLO:TAN FLO:ASIN FLO:ACOS FLO:ATAN FLO:ATAN2 FLO:SQRT FLO:EXPT FLO:FLOOR
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
     )))

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

(define additional-boolean-valued-function-primitives
  (list (ucode-primitive zero?)
	(ucode-primitive positive?)
	(ucode-primitive negative?)
	(ucode-primitive &=)
	(ucode-primitive &<)
	(ucode-primitive &>)))

(define additional-function-primitives
  (list (ucode-primitive 1+)
	(ucode-primitive -1+)
	(ucode-primitive &+)
	(ucode-primitive &-)
	(ucode-primitive &*)
	(ucode-primitive &/)))

;;;; "Foldable" and side-effect-free operators

(define boolean-valued-function-variables)
(define function-variables)
(define side-effect-free-variables)
(define boolean-valued-function-primitives)
(define function-primitives)
(define side-effect-free-primitives)

(let ((global-valued
       (lambda (names)
	 (list-transform-negative names
	   (lambda (name)
	     (lexical-unreferenceable? system-global-environment name)))))
      (global-value
       (lambda (name)
	 (lexical-reference system-global-environment name)))
      (primitives
       (let ((primitive-procedure?
	      (lexical-reference system-global-environment
				 'PRIMITIVE-PROCEDURE?)))
	 (lambda (procedures)
	   (list-transform-positive procedures primitive-procedure?)))))
  (let ((names (global-valued boolean-valued-function-names)))
    (let ((procedures (map global-value names)))
      (set! boolean-valued-function-variables (map cons names procedures))
      (set! boolean-valued-function-primitives
	    (append! (primitives procedures)
		     additional-boolean-valued-function-primitives))))
  (let ((names (global-valued function-names)))
    (let ((procedures (map global-value names)))
      (set! function-variables
	    (map* boolean-valued-function-variables cons names procedures))
      (set! function-primitives
	    (append! (primitives procedures)
		     (append additional-function-primitives
			     boolean-valued-function-primitives)))))
  (let ((names (global-valued side-effect-free-additional-names)))
    (let ((procedures (map global-value names)))
      (set! side-effect-free-variables
	    (map* function-variables cons names procedures))
      (set! side-effect-free-primitives
	    (append! (primitives procedures)
		     function-primitives))
      unspecific)))

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

;;!(define (careful-object-datum object)
;;!  ;; This works correctly when cross-compiling.
;;!  (if (and (object-type? (ucode-type fixnum) object)
;;!	   (negative? object))
;;!      (+ object unsigned-fixnum/upper-limit)
;;!      (object-datum object)))

(define (careful-object-datum object)
  ;; This works correctly when cross-compiling.
  (if (and (fix:fixnum? object)
	   (negative? object))
      (+ object unsigned-fixnum/upper-limit)
      (object-datum object)))

(define (list-split ol predicate)
  ;; (values yes no)
  (let loop ((l (reverse ol))
	     (yes '())
	     (no '()))
    (cond ((null? l)
	   (values yes no))
	  ((predicate (car l))
	   (loop (cdr l) (cons (car l) yes) no))
	  (else
	   (loop (cdr l) yes (cons (car l) no))))))

;; Integrated procedures from Statistics gathering machinery

(define-integrable (sample/1 statistic datum)
  (if compiler:enable-statistics?
      (sample/1/really statistic datum)
      unspecific))

(define-integrable (sample/2 statistic datum1 datum2)
  (if compiler:enable-statistics?
      (sample/2/really statistic datum1 datum2)
      unspecific))
