#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; SCode Optimizer: Usual Integrations: Combination Expansions
;;; package: (scode-optimizer expansion)

(declare (usual-integrations)
	 (integrate-external "object"))

;;;; Fixed-arity arithmetic primitives

(define (pcall expr block operator . operands)
  (papply expr block operator operands))

(define (papply expr block operator operands)
  (combination/make expr
		    block
		    (if (primitive-procedure? operator)
			(pconst #f operator)
			operator)
		    operands))

(define (pconst expr datum)
  (constant/make (and expr (object/scode expr)) datum))

(define (pif expr p c a)
  (conditional/make (and expr (object/scode expr)) p c a))

(define (make-operand-binding expression block operand make-body)
  (combination/make expression
		    block
		    (let ((block (block/make block #t '()))
			  (name (string->uninterned-symbol "operand")))
		      (let ((variable (variable/make&bind! block name)))
			(procedure/make
			 #f
			 block scode-lambda-name:let (list variable) '() #f
			 (make-body block
				    (reference/make #f block variable #f)))))
		    (list operand)))

(define (constant-eq? expression constant)
  (and (constant? expression)
       (eq? (constant/value expression) constant)))

(define (unary-arithmetic primitive)
  (lambda (expr operands block)
    (if (and (pair? operands)
	     (null? (cdr operands)))
	(papply expr block primitive operands)
	#f)))

(define (binary-arithmetic primitive)
  (lambda (expr operands block)
    (if (and (pair? operands)
	     (pair? (cdr operands))
	     (null? (cddr operands)))
	(papply expr block primitive operands)
	#f)))

(define zero?-expansion
  (unary-arithmetic (ucode-primitive zero?)))

(define positive?-expansion
  (unary-arithmetic (ucode-primitive positive?)))

(define negative?-expansion
  (unary-arithmetic (ucode-primitive negative?)))

(define 1+-expansion
  (unary-arithmetic (ucode-primitive 1+)))

(define -1+-expansion
  (unary-arithmetic (ucode-primitive -1+)))

(define quotient-expansion
  (binary-arithmetic (ucode-primitive quotient 2)))

(define remainder-expansion
  (binary-arithmetic (ucode-primitive remainder 2)))

(define modulo-expansion
  (binary-arithmetic (ucode-primitive modulo 2)))

;;;; N-ary Arithmetic Predicates

(define (pairwise-test binary-predicate if-left-zero if-right-zero)
  (lambda (expr operands block)
    (if (and (pair? operands)
	     (pair? (cdr operands))
	     (null? (cddr operands)))
	(cond ((constant-eq? (car operands) 0)
	       (pcall expr block if-left-zero (cadr operands)))
	      ((constant-eq? (cadr operands) 0)
	       (pcall expr block if-right-zero (car operands)))
	      (else
	       (papply expr block binary-predicate operands)))
	#f)))

(define =-expansion
  (pairwise-test (ucode-primitive &=)
		 (ucode-primitive zero?)
		 (ucode-primitive zero?)))

(define <-expansion
  (pairwise-test (ucode-primitive &<)
		 (ucode-primitive positive?)
		 (ucode-primitive negative?)))

(define >-expansion
  (pairwise-test (ucode-primitive &>)
		 (ucode-primitive negative?)
		 (ucode-primitive positive?)))

;;;; Fixnum Operations

(define (fx-compare prim)
  (lambda (expr ops block)
    (case (length ops)
      ((2)
       (pcall expr block prim (car ops) (cadr ops)))
      ((3)
       (pif expr
	    (pcall #f block prim (car ops) (cadr ops))
	    (pcall #f block prim (car ops) (caddr ops))
	    (pconst #f #f)))
      ((4)
       (pif expr
	    (pcall #f block prim (car ops) (cadr ops))
	    (pif #f
		 (pcall #f block prim (cadr ops) (caddr ops))
		 (pcall #f block prim (caddr ops) (cadddr ops))
		 (pconst #f #f))
	    (pconst #f #f)))
      (else #f))))

(define fx=?-expansion (fx-compare (ucode-primitive eq?)))
(define fx<?-expansion (fx-compare (ucode-primitive less-than-fixnum?)))
(define fx>?-expansion (fx-compare (ucode-primitive greater-than-fixnum?)))

(define (fxnot-compare prim)
  (lambda (expr ops block)

    (define (pnot expr operand)
      (pcall expr block (ucode-primitive not) operand))

    (case (length ops)
      ((2)
       (pnot expr (pcall #f block prim (car ops) (cadr ops))))
      ((3)
       (pif expr
	    (pcall #f block prim (car ops) (cadr ops))
	    (pconst #f #f)
	    (pnot #f (pcall #f block prim (car ops) (caddr ops)))))
      ((4)
       (pif expr
	    (pcall #f block prim (car ops) (cadr ops))
	    (pconst #f #f)
	    (pif #f
		 (pcall #f block prim (cadr ops) (caddr ops))
		 (pconst #f #f)
		 (pnot #f (pcall #f block prim (caddr ops) (cadddr ops))))))
      (else #f))))

(define fx<=?-expansion (fxnot-compare (ucode-primitive greater-than-fixnum?)))
(define fx>=?-expansion (fxnot-compare (ucode-primitive less-than-fixnum?)))

(define (fxneg-expansion expr ops block)
  (if (and (pair? ops)
	   (null? (cdr ops)))
      (pcall expr block (ucode-primitive minus-fixnum) (pconst #f 0) (car ops))
      #f))

(define (fxarithmetic-shift-right-expansion expr ops block)
  (if (and (pair? ops)
	   (pair? (cdr ops))
	   (null? (cddr ops)))
      (pcall expr block (ucode-primitive fixnum-lsh)
	     (car ops)
	     (pcall #f block (ucode-primitive minus-fixnum)
		    (pconst #f 0)
		    (cadr ops)))
      #f))

;;;; Flonum Operations

(define (flonum-fmsub-expansion expr ops block)
  (if (and (pair? ops)
	   (pair? (cdr ops))
	   (pair? (cddr ops))
	   (null? (cdddr ops)))
      (pcall expr block (ucode-primitive flonum-fma 3)
	     (car ops)
	     (cadr ops)
	     (pcall #f block (ucode-primitive flonum-negate 1) (caddr ops)))
      #f))

;;;; N-ary Arithmetic Field Operations

(define (right-accumulation identity make-binary)
  (lambda (expr operands block)
    (let ((operands (delq identity operands)))
      (let ((n (length operands)))
	(cond ((zero? n)
	       (constant/make
		(and expr (object/scode expr))
		identity))
	      ((< n 5)
	       (let loop
		   ((expr expr)
		    (first (car operands))
		    (rest (cdr operands)))
		 (if (null? rest)
		     first
		     (make-binary expr
				  block
				  first
				  (loop #f (car rest) (cdr rest))))))
	      (else #f))))))

(define +-expansion
  (right-accumulation 0
    (lambda (expr block x y)
      (cond ((constant-eq? x 1)
	     (pcall expr block (ucode-primitive 1+) y))
	    ((constant-eq? y 1)
	     (pcall expr block (ucode-primitive 1+) x))
	    (else
	     (pcall expr block (ucode-primitive &+) x y))))))

(define *-expansion
  (right-accumulation 1
    (lambda (expr block x y)
      (pcall expr block (ucode-primitive &*) x y))))

(define bitwise-and-expansion
  (right-accumulation -1
    (lambda (expr block x y)
      (pcall expr block (ucode-primitive integer-bitwise-and 2) x y))))

(define bitwise-xor-expansion
  (right-accumulation 0
    (lambda (expr block x y)
      (pcall expr block (ucode-primitive integer-bitwise-xor 2) x y))))

(define bitwise-ior-expansion
  (right-accumulation 0
    (lambda (expr block x y)
      (pcall expr block (ucode-primitive integer-bitwise-ior 2) x y))))

(define bitwise-eqv-expansion
  (right-accumulation -1
    (lambda (expr block x y)
      (pcall expr block (ucode-primitive integer-bitwise-eqv 2) x y))))

(define (bitwise-andc1-expansion expr operands block)
  (and (length=? operands 2)
       (let ((x (car operands))
	     (y (cadr operands)))
	 (pcall expr block (ucode-primitive integer-bitwise-andc2 2) y x))))

(define (arithmetic-shift-expansion expr operands block)
  (and (length=? operands 2)
       (let ((x (car operands))
	     (s (cadr operands)))
	 (and (constant? (cadr operands))
	      (if (negative? (constant/value s))
		  (pcall expr block (ucode-primitive integer-shift-right 2)
			 x
			 (constant/make #f (- (constant/value s))))
		  (pcall expr block (ucode-primitive integer-shift-left 2)
			 x s))))))

(define (expt-expansion expr operands block)
  (let ((make-binder
	 (lambda (make-body)
	   (make-operand-binding expr
				 block
				 (car operands)
				 make-body))))
    (cond ((not (and (pair? operands)
		     (pair? (cdr operands))
		     (null? (cddr operands))))
	   #f)
	  ;;((constant-eq? (cadr operands) 0)
	  ;; (if-expanded (constant/make (and expr (object/scode expr)) 1)))
	  ((constant-eq? (cadr operands) 1)
	   (car operands))
	  ((constant-eq? (cadr operands) 2)
	   (make-binder
	    (lambda (block operand)
	      (pcall #f block (ucode-primitive &*) operand operand))))
	  ((constant-eq? (cadr operands) 3)
	   (make-binder
	    (lambda (block operand)
	      (pcall #f
		     block
		     (ucode-primitive &*)
		     operand
		     (pcall #f block (ucode-primitive &*) operand operand)))))
	  ((constant-eq? (cadr operands) 4)
	   (make-binder
	    (lambda (block operand)
	      (pcall #f
		     block
		     (ucode-primitive &*)
		     (pcall #f block (ucode-primitive &*) operand operand)
		     (pcall #f block (ucode-primitive &*) operand operand)))))
	  (else #f))))

(define (right-accumulation-inverse identity inverse-expansion make-binary)
  (lambda (expr operands block)
    (cond ((null? operands) #f)
	  ((null? (cdr operands))
	   (let ((y (car operands)))
	     (if (constant-eq? y identity)
		 (constant/make #f identity)
		 ;; Converting (- x) to (- 0 x) breaks signed zero.
		 #f)))
	  ((inverse-expansion #f (cdr operands) block)
	   => (lambda (inverse)
		(let ((x (car operands))
		      (y inverse))
		  (if (constant-eq? inverse identity)
		      x
		      (make-binary expr block x y)))))
	  (else #f))))

(define --expansion
  (right-accumulation-inverse 0 +-expansion
    (lambda (expr block x y)
      (if (constant-eq? y 1)
	  (pcall expr block (ucode-primitive -1+) x)
	  (pcall expr block (ucode-primitive &-) x y)))))

(define /-expansion
  (right-accumulation-inverse 1 *-expansion
    (lambda (expr block x y)
      (pcall expr block (ucode-primitive &/) x y))))

;;;; N-ary List Operations

(define (apply*-expansion expr operands block)
  (cond ((length=? operands 2)
	 (papply expr block (ucode-primitive apply) operands))
	((not (pair? operands)) #f)
	((pair? (cdr operands))
	 (apply*-expansion
	  expr
	  (list (car operands)
		(cons*-expansion-loop #f block (cdr operands)))
	  block))
	(else #f)))

(define (cons*-expansion expr operands block)
  (cons*-expansion-loop expr block operands))

(define (cons*-expansion-loop expr block rest)
  (if (null? (cdr rest))
      (car rest)
      (pcall expr
	     block
	     (ucode-primitive cons)
	     (car rest)
	     (cons*-expansion-loop #f block (cdr rest)))))

(define (list-expansion expr operands block)
  (list-expansion-loop expr block operands))

(define (list-expansion-loop expr block rest)
  (cond ((pair? rest)
	 (pcall expr block (ucode-primitive cons)
		(car rest)
		(list-expansion-loop #f block (cdr rest))))
	((null? rest)
	 (constant/make (and expr (object/scode expr)) '()))
	(else
	 (error "Improper list."))))

;;;; General CAR/CDR Encodings

(define (call-to-car? expression)
  (and (combination? expression)
       (constant-eq? (combination/operator expression) (ucode-primitive car))
       (length=? (combination/operands expression) 1)))

(define (call-to-cdr? expression)
  (and (combination? expression)
       (constant-eq? (combination/operator expression) (ucode-primitive cdr))
       (length=? (combination/operands expression) 1)))

(define (call-to-general-car-cdr? expression)
  (and (combination? expression)
       (constant-eq? (combination/operator expression)
		     (ucode-primitive general-car-cdr))
       (length=? (combination/operands expression) 2)
       (constant? (second (combination/operands expression)))))

(define (car-expansion expr operands block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (let ((operand (first operands)))
	(cond ((call-to-car? operand)
	       ;; (car (car x)) => (caar x)
	       (pcall expr block
		      (ucode-primitive general-car-cdr)
		      (first (combination/operands operand))
		      (constant/make #f #b111)))
	      ;; (car (cdr x)) => (cadr x)
	      ((call-to-cdr? operand)
	       (pcall expr block
		      (ucode-primitive general-car-cdr)
		      (first (combination/operands operand))
		      (constant/make #f #b110)))

	      ((call-to-general-car-cdr? operand)
	       (pcall expr block
		      (ucode-primitive general-car-cdr)
		      (first (combination/operands operand))
		      (constant/make
		       #f
		       (encode-general-car-cdr
			(cons 'car
			      (decode-general-car-cdr
			       (constant/value
				(second (combination/operands operand)))))))))
	      (else
	       (papply expr block (ucode-primitive car) operands))))
      ;; ill-formed call
      (begin
	(warn "Wrong number of arguments in call to CAR.")
	#f)))

(define (cdr-expansion expr operands block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (let ((operand (first operands)))
	(cond ((call-to-car? operand)
	       ;; (cdr (car x)) => (cdar x)
	       (pcall expr block
		      (ucode-primitive general-car-cdr)
		      (first (combination/operands operand))
		      (constant/make #f #b101)))
	      ;; (cdr (car x)) => (cddr x)
	      ((call-to-cdr? operand)
	       (pcall expr block
		      (ucode-primitive general-car-cdr)
		      (first (combination/operands operand))
		      (constant/make #f #b100)))

	      ((call-to-general-car-cdr? (car operands))
	       (pcall expr block
		      (ucode-primitive general-car-cdr)
		      (first (combination/operands operand))
		      (constant/make
		       #f
		       (encode-general-car-cdr
			(cons 'cdr
			      (decode-general-car-cdr
			       (constant/value
				(second (combination/operands operand)))))))))
	      (else
	       (papply expr block (ucode-primitive cdr) operands))))
      ;; ill-formed call
      (begin
	(warn "Wrong number of arguments in call to CDR.")
	#f)))

(define (general-car-cdr-expansion encoding)
  (lambda (expr operands block)
    (if (length=? operands 1)
	(pcall expr
	       block
	       (ucode-primitive general-car-cdr)
	       (car operands)
	       (constant/make #f encoding))
	#f)))

(define caar-expansion (general-car-cdr-expansion #b111))
(define cadr-expansion (general-car-cdr-expansion #b110))
(define cdar-expansion (general-car-cdr-expansion #b101))
(define cddr-expansion (general-car-cdr-expansion #b100))

(define caaar-expansion (general-car-cdr-expansion #b1111))
(define caadr-expansion (general-car-cdr-expansion #b1110))
(define cadar-expansion (general-car-cdr-expansion #b1101))
(define caddr-expansion (general-car-cdr-expansion #b1100))
(define cdaar-expansion (general-car-cdr-expansion #b1011))
(define cdadr-expansion (general-car-cdr-expansion #b1010))
(define cddar-expansion (general-car-cdr-expansion #b1001))
(define cdddr-expansion (general-car-cdr-expansion #b1000))

(define caaaar-expansion (general-car-cdr-expansion #b11111))
(define caaadr-expansion (general-car-cdr-expansion #b11110))
(define caadar-expansion (general-car-cdr-expansion #b11101))
(define caaddr-expansion (general-car-cdr-expansion #b11100))
(define cadaar-expansion (general-car-cdr-expansion #b11011))
(define cadadr-expansion (general-car-cdr-expansion #b11010))
(define caddar-expansion (general-car-cdr-expansion #b11001))
(define cadddr-expansion (general-car-cdr-expansion #b11000))
(define cdaaar-expansion (general-car-cdr-expansion #b10111))
(define cdaadr-expansion (general-car-cdr-expansion #b10110))
(define cdadar-expansion (general-car-cdr-expansion #b10101))
(define cdaddr-expansion (general-car-cdr-expansion #b10100))
(define cddaar-expansion (general-car-cdr-expansion #b10011))
(define cddadr-expansion (general-car-cdr-expansion #b10010))
(define cdddar-expansion (general-car-cdr-expansion #b10001))
(define cddddr-expansion (general-car-cdr-expansion #b10000))

(define first-expansion   (general-car-cdr-expansion #b11))
(define second-expansion  (general-car-cdr-expansion #b110))
(define third-expansion   (general-car-cdr-expansion #b1100))
(define fourth-expansion  (general-car-cdr-expansion #b11000))
(define fifth-expansion   (general-car-cdr-expansion #b110000))
(define sixth-expansion   (general-car-cdr-expansion #b1100000))
(define seventh-expansion (general-car-cdr-expansion #b11000000))
(define eighth-expansion  (general-car-cdr-expansion #b110000000))

;;;; Miscellaneous

(define (eq?-expansion expr operands block)
  (if (and (pair? operands)
	   (pair? (cdr operands))
	   (null? (cddr operands)))
      ;; Convert (eq? <expr> #f) and (eq? #f <expr>) to (not <expr>)
      ;; Conditional inversion will remove the call to not.
      (cond ((expression/always-false? (first operands))
	     (sequence/make
	      (and expr (object/scode expr))
	      (list (first operands)
		    (pcall #f block (ucode-primitive not) (cadr operands)))))
	    ((expression/always-false? (second operands))
	     (sequence/make
	      (and expr (object/scode expr))
	      (list (second operands)
		    (pcall #f block (ucode-primitive not) (car operands)))))
	    (else
	     (papply expr block (ucode-primitive eq?) operands)))
      #f))

(define (char=?-expansion expr operands block)
  (if (and (pair? operands)
	   (pair? (cdr operands))
	   (null? (cddr operands)))
      (papply expr block (ucode-primitive eq?) operands)
      #f))

(define (make-string-expansion expr operands block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (papply expr block (ucode-primitive string-allocate) operands)
      #f))

(define (make-bytevector-expansion expr operands block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (papply expr block (ucode-primitive allocate-bytevector 1) operands)
      #f))

(define (not-expansion expr operands block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (cond ((expression/always-false? (first operands))
	     (sequence/make (and expr (object/scode expr))
			    (list (first operands) (constant/make #f #t))))
	    ((expression/never-false? (first operands))
	     (sequence/make (and expr (object/scode expr))
			    (list (first operands) (constant/make #f #f))))
	    (else (papply expr block (ucode-primitive not) operands)))
      #f))

(define (guarantee-expansion expr operands block)
  (if (and (pair? operands)
	   (pair? (cdr operands))
	   (or (null? (cddr operands))
	       (and (pair? (cddr operands))
		    (null? (cdddr operands)))))
      (let ((predicate-expr (car operands))
	    (object-expr (cadr operands))
	    (caller-expr (and (pair? (cddr operands)) (caddr operands))))
	(combination/make
	 expr
	 block
	 (let ((block (block/make block #t '())))
	   (define (*const v) (constant/make #f v))
	   (define (*ref var) (reference/make #f block var #f))
	   (define (*begin . actions) (sequence/make #f actions))
	   (define (*app operator operands)
	     (combination/make #f block operator operands))
	   (define (*lambda name variables body)
	     (procedure/make #f block name variables '() #f body))
	   (define (*declare declarations body)
	     (declaration/make #f
			       (declarations/parse block declarations)
			       body))
	   (define (*if predicate consequent alternative)
	     (conditional/make #f predicate consequent alternative))
	   (define (make-variable name)
	     (variable/make&bind!
	      block
	      (string->uninterned-symbol (symbol->string name))))
	   (let ((predicate-var (make-variable 'predicate))
		 (object-var (make-variable 'object))
		 (caller-var (and caller-expr (make-variable 'caller))))
	     (let* ((variables
		     (cons* predicate-var
			    object-var
			    (if caller-var (list caller-var) '()))))
	       (*lambda scode-lambda-name:let variables
		 (*declare
		   ;; This declaration is not generally valid in
		   ;; substituting the definition of GUARANTEE as
		   ;; written; it encodes the assumption that
		   ;; predicates do not modify their own definitions.
		   ;; For example,
		   ;;
		   ;;	(define (foo? x) (set! foo? (lambda (x) #f)) #t)
		   ;;	(guarantee foo? x)
		   ;;
		   ;; violates the assumption.
		   (if (reference? predicate-expr)
		       `((INTEGRATE ,(variable/name predicate-var)))
		       '())
		   (*begin
		     (*if (*app (*ref predicate-var) (list (*ref object-var)))
			  (*const unspecific)
			  (*app (access/make #f block (*const #f) 'error:not-a)
				(cons* (*ref predicate-var)
				       (*ref object-var)
				       (if caller-var
					   (list (*ref caller-var))
					   '()))))
		     (*ref object-var)))))))
	 (cons* predicate-expr
		object-expr
		(if caller-expr (list caller-expr) '()))))
      #f))

(define (type-test-expansion type)
  (lambda (expr operands block)
    (if (and (pair? operands)
	     (null? (cdr operands)))
	(make-type-test expr block type (car operands))
	#f)))

(define weak-pair?-expansion (type-test-expansion (ucode-type weak-cons)))

(define (exact-integer?-expansion expr operands block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (make-operand-binding
       expr block (car operands)
       (lambda (block operand)
	 (make-disjunction
	  expr
	  (make-type-test #f block (ucode-type fixnum) operand)
	  (make-type-test #f block (ucode-type big-fixnum) operand))))
      #f))

(define (exact-rational?-expansion expr operands block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
       (make-operand-binding expr block (car operands)
	 (lambda (block operand)
	   (make-disjunction
	    expr
	    (make-type-test #f block (ucode-type fixnum) operand)
	    (make-type-test #f block (ucode-type big-fixnum) operand)
	    (make-type-test #f block (ucode-type ratnum) operand))))
       #f))

(define (complex?-expansion expr operands block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
       (make-operand-binding expr block (car operands)
	 (lambda (block operand)
	   (make-disjunction
	    expr
	    (make-type-test #f block (ucode-type fixnum) operand)
	    (make-type-test #f block (ucode-type big-fixnum) operand)
	    (make-type-test #f block (ucode-type ratnum) operand)
	    (make-type-test #f block (ucode-type big-flonum) operand)
	    (make-type-test #f block (ucode-type recnum) operand))))
       #f))

(define (symbol?-expansion expr operands block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (make-operand-binding
       expr block (car operands)
       (lambda (block operand)
	 (make-disjunction
	  expr
	  (make-type-test #f block (ucode-type interned-symbol) operand)
	  (make-type-test #f block (ucode-type uninterned-symbol)
			  operand))))
      #f))

(define (interned-symbol?-expansion expr operands block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (make-type-test expr block (ucode-type interned-symbol) (car operands))
      #f))

(define (uninterned-symbol?-expansion expr operands block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (make-type-test expr block (ucode-type uninterned-symbol) (car operands))
      #f))

(define (make-disjunction expr . clauses)
  (let loop ((clauses clauses))
    (if (null? (cdr clauses))
	(car clauses)
	(disjunction/make (and expr (object/scode expr))
			  (car clauses) (loop (cdr clauses))))))

(define (make-type-test expr block type operand)
  (pcall expr block
	 (ucode-primitive object-type?)
	 (constant/make #f type)
	 operand))

(define (string->symbol-expansion expr operands block)
  (declare (ignore block))
  (if (and (pair? operands)
	   (constant? (car operands))
	   (string? (constant/value (car operands)))
	   (null? (cdr operands)))
      (constant/make (and expr (object/scode expr))
		     (string->symbol (constant/value (car operands))))
      #f))

(define (intern-expansion expr operands block)
  (declare (ignore block))
  (if (and (pair? operands)
	   (constant? (car operands))
	   (string? (constant/value (car operands)))
	   (null? (cdr operands)))
      (constant/make (and expr (object/scode expr))
		     (intern (constant/value (car operands))))
      #f))

(define (int:->flonum-expansion expr operands block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (pcall expr
	     block
	     (ucode-primitive integer->flonum 2)
	     (car operands)
	     (constant/make #f #b10))
      #f))

(define (make-primitive-expander primitive)
  (lambda (expr operands block)
    (if (procedure-arity-valid? primitive (length operands))
	(papply expr block primitive operands)
	#f)))

(define (default-object-expansion expr operands block)
  (declare (ignore block))
  (if (null? operands)
      (constant/make expr (default-object))
      #f))

(define (default-object?-expansion expr operands block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (pcall expr block (ucode-primitive eq?)
	     (car operands)
	     (constant/make #f (default-object)))
      #f))

(define (eof-object-expansion expr operands block)
  (declare (ignore block))
  (if (null? operands)
      (constant/make expr (eof-object))
      #f))

(define (eof-object?-expansion expr operands block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (pcall expr block (ucode-primitive eq?)
	     (car operands)
	     (constant/make #f (eof-object)))
      #f))

;;; RELNOTE: Don't integrate gc-reclaimed-object unless we're running on a
;;; microcode that supports it.  After release, this can be made unconditional.

(define (gc-reclaimed-object-expansion expr operands block)
  (declare (ignore block))
  (let ((object (vector-ref ((ucode-primitive get-fixed-objects-vector)) #x18)))
    (if (and object (null? operands))
	(constant/make expr object)
	#f)))

(define (gc-reclaimed-object?-expansion expr operands block)
  (let ((object (vector-ref ((ucode-primitive get-fixed-objects-vector)) #x18)))
    (if (and object
	     (pair? operands)
	     (null? (cdr operands)))
	(pcall expr block (ucode-primitive eq?)
	       (car operands)
	       (constant/make #f object))
	#f)))

;;;; Tables

(define usual-integrations/expansion-alist
  (cons* (cons '* *-expansion)
	 (cons '+ +-expansion)
	 (cons '- --expansion)
	 (cons '-1+ -1+-expansion)
	 (cons '/ /-expansion)
	 (cons '|1+| |1+-expansion|)
	 (cons '< <-expansion)
	 (cons '= =-expansion)
	 (cons '> >-expansion)
	 (cons 'apply apply*-expansion)
	 (cons 'arithmetic-shift arithmetic-shift-expansion)
	 (cons 'bitwise-and bitwise-and-expansion)
	 (cons 'bitwise-andc1 bitwise-andc1-expansion)
	 (cons 'bitwise-eqv bitwise-eqv-expansion)
	 (cons 'bitwise-ior bitwise-ior-expansion)
	 (cons 'bitwise-xor bitwise-xor-expansion)
	 (cons 'caaaar caaaar-expansion)
	 (cons 'caaadr caaadr-expansion)
	 (cons 'caaar caaar-expansion)
	 (cons 'caadar caadar-expansion)
	 (cons 'caaddr caaddr-expansion)
	 (cons 'caadr caadr-expansion)
	 (cons 'caar caar-expansion)
	 (cons 'cadaar cadaar-expansion)
	 (cons 'cadadr cadadr-expansion)
	 (cons 'cadar cadar-expansion)
	 (cons 'caddar caddar-expansion)
	 (cons 'cadddr cadddr-expansion)
	 (cons 'caddr caddr-expansion)
	 (cons 'cadr cadr-expansion)
	 (cons 'car car-expansion)
	 (cons 'cdaaar cdaaar-expansion)
	 (cons 'cdaadr cdaadr-expansion)
	 (cons 'cdaar cdaar-expansion)
	 (cons 'cdadar cdadar-expansion)
	 (cons 'cdaddr cdaddr-expansion)
	 (cons 'cdadr cdadr-expansion)
	 (cons 'cdar cdar-expansion)
	 (cons 'cddaar cddaar-expansion)
	 (cons 'cddadr cddadr-expansion)
	 (cons 'cddar cddar-expansion)
	 (cons 'cdddar cdddar-expansion)
	 (cons 'cddddr cddddr-expansion)
	 (cons 'cdddr cdddr-expansion)
	 (cons 'cddr cddr-expansion)
	 (cons 'cdr cdr-expansion)
	 (cons 'char=? char=?-expansion)
	 (cons 'complex? complex?-expansion)
	 (cons 'cons* cons*-expansion)
	 (cons 'default-object default-object-expansion)
	 (cons 'default-object? default-object?-expansion)
	 (cons 'eighth eighth-expansion)
	 (cons 'eof-object eof-object-expansion)
	 (cons 'eof-object? eof-object?-expansion)
	 (cons 'exact-integer? exact-integer?-expansion)
	 (cons 'exact-rational? exact-rational?-expansion)
	 (cons 'expt expt-expansion)
	 (cons 'eq? eq?-expansion)
	 (cons 'fifth fifth-expansion)
	 (cons 'first first-expansion)
	 (cons 'fix:<= fx<=?-expansion)
	 (cons 'fix:= fx=?-expansion)
	 (cons 'fix:>= fx>=?-expansion)
	 (cons 'flo:*- flonum-fmsub-expansion)
	 (cons 'fourth fourth-expansion)
	 (cons 'fx<? fx<?-expansion)
	 (cons 'fx<=? fx<=?-expansion)
	 (cons 'fx=? fx=?-expansion)
	 (cons 'fx>? fx>?-expansion)
	 (cons 'fx>=? fx>=?-expansion)
	 (cons 'fxarithmetic-shift-right fxarithmetic-shift-right-expansion)
	 (cons 'fxneg fxneg-expansion)
	 (cons 'gc-reclaimed-object gc-reclaimed-object-expansion)
	 (cons 'gc-reclaimed-object? gc-reclaimed-object?-expansion)
	 (cons 'guarantee guarantee-expansion)
	 (cons 'int:->flonum int:->flonum-expansion)
	 (cons 'int:integer? exact-integer?-expansion)
	 (cons 'intern intern-expansion)
	 (cons 'interned-symbol? interned-symbol?-expansion)
	 (cons 'list list-expansion)
	 (cons 'make-bytevector make-bytevector-expansion)
	 (cons 'negative? negative?-expansion)
	 (cons 'not not-expansion)
	 (cons 'number? complex?-expansion)
	 (cons 'positive? positive?-expansion)
	 (cons 'quotient quotient-expansion)
	 (cons 'remainder remainder-expansion)
	 (cons 'second second-expansion)
	 (cons 'seventh seventh-expansion)
	 (cons 'sixth sixth-expansion)
	 (cons 'string->symbol string->symbol-expansion)
	 (cons 'symbol? symbol?-expansion)
	 (cons 'third third-expansion)
	 (cons 'uninterned-symbol? uninterned-symbol?-expansion)
	 (cons 'weak-pair? weak-pair?-expansion)
	 (cons 'zero? zero?-expansion)
	 (map (lambda (p)
		(cons (car p)
		      (make-primitive-expander
		       (apply make-primitive-procedure (cdr p)))))
	      global-primitives)))

(define usual-integrations/expansion-names
  (map car usual-integrations/expansion-alist))

(define usual-integrations/expansion-values
  (map cdr usual-integrations/expansion-alist))

;;;;  Hooks and utilities for user defined reductions and expanders

;;; User defined reductions appear in reduct.scm

;;; Scode->Scode expanders

(define (scode->scode-expander scode-expander)
  (lambda (expr operands block)
    (scode-expander
     (map cgen/external-with-declarations operands)
     (lambda (scode-expression)
       (reassign
	expr
	(transform/recursive
	 block
	 (integrate/get-top-level-block)
	 scode-expression)))
     false-procedure)))

;;; Kludge for EXPAND-OPERATOR declaration.
(define expander-evaluation-environment
  (->environment '(scode-optimizer expansion)))