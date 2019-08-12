#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

(define (make-combination expression block primitive operands)
  (combination/make expression
		    block
		    (constant/make #f primitive)
		    operands))

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
				    (reference/make #f block variable)))))
		    (list operand)))

(define (constant-eq? expression constant)
  (and (constant? expression)
       (eq? (constant/value expression) constant)))

(define (unary-arithmetic primitive)
  (lambda (expr operands block)
    (if (and (pair? operands)
	     (null? (cdr operands)))
	(make-combination expr block primitive operands)
	#f)))

(define (binary-arithmetic primitive)
  (lambda (expr operands block)
    (if (and (pair? operands)
	     (pair? (cdr operands))
	     (null? (cddr operands)))
	(make-combination expr block primitive operands)
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
	       (make-combination expr block if-left-zero
				 (list (cadr operands))))
	      ((constant-eq? (cadr operands) 0)
	       (make-combination expr block if-right-zero
				 (list (car operands))))
	      (else
	       (make-combination expr block binary-predicate operands)))
	#f)))

(define (pairwise-test-inverse inverse-expansion)
  (lambda (expr operands block)
    (let ((inverse (inverse-expansion expr operands block)))
      (if inverse
	  (make-combination expr block (ucode-primitive not)
			    (list inverse))
	  #f))))

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

(define <=-expansion (pairwise-test-inverse >-expansion))
(define >=-expansion (pairwise-test-inverse <-expansion))

;;;; Fixnum Operations

(define (fix:zero?-expansion expr operands block)
  (if (and (pair? operands) (null? (cdr operands)))
      (make-combination expr block (ucode-primitive eq?)
			(list (car operands) (constant/make #f 0)))
      #f))

(define (fix:=-expansion expr operands block)
  (if (and (pair? operands)
	   (pair? (cdr operands))
	   (null? (cddr operands)))
      (make-combination expr block (ucode-primitive eq?) operands)
      #f))

(define char=?-expansion
  fix:=-expansion)

(define (fix:<=-expansion expr operands block)
  (if (and (pair? operands)
	   (pair? (cdr operands))
	   (null? (cddr operands)))
      (make-combination
       expr
       block
       (ucode-primitive not)
       (list (make-combination #f
			       block
			       (ucode-primitive greater-than-fixnum?)
			       operands)))
      #f))

(define (fix:>=-expansion expr operands block)
  (if (and (pair? operands)
	   (pair? (cdr operands))
	   (null? (cddr operands)))
      (make-combination
       expr
       block
       (ucode-primitive not)
       (list (make-combination #f
			       block
			       (ucode-primitive less-than-fixnum?)
			       operands)))
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
	     (make-combination expr block (ucode-primitive 1+) (list y)))
	    ((constant-eq? y 1)
	     (make-combination expr block (ucode-primitive 1+) (list x)))
	    (else
	     (make-combination expr block (ucode-primitive &+) (list x y)))))))

(define *-expansion
  (right-accumulation 1
    (lambda (expr block x y)
      (make-combination expr block (ucode-primitive &*) (list x y)))))

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
	      (make-combination #f
				block
				(ucode-primitive &*)
				(list operand operand)))))
	  ((constant-eq? (cadr operands) 3)
	   (make-binder
	    (lambda (block operand)
	      (make-combination
	       #f
	       block
	       (ucode-primitive &*)
	       (list operand
		     (make-combination #f
				       block
				       (ucode-primitive &*)
				       (list operand operand)))))))
	  ((constant-eq? (cadr operands) 4)
	   (make-binder
	    (lambda (block operand)
	      (make-combination
	       #f
	       block
	       (ucode-primitive &*)
	       (list (make-combination #f
				       block
				       (ucode-primitive &*)
				       (list operand operand))
		     (make-combination #f
				       block
				       (ucode-primitive &*)
				       (list operand operand)))))))
	  (else #f))))

(define (right-accumulation-inverse identity inverse-expansion make-binary)
  (lambda (expr operands block)
    (let ((expand
	   (lambda (expr x y)
	      (if (constant-eq? y identity)
		  x
		  (make-binary expr block x y)))))
      (cond ((null? operands) #f)
	    ((null? (cdr operands))
	     (expand expr (constant/make #f identity) (car operands)))
	    (else
	     (let ((inverse (inverse-expansion #f (cdr operands) block)))
	       (if inverse
		   (expand expr (car operands) inverse)
		   #f)))))))

(define --expansion
  (right-accumulation-inverse 0 +-expansion
    (lambda (expr block x y)
      (if (constant-eq? y 1)
	  (make-combination expr block (ucode-primitive -1+) (list x))
	  (make-combination expr block (ucode-primitive &-) (list x y))))))

(define /-expansion
  (right-accumulation-inverse 1 *-expansion
    (lambda (expr block x y)
      (make-combination expr block (ucode-primitive &/) (list x y)))))

;;;; N-ary List Operations

(define (apply*-expansion expr operands block)
  (cond ((length=? operands 2)
	 (make-combination expr block (ucode-primitive apply) operands))
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
      (make-combination expr
			block
			(ucode-primitive cons)
			(list (car rest)
			      (cons*-expansion-loop #f block (cdr rest))))))

(define (list-expansion expr operands block)
  (list-expansion-loop expr block operands))

(define (list-expansion-loop expr block rest)
  (cond ((pair? rest) (make-combination expr block (ucode-primitive cons)
			(list (car rest)
			      (list-expansion-loop #f block (cdr rest)))))
	((null? rest) (constant/make (and expr (object/scode expr)) '()))
	(else (error "Improper list."))))

;;;; General CAR/CDR Encodings

(define (call-to-car? expression)
  (and (scode-combination? expression)
       (constant-eq? (combination/operator expression) (ucode-primitive car))
       (length=? (combination/operands expression) 1)))

(define (call-to-cdr? expression)
  (and (scode-combination? expression)
       (constant-eq? (combination/operator expression) (ucode-primitive cdr))
       (length=? (combination/operands expression) 1)))

(define (call-to-general-car-cdr? expression)
  (and (scode-combination? expression)
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
	       (make-combination
		expr block
		(ucode-primitive general-car-cdr)
		(list (first (combination/operands operand))
		      (constant/make #f #b111))))
	      ;; (car (cdr x)) => (cadr x)
	      ((call-to-cdr? operand)
	       (make-combination
		expr block
		(ucode-primitive general-car-cdr)
		(list (first (combination/operands operand))
		      (constant/make #f #b110))))

	      ((call-to-general-car-cdr? operand)
	       (make-combination
		expr block
		(ucode-primitive general-car-cdr)
		(list (first (combination/operands operand))
		      (constant/make
		       #f
		       (encode-general-car-cdr
			(cons 'car
			      (decode-general-car-cdr
			       (constant/value
				(second (combination/operands operand))))))))))
	      (else
	       (make-combination expr block (ucode-primitive car) operands))))
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
	       (make-combination
		expr block
		(ucode-primitive general-car-cdr)
		(list (first (combination/operands operand))
		      (constant/make #f #b101))))
	      ;; (cdr (car x)) => (cddr x)
	      ((call-to-cdr? operand)
	       (make-combination
		expr block
		(ucode-primitive general-car-cdr)
		(list (first (combination/operands operand))
		      (constant/make #f #b100))))

	      ((call-to-general-car-cdr? (car operands))
	       (make-combination
		expr block
		(ucode-primitive general-car-cdr)
		(list (first (combination/operands operand))
		      (constant/make
		       #f
		       (encode-general-car-cdr
			(cons 'cdr
			      (decode-general-car-cdr
			       (constant/value
				(second (combination/operands operand))))))))))
	      (else
	       (make-combination expr block (ucode-primitive cdr) operands))))
      ;; ill-formed call
      (begin
	(warn "Wrong number of arguments in call to CDR.")
	#f)))

(define (general-car-cdr-expansion encoding)
  (lambda (expr operands block)
    (if (length=? operands 1)
	(make-combination expr
			  block
			  (ucode-primitive general-car-cdr)
			  (list (car operands)
				(constant/make #f encoding)))
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
		    (make-combination #f block
				      (ucode-primitive not) (cdr operands)))))
	    ((expression/always-false? (second operands))
	     (sequence/make
	      (and expr (object/scode expr))
	      (list (second operands)
		    (make-combination #f block
				      (ucode-primitive not)
				      (list (car operands))))))
	    (else
	     (make-combination expr block (ucode-primitive eq?) operands)))
      #f))

(define (make-string-expansion expr operands block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (make-combination expr block (ucode-primitive string-allocate)
			operands)
      #f))

(define (make-bytevector-expansion expr operands block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (make-combination expr block (ucode-primitive allocate-bytevector 1)
			operands)
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
	    (else (make-combination expr block (ucode-primitive not) operands)))
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

(define (default-object?-expansion expr operands block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (make-combination expr block (ucode-primitive eq?)
			(list (car operands)
			      (constant/make #f #!default)))
      #f))

(define (make-disjunction expr . clauses)
  (let loop ((clauses clauses))
    (if (null? (cdr clauses))
	(car clauses)
	(disjunction/make (and expr (object/scode expr))
			  (car clauses) (loop (cdr clauses))))))

(define (make-type-test expr block type operand)
  (make-combination expr block
		    (ucode-primitive object-type?)
		    (list (constant/make #f type) operand)))

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
      (make-combination expr
			block
			(ucode-primitive integer->flonum 2)
			(list (car operands) (constant/make #f #b10)))
      #f))

(define (make-primitive-expander primitive)
  (lambda (expr operands block)
    (if (procedure-arity-valid? primitive (length operands))
	(make-combination expr block primitive operands)
	#f)))

;;;; Tables

(define usual-integrations/expansion-names
  (append '(*
	    +
	    -
	    -1+
	    /
	    1+
	    <
	    <=
	    =
	    >
	    >=
	    apply
	    caaaar
	    caaadr
	    caaar
	    caadar
	    caaddr
	    caadr
	    caar
	    cadaar
	    cadadr
	    cadar
	    caddar
	    cadddr
	    caddr
	    cadr
	    car
	    cdaaar
	    cdaadr
	    cdaar
	    cdadar
	    cdaddr
	    cdadr
	    cdar
	    cddaar
	    cddadr
	    cddar
	    cdddar
	    cddddr
	    cdddr
	    cddr
	    cdr
	    char=?
	    complex?
	    cons*
	    default-object?
	    eighth
	    exact-integer?
	    exact-rational?
	    expt
	    eq?
	    fifth
	    first
	    fix:<=
	    fix:=
	    fix:>=
	    fourth
	    int:->flonum
	    int:integer?
	    intern
	    list
	    make-bytevector
	    ;; modulo	; Compiler does not currently open-code it.
	    negative?
	    not
	    number?
	    positive?
	    quotient
	    remainder
	    second
	    seventh
	    sixth
	    string->symbol
	    symbol?
	    third
	    weak-pair?
	    zero?)
	  (map car global-primitives)))

(define usual-integrations/expansion-values
  (append (list
	   *-expansion
	   +-expansion
	   --expansion
	   -1+-expansion
	   /-expansion
	   1+-expansion
	   <-expansion
	   <=-expansion
	   =-expansion
	   >-expansion
	   >=-expansion
	   apply*-expansion
	   caaaar-expansion
	   caaadr-expansion
	   caaar-expansion
	   caadar-expansion
	   caaddr-expansion
	   caadr-expansion
	   caar-expansion
	   cadaar-expansion
	   cadadr-expansion
	   cadar-expansion
	   caddar-expansion
	   cadddr-expansion
	   caddr-expansion
	   cadr-expansion
	   car-expansion
	   cdaaar-expansion
	   cdaadr-expansion
	   cdaar-expansion
	   cdadar-expansion
	   cdaddr-expansion
	   cdadr-expansion
	   cdar-expansion
	   cddaar-expansion
	   cddadr-expansion
	   cddar-expansion
	   cdddar-expansion
	   cddddr-expansion
	   cdddr-expansion
	   cddr-expansion
	   cdr-expansion
	   char=?-expansion
	   complex?-expansion
	   cons*-expansion
	   default-object?-expansion
	   eighth-expansion
	   exact-integer?-expansion
	   exact-rational?-expansion
	   expt-expansion
	   eq?-expansion
	   fifth-expansion
	   first-expansion
	   fix:<=-expansion
	   fix:=-expansion
	   fix:>=-expansion
	   fourth-expansion
	   int:->flonum-expansion
	   exact-integer?-expansion
	   intern-expansion
	   list-expansion
	   make-bytevector-expansion
	   ;; modulo-expansion
	   negative?-expansion
	   not-expansion
	   complex?-expansion
	   positive?-expansion
	   quotient-expansion
	   remainder-expansion
	   second-expansion
	   seventh-expansion
	   sixth-expansion
	   string->symbol-expansion
	   symbol?-expansion
	   third-expansion
	   weak-pair?-expansion
	   zero?-expansion)
	  (map (lambda (p)
		 (make-primitive-expander
		  (apply make-primitive-procedure (cdr p))))
	       global-primitives)))

(define usual-integrations/expansion-alist
  (map cons
       usual-integrations/expansion-names
       usual-integrations/expansion-values))

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